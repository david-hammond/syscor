#' Creates a Correlation Matrix
#'
#' This takes a raw data file and creates a systems correlation database
#'
#' @param df dataframe in format
#' @param use_geocode_and_time_as_obs dataframe in format
#'
#' @return Returns a correlation matrix
#'
#' @examples
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr summarise
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr arrange
#' @importFrom tidyr spread 
#' @importFrom rlang .data
#' @importFrom corrr correlate
#' @importFrom corrr stretch
#' @importFrom corrr as_cordf
#' @importFrom stats complete.cases
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph degree
#' @importFrom igraph betweenness
#' @importFrom igraph E<-
#' @importFrom igraph E

#' @author David Hammond

granger_calc = function(x, granger_corpus){
        library(vars)
        library(tidyverse)
        a = granger_corpus %>% filter(uid %in% x$uid.x)
        b = granger_corpus %>% filter(uid %in% x$uid.y)
        if(nrow(a) > 0 & nrow(b) > 0){
                #granger + interpolation
                tmp = rbind(a, b)
                
                tmp = tmp %>% mutate(variablename = factor(uid, c(x$uid.x, x$uid.y), ordered = T)) %>%
                        group_by(variablename, year) %>%
                        dplyr::summarise(value = mean(value, na.rm = T), .groups = 'drop') %>% ungroup() %>%
                        mutate(year = as.POSIXct(as.Date(paste0(year, "-01-01")))) %>%
                        group_by(variablename) %>% padr::pad(interval = "month") %>%
                        mutate(value = imputeTS::na_interpolation(value)) %>%
                        ungroup() %>%
                        spread(variablename, value) %>% filter(complete.cases(.)) 
                
                granger = tmp
                names(granger)[2:3] = c("uid.x", "uid.y")

                max_lag = 30
                if(round(sd(granger$uid.x),1) > 0 & round(sd(granger$uid.y),1) > 0){
                        lags = as.numeric(vars::VARselect(granger[,2:3], lag.max = max_lag)$selection[1])
                        lags = ifelse(is.na(lags), 2, lags)
                        gdata = VAR(granger[,2:3], p = lags)
                        gtest = try(causality(gdata, cause = "uid.x"),  silent=TRUE)
                        if(class(gtest) != "try-error"){
                                x$granger_v1_v2 = as.numeric(gtest$Granger$p.value)
                        }
                        
                        lags = as.numeric(vars::VARselect(granger[,3:2], lag.max = max_lag)$selection[1])
                        lags = ifelse(is.na(lags), 2, lags)
                        gdata = try(VAR(granger[,3:2], p = lags))
                        gtest = try(causality(gdata, cause = "uid.y"),  silent=TRUE)
                        if(class(gtest) != "try-error"){
                                x$granger_v2_v1 = as.numeric(gtest$Granger$p.value)
                        }
                        return(x)
                }
        }
        
}


