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
#' @importFrom dplyr relocate
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 aes
#' @importFrom tibble tibble
#' @importFrom tidyr spread 
#' @importFrom tidyr gather 
#' @importFrom rlang .data
#' @importFrom vars VARselect
#' @importFrom vars VAR
#' @importFrom vars causality
#' @importFrom padr pad
#' @importFrom imputeTS na_interpolation
#' @importFrom lubridate year


#' @author David Hammond

granger_calc = function(x, corpus){
        a = corpus %>% filter(uid %in% x$uid.x) 
        b = corpus %>% filter(uid %in% x$uid.y) 
        if(nrow(a) > 0 & nrow(b) > 0){
                #granger + interpolation
                tmp = rbind(a, b)
                
                tmp = tmp %>% mutate(variablename = factor(uid, c(x$uid.x, x$uid.y), ordered = T)) %>%
                        group_by(variablename, year) %>%
                        summarise(value = mean(value, na.rm = T), .groups = 'drop') %>% ungroup() %>%
                        mutate(year = as.POSIXct(as.Date(paste0(year, "-01-01")))) %>%
                        group_by(variablename) %>% pad(interval = "month") %>%
                        mutate(value = na_interpolation(value)) %>%
                        ungroup() %>%
                        spread(variablename, value) %>% filter(complete.cases(.)) 
                
                granger = tmp
                names(granger)[2:3] = c("uid.x", "uid.y")

                max_lag = 30
                if(round(sd(granger$uid.x),1) > 0 & round(sd(granger$uid.y),1) > 0){
                        granger = ts(granger[,2:3], start = c(year(min(granger$year)), 1), frequency = 12)
                        lags = VARselect(granger, lag.max = max_lag)
                        lags = ceiling(as.numeric(mean(lags$selection, na.rm = T)))
                        lags = ifelse(is.na(lags), 2, lags)
                        gdata = VAR(granger, p = lags)
                        gtest = try(causality(gdata, cause = "uid.x"),  silent=TRUE)
                        if(class(gtest) != "try-error"){
                                x$f_test = as.numeric(gtest$Granger$p.value)
                                x$chi_test = as.numeric(gtest$Instant$p.value)
                                x$lag = lags
                                
                                x$geocode = unique(corpus$geocode)
                                x = x %>% relocate(geocode) %>% add_info(get_meta()) 
                                tmp = get_changes() %>% filter(geocode %in% unique(corpus$geocode)) %>% 
                                        relocate(geocode)
                                names(tmp)[-1] = paste0(names(tmp[-1]), ".x")
                                x = x %>% left_join(tmp)
                                names(tmp)[-1] = gsub(".x", ".y", names(tmp[-1]))
                                x = x %>% left_join(tmp)
                                x = tibble(x, ts = list(granger))
                        }
        
                        return(x)
                }
        }
        
}


