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
#' @importFrom tidyr spread 
#' @importFrom rlang .data
#' @importFrom corrr correlate
#' @importFrom corrr stretch
#' @importFrom corrr as_cordf
#' @importFrom stats complete.cases

#' @author David Hammond


corr_db <- function(folder, use_geocode_and_time_as_obs = T) {
        df <- readRDS(systr_file$scaled) %>% 
                mutate(value = rescaled) %>%
                select(-rescaled)

        
        if(use_geocode_and_time_as_obs){
                df$obs <- paste(df$geocode, df$year) 
                df <- df %>%
                        select(-year, -geocode)
        }else{
                df <- df %>% group_by(uid, geocode) %>%
                        filter(year == max(year)) %>% 
                        select(-year) %>% rename(obs = geocode)
        }
        test = df %>% group_by(uid) %>%
                summarise(sd = var(value, na.rm = T)) %>%
                filter(sd != 0)
        if(nrow(test) > 0){
                message("Note, removed vars with zero variance")
                df <- df %>% filter(uid %in% test$uid)
        }

        df <- df %>% spread(uid, value)
        df <- correlate(df[,-1], use = 'pairwise.complete.obs')
        #df <- rearrange(df, "HC")
        df <- stretch(df) %>%
                rename(uid.x = x, uid.y = y)
        pos <- df$uid.x == df$uid.y
        df$r[pos] <- 1
        saveRDS(df, systr_file$correlations, compress = "xz")
}
