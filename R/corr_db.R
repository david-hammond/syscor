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
#' @importFrom tidyr spread 
#' @importFrom rlang .data
#' @importFrom corrr correlate
#' @importFrom corrr stretch
#' @importFrom corrr as_cordf
#' @importFrom stats complete.cases

#' @author David Hammond
#' @export

corr_db <- function(df, use_geocode_and_time_as_obs = T) {
        if(use_geocode_and_time_as_obs){
                df$obs <- paste(df$geocode, df$year) 
                df <- df %>%
                        select(-year, -geocode)
        }else{
                df <- df %>% group_by(uid, geocode) %>%
                        filter(year == max(year)) %>% 
                        select(-year) %>% rename(obs = geocode)
        }
        df <- df %>% spread(uid, value)
        df <- as_cordf(cor(df[,-1], use = 'pairwise.complete.obs'))
        df <- stretch(df) %>% filter(complete.cases(r))
        saveRDS(df, "./data/corr_db.rds", compress = "xz")
        return(df)
}
