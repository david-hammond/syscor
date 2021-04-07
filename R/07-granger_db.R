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
#' @importFrom dplyr pull
#' @importFrom pbapply pblapply
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom parallel clusterExport
#' @importFrom parallel detectCores

#' @author David Hammond
#' @export

systr_granger <- function(corpus) {
        
        changes = readRDS(systr_file$changes) %>%
                filter(uid %in% corpus$uid, geocode %in% corpus$geocode)
        
        bivariates = expand.grid(uid.x = corpus$uid, 
                                 uid.y = corpus$uid, 
                                 stringsAsFactors = F) %>% 
                as.data.frame() %>%
                filter(uid.x != uid.y)
        
        gcodes = subset_granger %>% pull(geocode) %>% unique()
        tmp <- pblapply(gcodes, granger_execute, 
                        changes = changes,
                        bivariates = bivariates,
                        corpus = corpus)
        tmp <- bind_rows(tmp)
        return(tmp)
}
