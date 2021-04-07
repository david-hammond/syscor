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
        
        gcodes = subset_granger %>% pull(geocode) %>% unique()
        tmp <- pblapply(gcodes, granger_execute, 
                        corpus = corpus)
        tmp <- bind_rows(tmp)
        return(tmp)
}
