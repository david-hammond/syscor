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
#' @importFrom igraph E<-
#' @importFrom igraph E
#' @importFrom igraph V<-
#' @importFrom igraph V

#' @author David Hammond
#' @export


systr_country_indicator_summary = function(geocode, indicator, pval = 0.1){
        x = readRDS(paste0("granger_", geocode, ".rds")) %>% 
                filter(variablename.x == indicator | variablename.y == indicator) %>%
                filter(f_test < pval) %>%
                select(variablename.x, variablename.y) %>%
                rename(leading = variablename.x, lagging = variablename.y)
        return(x)
        
}