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


granger_execute <- function(gcode, systr_file) {

        changes = readRDS(systr_file$changes) %>% filter(geocode == gcode)

        
        bivariates = readRDS(systr_file$correlations) %>% add_info(changes) %>%
                filter(complete.cases(.), uid.x != uid.y) 

        bivariates = split(bivariates, 1:nrow(bivariates))
        
        corpus = readRDS(systr_file$scaled) %>% 
                filter(geocode == gcode)

        raw <- lapply(bivariates, systr_granger, granger_corpus = corpus)

        results = bind_rows(raw)
        if(nrow(results) > 0){
               return(results)
        }
}
