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
#' @importFrom parallel parLapply

#' @author David Hammond


granger_execute <- function(gcode, changes, bivariates, corpus, subset_granger) {

                changes = changes %>% filter(geocode == gcode)

        
        bivariates = bivariates %>% add_info(changes) %>%
                add_info(readRDS(systr_file$meta)) %>%
                filter(uid.x != uid.y) %>%
                filter(!is.na(geocode.x) & !is.na(geocode.y))
        
        if(nrow(bivariates) > 0){
                if(!is.null(subset_granger)){
                        pos = as.logical(bivariates[,subset_granger[1]] == subset_granger[2])
                        bivariates = bivariates[pos, ] 
                }

                bivariates = split(bivariates, 1:nrow(bivariates))
                
                corpus = corpus %>% 
                        filter(geocode == gcode)
                
                cl <- makeCluster(detectCores())
                
                raw <- parLapply(bivariates, granger_calc, granger_corpus = corpus, cl = cl)
                
                stopCluster(cl)
                
                results = bind_rows(raw)
                
                return(results)
        }else{
                message(paste("Skipping", gcode, "- No Data"))
        }


}
