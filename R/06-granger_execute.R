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


granger_execute <- function(gcode, corpus) {
        
        message(paste("Calculating granger for", gcode))
        
        changes = readRDS(systr_file$changes) %>%
                filter(uid %in% corpus$uid, geocode %in% corpus$geocode)

        corpus = corpus %>% 
                filter(geocode == gcode)
        
        bivariates = expand.grid(uid.x = unique(corpus$uid), 
                                 uid.y = unique(corpus$uid), 
                                 stringsAsFactors = F) %>% 
                as.data.frame() %>%
                filter(uid.x != uid.y)


        if(nrow(bivariates) > 0){

                bivariates = split(bivariates, 1:nrow(bivariates))
                
                
                
                cl <- makeCluster(detectCores())
                
                raw <- pblapply(bivariates, granger_calc, corpus = corpus, cl = cl)
                
                stopCluster(cl)
                
                results = bind_rows(raw)
                
                return(results)
        }else{
                message(paste("Skipping", gcode, "- No Data"))
        }


}
