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


granger_db <- function(test = F, subset_granger) {
        
        changes = readRDS(systr_file$changes) 
        
        bivariates = readRDS(systr_file$correlations) 
        
        corpus = readRDS(systr_file$scaled) 
        
        

        gcodes = readRDS(systr_file$changes) %>% pull(geocode) %>% unique()
        if(test){
               gcodes = gcodes[1:2] 
        }
        tmp <- pblapply(gcodes, granger_execute, 
                        changes = changes,
                        bivariates = bivariates,
                        corpus = corpus,
                        subset = subset_granger)
        tmp <- bind_rows(tmp)
        saveRDS(tmp, file = systr_file$granger)
}
