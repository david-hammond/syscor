#' Creates a Correlation Matrix
#'
#' This takes a raw data file and creates a systems correlation database
#'
#' @param df dataframe in format
#' @param db dataframe in format
#'
#' @return Returns a correlation matrix
#'
#' @examples
#'
#' @author David Hammond


get_db <- function(folder, db) {
        
        return(switch(db,
               "meta" = file.path(folder, "00-meta_db.rds"),
               "rawdata" = file.path(folder, "01-rawdata_db.rds"),
               "pc_changes" = file.path(folder, "02-changes_db.rds"),
               "gradients" = file.path(folder, "03-gradients_db.rds"),
               "scaled" = file.path(folder, "04-scaled_db.rds"),
               "trend_together" = file.path(folder, "05-trend_together_db.rds"),
               "correlations" = file.path(folder, "06-corr_db.rds"),
               "systemic_centrality" = file.path(folder, "07-systemic_centrality.rds")))
        
}
