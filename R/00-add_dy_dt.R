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


add_dy_dt <- function(uids) {
        
        return(paste0(uids, "/dt"))
        
}

which_is_dy_dt <- function(uids) {
        pos = grepl("/dt", uids)
        return(pos)
}