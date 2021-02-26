#' Creates a Correlation Matrix
#'
#' This takes a raw data file and creates a systems correlation database
#'
#' @param cluster1 dataframe in format
#' @param cluster2 dataframe in format
#' @param signif dataframe in format
#' @param stock fff
#' @param changes ddd
#'
#' @return Returns a correlation matrix
#'
#' @examples
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @author David Hammond
#' @export

whats_the_difference_between <- function(cluster1, cluster2, 
                                                signif = 0.05,
                                         stock = NULL,
                                         changes = NULL) {
        if(is.null(stock)){
                stock = readRDS("./data/rawdata_db.rds")
        }
        if(is.null(changes)){
                changes = readRDS("./data/pc_db.rds")
        }
        df <- whats_the_difference_between_stocks(stock, cluster1, cluster2, signif)
        df <- rbind(df, whats_the_difference_between_changes(changes, cluster1, cluster2,  signif))
        return(df)
}
