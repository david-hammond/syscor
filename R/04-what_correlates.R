#' Creates a Correlation Matrix
#'
#' This takes a raw data file and creates a systems correlation database
#'
#' @param uids dataframe in format
#' @param rval dataframe in format
#'
#' @return Returns a correlation matrix
#'
#' @examples
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @author David Hammond
#' @export

what_correlates <- function(uids, rval = 0.5) {
        df <- readRDS("./data/corr_db.rds")
        df <- df %>% filter(x %in% uids | y %in% uids)
        return(df)
}
