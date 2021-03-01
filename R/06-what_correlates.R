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

what_correlates <- function(uids, folder, rval = 0.5) {
        fname <- get_db(folder, "correlations")
        df <- readRDS(fname)
        df <- df %>% filter(uid.x %in% uids) %>%
                filter(abs(r) > rval, abs(r) < 0.99) %>%
                add_meta(folder)
        pos <- which_is_dy_dt(df$uid.y)
        df$relationship <- "direct"
        df$relationship[pos] <-  "tipping"
        pos <- which_is_dy_dt(df$uid.y) & which_is_dy_dt(df$uid.x)
        df$relationship[pos] = "trends together"
        return(df)
}
