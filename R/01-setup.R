#' Creates a Correlation Matrix
#'
#' This takes a raw data file and creates a systems correlation database
#'
#' @param df dataframe in format
#' @param newscale dataframe in format
#'
#' @return Returns a correlation matrix
#'
#' @examples
#'
#' @importFrom fs dir_create
#' @author David Hammond
#' @export

syssetup <- function(df, newscale = c(1,10)) {
        dir_create("data")

        corr_db(df)
        
        df <- df %>% group_by(uid) %>% 
                mutate(rescaled = rescale(value, to = newscale))
        
        saveRDS(df, "./data/rawdata_db.rds", compress = "xz")
        
        changes_db(df)
}
