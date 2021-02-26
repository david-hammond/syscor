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
#' @importFrom dplyr %>%
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom tidyr spread 
#' @importFrom rlang .data
#' @importFrom corrr correlate
#' @importFrom corrr stretch
#' @importFrom stats complete.cases
#' @importFrom scales rescale

#' @author David Hammond
#' @export

changes_db <- function(df) {
        df <- df %>% group_by(uid, geocode) %>%
                summarise(earliest_yr = min(year), 
                          latest_yr = max(year), 
                          earliest_value = rescaled[year == min(year)],
                          latest_value = rescaled[year == max(year)]) %>%
                mutate(absolute = latest_value-earliest_value,
                       percentage = absolute/earliest_value) %>%
                ungroup() 
        
        saveRDS(df, "./data/changes_db.rds", compress = "xz")
}
