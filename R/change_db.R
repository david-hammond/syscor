#' Creates a Correlation Matrix
#'
#' This takes a raw data file and creates a systems correlation database
#'
#' @param df dataframe in format
#' @param scale dataframe in format
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
#' @importFrom scale rescale

#' @author David Hammond
#' @export

change_db <- function(df, time_col, group_by_col, value_col, newscale = NULL) {
        if(!is.null(scale)){
                df <- df %>% group_by(uid) %>% 
                        mutate(value = rescale(value, to = newscale))
        }
        df <- df %>% group_by(uid, geocode) %>%
                summarise(earliest_yr = min(year), 
                          latest_yr = max(year), 
                          earliest_value = value[year == min(year)],
                          latest_value = value[year == max(year)]) %>%
                mutate(absolute = latest_value-earliest_value,
                       percentage = absolute/earliest_value) %>%
                ungroup()
        saveRDS(df, "./data/pc_db.rds", compress = "xz")
        return(df)
}
