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
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom tidyr spread 
#' @importFrom rlang .data
#' @author David Hammond


changes_db <- function(df) {
        pc <- df %>% group_by(uid, geocode) %>%
                summarise(earliest_yr = min(year), 
                          latest_yr = max(year), 
                          earliest_value = rescaled[year == min(year)],
                          latest_value = rescaled[year == max(year)],
                          higher_ratio = 1-mean(rescaled[year != min(year)])/rescaled[year == min(year)]) %>%
                mutate(absolute = latest_value-earliest_value,
                       percentage = absolute/earliest_value,
                       dydt = absolute/(latest_yr-earliest_yr)) %>%
                ungroup() 
        saveRDS(pc, systr_file$changes, compress = "xz")
}
