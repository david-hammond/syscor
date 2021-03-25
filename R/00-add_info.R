#' Creates a Correlation Matrix
#'
#' This takes a raw data file and creates a systems correlation database
#'
#' @param df dataframe in format
#' @param folder dataframe in format
#'
#' @return Returns a correlation matrix
#'
#' @examples
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @author David Hammond
#' @export

add_info <- function(df, info) {

        
        if("uid" %in% names(df)){
                df <- df %>% left_join(info, by = "uid")
        }
        if("uid.x" %in% names(df)){
                tmp <- info
                names(tmp) <- paste0(names(tmp), ".x")
                df <- df %>% left_join(tmp, by = "uid.x")
        }
        if("uid.y" %in% names(df)){
                tmp <- info
                names(tmp) <- paste0(names(tmp), ".y")
                df <- df %>% left_join(tmp, by = "uid.y")
        }
        return(df)
}
