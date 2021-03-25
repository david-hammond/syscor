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
#' @importFrom fs dir_create
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom scales rescale
#' @author David Hammond
#' @export

add_info <- function(df, info) {

        
        if("uid" %in% names(df)){
                df <- df %>% left_join(info)
        }
        if("uid.x" %in% names(df)){
                tmp <- info
                names(tmp) <- paste0(names(tmp), ".x")
                df <- df %>% left_join(tmp)
        }
        if("uid.y" %in% names(df)){
                tmp <- info
                names(tmp) <- paste0(names(tmp), ".y")
                df <- df %>% left_join(tmp)
        }
        return(df)
}
