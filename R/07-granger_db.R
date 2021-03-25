#' Creates a Correlation Matrix
#'
#' This takes a raw data file and creates a systems correlation database
#'
#' @param df dataframe in format
#' @param use_geocode_and_time_as_obs dataframe in format
#'
#' @return Returns a correlation matrix
#'
#' @examples
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr pull
#' @importFrom pbapply pblapply
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom parallel clusterExport

#' @author David Hammond


granger_db <- function() {
        cl <- makeCluster(detectCores())
        gcodes = readRDS(systr_file$changes) %>% pull(geocode) %>% unique()
        clusterExport(cl, list("systr_file"),
                      envir=environment())
        tmp <- pblapply(gcodes, granger_execute, cl = cl)
        stopCluster(cl)
        tmp <- bind_rows(tmp)
        saveRDS(tmp, file = systr_file$granger)
}
