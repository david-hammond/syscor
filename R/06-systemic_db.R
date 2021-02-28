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
#' @importFrom dplyr summarise
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr arrange
#' @importFrom tidyr spread 
#' @importFrom rlang .data
#' @importFrom corrr correlate
#' @importFrom corrr stretch
#' @importFrom corrr as_cordf
#' @importFrom stats complete.cases
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph degree

#' @author David Hammond


systemic_db <- function(folder, rval = 0.5) {
        fname <- get_db(folder, "correlations")
        df <- readRDS(fname) 
        df <- df %>% filter(complete.cases(r)) 
        g_total <- igraph::graph_from_data_frame(df[, c("uid.x", "uid.y")])
        df <- df %>% filter(abs(r) > rval)
        g_signif <- igraph::graph_from_data_frame(df[, c("uid.x", "uid.y")])
        centrality_total <- data.frame(uid = names(degree(g_total)), total =degree(g_total))
        centrality_signif <- data.frame(uid = names(degree(g_signif)), signif =degree(g_signif))
        centrality <- centrality_signif %>% left_join(centrality_total) %>%
                mutate(centrality = signif/total) %>% 
                arrange(desc(centrality))
        fname <- get_db(folder, "systemic_centrality")
        saveRDS(centrality, fname, compress = "xz")
}
