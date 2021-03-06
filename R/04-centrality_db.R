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
#' @importFrom igraph betweenness
#' @importFrom igraph E<-
#' @importFrom igraph E

#' @author David Hammond


centrality_db <- function(rval = 0.5) {
        df <- readRDS(systr_file$correlations) 
        df <- df %>% filter(complete.cases(r)) 
        g_total <- igraph::graph_from_data_frame(df[, c("uid.x", "uid.y")])
        df <- df %>% filter(abs(r) > rval)
        df <- df %>% filter(abs(r) < 0.99)
        g_signif <- igraph::graph_from_data_frame(df[, c("uid.x", "uid.y")])
        E(g_signif)$weights = 1-abs(df$r)
        centrality_total <- data.frame(uid = names(degree(g_total)), total =degree(g_total))
        centrality_signif <- data.frame(uid = names(degree(g_signif)), signif =degree(g_signif))
        centrality_betweenness <- data.frame(uid = names(degree(g_signif)), 
                                             betweenness =betweenness(g_signif, weights = E(g_signif)$weights))
        centrality <- centrality_signif %>% left_join(centrality_total) %>%
                mutate(centrality = signif/total) %>% 
                arrange(desc(centrality)) %>% left_join(centrality_betweenness)
        saveRDS(centrality, systr_file$centrality, compress = "xz")
}
