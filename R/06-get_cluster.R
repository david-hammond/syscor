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
#' @importFrom tidyr spread 
#' @importFrom rlang .data
#' @importFrom corrr correlate
#' @importFrom corrr stretch
#' @importFrom corrr as_cordf
#' @importFrom stats hclust
#' @importFrom stats as.dendrogram
#' @importFrom dendextend cutree
#' @importFrom dendextend color_branches 
#' @importFrom dendextend ladderize	
#' @importFrom gplots heatmap.2
#' @export
#' @author David Hammond


get_cluster = function(x, which_cluster, folder){
        x <- x %>% filter(cluster == levels(x$cluster)[which_cluster])
        fname <- get_db(folder, "correlations")
        df <- readRDS(fname)
        df <- df %>% 
                filter(!which_is_dy_dt(uid.x)) %>%
                filter(!which_is_dy_dt(uid.y)) %>%
                filter(uid.x %in% x$uid | uid.y %in% x$uid) %>%
                add_meta(folder)
        return(df)
}