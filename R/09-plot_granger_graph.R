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
#' @importFrom igraph E<-
#' @importFrom igraph E
#' @importFrom igraph V<-
#' @importFrom igraph V

#' @author David Hammond
#' @export


systr_granger_graph = function(filename, pval = 0.05, filter_for_same_direction = T){
        library(igraph)
        x = readRDS(filename) %>% filter(f_test < pval)
        if(filter_for_same_direction){
         x = x %>% filter(sign(absolute.x) == sign(absolute.y))
        }
        cols = data.frame(v = c(x$variablename.x, x$variablename.y), col = c(x$absolute.x, x$absolute.y))
        good_colour = "#00BFC480"
        bad_colour = "#F8766D80"
        cols$col = ifelse(cols$col < 0, good_colour, bad_colour)
        g = igraph::graph_from_data_frame(x[,c("variablename.x", "variablename.y")], directed = T)
        coords <- igraph::layout_with_kk(g)
        key = match(names(V(g)), cols$v)
        V(g)$color = cols$col[key]
        V(g)$size = 10+igraph::degree(g)
        l <- norm_coords(coords, ymin=-1, ymax=1, xmin=-1, xmax=1)
        plot(g, edge.arrow.size=0.5, layout = l*1,
             vertex.label.cex = 0.9, edge.curved=0.15, rescale=F, 
             vertex.label.family = "Helvetica", main.label.family = "Helvetica",
             vertex.frame.width = 2, main = filename)
        legend("topright",legend=c("Value Decreased", "Value Increased"),col='black',pch=21, pt.cex = 1, 
               pt.bg=c(good_colour, bad_colour), bg= NA)
        return(g)
}