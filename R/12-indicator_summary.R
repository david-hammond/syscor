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
#' @importFrom dplyr n
#' @importFrom dplyr top_n
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


systr_indicator_summary = function(indicator, rval = 0.5, pval = 0.1, number_to_return = 10){
        corrs = get_corrs() %>% add_info(get_meta()) %>% 
                filter(variablename.x == indicator) %>%
                top_n(number_to_return, abs(r)) %>% select(variablename.x, variablename.y, r) %>%
                arrange(desc(abs(r))) %>% mutate(r = round(r, 2)) %>%
                filter(variablename.x != variablename.y)
        files = list.files()[grepl("granger", list.files())]
        grg = readRDS(files[1]) %>% filter(variablename.x == indicator | variablename.y == indicator) %>%
                filter(f_test < pval)
        for (i in files[-1]){
                tmp = readRDS(i)
                grg = rbind(grg, tmp) %>% filter(variablename.x == indicator | variablename.y == indicator) %>%
                        filter(f_test < pval)
        }
        grg = grg %>% mutate(direction = ifelse(dydt.x == dydt.y, "Same", "Inverse")) %>%
                                     group_by(variablename.x, variablename.y, direction) %>%
                summarise(n = n()) %>% top_n(number_to_return, n) %>% as.data.frame()  %>% 
                ungroup() %>%
                filter(variablename.x == indicator | variablename.y == indicator) %>%
                arrange(desc(n))
        x = list(corrs = as.data.frame(corrs), granger = as.data.frame(grg))
        
        return(x)
        
}
