#' Creates a Correlation Matrix
#'
#' This takes a raw data file and creates a systems correlation database
#' 
#' @param df dataframe in format
#' @param cluster1 dataframe in format
#' @param cluster2 dataframe in format
#' @param signif dataframe in format
#'
#' @return Returns a correlation matrix
#'
#' @examples
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @author David Hammond
#' @export

whats_the_difference_between <- function(cluster1, cluster2, signif = 0.05) {
        df <- readRDS(systr_file$scaled)
        cluster1 <- df %>% filter(geocode %in% cluster1) %>%
                mutate(cluster = 1) 
        cluster2 <- df %>% filter(geocode %in% cluster2) %>%
                mutate(cluster = 2)
        clusters = rbind(cluster1, cluster2) %>%
                select(uid, cluster, year, rescaled) %>%
                filter(year == min(year))
        clusters = split(clusters, clusters$uid)
        for (i in names(clusters)){
                p = try(t.test(data = clusters[[i]], rescaled~cluster)$p.value)
                if(class(p) != "try-error"){
                        clusters[[i]] = data.frame(uid = unique(clusters[[i]]$uid), 
                                                   cluster1_mean = mean(clusters[[i]]$rescaled[clusters[[i]]$cluster == 1]),
                                                   cluster2_mean = mean(clusters[[i]]$rescaled[clusters[[i]]$cluster == 2]),
                                                   p = p)
                }else{
                        clusters[[i]] = NULL
                }
        }
        clusters = bind_rows(clusters) %>%
                filter(p <= signif) %>%
                mutate(diff = cluster2_mean - cluster1_mean) %>%
                arrange(desc(diff))
        clusters <- add_meta(clusters, folder)
        return(clusters)
}
