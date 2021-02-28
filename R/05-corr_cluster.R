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


corr_cluster <- function(folder, num_clust = 4) {
        set.seed(538)
        fname <- get_db(folder, "correlations")
        df <- readRDS(fname)

        df_raw <- df %>% 
                filter(!which_is_dy_dt(uid.x)) %>%
                filter(!which_is_dy_dt(uid.y)) %>%
                spread(uid.y, r) %>% select(-uid.x)

        df_raw[is.na(df_raw)] <- 0
        df_raw = 1-abs(df_raw)
        df_dist <- as.dist(df_raw)
        df_clust <- hclust(df_dist, method="complete")
        df_dend <- as.dendrogram(df_clust) # create dendrogram object
        clusters <- cutree(df_dend, k=num_clust)
        plot(color_branches(df_dend, k=num_clust),leaflab="none")
        mydata = as.matrix(df_raw)
        
        x = heatmap.2(mydata, scale = "none", 
                  trace = "none", density.info = "none",
                  Rowv = ladderize(df_dend), 
                  Colv = ladderize(df_dend))
        y = cutree(as.hclust(x$rowDendrogram), 1:dim(mydata)[1]) #this is how you identify systems
        var = "iep-proxy-Acceptance-of-the-Rights-of-Others"

        # pos = which(mydata < 0.2, arr.ind = T)
        # tmp <- data.frame(uid.x = colnames(mydata)[pos[,1]], uid.y = colnames(mydata)[pos[,2]])
        clst <- data.frame(uid = names(clusters), cluster = clusters)
        # tmp <- tmp %>% left_join(clst %>% rename(uid.x = uid, cluster1 = cluster)) %>%
        #         left_join(clst %>% rename(uid.y = uid, cluster2 =  cluster)) %>%
        #         filter(cluster1 == cluster2, uid.x != uid.y)
        clst = clst[x$rowInd,]
        pos = unique(clst$cluster)
        clst$cluster = factor(clst$cluster, unique(clst$cluster), ordered = T)
        return(clst)
}
