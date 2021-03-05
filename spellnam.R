library(tidyverse)
library(RColorBrewer)
library(dendextend)
library(gplots)
spellman <- read_csv("https://github.com/Bio723-class/example-datasets/raw/master/spellman-wide.csv")
dim(spellman)

spellman.cor <- dplyr::select(spellman, -time, -expt) %>% 
        cor(use="pairwise.complete.obs")

sdg16 <- readRDS("./data/global/06-corr_db.rds") %>%
        spread(uid.y, r) %>% as.data.frame() %>%
add_meta(folder) %>% filter(goal.x == 16)
spellman.cor <- readRDS("./data/global/06-corr_db.rds") %>%
        #filter(uid.x %in% sdg16$uid.x, !(uid.y %in% sdg16$uid.x)) %>%
        spread(uid.y, r) %>% as.data.frame()
rownames(spellman.cor) <- spellman.cor$uid.x
spellman.cor <- spellman.cor %>% select(-uid.x)
spellman.cor[is.na(spellman.cor)] <- 0
spellman.cor = (as.matrix(spellman.cor))
heatmap.2(spellman.cor, col = color.scheme)
sdg16.dend 
spellman.dist <- as.dist(1 - (spellman.cor))

spellman.tree <- hclust(spellman.dist, method="complete")

spellman.dend <- as.dendrogram(spellman.tree) # create dendrogram object
nclust = 15
clusters <- cutree(spellman.dend, k=nclust)
table(clusters)

plot(color_branches(spellman.dend, k=nclust),leaflab="none")

clusters.df <- data.frame(gene = names(clusters), cluster = clusters)

sub.trees <- cut(spellman.dend, h = 1.6)

clusters.df$col = RColorBrewer::brewer.pal(nclust, "Spectral")[clusters.df$cluster]

plot(spellman.dend)
color.scheme <- (brewer.pal(8,"RdBu")) # generate the color scheme to use
x = heatmap.2(spellman.cor, 
          Rowv = ladderize(spellman.dend), 
          Colv = ladderize(spellman.dend), 
          dendrogram = "both", 
          revC = TRUE,  # rev column order of dendrogram so conforms to natural representation
          trace = "none", 
          #density.info = "none",
          col = color.scheme, #key = FALSE,
          labRow = FALSE, labCol = FALSE,
          ColSideColors = clusters.df$col)

new_dend = x$colDendrogram
plot(color_branches(new_dend, k=nclust),leaflab="none")

clusters <- cutree(new_dend, k=nclust)
clusters.df <- data.frame(uid = names(clusters), cluster = clusters)
firstcluster = clusters.df %>% dplyr::filter(cluster == 4)
secondcluster = clusters.df %>% dplyr::filter(cluster == 7)
folder = "data/global"
library(systr)
firstcluster = firstcluster %>% add_meta(folder)
secondcluster = secondcluster %>% add_meta(folder)
source("https://raw.githubusercontent.com/talgalili/dendextend/master/R/attr_access.R")

library(cluster)
spellman.kmedoids <- pam(spellman.dist, 8) # create k-medoids clustering with 8 clusters
kclusters <- spellman.kmedoids$cluster
table(kclusters)
kclusters.reordered <- kclusters[order.dendrogram(spellman.dend)]

# get  branch colors so we're using the same palette
dend.colors <- unique(get_leaves_branches_attr(color_branches(spellman.dend, k=8), attr="col"))

# color the branches according to their k-means cluster assignment
plot(branches_attr_by_clusters(spellman.dend, kclusters.reordered , dend.colors),leaflab="none")
kmedoids.cor <- spellman.cor[order(kclusters), order(kclusters)]
heatmap.2(kmedoids.cor, Rowv = NULL, Colv = NULL, 
          dendrogram = "none", 
          trace = "none", density.info = "none",
          col = color.scheme, key = FALSE,
          labRow = FALSE, labCol = FALSE)
