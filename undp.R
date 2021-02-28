library(systr)
library(tidyverse)
df = readRDS("./data/original_data/sdgs.rds")
meta = readRDS("./data/original_data/sdg_meta.rds")
#setup global
folder = "data/global"
newscale = c(1,10)
systr_setup(df, meta, folder, newscale)
x = corr_cluster(folder, num_clust = 4)

x = get_cluster(x, which_cluster = 4, folder) %>% filter(abs(r) > 0.5)

#High med low
gpi = readRDS("./data/gpi.rds")
gpi$percentile = findInterval(gpi$from.value, quantile(gpi$from.value, probs = seq(0,1, length.out = 4)), all.inside = T)

folder = "data/high"
tmp = df %>% filter(geocode %in% gpi$geocode[gpi$percentile == 1], 
                    uid %in% x$uid.x)
systr_setup(tmp, meta, folder, newscale)
high = readRDS("./data/high/07-systemic_centrality.rds") %>%
        select(uid, centrality) %>%
        rename(high = centrality)

folder = "data/medium"
tmp = df %>% filter(geocode %in% gpi$geocode[gpi$percentile == 2], 
                    uid %in% x$uid.x)
systr_setup(tmp, meta, folder, newscale)
med = readRDS("./data/medium/07-systemic_centrality.rds") %>%
        select(uid, centrality) %>%
        rename(medium = centrality)

folder = "data/low"
tmp = df %>% filter(geocode %in% gpi$geocode[gpi$percentile == 3], 
                    uid %in% x$uid.x)
systr_setup(tmp, meta, folder, newscale)
low = readRDS("./data/low/07-systemic_centrality.rds") %>%
        select(uid, centrality) %>%
        rename(low = centrality)

#get centrality
centrality = low %>% left_join(med) %>% left_join(high) %>%
        add_meta(folder) %>% select(goal_name, low, medium, high) %>%
        gather("cluster", "value", -goal_name) %>%
        group_by(goal_name, cluster) %>% summarise(value = mean(value)) 
centrality$value[is.na(centrality$value)]=0 
centrality = centrality %>% 
        group_by(cluster) %>%
        mutate(rank = rank(-value, ties.method = "max")) %>%
        ungroup()
