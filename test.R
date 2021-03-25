library(systr)
library(tidyverse)
library(scales)
library(corrr)
df = readRDS("./data/original_data/sdgs.rds")
meta = readRDS("./data/original_data/sdg_meta.rds")
folder = "data/global"
newscale = c(1,10)
systr_setup(df, meta, newscale)
x = corr_cluster(folder, num_clust = 4)

x = get_cluster(x, which_cluster = 4) %>% filter(abs(r) > 0.5)
y = readRDS("./data/global/06-corr_db.rds")

y = readRDS("./data/global/07-systemic_centrality.rds") %>%
        add_meta()

x = what_correlates("iep-proxy-Acceptance-of-the-Rights-of-Others", folder)
cluster1 = sample(df$geocode, 30)
cluster2 = sample(df$geocode, 30)
cluster1 = c("SLE", "LBR")#improvers
cluster2 = hammond::countryinfo %>% filter(grepl("Africa", region)) %>%
        pull(geocode)
cluster2 = setdiff(cluster2, cluster1)
x = whats_the_difference_between(folder, cluster1, cluster2)
tmp = add_meta(cors, folder)
cors = readRDS("./data/06-corr_db.rds")
meta = readRDS("./data/original_data/sdg_meta.rds")
folder = "data"
newscale = c(1,5)
systr_setup(df, meta, folder, newscale)
df <- df %>% group_by(uid) %>% 
        mutate(rescaled = rescale(value, to = newscale))
changes_db(df, "data")
saveRDS(df, "./data/rawdata_db.rds", compress = "xz")
