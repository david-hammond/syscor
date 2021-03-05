library(systr)
library(tidyverse)
library(hammond)
hammond::hdb_login(host = "192.168.0.67", db = "nationalhdb_march2021", user = "admin", password = "admin")
all = hdb_get(hdb_search("ppi.R") %>% filter(grepl(":", variablename)))
df = all %>% mutate(uid = variablename) %>% select(uid, geocode, year, value)
meta = all %>% select(-c(geocode, year, value, country)) %>% distinct()
x = left_join(df, meta)
#setup global
folder = "data/ppi"
newscale = c(1,10)
systr_setup(df, meta, folder, newscale)
x = readRDS("./data/ppi/06-corr_db.rds") %>% 
        filter(abs(r)>0.5, grepl("/dt", uid.y), !grepl("/dt", uid.x))

x = corr_cluster(folder, num_clust = 4)

x = get_cluster(x, which_obs = 253, folder) %>% filter(abs(r) > 0.5, abs(r) <1) 
x2 = readRDS("./data/global/06-corr_db.rds") %>% filter(uid.x %in% c(x$uid.x, x$uid.y)) %>%
        add_meta(folder) %>% filter(abs(r) < 1)

classify_correlation_relationships = function(x){
        bands = rev(c(1, 0.7, 0.5, 0.2, -0.2, -0.5, -0.7, -1))
        label = rev(c("indivisible", "reinforcing", "enabling",
                      "neutral", "constraining", "counteracting", "cancelling"))
        x = findInterval(x, bands, all.inside = T)
        x = label[x]
        x = factor(x, rev(label), ordered = T)
        return(x)
}


format_results = function(x){
        y = x %>% filter(goal.x == 16, goal.y != 16)
        y = y %>% mutate(indicator.x = paste(goal_name.x, target.x, indicator.x, sep = "."),
                         indicator.y = paste(goal_name.y, target.y, indicator.y, sep = ".")) %>%
                  mutate(variablename.x = paste(indicator.x, variablename.x, sep = ": "), 
                        variablename.y = paste(indicator.y, variablename.y, sep = ": ")) %>%
                 select(variablename.x, variablename.y, r) %>%
                distinct() %>%
                group_by(variablename.x, variablename.y) %>%
                top_n(1, abs(r)) %>%
                ungroup() %>%
                mutate(r = classify_correlation_relationships(r))
        y = y %>% 
                spread(variablename.x, r)
        return(y)
}

format_results2 = function(x){
        y = x %>% filter(goal.x == 16, goal.y != 16)
        y = y %>% mutate(indicator.x = paste(goal_name.x, target.x, indicator.x, sep = "."),
                         indicator.y = paste(goal_name.y, target.y, indicator.y, sep = ".")) %>%
                select(indicator.x, indicator.y, r) %>%
                distinct() %>%
                group_by(indicator.x, indicator.y) %>%
                top_n(1, abs(r)) %>%
                ungroup()
        y = y %>% 
                spread(indicator.x, r)
        return(y)
}

format_plot_results = function(x){
        y = x %>% filter(goal.x == 16, goal.y != 16)
        y = y %>% mutate(indicator.x = paste(goal_name.x, target.x, indicator.x, sep = "."),
                         indicator.y = paste(goal_name.y, target.y, indicator.y, sep = ".")) %>%
                select(indicator.x, indicator.y, r) %>%
                distinct() %>%
                group_by(indicator.x, indicator.y) %>%
                top_n(1, abs(r)) %>%
                ungroup() %>%
                mutate(r = classify_correlation_relationships(r))
        return(y)
}
global = format_results(x)
p = format_plot_results(x2)
cols = rev(RColorBrewer::brewer.pal(7, "Spectral"))

p = ggplot(p, aes(indicator.x, indicator.y, fill = r)) + geom_tile() +
        scale_fill_manual(values = cols)
p 

betweeness = readRDS("./data/global/07-systemic_centrality.rds") %>% add_meta(folder) %>%
        filter(!grepl("/dt", uid)) %>%
        group_by(goal_name) %>% top_n(1, betweenness) %>% 
        ungroup()

betweeness = betweeness %>% dplyr::select(uid, goal_name, target, indicator, variablename, betweenness) %>%
        arrange(-betweenness)
#High med low
gpi = readRDS("./data/gpi.rds")
gpi$percentile = findInterval(gpi$from.value, quantile(gpi$from.value, probs = seq(0,1, length.out = 4)), all.inside = T)

folder = "data/high"
tmp = df %>% filter(geocode %in% gpi$geocode[gpi$percentile == 1], uid %in% uid)
systr_setup(tmp, meta, folder, newscale)

high =  readRDS("./data/high/07-systemic_centrality.rds") %>% add_meta(folder) %>%
        filter(!grepl("/dt", uid)) %>% filter(uid %in% betweeness$uid) %>% 
        group_by(goal_name) %>% top_n(1, -betweenness) %>% ungroup() %>%
        mutate(rank = rank(-betweenness, ties.method = "last")) %>% 
        ungroup() %>% select(goal_name, rank) %>% mutate(set = "High Peace")


folder = "data/medium"
tmp = df %>% filter(geocode %in% gpi$geocode[gpi$percentile == 2], uid %in% uid)
systr_setup(tmp, meta, folder, newscale)
medium =  readRDS("./data/medium/07-systemic_centrality.rds") %>% add_meta(folder) %>%
        filter(!grepl("/dt", uid)) %>% filter(uid %in% betweeness$uid)  %>% 
        group_by(goal_name) %>% top_n(1, -betweenness)  %>% ungroup() %>%
        mutate(rank = rank(-betweenness, ties.method = "last")) %>% 
        ungroup() %>% select(goal_name, rank) %>% mutate(set = "Medium Peace")


folder = "data/low"
tmp = df %>% filter(geocode %in% gpi$geocode[gpi$percentile == 3], uid %in% uid)
systr_setup(tmp, meta, folder, newscale)
low =  readRDS("./data/low/07-systemic_centrality.rds") %>% add_meta(folder) %>%
        filter(!grepl("/dt", uid)) %>% filter(uid %in% betweeness$uid)  %>% 
        group_by(goal_name) %>% top_n(1, -betweenness)  %>% ungroup() %>%
        mutate(rank = rank(-betweenness, ties.method = "last")) %>% 
        ungroup() %>% select(goal_name, rank) %>% mutate(set = "Low Peace")

tmp = high %>% rbind(medium) %>% rbind(low) %>% filter()
tmp$set = factor(tmp$set, c("Low Peace", "Medium Peace","High Peace"), ordered = T)
library(ggbump)
library(cowplot)
p = ggplot(tmp, aes(set, rank, color = goal_name)) +
        geom_point(size = 7) + 
        #geom_text(data = tmp, aes(set, rank, label = label), colour = "black") +
        geom_text(data = tmp %>% filter(set == "High Peace"),
                  aes(x = "High Peace", label = goal_name), size = 3, hjust = -0.2) +
        geom_text(data = tmp %>% filter(set == "Low Peace"),
                  aes(x = "Low Peace", label = goal_name), size = 3, hjust = 1.2)+
        geom_bump(size = 2, smooth = 8) +
        theme_minimal_grid(font_size = 14, line_size = 0) +
        theme(legend.position = "none",
              panel.grid.major = element_blank(),
              axis.text.y=element_blank()) +
        labs(y = paste("Higher Interlinkage", sprintf('\u2192')),
             x = NULL) +
        scale_y_reverse()

print(p)
ggsave(p, filename = "SDG Peace Levels.png")

n = 20
cluster1 = gpi %>% top_n(n, -absolute.diff) %>% pull(geocode)#improvers
cluster2 = gpi %>% top_n(n, absolute.diff) %>% pull(geocode)#deteriorators
folder = "data/global"
y = whats_the_difference_between(folder, cluster1, cluster2)


cluster1 = c("SLE", "LBR")#improvers
cluster2 = hammond::countryinfo %>% filter(grepl("Africa", region)) %>%
        pull(geocode)
cluster2 = setdiff(cluster2, cluster1)
folder = "data/global"
y = whats_the_difference_between(folder, cluster1, cluster2) %>%
        filter(grepl("dt", uid)) %>% mutate(diff = cluster2_mean - cluster1_mean) %>%
        select(uid, goal_name, target, indicator, variablename, cluster1_mean, cluster2_mean, diff) %>%
        arrange(desc(diff)) %>% group_by(variablename) %>%
        top_n(1, diff) %>% ungroup()
y[,6:8] = apply(y[,6:8], 2, round, 3)

y = y[c(7,9,12),]
uids = c(y$uid, gsub("/dt", "", gsub("dy-", "", y$uid)))
y = what_correlates(uids, folder, rval = 0.5)
y = y %>% 
        mutate(goal_name.x = paste(goal_name.x, paste(target.x, indicator.x, sep = ".")),
               goal_name.y = paste(goal_name.y, paste(target.y, indicator.y, sep = "."))) %>%
               group_by(goal_name.x, variablename.x, 
                   goal_name.y, variablename.y) %>%
        top_n(1, abs(r)) %>% ungroup() %>%
        filter(variablename.x != variablename.y) %>% select(goal_name.x, variablename.x, goal_name.y, variablename.y, r)
y$r = classify_correlation_relationships(y$r)
# #get centrality
# low = readRDS("./data/low/07-systemic_centrality.rds") %>%
#         select(uid, centrality) %>% rename(low = centrality)
# med = readRDS("./data/medium/07-systemic_centrality.rds") %>%
#         select(uid, centrality) %>% rename(med = centrality)
# high = readRDS("./data/high/07-systemic_centrality.rds") %>%
#         select(uid, centrality) %>% rename(high = centrality)
# centrality = low %>% left_join(med) %>% left_join(high) %>%
#         gather("cluster", "centrality", - uid) %>%
#         add_meta(folder) %>%
#         group_by(variablename, cluster) %>%
#         top_n(1, centrality) %>%
#         group_by(cluster) %>%
#         mutate(rank = rank(-centrality)) %>%
#         top_n(40, -rank) %>%
#         ungroup()
#         
# centrality$value[is.na(centrality$value)]=0 
# centrality = centrality %>% 
#         group_by(cluster) %>%
#         mutate(rank = rank(-value, ties.method = "max")) %>%
#         ungroup()
