library(systr)
library(tidyverse)
df = readRDS("./data/original_data/sdgs.rds")
meta = readRDS("./data/original_data/sdg_meta.rds")
#setup global
folder = "data/global"
newscale = c(1,10)
systr_setup(df, meta, folder, newscale)
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
        y = x %>% filter(goal.x == 16)
        y = y %>% mutate(indicator.x = paste(goal_name.x, target.x, indicator.x, sep = "."),
                         indicator.y = paste(goal_name.y, target.y, indicator.y, sep = ".")) %>%
                  mutate(variablename.x = paste(indicator.x, variablename.x, sep = ": "), 
                        variablename.y = paste(indicator.y, variablename.y, sep = ": ")) %>%
                 select(variablename.x, variablename.y, r) %>%
                distinct() %>%
                group_by(variablename.x, variablename.y) %>%
                top_n(1, abs(r)) %>%
                ungroup()
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
global = format_results(x2)
p = format_plot_results(x2)
cols = rev(RColorBrewer::brewer.pal(7, "Spectral"))

p = ggplot(p, aes(indicator.x, indicator.y, fill = r)) + geom_tile() +
        scale_fill_manual(values = cols)
p 

#High med low
gpi = readRDS("./data/gpi.rds")
gpi$percentile = findInterval(gpi$from.value, quantile(gpi$from.value, probs = seq(0,1, length.out = 4)), all.inside = T)

folder = "data/high"
tmp = df %>% filter(geocode %in% gpi$geocode[gpi$percentile == 1], uid %in% uid)
systr_setup(tmp, meta, folder, newscale)

x2 = readRDS("./data/high/06-corr_db.rds") %>% filter(uid.x %in% c(x$uid.x, x$uid.y)) %>%
        add_meta(folder) %>% filter(abs(r) < 1)
high = format_results2(x2)
make_stronger_or_weaker = function(tmp){
        tmp2 = tmp
        tmp2[,-1] = apply(tmp[,-1], 2, as.character)
        for(i in 1:nrow(tmp)){
                for(j in 2:ncol(tmp)){
                        tmp2[i,j] = ifelse(abs(tmp[i,j]) < abs(global[i,j]), "", "Higher Priority")
                }
        }
        tmp2 = tmp2 %>% gather("indicator.x", "r", -indicator.y)
        return(tmp2)
}

high = make_stronger_or_weaker(high) %>%
        filter(complete.cases(.))
cols = rev(RColorBrewer::brewer.pal(2, "Spectral"))
p = high
p = ggplot(p, aes(indicator.x, indicator.y, fill = r)) + geom_tile() +
        scale_fill_manual(values = cols)
p 

folder = "data/medium"
tmp = df %>% filter(geocode %in% gpi$geocode[gpi$percentile == 2], uid %in% uid)
systr_setup(tmp, meta, folder, newscale)
x = corr_cluster(folder, num_clust = 10)
x = get_cluster(x, which_obs =  18, folder) %>% filter(abs(r) > 0.5, r < 1)
med = format_results(x)
setdiff(med$variablename.y, global$variablename.y)
setdiff(global$variablename.y, med$variablename.y)


folder = "data/low"
tmp = df %>% filter(geocode %in% gpi$geocode[gpi$percentile == 3], uid %in% uid)
systr_setup(tmp, meta, folder, newscale)
x = corr_cluster(folder, num_clust = 4)
x = get_cluster(x, which_obs = 218, folder) %>% filter(abs(r) > 0.5, r < 1)
low = format_results(x)
setdiff(low$variablename.y, global$variablename.y)
setdiff(global$variablename.y, low$variablename.y)

n = 20
cluster1 = gpi %>% top_n(n, -absolute.diff) %>% pull(geocode)#improvers
cluster2 = gpi %>% top_n(n, absolute.diff) %>% pull(geocode)#deteriorators
folder = "data/global"
y = whats_the_difference_between(folder, cluster1, cluster2)

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
