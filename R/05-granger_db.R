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
#' @importFrom igraph betweenness
#' @importFrom igraph E<-
#' @importFrom igraph E

#' @author David Hammond


granger_db <- function(rval = 0.5) {
        dydt = readRDS(systr_file$changes) %>%
                select(-rescaled)
        #create trends_together
        trends_together = data.frame(expand.grid(uid.x = unique(dydt$uid), uid.y = unique(dydt$uid), 
                                                 stringsAsFactors = F)) %>%
                filter(uid.x != uid.y)
        
        sdg16_indicators = readRDS(file.path(folder, "06-corr_db.rds")) %>% add_meta2() %>%
                filter( !(variablename.x %in% c("Proportion of members and voting rights of developing countries in international organizations")),
                        !(variablename.y %in% c("Proportion of members and voting rights of developing countries in international organizations")),
                        goal.x == 16, goal.x != goal.y,
                        !grepl("/dt", uid.x),
                        !grepl("/dt", uid.y))
        
        
        trends_together <- trends_together %>% left_join(dydt %>% rename(uid.x = uid, value.x = value))
        trends_together <- trends_together %>% left_join(dydt %>% rename(uid.y = uid, value.y = value))
        
        granger_corpus = corpus %>% filter(geocode == country, uid %in% c(unique(sdg16_indicators$uid.x), unique(sdg16_indicators$uid.y)), 
                                           !(uid %in% c("3fdcc0fa-c0f8-11ea-8e30-1c1b0d61dc59", "3fdcc5f0-c0f8-11ea-8e30-1c1b0d61dc59",
                                                        "iep-proxy-Human-rights-protection-scores")))
        
        cors_split = split(sdg16_indicators, 1:nrow(sdg16_indicators))
        
        
        source("./lib/02-granger.R")
        system.time({
                cl <- makeCluster(detectCores())
                clusterExport(cl, list("granger_corpus"),
                              envir=environment())
                raw <- pblapply(cors_split,
                                granger,
                                granger_corpus = granger_corpus,
                                cl = cl)
                stopCluster(cl)
        })
        results = bind_rows(raw)
        if(nrow(results) > 0){
                results = results %>% add_meta2()
                results$forward = results$granger_v1_v2 < pval 
                results$backward = results$granger_v2_v1 < pval
                results$loop = results$forward & results$backward
                results$relationship = NA
                results$relationship[results$forward] = "Leading"
                results$relationship[results$backward] = "Lagging"
                results$relationship[results$loop] = "Feedback"
                results$causality = apply(results[, c("granger_v1_v2", "granger_v2_v1")], 1, min)
                results$causality = results$forward | results$backward
                trends = trends_together %>% filter(geocode == country) %>%
                        mutate(uid.x = gsub("/dt", "", uid.x), 
                               uid.y = gsub("/dt", "", uid.y),
                               same_direction = sign(value.x) == sign(value.y)) %>%
                        select(uid.x, uid.y, value.x, value.y, same_direction)
                results = results %>% left_join(trends)
                
                results = is_more_better(results)
                results$direction.x = NA
                results$direction.x[results$is_more_better.x == 1 & results$value.x > 0] = "Improvement"
                results$direction.x[results$is_more_better.x == 1 & results$value.x < 0] = "Deterioration"
                results$direction.x[results$is_more_better.x == 0 & results$value.x < 0] = "Improvement"
                results$direction.x[results$is_more_better.x == 0 & results$value.x > 0] = "Deterioration"
                results$direction.y = NA
                results$direction.y[results$is_more_better.y == 1 & results$value.y > 0] = "Improvement"
                results$direction.y[results$is_more_better.y == 1 & results$value.y < 0] = "Deterioration"
                results$direction.y[results$is_more_better.y == 0 & results$value.y < 0] = "Improvement"
                results$direction.y[results$is_more_better.y == 0 & results$value.y > 0] = "Deterioration"
                results$intuitive = results$is_more_better.x == results$is_more_better.y
                results$intuitive = results$intuitive == results$same_direction
                tmp = results %>% filter(intuitive, !is.na(direction.x), !is.na(direction.y), causality) %>% 
                        group_by(nicename.x, nicename.y) %>% top_n(1, -causality) %>%
                        ungroup()
                tmp = create_edge_list(tmp)
                try(plot_granger(tmp, plot_threshold, country))
                results$geocode = country
                results = results %>% relocate(geocode)
                return(results)
        }
}
