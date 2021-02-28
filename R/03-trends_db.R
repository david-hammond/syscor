#' Creates a Correlation Matrix
#'
#' This takes a raw data file and creates a systems correlation database
#'
#' @param folder dataframe in format
#'
#' @return Returns a correlation matrix
#'
#' @examples
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @author David Hammond


trends_together_db <- function(folder) {
        fname <- get_db(folder, "gradients")
        dydt = readRDS(fname) %>%
                select(-rescaled)
        #create trends_together
        trends_together = data.frame(expand.grid(uid.x = unique(dydt$uid), uid.y = unique(dydt$uid), 
                                                 stringsAsFactors = F)) %>%
                filter(uid.x != uid.y)
        
        trends_together <- trends_together %>% left_join(dydt %>% rename(uid.x = uid, value.x = value))
        trends_together <- trends_together %>% left_join(dydt %>% rename(uid.y = uid, value.y = value))
        
        fname <- get_db(folder, "trend_together")
        saveRDS(trends_together, fname, compress = "xz")
}
