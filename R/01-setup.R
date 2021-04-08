#' Creates a Correlation Matrix
#'
#' This takes a raw data file and creates a systems correlation database
#'
#' @param df dataframe in format
#' @param folder dataframe in format
#' @param newscale dataframe in format
#'
#' @return Returns a correlation matrix
#'
#' @examples
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom scales rescale
#' @author David Hammond
#' @export

systr_setup <- function(df, meta, newscale = c(1,5)) {
        saveRDS(meta, systr_file$meta, compress = "xz")
        
        saveRDS(df, systr_file$rawdata, compress = "xz")

        df <- df %>% group_by(uid) %>% 
                mutate(rescaled = rescale(value, to = newscale)) %>%
                as.data.frame()
        
        test = df %>% group_by(uid) %>%
                summarise(sd = var(value, na.rm = T)) %>%
                filter(sd != 0)
        if(nrow(test) > 0){
                message("Note, removed vars with zero variance")
                df <- df %>% filter(uid %in% test$uid)
        }
        
        # test = df %>% group_by(uid, geocode) %>%
        #         summarise(min.year = min(year), max.year = max(year)) %>%
        #         mutate(span = max.year - min.year) %>%
        #         filter(span >= min_yrs)
        # if(nrow(test) > 0){
        #         message("Note, removed uid, geocode combos with not enough time series")
        #         df <- df %>% filter(paste(uid, geocode) %in% paste(test$uid, test$geocode))
        # }
        
        saveRDS(df, systr_file$scaled, compress = "xz")
        message("Calculating changes...")
        changes_db(df)
        message("Correlating...")
        corr_db()
        message("Calculating centrality...")
        centrality_db()
}
