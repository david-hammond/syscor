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

systr_setup <- function(df, meta, newscale = c(1,5), do_trends = F) {
        saveRDS(df, systr_file$rawdata, compress = "xz")
        tmp = meta
        meta = rbind(meta, tmp)
        saveRDS(meta, systr_file$meta, compress = "xz")
        df <- df %>% group_by(uid) %>% 
                mutate(rescaled = rescale(value, to = newscale)) %>%
                as.data.frame()
        saveRDS(df, systr_file$scaled, compress = "xz")
        message("Calculating changes...")
        changes_db(df)
        message("Correlating...")
        corr_db()
        message("Calculating systemic centrality...")
        centrality_db()
        message("Calculating granger causality...")
        granger_db()
}
