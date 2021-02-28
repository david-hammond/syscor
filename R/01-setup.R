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
#' @importFrom fs dir_create
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom scales rescale
#' @author David Hammond
#' @export

systr_setup <- function(df, meta, folder = "data", newscale = c(1,5)) {
        dir_create(folder)
        tmp = meta
        tmp$uid = add_dy_dt(tmp$uid)
        meta = rbind(meta, tmp)
        fname <- get_db(folder, "meta")
        saveRDS(meta, fname, compress = "xz")
        df <- df %>% group_by(uid) %>% 
                mutate(rescaled = rescale(value, to = newscale)) %>%
                as.data.frame()
        fname <- get_db (folder, "rawdata")
        saveRDS(df, fname, compress = "xz")
        message("Calculating dy/dt...")
        changes_db(df, folder)
        message("Correlating...")
        corr_db(folder)
        message("Calculating systemic centrality...")
        systemic_db(folder)
        message("Calculating vars that trend together...can take a while")
        trends_together_db(folder)


}
