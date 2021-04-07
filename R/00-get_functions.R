#' Creates a Correlation Matrix
#'
#' This takes a raw data file and creates a systems correlation database
#'
#' @param df dataframe in format
#' @param folder dataframe in format
#'
#' @return Returns a correlation matrix
#'
#' @examples
#'
#' @export

get_corpus <- function() {

        df = readRDS(systr_file$scaled)
        return(df)
}


#' Creates a Correlation Matrix
#'
#' This takes a raw data file and creates a systems correlation database
#'
#' @param df dataframe in format
#' @param folder dataframe in format
#'
#' @return Returns a correlation matrix
#'
#' @examples
#'
#' @export

get_meta <- function() {
        
        df = readRDS(systr_file$meta)
        return(df)
}


#' Creates a Correlation Matrix
#'
#' This takes a raw data file and creates a systems correlation database
#'
#' @param df dataframe in format
#' @param folder dataframe in format
#'
#' @return Returns a correlation matrix
#'
#' @examples
#'
#' @export

get_changes <- function() {
        
        df = readRDS(systr_file$changes)
        return(df)
}

#' Creates a Correlation Matrix
#'
#' This takes a raw data file and creates a systems correlation database
#'
#' @param df dataframe in format
#' @param folder dataframe in format
#'
#' @return Returns a correlation matrix
#'
#' @examples
#'
#' @export

get_corrs <- function() {
        
        df = readRDS(systr_file$correlations)
        return(df)
}

#' Creates a Correlation Matrix
#'
#' This takes a raw data file and creates a systems correlation database
#'
#' @param df dataframe in format
#' @param folder dataframe in format
#'
#' @return Returns a correlation matrix
#'
#' @examples
#'
#' @export

get_centrality <- function() {
        
        df = readRDS(systr_file$centrality)
        return(df)
}