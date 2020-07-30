#' Wrapper function for all SEMRush report types.
#' 
#' @description This function is a wrapper for all SEMRush reports functions included in this package. It takes a single argument ('report'), and passes
#' all other arguments to the associated '*_reports' functions.
#' 
#' @param report \emph{string}. The report type on which to pass all additional arguments. Choices are 'advertising', 'backlinks', 'domain', 'keywords', or 'overview'.
#' @param `...` Additional arguments to pass on to underlying functions.
#
#' @importFrom assertthat assert_that
#' @importFrom assertthat noNA
#' @importFrom assertthat is.string
#' @importFrom assertthat not_empty
#' @importFrom rlang .data
#'
#'
#' @return A data table (tibble) with columns for each requested variable.
#' @export
#'
#' @examples
#'\dontrun{
#'
#' report <-
#'   semrush_reports(
#'     report = "domain",
#'     type = "domain_organic",
#'     key = key,
#'     database = "us",
#'     domain = "cran.r-project.org"
#'   )
#'
#'}
#'
semrush_reports <- function(report, ...){
  
  assert_that(
    report %in% c('advertising', 'backlinks', 'domain', 'keywords', 'overview'),
    noNA(report),
    is.string(report),
    not_empty(report)
  )
  
  if(report=="advertising"){
    x <- advertising_reports(...)
  }
  
  if(report=="backlinks"){
    x <- backlinks_reports(...)
  }
  
  if(report=="domain"){
    x <- domain_reports(...)
  }
  
  if(report=="keyword"){
    x <- keyword_reports(...)
  }
  
  if(report=="overview"){
    x <- overview_reports(...)
  }
  
  return(x)
  
}