#' View SEMRush API Units balance for a subscriber account.
#'
#' @description Returns the SEMRush account API Units balance. API Units are consumed when requests to the SEMRush database are made through the API.
#' See the SEMRush API website \href{https://www.semrush.com/api-analytics/}{(https://www.semrush.com/api-analytics/)} for more information.
#' @param key \emph{string}. API key assigned to a user after subscribing to SEMrush. The key is available from the account profile page.
#' @param character_format \emph{logical}. If FALSE (default), returns the unit balance as a numeric. If TRUE, value returned is a character string formatted with a "," every three digits.
#' @export
#' @examples
#' ##define object with your SEMRush account API key.
#'\dontrun{
#' api_key <- ""
#'
#'## without formatting
#'account_api_units(api_key)
#'10000
#'
#'## with formatting
#'account_api_units(api_key, character_format=TRUE)
#'"10,000"}
account_api_units <- function(key, character_format=FALSE){

  ## Check that universal required arguements are present and valid
  assert_that(noNA(key), not_empty(key), is.string(key))

  ## Create URL request (base)
  request_url <- sprintf("http://www.semrush.com/users/countapiunits.html?key=%s",key)

  response <- httr::GET(request_url)

  if(response$status_code == 200){

    #get content from return
    cont <- httr::content(response, as="text")

    if(stringr::str_detect(cont, "ERROR")){
      stop(sprintf("Something went wrong. Check input arguments. (%s)",cont))
    }

    d <- cont %>%
      as.numeric

    if(character_format){
      d <- formatC(d, big.mark = ",", format = "d")
    }

    return(d)


  } else{

    stop(sprintf(
      "Status code returned was not 200 (status: %s)",
      as.character(response$status_code)
    ))

  }


}
