#' Request SEMRush Keyword Reports
#'
#' @description This function creates a request to the SEMRush SEO database for users with a subscription and sufficient API units.
#' See the SEMRush API website \href{https://www.semrush.com/api-analytics/}{(https://www.semrush.com/api-analytics/)} for additional information, including a list of codes for the regional databases and export variables.
#' @param type \emph{string}. REQUIRED. The report type to be generated. Available report types are 'phrase_all', 'phrase_this','phrase_these','phrase_organic','phrase_adwords',
#''phrase_related',
#''phrase_adwords_historical',
#''phrase_fullsearch',
#''phrase_questions', or
#''phrase_kdi'.
#' @param key \emph{string}. REQUIRED. An identification key assigned to a user after subscribing to SEMrush. The key is available on the Profile page.
#' @param phrase 	\emph{string, vector}. REQUIRED. A keyword or keyword expression you'd like to investigate. Also, a vector of character strings.
#' @param database 	string. A regional database. If this parameter is not specified (i.e., ""), your request will be sent to all regional databases, if possible. Default is the United States.
#' @param display_limit \emph{integer}. The number of results returned to a request. If this parameter is set to 0, up to 10,000 lines will be returned. Default is 5 lines.
#' @param display_offset \emph{integer}. This parameter allows you to skip a specified number of results before sending a report. Please keep in mind that if you use the option display_offset, display_limit, the value should be increased by value display_offset.
#' @param display_date \emph{string}. Date in format "YYYYMM15". A date on which a report will be shown. You can roll back to the past or choose an actual date.
#' @param export_columns \emph{vector}. A vector of character strings specifying the variables to be included in the report, which vary according to the report type (see 'type' argument). If this parameter is not specified, default columns will be sent.
#' @param return_url logical. If TRUE, prints the request URL used to generate the report. Default value is FALSE.
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat noNA
#' @importFrom assertthat is.string
#' @importFrom assertthat not_empty
#' @importFrom methods hasArg
#' @importFrom tibble as_tibble
#' @importFrom rlang .data
#'
#' @return A data table (tibble) with columns for each requested variable.
#' @export
#'
#' @examples
#'#Enter your SEMRush account API key
#'key <- ""
#'
#'\dontrun{
#'## Generate 'phrase_organic' report
#' report <- keyword_reports(
#'     type = "phrase_organic",
#'     key = key,
#'     phrase = "r software",
#'     database = "us",
#'     display_limit=10,
#'     export_columns = c("Dn","Ur")
#' )
#'
#'print(report)
#'# A tibble: 10 x 2
#'  Domain          Url                                                                  
#'  <chr>           <chr>                                                                
#'1 r-project.org   https://www.r-project.org/                                           
#'2 wikipedia.org   https://en.wikipedia.org/wiki/R_(programming_language)               
#'3 rstudio.com     https://rstudio.com/                                                 
#'4 epa.gov         https://archive.epa.gov/nheerl/arm/web/pdf/irss_2.6.pdf              
#'5 umich.edu       https://www.icpsr.umich.edu/icpsrweb/content/shared/ICPSR/faqs/what-~
#'6 datamentor.io   https://www.datamentor.io/r-programming/                             
#'7 psu.edu         https://online.stat.psu.edu/statprogram/tutorials/statistical-softwa~
#'8 utoledo.edu     https://libguides.utoledo.edu/stats-software/R                       
#'9 statmethods.n~  https://www.statmethods.net/r-tutorial/index.html                    
#'10 statmethods.n~ https://www.statmethods.net/
#'
#'## Generate 'phrase_these' report
#'
#' report <- keyword_reports(
#'     type = "phrase_these",
#'     key = key,
#'     phrase = c("statistical programming","r software","r programming"),
#'     database = "us",
#'     display_limit=10,
#'     export_columns = c("Ph", "Nq", "Cp", "Nr")
#' )
#'
#'print(report)
#' # A tibble: 3 x 4
#'  Keyword                 Search.Volume   CPC Number.of.Results
#'  <chr>                           <int> <dbl>             <dbl>
#'1 r programming                   22200  4.36         266000000
#'2 r software                       4400  5.87        4540000000
#'3 statistical programming          1000  3.91         407000000
#'}
keyword_reports <- function(type, key, phrase,#required arguments
                            database, display_limit=5, display_offset,
                            display_date, export_columns,
                            return_url = FALSE
)
{

  ## Set defaults
  display_limit <- display_limit

  ## Check that universal required arguements are present and valid
  assert_that(noNA(type), not_empty(type), is.string(type),
              noNA(key), not_empty(key), is.string(key),
              noNA(phrase), not_empty(phrase), all(sapply(phrase, is.string))
              )

  ## Additional formatting for phrase
  phrase = sapply(phrase, function(x) str_replace_all(str_trim(x,"both"), pattern = "\\s{1,}", "+"))

  ## Check that requested report is a valid choice.
  #A list of the available types of reports that can be generated.
  report_types = c("phrase_all",
                   "phrase_this",
                   "phrase_these",
                   "phrase_organic",
                   "phrase_adwords",
                   "phrase_related",
                   "phrase_adwords_historical",
                   "phrase_fullsearch",
                   "phrase_questions",
                   "phrase_kdi"
                   )

  assert_that(length(type)==1, is.string(type))
  if(!type %in% report_types){
    stop("Invalid report type requested.")
  }

  ## Check that API key format is valid
  assert_that(length(key)==1, is.string(key))

  ## Match report types with appropriate export columns
  if(type == "phrase_all"){

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Dt","Db","Ph","Nq","Cp","Co","Nr")

  } #end report type: phrase_all

  if(type == "phrase_this"){

    assert_that(noNA(database), not_empty(database), is.string(database))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Ph","Nq","Cp","Co","Nr","Td")

  } #end report type: phrase_this

  if(type == "phrase_these"){

    assert_that(noNA(database), not_empty(database), is.string(database))

    if(length(phrase)>1){
      phrase <- paste0(phrase, collapse = ";")
    }

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Ph","Nq","Cp","Co","Nr","Td")

  } #end report type: phrase_these

  if(type == "phrase_organic"){

    assert_that(noNA(database), not_empty(database), is.string(database))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Dn","Ur","Fk")

  } #end report type: phrase_organic

  if(type == "phrase_adwords"){

    assert_that(noNA(database), not_empty(database), is.string(database))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Dn","Ur","Vu")

  } #end report type: phrase_organic

  if(type == "phrase_related"){

    assert_that(noNA(database), not_empty(database), is.string(database))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Ph","Nq","Cp","Co","Nr","Td","Rr","Fk")

  } #end report type: phrase_related

  if(type == "phrase_adwords_historical"){

    assert_that(noNA(database), not_empty(database), is.string(database))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Dn","Dt","Po","Ur","Tt","Ds","Vu","At","Ac","Ad")

  } #end report type: phrase_adwords_historical

  if(type == "phrase_fullsearch"){

    assert_that(noNA(database), not_empty(database), is.string(database))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Ph","Nq","Cp","Co","Nr","Td","Fk")

  } #end report type: phrase_fullsearch

  if(type == "phrase_questions"){

    assert_that(noNA(database), not_empty(database), is.string(database))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Ph","Nq","Cp","Co","Nr","Td")

  } #end report type: phrase_questions

  if(type == "phrase_kdi"){

    assert_that(noNA(database), not_empty(database), is.string(database))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Ph","Kd")

  } #end report type: phrase_kdi

  ## Create URL request (base)
  request_url <- paste0("https://api.semrush.com/?key=",key,
                        "&type=",type,
                        "&phrase=",phrase)

  ## Append optional parameters to the URL request
  if(hasArg(database)){
    assert_that(noNA(database), not_empty(database), is.string(database))
    request_url <- paste0(request_url, "&database=", database)
  }
  if(hasArg(display_limit)){
    assert_that(display_limit == as.integer(display_limit))
    request_url <- paste0(request_url, "&display_limit=", display_limit)
  }
  if(hasArg(display_offset)){
    assert_that(display_offset == as.integer(display_offset))
    request_url <- paste0(request_url, "&display_offset=", display_offset)
  }
  if(hasArg(display_date)){
    assert_that(is.string(display_date), grepl("^20[0-2][0-9][0|1][0-9]15", x=display_date))
    request_url <- paste0(request_url, "&display_date=", display_date)
  }
  if(hasArg(export_columns)){
    assert_that(noNA(export_columns), all(sapply(export_columns, is.string)))
    if(any(!export_columns %in% export_columns_default)){
      stop("Invalid export columns for requested report type.")
    }
    if(length(export_columns)>1){
      export_columns <- paste0(export_columns, collapse = ",")
    }
    request_url <- paste0(request_url, "&export_columns=", export_columns)
  }

  ## Get the result and format for output
  if(return_url){
    print(paste0("Request URL: ",request_url))
  }
  response <- httr::GET(request_url)
  #get content from return
  cont <- httr::content(response, as="text")
  
  if(response$status_code == 200){
    
    if(stringr::str_detect(cont, "ERROR")){
      stop(sprintf("Something went wrong. Check input arguments. (%s)",cont))
    }
    
    d <- cont %>%
      textConnection() %>%
      utils::read.table(
        numerals = "no.loss",
        dec = ".",
        quote = "",
        sep = ";",
        header = TRUE,
        stringsAsFactors = FALSE
      ) %>%
      tibble::as_tibble(.data)
    
    return(d)
    
    
  } else{
    
    if(stringr::str_detect(cont, "ERROR")){
      stop(sprintf("Something went wrong. Check input arguments. (%s)",cont))
    }
    
    stop(sprintf(
      "Status code returned was not 200 (status: %s)",
      as.character(response$status_code)
    ))
    
  }

}
