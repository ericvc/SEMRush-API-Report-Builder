#' Request SEMRush Overview Reports
#'
#' @description This function creates a request to the SEMRush SEO database for users with a subscription and sufficient API units.
#' See the SEMRush API website \href{https://www.semrush.com/api-analytics/}{(https://www.semrush.com/api-analytics/)} for additional information, including a list of codes for the regional databases and export variables.
#' @param type \emph{string}. The report type to be generated. Available report types are 'domain_ranks','domain_rank','domain_rank_history','rank_difference', or 'rank'.
#' @param key \emph{string}. An identification key assigned to a user after subscribing to SEMrush. The key is available on the Profile page.
#' @param domain 	\emph{string}. A unique name of a website you would like to investigate (example.com).
#' @param database 	\emph{string}. A regional database. If this parameter is not specified (i.e., ""), your request will be sent to all regional databases.
#' @param display_limit \emph{integer}. The number of results returned to a request. If this parameter is set to 0, up to 10,000 lines will be returned. Default is 5 lines.
#' @param display_offset \emph{integer}. This parameter allows you to skip a specified number of results before sending a report. Please keep in mind that if you use the option display_offset, display_limit, the value should be increased by value display_offset.
#' @param display_date \emph{string}. Date in format "YYYYMM15". A date on which a report will be shown. You can roll back to the past or choose an actual date.
#' @param display_daily \emph{integer}. Only used for some report types. This parameter allows you to get daily updates on position changes that occurred in the last 30 days or more. If the parameter is not specified, your report will show monthly results for the current month and for previous months.
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
#'\dontrun{
#'key <- ""
#'
#'## Get 'domain_ranks' report for a single domain
#'report <- overview_reports(
#'   type = "domain_ranks",
#'   key = key,
#'   domain = "cran.r-project.org",
#'   display_limit = 5,
#'   export_columns = c("Db", "Dt", "Dn", "Rk")
#')
#'
#'print(report)
#'# A tibble: 5 x 4
#' Database     Date Domain          Rank
#' <chr>       <int> <chr>          <int>
#'1 ph       20200617 r-project.org   353
#'2 ge       20200617 r-project.org   906
#'3 kh       20200617 r-project.org   971
#'4 is       20200616 r-project.org  1019
#'5 ma       20200616 r-project.org  1034
#'
#'## Get 'rank' report
#'report <- overview_reports(
#'   type = "rank",
#'   key = key,
#'   domain = "cran.r-project.org",
#'   database = "us", #United States
#'   display_limit = 5,
#'   export_columns = c("Dn", "Rk", "Or", "Ot")
#')
#'
#'print(report)
#'# A tibble: 5 x 5
#'  Keyword                    Position Previous.Position Search.Volume   CPC
#'  <chr>                         <int>             <int>         <int> <dbl>
#'1 cran                              1                 1          8100  1.42
#'2 r cran                            1                 1          5400  0   
#'3 r cran download                   1                 1           590  0   
#'4 cran r project org windows        1                 1           590  0   
#'5 cran r project                    1                 1           480  0   
#'}
overview_reports <- function(type, key, domain, #required arguments
                             database, display_limit=5, display_offset,
                             display_date, export_columns, display_daily,
                             return_url = FALSE
                             )
{

  ## Set defaults
  display_limit <- display_limit

  ## Check that universal required arguements are present and valid
  assert_that(noNA(type), not_empty(type), is.string(type),
              noNA(key), not_empty(key), is.string(key))

  ## Check that requested report is a valid choice.
  #A list of the available types of reports that can be generated.
  report_types = c("domain_ranks","domain_rank","domain_rank_history","rank_difference","rank")
  assert_that(length(type)==1, is.string(type))
  if(!type %in% report_types){
    stop("Invalid report type requested.")
  }

  ## Check that API key format is valid
  assert_that(length(key)==1, is.string(key))

  ## Match report types with appropriate export columns
  if(type == "domain_ranks"){

    assert_that(noNA(domain), not_empty(domain), is.string(domain))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Db","Dt","Dn","Rk","Or","Ot","Oc","Ad","At","Ac","Sh","Sv","FKn","FPn")

  } #end report type: domain_ranks

  if(type == "domain_rank"){

    ## Check that report-specific parameters are valid
    assert_that(noNA(domain), not_empty(domain), is.string(domain))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Dn","Rk","Or","Xn","Ot","Oc","Ad","At","Ac","FKn","FPn")

    if(!hasArg(database)){
      stop("You must select a regional database for a report of type 'domain_rank'.")
    }

  } #end report type: domain_rank

  if(type == "domain_rank_history"){

    ## Check that report-specific parameters are valid
    ## Check that report-specific parameters are valid
    if(!hasArg(database)){
      stop("You must select a regional database for a report of type 'rank_difference'.")
    }
    assert_that(
      noNA(domain), not_empty(domain), is.string(domain)
    )

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Rk","Or","Xn","Ot","Oc","Ad","At","Ac","Dt","FKn","FPn")

  } #end report type: domain_rank_history

  if(type == "rank_difference"){

    ## Check that report-specific parameters are valid
    if(!hasArg(database)){
      stop("You must select a regional database for a report of type 'rank_difference'.")
    }

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Dn","Rk","Or","Ot","Oc","Ad","At","Ac","Om","Tm","Um","Am","Bm","Cm")

  } #end report type: rank_difference

  if(type == "rank"){

    ## Check that report-specific parameters are valid
    if(!hasArg(database)){
      stop("You must select a regional database for a report of type 'rank'.")
    }

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Dn","Rk","Or","Ot","Oc","Ad","At","Ac")

  } #end report type: rank

  ## Create URL request (base)
  request_url <- paste0("https://api.semrush.com/?key=",key,
                        "&type=",type)

  ## Append optional parameters to the URL request
  if(hasArg(domain)){
    assert_that(noNA(domain), not_empty(domain), is.string(domain))
    request_url <- paste0(request_url, "&domain=", domain)
  }
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
  if(hasArg(display_daily)){
    assert_that(display_daily == as.integer(display_daily) & display_daily >= 1 & display_daily <= 30)
    request_url <- paste0(request_url, "&display_daily=", display_daily)
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

} #end function
