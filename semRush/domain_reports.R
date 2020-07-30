#' Request SEMRush Domain Reports
#'
#' @description This function creates a request to the SEMRush SEO database for users with a subscription and sufficient API units.
#'
#' @param type string. The report type to be generated. Available report types are 'domain_organic', 'domain_adwords', 'domain_adwords_unique', 'domain_adwords_adwords', 'domain_adwords_historical', 'domain_domains', 'domain_shopping', 'domain_shopping_unique', 'domain_shopping_shopping', 'domain_organic_unique', and 'domain_organic_subdomains'.
#' @param key string. An identification key assigned to a user after subscribing to SEMrush. The key is available on the Profile page.
#' @param domain 	string. A unique name of a website you would like to investigate (example.com).
#' @param domains vector. A vector of strings for each domain name that contains domains in a specified format, separated by "|". This format requires a domain to consist of <sign>|<type>|<domain>, where the <sign> means possible operations: "+", "-", "*", "/"; <type> has to take the value of "or" for organic keywords or "ad" for paid keywords; and( <domain> indicates a domain.
#' @param database 	string. A regional database. If this parameter is not specified (i.e., ""), your request will be sent to all regional databases. Default is the United States.
#' @param display_limit integer. The number of results returned to a request. If this parameter is set to 0, up to 10,000 lines will be returned. Default is 5 lines.
#' @param display_offset integer. This parameter allows you to skip a specified number of results before sending a report. Please keep in mind that if you use the option display_offset, display_limit, the value should be increased by value display_offset.
#' @param display_date string. Date in format "YYYYMM15". A date on which a report will be shown. You can roll back to the past or choose an actual date.
#' @param display_daily integer. Only used for some report types. This parameter allows you to get daily updates on position changes that occurred in the last 30 days or more. If the parameter is not specified, your report will show monthly results for the current month and for previous months.
#' @param export_columns \emph{vector}. A vector of character strings specifying the variables to be included in the report, which vary according to the report type (see 'type' argument). If this parameter is not specified, default columns will be sent.
#' @param return_url logical. If TRUE, prints the request URL used to generate the report. Default value is FALSE.
#' @return A data table (tibble) with columns for each requested variable.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat noNA
#' @importFrom assertthat is.string
#' @importFrom assertthat not_empty
#' @importFrom methods hasArg
#' @import tibble
#' @importFrom rlang .data
#' 
#' @examples
#'## Enter your SEMRush account API key
#'\dontrun{
#'key <- ""
#'
#'## Request example for domain_domains
#'
#'#define 'domains' argument (see https://www.semrush.com/api-analytics/#domain_domains)
#'domains <- "*|or|nike.com|*|or|adidas.com|*|or|reebok.com"
#' 
#'#Use function to request report
#'report <-
#' domain_reports(
#'   type = "domain_domains",
#'   key = key,
#'   domains = domains,
#'   database = "us",
#'   export_columns = c("Ph", "Nq", "Kd", "Co")
#')
#'
#'print(report)
#'# A tibble: 5 x 4
#'  Keyword          Search.Volume Keyword.Difficulty Competition
#'  <chr>                    <int>              <dbl>       <dbl>
#'1 shoes                  1500000               90.3        1   
#'2 basketball shoes        368000               89.8        1   
#'3 women                   368000               87.1        0.12
#'4 man                     301000               87          0   
#'5 shoes for men           301000               90.2        1
#'}   
domain_reports <- function(type, key, domain, domains,#required arguments
                           database="us", display_limit=5, display_offset,
                           display_date, export_columns, display_daily, return_url=FALSE,
                           ...)
{

  ## All domain_report types require an API key and regional database to be specified. All will accept
  #optional arguments display_limit and display_offset

  ## Set defaults
  display_limit <- display_limit

  ## Check that universal required arguements are present and valid
  assert_that(noNA(type), not_empty(type), is.string(type),
              noNA(key), not_empty(key), is.string(key),
              noNA(database), not_empty(database), is.string(database))

  ## Check that requested report is a valid choice.
  #A list of the available types of reports that can be generated.
  report_types = c("domain_organic",
                   "domain_organic_organic",
                   "domain_adwords",
                   "domain_adwords_unique",
                   "domain_adwords_adwords",
                   "domain_adwords_historical",
                   "domain_domains",
                   "domain_shopping",
                   "domain_shopping_unique",
                   "domain_shopping_shopping",
                   "domain_organic_unique",
                   "domain_organic_subdomains"
  )
  assert_that(length(type)==1, is.string(type))
  if(!type %in% report_types){
    stop("Invalid report type requested.")
  }

  ## Check that API key format is valid
  assert_that(length(key)==1, is.string(key))

  ## Match report types with appropriate export columns
  if(type == "domain_organic"){

    assert_that(noNA(domain), not_empty(domain), is.string(domain))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Ph","Po","Pp","Pd","Nq","Cp","Ur","Tr","Tg","Tc","Co","Nr","Td","Kd","Fp","Fk","Ts")

  } #end report type: domain_organic
  
  if(type == "domain_organic_organic"){
    
    assert_that(noNA(domain), not_empty(domain), is.string(domain))
    
    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Dn","Cr","Np","Or","Ot","Oc","Ad")
    
  } #end report type: domain_organic

  ## Match report types with appropriate export columns
  if(type == "domain_adwords"){

    assert_that(noNA(domain), not_empty(domain), is.string(domain))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Ph","Po","Pp","Pd","Ab","Nq","Cp","Tg","Tr","Tc","Co","Nr","Td","Tt","Ds","Vu","Ur","Ts","Un")

  } #end report type: domain_adwords

  if(type == "domain_adwords_unique"){

    assert_that(noNA(domain), not_empty(domain), is.string(domain))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Ph","Un","Tt","Ds","Vu","Ur","Pc","Ts")

  } #end report type: domain_adwords_unique

  if(type == "domain_adwords_adwords"){

    assert_that(noNA(domain), not_empty(domain), is.string(domain))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Dn","Cr","Np","Or","Ot","Oc","Ad")

  } #end report type: domain_adwords_adwords

  if(type == "domain_adwords_historical"){

    assert_that(noNA(domain), not_empty(domain), is.string(domain))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Ph","Dt","Po","Cp","Nq","Tr","Ur","Tt","Ds","Vu","Cv")

  } #end report type: domain_adwords_historical

  if(type == "domain_domains"){

    assert_that(noNA(domains), not_empty(domains), is.string(domains))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Ph","P0","P1","P2","P3","P4","Nr","Cp","Nq","Kd","Co","Td")

  } #end report type: domain_domains

  if(type == "domain_shopping"){

    assert_that(noNA(domain), not_empty(domain), is.string(domain))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Ph","Po","Pp","Pd","Nq","Sn","Ur","Tt","Pr","Ts")

  } #end report type: domain_shopping

  if(type == "domain_shopping_unique"){

    assert_that(noNA(domain), not_empty(domain), is.string(domain))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Tt","Pr","Ur","Pc","Un","Ts")

  } #end report type: domain_shopping_unique

  if(type == "domain_shopping_shopping"){

    assert_that(noNA(domain), not_empty(domain), is.string(domain))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Dn","Cr","Np","Sh","Ad","At","Ac","Or")

  } #end report type: domain_shopping_shopping

  if(type == "domain_shopping_shopping"){

    assert_that(noNA(domain), not_empty(domain), is.string(domain))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Dn","Cr","Np","Sh","Ad","At","Ac","Or")

  } #end report type: domain_shopping_shopping

  if(type == "domain_organic_unique" | type == "domain_organic_subdomains"){

    assert_that(noNA(domain), not_empty(domain), is.string(domain))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("Ur","Pc","Tg","Tr")

  } #end report type: domain_shopping_shopping

  ## Create URL request (base)
  request_url <- paste0("https://api.semrush.com/?key=",key,
                        "&type=",type)

  ## Append optional parameters to the URL request
  if(hasArg(domain)){
    assert_that(noNA(domain), not_empty(domain), is.string(domain))
    request_url <- paste0(request_url, "&domain=", domain)
  }
  if(hasArg(domains)){
    assert_that(noNA(domains), not_empty(domains), is.string(domains))
    request_url <- paste0(request_url, "&domains=", domains)
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
    print(d)
    
    
  } else{
    
    stop(sprintf(
      "Status code returned was not 200 (status: %s)",
      as.character(response$status_code)
    ))
    
  }

}
