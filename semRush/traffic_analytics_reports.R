#' Request SEMRush Overview Reports
#'
#' @description This function creates a request to the SEMRush SEO database for users with a subscription and sufficient API units.
#' See the SEMRush API website \href{https://www.semrush.com/api-analytics/}{(https://www.semrush.com/api-analytics/)} for additional information, including a list of codes for the regional databases and export variables.
#' @param type \emph{string}. The report type to be generated. Available report types are 'traffic_summary','traffic_sources','traffic_destinations','traffic_geo', or 'traffic_subdomains'.
#' @param key \emph{string}. An identification key assigned to a user after subscribing to SEMrush. The key is available on the Profile page.
#' @param domain \emph{string}. The unique name of a domain that you want to analyze (example.com).
#' @param domains \emph{vector}. A vector of character strings where each element is a unique name of a domain you would like to investigate [c("example-1.com","example-2.com",...)].
#' @param country \emph{string}. The country code parameter ("XX") allows you to filter traffic data for a particular country. If the parameter is not specified, global data is shown by default.
#' @param display_date \emph{string}. Date in format "YYYYMM15". A date on which a report will be shown. You can roll back to the past or choose an actual date.
#' @param display_limit \emph{integer}. The number of results returned to a request. If this parameter is set to 0, up to 10,000 lines will be returned. Default is 5 lines.
#' @param display_offset \emph{integer}. This parameter allows you to skip a specified number of results before sending a report. Please keep in mind that if you use the option display_offset, display_limit, the value should be increased by value display_offset.
#' @param channel_type \emph{string}. The traffic channel filter. This parameter allows you to filter traffic sources by a specific channel type. If the channel type is not specified, data for all channels will be shown by default. Options are 'direct','referral','search','social', or 'paid'.
#' @param device_type \emph{string}. The device filter. This parameter allows you to choose whether to retrieve desktop or mobile data, or both. If the device filter is not specified, data for all devices will be shown by default. Options are 'all', 'desktop', or 'mobile'.
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
traffic_analytics_reports <- function(type, key, domain, domains, #required arguments
                             channel_type, device_type, display_offset, display_limit,
                             country, display_date, export_columns,
                             return_url = FALSE
)
{

  ## Check that universal required arguements are present and valid
  assert_that(noNA(type), not_empty(type), is.string(type),
              noNA(key), not_empty(key), is.string(key))

  ## Check that requested report is a valid choice.
  #A list of the available types of reports that can be generated.
  report_types = c("traffic_summary","traffic_sources","traffic_destinations","traffic_geo","traffic_subdomains")
  assert_that(length(type)==1, is.string(type))
  if(!type %in% report_types){
    stop("Invalid report type requested.")
  }

  ## Check that API key format is valid
  assert_that(length(key)==1, is.string(key))

  ## Match report types with appropriate export columns
  if(type == "traffic_summary"){

    assert_that(noNA(domains), not_empty(domains), all(sapply(domains, is.string)))
    assert_that(!hasArg(domain))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("domain",
                               "display_date",
                               "country",
                               "total_rank",
                               "desktop_rank",
                               "mobile_rank",
                               "desktop_share",
                               "mobile_share",
                               "total_visits",
                               "mobile_visits",
                               "desktop_visits",
                               "total_unique_visitors",
                               "mobile_unique_visitors",
                               "desktop_unique_visitors",
                               "total_pages_per_visit",
                               "mobile_pages_per_visit",
                               "desktop_pages_per_visit",
                               "total_avg_visit_duration",
                               "mobile_avg_visit_duration",
                               "desktop_avg_visit_duration",
                               "total_bounce_rate",
                               "mobile_bounce_rate",
                               "desktop_bounce_rate")

  } #end report type: traffic_summary

  if(type == "traffic_sources"){

    assert_that(noNA(domain), not_empty(domain), is.string(domain))
    assert_that(!hasArg(domains))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("source_domain",
                               "display_date",
                               "country",
                               "traffic_share",
                               "traffic",
                               "traffic_channel")

  } #end report type: traffic_sources

  if(type == "traffic_destinations"){

    assert_that(noNA(domain), not_empty(domain), is.string(domain))
    assert_that(!hasArg(domains))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("destination_domain",
                               "display_date",
                               "country",
                               "traffic_share",
                               "traffic")

  } #end report type: traffic_destinations

  if(type == "traffic_geo"){

    assert_that(noNA(domain), not_empty(domain), is.string(domain))
    assert_that(!hasArg(domains))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("domain",
                               "country",
                               "display_date",
                               "device_type",
                               "total_visits",
                               "total_share",
                               "total_users",
                               "total_bounce_rate",
                               "total_avg_visit_duration",
                               "desktop_visits",
                               "desktop_share",
                               "desktop_users",
                               "desktop_bounce_rate",
                               "desktop_avg_visit_duration",
                               "mobile_visits",
                               "mobile_share",
                               "mobile_users",
                               "mobile_bounce_rate",
                               "mobile_avg_visit_duration")

  } #end report type: traffic_destinations

  if(type == "traffic_subdomains"){

    assert_that(noNA(domain), not_empty(domain), is.string(domain))
    assert_that(!hasArg(domains))

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("domain",
                               "subdomain",
                               "display_date",
                               "device_type",
                               "country",
                               "traffic_share",
                               "desktop_share",
                               "mobile_share",
                               "total_hits",
                               "desktop_hits",
                               "mobile_hits",
                               "total_visits",
                               "desktop_visits",
                               "mobile_visits",
                               "total_users",
                               "desktop_users",
                               "mobile_users")

  } #end report type: traffic_subdomains

  ## Create URL request (base)
  request_url <- paste0("https://api.semrush.com/analytics/ta/v2/?key=",key,
                        "&type=",type)

  ## Append optional parameters to the URL request
  if(hasArg(domain)){
    assert_that(noNA(domain), not_empty(domain), is.string(domain))
    request_url <- paste0(request_url, "&domain=", domain)
  }
  if(hasArg(domains)){
    assert_that(noNA(domains), all(sapply(domains, is.string)))
    if(length(domains)>1){
      domains <- paste0(domains, collapse = ",")
    }
    request_url <- paste0(request_url, "&domains=", domains)
  }
  if(hasArg(country)){
    assert_that(noNA(country), not_empty(country), is.string(country), grepl("^[a-zA-Z]{2}$",country))
    request_url <- paste0(request_url, "&country=", toupper(country))
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
  if(hasArg(device_type)){
    assert_that(noNA(device_type), not_empty(device_type), is.string(device_type), (length(device_type)==1))
    if(!device_type %in% c("all","desktop","mobile")){
      stop("Invalid device type requested. Options are 'all', 'desktop', or 'mobile'. Defaults to 'all' if not specified")
    }
    request_url <- paste0(request_url, "&device_type=", device_type)
  }
  if(hasArg(export_columns)){
    assert_that(noNA(export_columns), !any(!sapply(export_columns, is.string)))
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
      utils::read.table(.data, sep=";", header=TRUE, stringsAsFactors = FALSE) %>%
      tibble::as_tibble
    
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
