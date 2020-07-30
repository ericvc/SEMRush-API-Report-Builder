#' Request SEMRush Advertising Reports
#'
#' @description This function creates a request to the SEMRush SEO database for users with a subscription and sufficient API units.
#'
#' @param type string. The report type to be generated. Available report types are "publisher_text_ads", "publisher_advertisers", "advertiser_publishers", "advertiser_text_ads", "advertiser_landings", "advertiser_publisher_text_ads", "advertiser_rank", or "publisher_rank".
#' @param key string. An identification key assigned to a user after subscribing to SEMrush. The key is available on the Profile page.
#' @param domain 	string. A unique name of a website you would like to investigate (example.com).
#' @param advertiser_domain 	string. A unique name of a website you would like to investigate (example.com). Used for 'advertiser_publisher_text_ads' report only.
#' @param publisher_domain 	string. A unique name of a website you would like to investigate (example.com). Used for 'advertiser_publisher_text_ads' report only.
#' @param display_limit integer. The number of results returned to a request. If this parameter is set to 0, up to 10,000 lines will be returned. Default is 5 lines.
#' @param display_offset integer. This parameter allows you to skip a specified number of results before sending a report. Please keep in mind that if you use the option display_offset, display_limit, the value should be increased by value display_offset.
#' @param device_type string. This parameter shows the type of device by which statistics have been collected-a PC, tablet, or smartphone. Only used for some report types. Options: "all", "desktop", "smartphone_apple", "smartphone_android", "tablet_apple", or "tablet_android".
#' @param export_columns \emph{vector}. A vector of character strings specifying the variables to be included in the report, which vary according to the report type (see 'type' argument). If this parameter is not specified, default columns will be sent.
#' @param return_url logical. If TRUE, prints the request URL used to generate the report. Default value is FALSE.
#' @param timestmap logical. If TRUE (default), converts 
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
#'key <- "" #enter your SEMRush account API key.
#'
#'##Get 'backlinks_overview' report
#'advertising_reports(
#' type = "advertiser_landings",
#' domain = "cran.r-project.org",
#' key = key,
#' device_type = "all",
#' return_url = FALSE
#')
#'
#'}
advertising_reports <-
  function(type,
           key,
           domain,
           advertiser_domain,
           publisher_domain,
           display_limit = 5,
           display_offset,
           device_type,
           export_columns,
           return_url = FALSE,
           timestamp = TRUE)
  {
    ## All domain_report types require an API key and regional database to be specified. All will accept
    #optional arguments display_limit and display_offset
    
    ## Set defaults
    display_limit <- display_limit
    
    ## Check that universal required arguements are present and valid
    assert_that(
      noNA(type),
      not_empty(type),
      is.string(type),
      noNA(key),
      not_empty(key),
      is.string(key)
    )
    
    ## Check that requested report is a valid choice.
    #A list of the available types of reports that can be generated.
    report_types <- c(
      "publisher_text_ads",
      "publisher_advertisers",
      "advertiser_publishers",
      "advertiser_text_ads",
      "advertiser_landings",
      "advertiser_publisher_text_ads",
      "advertiser_rank",
      "publisher_rank"
    )
	
    assert_that(length(type) == 1, is.string(type))
    if (!type %in% report_types) {
      stop("Invalid report type requested.")
    }
    ## Check that API key format is valid
    assert_that(length(key) == 1, is.string(key))
    
    ## Match report types with appropriate export columns
    if (type == "publisher_text_ads") {
      assert_that(noNA(domain), not_empty(domain), is.string(domain))
      
      ## Check requested data to ensure it matches selected report type
      #List of valid export columns for this report type
      export_columns_default = c("title",
                                 "text",
                                 "first_seen",
                                 "last_seen",
                                 "times_seen",
                                 "visible_url")
      
    } #end report type: publisher_text_ads
    
    if (type %in% c("publisher_advertisers", "advertiser_publishers")) {
      assert_that(noNA(domain), not_empty(domain), is.string(domain))
      
      ## Check requested data to ensure it matches selected report type
      #List of valid export columns for this report type
      export_columns_default = c("domain",
                                 "ads_count",
                                 "first_seen",
                                 "last_seen",
                                 "times_seen")
      
    } #end report type: advertiser_publishers
    
    if (type == "advertiser_text_ads") {
      assert_that(noNA(domain), not_empty(domain), is.string(domain))
      
      ## Check requested data to ensure it matches selected report type
      #List of valid export columns for this report type
      export_columns_default = c("title",
                                 "text",
                                 "first_seen",
                                 "last_seen",
                                 "times_seen",
                                 "visible_url")
      
    } #end report type: advertiser_text_ads
    
    if (type == "advertiser_landings") {
      assert_that(noNA(domain), not_empty(domain), is.string(domain))
      
      ## Check requested data to ensure it matches selected report type
      #List of valid export columns for this report type
      export_columns_default = c("target_url",
                                 "first_seen",
                                 "last_seen",
                                 "times_seen",
                                 "ads_count")
      
    } #end report type: advertiser_landings
    
    if (type == "advertiser_publisher_text_ads") {
      #check advertiser_domain argument
      assert_that(
        noNA(advertiser_domain),
        not_empty(advertiser_domain),
        is.string(advertiser_domain)
      )
      #check publisher_domain argument
      assert_that(
        noNA(publisher_domain),
        not_empty(publisher_domain),
        is.string(publisher_domain)
      )
      
      ## Check requested data to ensure it matches selected report type
      #List of valid export columns for this report type
      export_columns_default = c("title",
                                 "text",
                                 "first_seen",
                                 "last_seen",
                                 "times_seen",
                                 "visible_url")
      
    } #end report type: advertiser_publisher_text_ads
    
    if (type %in% c("advertiser_rank", "publisher_rank")) {
      assert_that(noNA(domain), not_empty(domain), is.string(domain))
      
      ## Check requested data to ensure it matches selected report type
      #List of valid export columns for this report type
      export_columns_default = c(
        "domain",
        "ads_overall",
        "text_ads_overall",
        "media_ads_overall",
        "first_seen",
        "last_seen",
        "times_seen",
        "domain_overall"
      )
      
    } #end report type: advertiser_rank
    
    ## Create URL request (base)
    request_url <-
      paste0(
        "https://api.semrush.com/analytics/da/v2/?",
        "&action=report",
        "&key=",
        key,
        "&type=",
        type
      )
    
    ## Append optional parameters to the URL request
    if (hasArg(domain)) {
      assert_that(noNA(domain), not_empty(domain), is.string(domain))
      request_url <- paste0(request_url, "&domain=", domain)
    }
    if (hasArg(advertiser_domain)) {
      assert_that(
        noNA(advertiser_domain),
        not_empty(advertiser_domain),
        is.string(advertiser_domain)
      )
      request_url <-
        paste0(request_url, "&advertiser_domain=", advertiser_domain)
    }
    if (hasArg(publisher_domain)) {
      assert_that(
        noNA(publisher_domain),
        not_empty(publisher_domain),
        is.string(publisher_domain)
      )
      request_url <-
        paste0(request_url, "&publisher_domain=", publisher_domain)
    }
    if (hasArg(display_limit)) {
      assert_that(display_limit == as.integer(display_limit))
      request_url <-
        paste0(request_url, "&display_limit=", display_limit)
    }
    if (hasArg(display_offset)) {
      assert_that(display_offset == as.integer(display_offset))
      request_url <-
        paste0(request_url, "&display_offset=", display_offset)
    }
    if (hasArg(device_type)) {
      assert_that(
        is.string(device_type),
        device_type %in% c(
          "all",
          "desktop",
          "smartphone_apple",
          "smartphone_android",
          "tablet_apple",
          "tablet_android"
        )
      )
      request_url <-
        paste0(request_url, "&device_type=", device_type)
    }
    if (hasArg(export_columns)) {
      assert_that(noNA(export_columns), all(sapply(export_columns, is.string)))
      if (any(!export_columns %in% export_columns_default)) {
        stop("Invalid export columns for requested report type.")
      }
      if (length(export_columns) > 1) {
        export_columns <- paste0(export_columns, collapse = ",")
      }
      request_url <-
        paste0(request_url, "&export_columns=", export_columns)
    }
    
    if (return_url) {
      print(paste0("Request URL: ", request_url))
    }
    
    ## Use request_url to get data from API
    
    response <- httr::GET(request_url)
    #get content from response
    cont <- httr::content(response, as = "text")
    #check response code status
    if (response$status_code == 200) {
      if (stringr::str_detect(cont, "ERROR")) {
        stop(sprintf("Something went wrong. Check input arguments. (%s)", cont))
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
      
      #option to convert timestamps (milliseconds) to POSIX format if present in the report
      if(timestamp & all(c("first_seen", "last_seen") %in% names(d))){
        d$first_seen = .POSIXct(d$first_seen / 1000, tz = "UTC") %>% as.character()
        d$last_seen = .POSIXct(d$last_seen / 1000, tz = "UTC") %>% as.character()
      }
      
      return(d)
      
      
    } else{
      stop(sprintf(
        "Status code returned was not 200 (status: %s)", #returns status code other than 200
        as.character(response$status_code)
      ))
      
    }
    
  }
