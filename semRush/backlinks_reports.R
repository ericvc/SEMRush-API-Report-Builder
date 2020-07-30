#' Request SEMRush Backlinks Reports
#'
#' @description This function creates a request to the SEMRush SEO database for users with a subscription and sufficient API units.
#' See the SEMRush API website \href{https://www.semrush.com/api-analytics/}{(https://www.semrush.com/api-analytics/)} for additional information, including a list of codes for the regional databases and export variables.
#' @param type \emph{string}. The report type to be generated. Available report types are "backlinks_overview",'backlinks','backlinks_refdomains',
#' 'backlinks_refips',
#' 'backlinks_tld',
#' 'backlinks_geo',
#' 'backlinks_anchors',
#' 'backlinks_pages',
#' 'backlinks_competitors',
#' 'backlinks_matrix',
#' 'backlinks_comparison',
#' 'backlinks_ascore_profile',
#' 'backlinks_categories_profile',
#' 'backlinks_categories', or
#' 'backlinks_historical'.
#' @param key \emph{string}. An identification key assigned to a user after subscribing to SEMrush. The key is available on the Profile page.
#' @param target 	\emph{string}. A unique name of a website you would like to investigate (example.com).
#' @param targets 	\emph{vector}. A vector of items (character strings), where an item is a root domain, domain or URL. Formatting: targets[]=example.com&targets[]=www.example.com&targets[]=http://www.example.com.
#' @param target_type \emph{string}. A type of requested target, one of 'root_domain', 'domain',  or 'url'.
#' @param target_types \emph{vector}. A vector of items (character strings), where an item is a type of requested target specified in the parameter "targets[]". Formatting: target_types[]=root_domain&target_types[]=domain&target_types[]=url.
#' @param display_limit \emph{integer}. The number of results returned to a request. If this parameter is set to 0, up to 10,000 lines will be returned. Default is 5 lines.
#' @param display_offset \emph{integer}. This parameter allows you to skip a specified number of results before sending a report. Please keep in mind that if you use the option display_offset, display_limit, the value should be increased by value display_offset.
#' @param timespan \emph{string}. One of 'weeks' or 'months'. Weeks - returns weekly historical dates over the period of last six months, so 27 lines maximum (if display_limit isn’t specified; otherwise returns number of most recent weekly historical dates specified in display_limit). Months - returns monthly historical dates over all the available historical period (if display_limit isn’t specified; otherwise returns number of most recent monthly historical dates specified in display_limit). Default: weeks
#' @param export_columns \emph{vector}. A vector of character strings specifying the variables to be included in the report, which vary according to the report type (see 'type' argument). If this parameter is not specified, default columns will be sent.
#' @param return_url logical. If TRUE, prints the request URL used to generate the report. Default value is FALSE.
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat noNA
#' @importFrom assertthat is.string
#' @importFrom assertthat not_empty
#' @importFrom methods hasArg
#' @import tibble
#' @importFrom rlang .data
#'
#'
#' @return A data table (tibble) with columns for each requested variable.
#' @export
#'
#' @examples
#'\dontrun{
#'key <- "" #enter your SEMRush account API key.
#'
#'##Get 'backlinks_overview' report
#'backlinks_reports(
#'   type = "backlinks_overview",
#'   target = "cran.r-project.org",
#'   key = key,
#'   target_type = "domain",
#'   return_url = FALSE
#' )
#'
#'##Get 'backlinks_comparison' report
#'backlinks_reports(
#'   type = "backlinks_comparison",
#'   targets = c("cran.r-project.org", "cran.r-project.org"),
#'   key = key,
#'   target_types = c("root_domain", "domain"),
#'   return_url = FALSE
#' )
#'}
backlinks_reports <- function(type, key, target, targets, target_type, target_types,#required arguments
                             display_limit=5, display_offset,
                             export_columns, timespan = "weeks",
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
  report_types = c("backlinks_overview",
                   "backlinks",
                   "backlinks_refdomains",
                   "backlinks_refips",
                   "backlinks_tld",
                   "backlinks_geo",
                   "backlinks_anchors",
                   "backlinks_pages",
                   "backlinks_competitors",
                   "backlinks_matrix",
                   "backlinks_comparison",
                   "backlinks_ascore_profile",
                   "backlinks_categories_profile",
                   "backlinks_categories",
                   "backlinks_historical")

  assert_that(length(type)==1, is.string(type))
  if(!type %in% report_types){
    stop("Invalid report type requested.")
  }

  ## Check that API key format is valid
  assert_that(length(key)==1, is.string(key))

  ## Match report types with appropriate export columns
  if(type == "backlinks_overview"){

    assert_that(
      noNA(target),
      not_empty(target),
      is.string(target),
      noNA(target_type),
      not_empty(target_type),
      is.string(target_type),
      target_type %in% c("root_domain", "domain", "url")
    )

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c(
      "ascore",
      "total",
      "domains_num",
      "urls_num",
      "ips_num",
      "ipclassc_num",
      "follows_num",
      "nofollows_num",
      "sponsored_num",
      "ugc_num",
      "texts_num",
      "images_num",
      "forms_num",
      "frames_num"
    )

  } #end report type: backlinks_overview

  if(type == "backlinks"){

    assert_that(
      noNA(target),
      not_empty(target),
      is.string(target),
      noNA(target_type),
      not_empty(target_type),
      is.string(target_type),
      target_type %in% c("root_domain", "domain", "url")
    )

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c(
      "page_ascore",
      "response_code",
      "source_size",
      "external_num",
      "internal_num",
      "redirect_url",
      "source_url",
      "source_title",
      "image_url",
      "target_url",
      "target_title",
      "anchor",
      "image_alt",
      "last_seen",
      "first_seen",
      "nofollow",
      "form",
      "frame",
      "image",
      "sitewide",
      "newlink",
      "lostlink"
    )

  } #end report type: backlinks

  if(type == "backlinks_refdomains"){

    assert_that(
      noNA(target),
      not_empty(target),
      is.string(target),
      noNA(target_type),
      not_empty(target_type),
      is.string(target_type),
      target_type %in% c("root_domain", "domain", "url")
    )

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c(
      "domain_ascore",
      "domain",
      "backlinks_num",
      "ip",
      "country",
      "first_seen",
      "last_seen"
    )

  } #end report type: backlinks_refdomains

  if(type == "backlinks_refips"){

    assert_that(
      noNA(target),
      not_empty(target),
      is.string(target),
      noNA(target_type),
      not_empty(target_type),
      is.string(target_type),
      target_type %in% c("root_domain", "domain", "url")
    )

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("ip",
                               "country",
                               "domains_num",
                               "backlinks_num",
                               "first_seen",
                               "last_seen"
                               )

  } #end report type: backlinks_refips

  if(type == "backlinks_tld"){

    assert_that(
      noNA(target),
      not_empty(target),
      is.string(target),
      noNA(target_type),
      not_empty(target_type),
      is.string(target_type),
      target_type %in% c("root_domain", "domain", "url")
    )

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("zone",
                               "domains_num",
                               "backlinks_num"
    )

  } #end report type: backlinks_tld

  if(type == "backlinks_geo"){

    assert_that(
      noNA(target),
      not_empty(target),
      is.string(target),
      noNA(target_type),
      not_empty(target_type),
      is.string(target_type),
      target_type %in% c("root_domain", "domain", "url")
    )

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("country",
                               "domains_num",
                               "backlinks_num"
    )

  } #end report type: backlinks_geo

  if(type == "backlinks_anchors"){

    assert_that(
      noNA(target),
      not_empty(target),
      is.string(target),
      noNA(target_type),
      not_empty(target_type),
      is.string(target_type),
      target_type %in% c("root_domain", "domain", "url")
    )

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c("anchor",
                               "domains_num",
                               "backlinks_num",
                               "first_seen",
                               "last_seen"
    )

  } #end report type: backlinks_anchors

  if(type == "backlinks_pages"){

    assert_that(
      noNA(target),
      not_empty(target),
      is.string(target),
      noNA(target_type),
      not_empty(target_type),
      is.string(target_type),
      target_type %in% c("root_domain", "domain", "url")
    )

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c(
      "source_url",
      "source_title",
      "response_code",
      "backlinks_num",
      "domains_num",
      "last_seen",
      "external_num",
      "internal_num"
    )

  } #end report type: backlinks_pages

  if(type == "backlinks_competitors"){

    assert_that(
      noNA(target),
      not_empty(target),
      is.string(target),
      noNA(target_type),
      not_empty(target_type),
      is.string(target_type),
      target_type %in% c("root_domain", "domain", "url")
    )

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c(
      "ascore",
      "neighbour",
      "similarity",
      "common_refdomains",
      "domains_num",
      "backlinks_num"
    )

  } #end report type: backlinks_competitors

  if(type == "backlinks_matrix"){

    assert_that(
      noNA(targets),
      not_empty(targets),
      all(sapply(targets, is.string)),
      noNA(target_types),
      not_empty(target_types),
      all(sapply(target_types, is.string)),
      all(target_types %in% c("root_domain", "domain", "url")),
      length(targets) == length(target_types)
    )

    targets <- paste0("&targets[]=",targets)
    target_types <- paste0("&target_types[]=",target_types)

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c(
      "domain",
      "domain_ascore",
      "matches_num",
      "backlinks_num"
    )

  } #end report type: backlinks_matrix

  if(type == "backlinks_comparison"){

    assert_that(
      noNA(targets),
      not_empty(targets),
      all(sapply(targets, is.string)),
      noNA(target_types),
      not_empty(target_types),
      all(sapply(target_types, is.string)),
      all(target_types %in% c("root_domain", "domain", "url")),
      length(targets) == length(target_types)
    )

    targets <- paste0("&targets[]=",targets)
    target_types <- paste0("&target_types[]=",target_types)

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c(
      "target",
      "target_type",
      "ascore",
      "backlinks_num",
      "domains_num",
      "ips_num",
      "follows_num",
      "nofollows_num",
      "texts_num",
      "images_num",
      "forms_num",
      "frames_num"
    )

  } #end report type: backlinks_comparison

  if(type == "backlinks_ascore_profile"){

    assert_that(
      noNA(target),
      not_empty(target),
      is.string(target),
      noNA(target_type),
      not_empty(target_type),
      is.string(target_type),
      target_type == "root_domain"
    )

  } #end report type: backlinks_ascore_profile

  if(type == "backlinks_categories_profile" | type == "backlinks_categories"){

    assert_that(
      noNA(target),
      not_empty(target),
      is.string(target),
      noNA(target_type),
      not_empty(target_type),
      is.string(target_type),
      target_type == "root_domain"
    )

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c(
      "category_name",
      "rating"
    )

  } #end report type: backlinks_categories_profile

  if(type == "backlinks_historical"){

    assert_that(
      noNA(target),
      not_empty(target),
      is.string(target),
      noNA(target_type),
      not_empty(target_type),
      is.string(target_type),
      target_type == "root_domain"
    )

    ## Check requested data to ensure it matches selected report type
    #List of valid export columns for this report type
    export_columns_default = c(
      "date",
      "backlinks_num",
      "backlinks_new_num",
      "backlinks_lost_num",
      "domains_num",
      "domains_new_num",
      "domains_lost_num"
    )

  } #end report type: backlinks_categories_profile

  ## Create URL request (base)
  request_url <- paste0("https://api.semrush.com/analytics/v1/?key=",key,
                        "&type=",type)

  ## Append optional parameters to the URL request
  if(hasArg(target)){
    assert_that(noNA(target), not_empty(target), is.string(target))
    request_url <- paste0(request_url, "&target=", target)
  }
  if(hasArg(targets)){
    request_url <- paste0(request_url, paste0(targets,collapse=""))
  }
  if(hasArg(target_type)){
    assert_that(noNA(target_type), not_empty(target_type), is.string(target_type))
    request_url <- paste0(request_url, "&target_type=", target_type)
  }
  if(hasArg(target_types)){
    request_url <- paste0(request_url, paste0(target_types,collapse=""))
  }
  if(hasArg(display_limit)){
    assert_that(display_limit == as.integer(display_limit))
    request_url <- paste0(request_url, "&display_limit=", display_limit)
  }
  if(hasArg(display_offset)){
    assert_that(display_offset == as.integer(display_offset))
    request_url <- paste0(request_url, "&display_offset=", display_offset)
  }
  if(hasArg(timespan)){
    assert_that(is.string(timespan), timespan %in% c("weeks","months"))
    request_url <- paste0(request_url, "&timespan=", timespan)
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
    
    stop(sprintf(
      "Status code returned was not 200 (status: %s)",
      as.character(response$status_code)
    ))
    
  }

}
