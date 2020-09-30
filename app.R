#Uncomment to install semRush package
# devtools::install_github("ericvc/semRush")


## SEMRush Report Builder (v1):
#This R Shiny app simplifies the creation of customized SEO / marketing reports from
#data obtained through the SEMRush REST API. Five report topics (advertising, backlinks, domain, keyword, and overview)
#are supported. For each report topic, the menu options are displayed dynamically (and conditionally) using tabsetPanel
#functionality. The sidebar menu for each report topic is defined by its own tabPanel function. Although some report
#topics share input variables (e.g., "database"), the input variables for each topic are unique for the purposes of
#creating this app. Input and UI variables will not work properly if they appear in more  than one tabPanel group.


## Load packages
library(tidyverse)
library(shiny)
library(shinyjs)
library(dashboardthemes)
library(shinydashboard)
library(assertthat)
library(semRush)


## Attach API keys
#See the "api_keys_example.json" for template
attach(jsonlite::read_json("api_keys.json"))


## Get menu content
db_codes = read.csv(
  "menu/semrush_database_codes.csv",
  encoding = "UTF-8",
  header = TRUE,
  stringsAsFactors = FALSE
)

menuOpts = read.csv("menu/sidebar_menu_options.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)


## Define body (UI)
body <- dashboardBody(
  shinyjs::useShinyjs(),
  tags$head(tags$style(
    HTML(
      ".checkbox-inline {
      margin-left: 0px;
      margin-right: 0px;
      -webkit-column-count: 3; /* Chrome, Safari, Opera */
      -moz-column-count: 3; /* Firefox */
      column-count: 3;
      }
      .checkbox-inline+.checkbox-inline {
      margin-left: 0px;
      margin-right: 0px;
      }
      "
    )
  )),
  shinyDashboardThemes(theme = "grey_light"),
  fluidPage(
    style = "height:100%;margin-left:6%;margin-right:6%",
    HTML(
      "<p style='font-size:18px'>Advertising Report generator. Select inputs for each feature or leave blank for default settings. Query the SEMRush data API and download a tabulated report. The generated report can be saved as a .CSV file using the 'Save' button.</p>"
    ),
    shinyjs::hidden(actionButton(
      "submit",
      "Submit",
      icon = icon("table"),
      width = "0%"
    ))
  ),
  fluidRow(
    style = "height:100%;margin-left:7%;margin-right:7%",
    HTML(
      "<b><a style='color:black' href='https://www.semrush.com/api-analytics/#columns'>SEMRush API Export Column Details</a><b>"
    ),
    br(),
    uiOutput("account_balance")
  ),
  fluidRow(
    style = "height:100%;margin-left:4%;margin-right:4%",
    br(),
    br(),
    DT::dataTableOutput(outputId = "Table", width = "100%"),
    downloadButton("dl_results", "Save as .csv")
  )
)


## Define sidebar (UI)
sidebar <- dashboardSidebar(
  width = "342px",
  h5(selectInput("report", "Topic", choices = sort(
    unique(menuOpts$report)
  ))),
  tabsetPanel(
    id = "switcher",
    type = "hidden",
    tabPanel(
      "advertising",
      h5(
        selectInput(
          "type_adv",
          "Type (select one)",
          choices = NULL,
          multiple = FALSE,
          size = 1,
          selectize = FALSE,
          selected = NULL
        ),
        br(),
        uiOutput("desc_adv"),
        br(),
        uiOutput("req_adv"),
        textInput("domain_adv", "Domain", placeholder = "example.com"),
        textInput("advertiser_domain", "Advertiser", placeholder = "example.com"),
        textInput("publisher_domain", "Publisher", placeholder = "example.com"),
        splitLayout(
          tags$head(tags$style(
            HTML("
                 .shiny-split-layout > div {
                 overflow: visible;
                 }
                 ")
          )),
          cellWidths = c("0%", "50%", "35%"),
          selectInput(
            "device_type_adv",
            "Device Type",
            selected = "all",
            choices = c(
              "all",
              "desktop",
              "smartphone_apple",
              "smartphone_android",
              "tablet_apple",
              "tablet_android"
            ),
            size = 1,
            selectize = FALSE
          ),
          numericInput(
            "display_limit_adv",
            "Display Limit",
            value = 5,
            min = 1,
            max = 1e3
          )
        ),
        selectInput(
          choices = "",
          multiple = TRUE,
          inputId = "export_columns_adv",
          label = "Export Columns (leave blank for all)"
        ),
      )
    ),
    tabPanel(
      "backlinks",
      h5(
        selectInput(
          "type_bac",
          "Type (select one)",
          choices = NULL,
          multiple = FALSE,
          size = 1,
          selectize = FALSE,
          selected = NULL
        ),
        br(),
        uiOutput("desc_bac"),
        br(),
        uiOutput("req_bac"),
        textInput("target_bac", "Target", placeholder = "example.com"),
        selectInput(
          "target_type_bac",
          "Target Type",
          choices = c("root_domain", "domain", "url"),
          selected = "domain"
        ),
        splitLayout(
          tags$head(tags$style(
            HTML("
                 .shiny-split-layout > div {
                 overflow: visible;
                 }
                 ")
          )),
          cellWidths = c("0%", "45%", "35%"),
          selectInput(
            "timespan_bac",
            "Timespan",
            choices = c("weeks", "months"),
            selected = "weeks"
          ),
          numericInput(
            "display_limit_bac",
            "Display Limit",
            value = 5,
            min = 1,
            max = 1e3
          )
        ),
        selectInput(
          choices = "",
          multiple = TRUE,
          inputId = "export_columns_bac",
          label = "Export Columns (leave blank for all)"
        )
      )
    ),
    tabPanel(
      "domain",
      h5(
        selectInput(
          "type_dom",
          "Type (select one)",
          choices = NULL,
          multiple = FALSE,
          size = 1,
          selectize = FALSE,
          selected = NULL
        ),
        br(),
        uiOutput("desc_dom"),
        br(),
        uiOutput("req_dom"),
        textInput("domain_dom", "Domain", placeholder = "example.com"),
        textInput("domains_dom", "Domains", placeholder = "*|or|example-1.com|*|or|example-2.com|"),
        splitLayout(
          tags$head(tags$style(
            HTML("
                 .shiny-split-layout > div {
                 overflow: visible;
                 }
                 ")
          )),
          cellWidths = c("0%", "45%", "35%"),
          selectInput(
            "database_dom",
            "Database",
            choices = db_codes$Database_code,
            selected = "us"
          ),
          numericInput(
            "display_limit_dom",
            "Display Limit",
            value = 5,
            min = 1,
            max = 1e3
          )
        ),
        selectInput(
          choices = "",
          multiple = TRUE,
          inputId = "export_columns_dom",
          label = "Export Columns (leave blank for all)"
        )
      )
    ),
    tabPanel(
      "keyword",
      h5(
        selectInput(
          "type_key",
          "Type (select one)",
          choices = NULL,
          multiple = FALSE,
          size = 1,
          selectize = FALSE,
          selected = NULL
        ),
        br(),
        uiOutput("desc_key"),
        br(),
        uiOutput("req_key"),
        textInput("phrase_key", "Keyword(s)", placeholder = "keyword1;keyword2"),
        splitLayout(
          tags$head(tags$style(
            HTML("
                 .shiny-split-layout > div {
                 overflow: visible;
                 }
                 ")
          )),
          cellWidths = c("0%", "45%", "35%"),
          selectInput(
            "database_key",
            "Database",
            choices = db_codes$Database_code,
            selected = "us"
          ),
          numericInput(
            "display_limit_key",
            "Display Limit",
            value = 5,
            min = 1,
            max = 1e3
          )
        ),
        selectInput(
          choices = "",
          multiple = TRUE,
          inputId = "export_columns_key",
          label = "Export Columns (leave blank for all)"
        )
      )
    ),
    tabPanel(
      "overview",
      h5(
        selectInput(
          "type_ove",
          "Type (select one)",
          choices = NULL,
          multiple = FALSE,
          size = 1,
          selectize = FALSE,
          selected = NULL
        ),
        br(),
        uiOutput("desc_ove"),
        br(),
        uiOutput("req_ove"),
        textInput("domain_ove", "Domain", placeholder = "example.com"),
        splitLayout(
          tags$head(tags$style(
            HTML("
                 .shiny-split-layout > div {
                 overflow: visible;
                 }
                 ")
          )),
          cellWidths = c("0%", "45%", "35%"),
          selectInput(
            "database_ove",
            "Database",
            choices = db_codes$Database_code,
            selected = "us"
          ),
          numericInput(
            "display_limit_ove",
            "Display Limit",
            value = 5,
            min = 1,
            max = 1e3
          )
        ),
        selectInput(
          choices = "",
          multiple = TRUE,
          inputId = "export_columns_ove",
          label = "Export Columns (leave blank for all)"
        )
      )
    )
  ),
  actionButton("submit",
               "Submit",
               icon = icon("table"))
)


## Assemble UI
ui = dashboardPage(title="SEMRush Report Builder",
  dashboardHeader(
  title = HTML("<b><i>SEMRush Report Builder</i></b>"),
  titleWidth =
    300
),
sidebar,
body)


## Define server logic
server <- function(input, output, session) {
  
  # Select menu options from report type
  observeEvent(input$report, {
    updateTabsetPanel(session, "switcher", selected = input$report)
  })
  
  report <- reactive({
    filter(menuOpts, report == input$report)
  })
  
  # Display report topic menu options based on report type
  observeEvent(report(), {
    choices <- sort(unique(report()$type))
    updateSelectInput(session, "type_adv", choices = choices)
    updateSelectInput(session, "type_bac", choices = choices)
    updateSelectInput(session, "type_dom", choices = choices)
    updateSelectInput(session, "type_key", choices = choices)
    updateSelectInput(session, "type_ove", choices = choices)
  })
  
  type_adv <- reactive({
    req(input$type_adv)
    filter(report(), type == input$type_adv)
  })
  
  type_bac <- reactive({
    req(input$type_bac)
    filter(report(), type == input$type_bac)
  })
  
  type_dom <- reactive({
    req(input$type_dom)
    filter(report(), type == input$type_dom)
  })
  
  type_key <- reactive({
    req(input$type_key)
    filter(report(), type == input$type_key)
  })
  
  type_ove <- reactive({
    req(input$type_ove)
    filter(report(), type == input$type_ove)
  })
  
  # Display export column menu options based on report topic
  
  #Prepare export columns - advertising reports
  export_columns_adv <- reactive({
    filter(type_adv(), option == "export_columns")
  })
  
  observeEvent(export_columns_adv(), {
    choices <- strsplit(export_columns_adv()$args, ";") %>% unlist()
    reorder <- order(sapply(choices, function(x) sapply(strsplit(x,""), length)))
    choices <- choices[reorder]
    
    updateSelectizeInput(session,
                         "export_columns_adv",
                         choices = choices)
  })
  
  #Prepare export columns - backlinks reports
  export_columns_bac <- reactive({
    filter(type_bac(), option == "export_columns")
  })
  
  observeEvent(export_columns_bac(), {
    choices <- strsplit(export_columns_bac()$args, ";") %>% unlist()
    reorder <- order(sapply(choices, function(x) sapply(strsplit(x,""), length)), decreasing = FALSE)
    choices <- choices[reorder]
    
    updateSelectizeInput(session,
                         "export_columns_bac",
                         choices = choices)
  })
  
  #Prepare export columns - domain reports
  export_columns_dom <- reactive({
    filter(type_dom(), option == "export_columns")
  })
  
  observeEvent(export_columns_dom(), {
    choices <- strsplit(export_columns_dom()$args, ";") %>% unlist()
    reorder <- order(sapply(choices, function(x) sapply(strsplit(x,""), length)))
    choices <- choices[reorder] %>% str_remove_all(" ")
    
    updateSelectizeInput(session,
                         "export_columns_dom",
                         choices = choices)
  })
  
  #Prepare export columns - keyword reports
  export_columns_key <- reactive({
    filter(type_key(), option == "export_columns")
  })
  
  observeEvent(export_columns_key(), {
    choices <- strsplit(export_columns_key()$args, ";") %>% unlist()
    reorder <- order(sapply(choices, function(x) sapply(strsplit(x,""), length)))
    choices <- choices[reorder]
    
    updateSelectizeInput(session,
                         "export_columns_key",
                         choices = choices)
  })
  
  #Prepare export columns - overview reports
  export_columns_ove <- reactive({
    filter(type_ove(), option == "export_columns")
  })
  
  observeEvent(export_columns_ove(), {
    choices <- strsplit(export_columns_ove()$args, ";") %>% unlist()
    reorder <- order(sapply(choices, function(x) sapply(strsplit(x,""), length)))
    choices <- choices[reorder]
    
    updateSelectizeInput(session,
                         "export_columns_ove",
                         choices = choices)
  })
  
  # Display required inputs
  required_adv <- reactive({
    x <- filter(type_adv(), option == "required")
    args. <- strsplit(x$args, ";") %>% unlist()
    tag <-
      HTML(
        paste0(
          "<b style='margin-left:14px;color:gray90'><u>Required inputs: </u></b>",
          "<i>",
          paste0(args., collapse = ",  "),
          "</i>",
          collapse = ""
        )
      )
    return(tag)
  })
  
  required_bac <- reactive({
    x <- filter(type_bac(), option == "required")
    args. <- strsplit(x$args, ";") %>% unlist()
    tag <-
      HTML(
        paste0(
          "<b style='margin-left:14px;color:gray90'><u>Required inputs: </u></b>",
          "<i>",
          paste0(args., collapse = ",  "),
          "</i>",
          collapse = ""
        )
      )
    return(tag)
  })
  
  required_dom <- reactive({
    x <- filter(type_dom(), option == "required")
    args. <- strsplit(x$args, ";") %>% unlist()
    tag <-
      HTML(
        paste0(
          "<b style='margin-left:14px;color:gray90'><u>Required inputs: </u></b>",
          "<i>",
          paste0(args., collapse = ",  "),
          "</i>",
          collapse = ""
        )
      )
    return(tag)
  })
  
  required_key <- reactive({
    x <- filter(type_dom(), option == "required")
    args. <- strsplit(x$args, ";") %>% unlist()
    tag <-
      HTML(
        paste0(
          "<b style='margin-left:14px;color:gray90'><u>Required inputs: </u></b>",
          "<i>",
          paste0(args., collapse = ",  "),
          "</i>",
          collapse = ""
        )
      )
    return(tag)
  })
  
  required_ove <- reactive({
    x <- filter(type_ove(), option == "required")
    args. <- strsplit(x$args, ";") %>% unlist()
    tag <-
      HTML(
        paste0(
          "<b style='margin-left:14px;color:gray90'><u>Required inputs: </u></b>",
          "<i>",
          paste0(args., collapse = ",  "),
          "</i>",
          collapse = ""
        )
      )
    return(tag)
  })
  
  # Display report descriptions
  description_adv <- reactive({
    x <- filter(type_adv(), option == "description")
    args. <- x$args#strsplit(x$args, ";") %>% unlist()
    tag <-
      HTML(
        paste0(
          "<b style='margin-left:10px;color:gray90;text-align:center;'><u>Description: </u></b>",
          "<p style='margin-left:10px;margin-right:10px'>",
          paste0(args., collapse = ",  "),
          "</p>",
          collapse = ""
        )
      )
    return(tag)
  })
  
  description_bac <- reactive({
    x <- filter(type_bac(), option == "description")
    args. <- x$args#strsplit(x$args, ";") %>% unlist()
    tag <-
      HTML(
        paste0(
          "<b style='margin-left:10px;color:gray90;text-align:center;'><u>Description: </u></b>",
          "<p style='margin-left:10px;margin-right:10px'>",
          paste0(args., collapse = ",  "),
          "</p>",
          collapse = ""
        )
      )
    return(tag)
  })
  
  description_dom <- reactive({
    x <- filter(type_dom(), option == "description")
    args. <- x$args#strsplit(x$args, ";") %>% unlist()
    tag <-
      HTML(
        paste0(
          "<b style='margin-left:10px;color:gray90;text-align:center;'><u>Description: </u></b>",
          "<p style='margin-left:10px;margin-right:10px'>",
          paste0(args., collapse = ",  "),
          "</p>",
          collapse = ""
        )
      )
    return(tag)
  })
  
  description_key <- reactive({
    x <- filter(type_key(), option == "description")
    args. <- x$args#strsplit(x$args, ";") %>% unlist()
    tag <-
      HTML(
        paste0(
          "<b style='margin-left:10px;color:gray90;text-align:center;'><u>Description: </u></b>",
          "<p style='margin-left:10px;margin-right:10px'>",
          paste0(args., collapse = ",  "),
          "</p>",
          collapse = ""
        )
      )
    return(tag)
  })
  
  description_ove <- reactive({
    x <- filter(type_ove(), option == "description")
    args. <- x$args#strsplit(x$args, ";") %>% unlist()
    tag <-
      HTML(
        paste0(
          "<b style='margin-left:10px;color:gray90;text-align:center;'><u>Description: </u></b>",
          "<p style='margin-left:10px;margin-right:10px'>",
          paste0(args., collapse = ",  "),
          "</p>",
          collapse = ""
        )
      )
    return(tag)
  })
  
  # Get API account balance
  balance <-  reactive({
    bal <- account_api_units(semrush_api_key, character_format = TRUE)
    bal <-
      HTML(
        paste0(
          "<b><a style='color:black' href='https://www.semrush.com/api-analytics/'>",
          sprintf("Account API Balance: %s", bal),
          "</a></b>"
        )
      )
    return(bal)
  })
  
  # Construct report from selected menu options
  report_out <- eventReactive(input$submit, {
    ex_col_name <-
      paste("export_columns", substr(input$report, 1, 3), sep = "_")
    ex_col = input[[ex_col_name]]
    if (is.null(ex_col)) {
      export_columns. <- NULL
    } else{
      export_columns. <- paste0(ex_col, collapse = ",")
    }
    
    if (input$report == "advertising") {
      report. <-
        advertising_reports(
          type = input$type_adv,
          key = semrush_api_key,
          domain = input$domain_adv,
          advertiser_domain = input$advertiser_domain,
          publisher_domain = input$publisher_domain,
          display_limit = input$display_limit_adv,
          device_type = input$device_type_adv,
          export_columns = export_columns.
        )
    }
    
    if (input$report == "backlinks") {
      report. <-
        backlinks_reports(
          type = input$type_bac,
          key = semrush_api_key,
          target = input$target_bac,
          target_type = input$target_type_bac,
          display_limit = input$display_limit_bac,
          timespan = input$timespan_bac,
          export_columns = export_columns.
        )
    }
    
    if (input$report == "domain") {
      report. <-
        domain_reports(
          type = input$type_dom,
          key = semrush_api_key,
          domain = input$domain_dom,
          domains = input$domains_dom,
          database = input$database_dom,
          display_limit = input$display_limit_dom,
          export_columns = export_columns.
        )
    }
    
    if (input$report == "keyword") {
      report. <-
        keyword_reports(
          type = input$type_key,
          key = semrush_api_key,
          phrase = input$phrase_key,
          database = input$database_key,
          display_limit = input$display_limit_key,
          export_columns = export_columns.
        )
    }
    
    if (input$report == "overview") {
      report. <-
        overview_reports(
          type = input$type_ove,
          key = semrush_api_key,
          domain = input$domain_ove,
          database = input$database_ove,
          display_limit = input$display_limit_ove,
          export_columns = export_columns.
        )
    }
    
    write.csv(report., "temp/output.csv")
    return(report.)
    
  })
  
  #Show required inputs
  output$req_adv <- renderUI(required_adv())
  output$req_bac <- renderUI(required_bac())
  output$req_dom <- renderUI(required_dom())
  output$req_key <- renderUI(required_key())
  output$req_ove <- renderUI(required_ove())

  #Show required inputs
  output$desc_adv <- renderUI(description_adv())
  output$desc_bac <- renderUI(description_bac())
  output$desc_dom <- renderUI(description_dom())
  output$desc_key <- renderUI(description_key())
  output$desc_ove <- renderUI(description_ove())
    
  #Dispay generated report
  output$Table <-
    DT::renderDataTable({
      report_out()
    }, rownames = FALSE, escape = FALSE, options = list(
      scrollX = TRUE,
      scrollCollapse = TRUE,
      lengthMenu = list(c(10,-1), c('10', 'All')),
      pageLength = -1
    ))
  
  output$account_balance = renderUI(balance())
  
  output$dl_results <- downloadHandler(
    filename = "download_results.csv",
    content = function(file) {
      tab <- read.csv("temp/output.csv")
      write.csv(tab, file)
    }
  )
  
  
}


## Run app
shinyApp(ui, server)