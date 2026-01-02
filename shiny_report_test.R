# testing r shiny app for NRI report printing to PDF
# sierra raby
# updated 1/2/2026

# setup ------------------------------------------------------------------------
library(tinytex)
library(shiny)
library(rmarkdown)
library(here)
library(httr)
library(jsonlite)
library(DT)

# get NRI data ---------------------------------------------------------------
# counties first
layer_url <- "https://services.arcgis.com/XG15cJAlne2vxtgt/arcgis/rest/services/National_Risk_Index_Counties/FeatureServer/0"

# get field display names
get_field_aliases <- function(layer_url) {
  res <- POST(paste0(layer_url, "?f=json"))
  txt <- content(res, "text")
  parsed <- fromJSON(txt)
  # parsed$fields is a data frame: columns "name" and "alias"
  aliases <- setNames(parsed$fields$alias, parsed$fields$name)
  return(aliases)
}
field_aliases <- get_field_aliases(layer_url)

# get state names
get_all_states <- function(layer_url, page_size = 1000) {
  all_states <- character()
  offset <- 0
  repeat {
    res <- POST(paste0(layer_url, "/query"), body = list(
      where = "1=1",
      outFields = "STATE",
      f = "json",
      returnGeometry = "false",
      resultRecordCount = page_size,
      resultOffset = offset
    ), encode = "form")
    txt <- content(res, "text")
    parsed <- fromJSON(txt)
    if (is.null(parsed$features) || length(parsed$features) == 0) break
    states <- unique(parsed$features$attributes$STATE)
    all_states <- unique(c(all_states, states))
    # Check if we've reached the end
    if (length(parsed$features$attributes$STATE) < page_size) break
    offset <- offset + page_size
  }
  return(sort(all_states))
}

state_list <- get_all_states(layer_url)

# query REST API to populate county drop down menu
# get_county_list <- function(layer_url, n = 100) {
#   res <- POST(paste0(layer_url, "/query"), body = list(
#     where = "1=1",
#     outFields = "COUNTY,OBJECTID",
#     f = "json",
#     returnGeometry = "false",
#     resultRecordCount = n
#   ), encode = "form")
#   txt <- content(res, "text")
#   parsed <- fromJSON(txt)
#   # get attributes
#   counties_df <- parsed$features$attributes
#   rownames(counties_df) <- NULL
#   return(counties_df)
# }
# 
# county_list <- get_county_list(layer_url, 100)
# print(county_list)

# app --------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("National Risk Index PDF Report Generator"),
  sidebarLayout(
    sidebarPanel(
      # selectInput("county", "Select a County:",
      #             choices = setNames(county_list$OBJECTID, county_list$COUNTY)),
      selectInput("state", "Select a State:", choices = state_list),
      selectInput("county", "Select a County:", choices = NULL),
      downloadButton("downloadReport", "Download PDF Report")
    ),
    mainPanel(
      # add preview or other UI elements here
    )
  )
)

server <- function(input, output, session) {
  # get counties for selected state
  get_counties_by_state <- function(layer_url, state) {
    res <- POST(paste0(layer_url, "/query"), body = list(
      where = paste0("STATE='", state, "'"),
      outFields = "COUNTY,OBJECTID",
      f = "json",
      returnGeometry = "false",
      resultRecordCount = 1000
    ), encode = "form")
    txt <- content(res, "text")
    parsed <- fromJSON(txt)
    counties_df <- parsed$features$attributes
    return(counties_df)
  }
  
  # Update county dropdown when state changes
  observeEvent(input$state, {
    counties_df <- get_counties_by_state(layer_url, input$state)
    updateSelectInput(session, "county",
                      choices = setNames(counties_df$OBJECTID, counties_df$COUNTY))
  })
  
  
  # get full attributes for selected county by OBJECTID
  get_county_attributes <- function(objectid) {
    res <- POST(paste0(layer_url, "/query"), body = list(
      where = paste0("OBJECTID=", objectid),
      outFields = "*",
      f = "json",
      returnGeometry = "false"
    ), encode = "form")
    txt <- content(res, "text")
    parsed <- fromJSON(txt)
    # Extract attributes
    if (!is.null(parsed$features) && length(parsed$features) > 0) {
      return(parsed$features$attributes)
    } else {
      return(data.frame())
    }
  }
  
  # selected county's attributes as a table
  output$county_table <- DT::renderDT({
    req(input$county)
    attrs <- get_county_attributes(input$county)
    DT::datatable(attrs)
  })
  
  # PDF download handler
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("county_report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      attrs <- get_county_attributes(input$county)
      # switch to display names
      field_names <- colnames(attrs)
      display_names <- ifelse(
        field_names %in% names(field_aliases),
        field_aliases[field_names],
        field_names
      )
      vertical_table <- data.frame(
        Attribute = display_names,
        Value = as.character(as.vector(t(attrs))),
        stringsAsFactors = FALSE
      )
      # take out unneeded columns
      remove_aliases <- c("OBJECTID", "Shape__Area", "Shape__Length")
      vertical_table_filtered <- vertical_table[!(vertical_table$Attribute %in% remove_aliases), ]
      params <- list(
        county_attributes = vertical_table_filtered
      )
      rmarkdown::render(
        input = here("report_template.Rmd"), # "spatial_data_scientist", "RAPT", "NRI", "report", "nri_report", 
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
}

shinyApp(ui, server)

# NEXT
# format table into subtables
# add map
# then add census tract option
# then add webmap
# then get preview 
# then build comparison option
