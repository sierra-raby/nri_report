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

# query REST API to populate county drop down menu
get_county_list <- function(layer_url, n = 100) {
  res <- POST(paste0(layer_url, "/query"), body = list(
    where = "1=1",
    outFields = "COUNTY,OBJECTID",
    f = "json",
    returnGeometry = "false",
    resultRecordCount = n
  ), encode = "form")
  txt <- content(res, "text")
  parsed <- fromJSON(txt)
  # get attributes
  counties_df <- parsed$features$attributes
  rownames(counties_df) <- NULL
  return(counties_df)
}

county_list <- get_county_list(layer_url, 100)
# print(county_list)

# app --------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("National Risk Index PDF Report Generator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("county", "Select a County:",
                  choices = setNames(county_list$OBJECTID, county_list$COUNTY)),
      downloadButton("downloadReport", "Download PDF Report")
    ),
    mainPanel(
      # add preview or other UI elements here
    )
  )
)

server <- function(input, output, session) {
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
        input = here("spatial_data_scientist", "RAPT", "NRI", "report", "nri_report", "report_template.Rmd"),
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
}

shinyApp(ui, server)

# NEXT
# format table
# then add state filter 
# then add census tract option
# then add webmap
# then get preview 
# then build comparison option
