# testing r shiny app for NRI report printing to PDF
# sierra raby
# updated 1/6/2026

# setup ------------------------------------------------------------------------
library(tinytex)
library(shiny)
library(rmarkdown)
library(here)
library(httr)
library(jsonlite)
library(DT)
library(leaflet)
library(sf)

# get NRI data ---------------------------------------------------------------
# get counties first
layer_url <- "https://services.arcgis.com/XG15cJAlne2vxtgt/arcgis/rest/services/National_Risk_Index_Counties/FeatureServer/0"

# get field display names for table
get_field_aliases <- function(layer_url) {
  res <- POST(paste0(layer_url, "?f=json"))
  txt <- content(res, "text")
  parsed <- fromJSON(txt)
  # parsed$fields is a data frame: columns "name" and "alias"
  aliases <- setNames(parsed$fields$alias, parsed$fields$name)
  return(aliases)
}
field_aliases <- get_field_aliases(layer_url)

# set up map
get_county_geometries <- function(layer_url, state) {
  res <- POST(paste0(layer_url, "/query"), body = list(
    where = paste0("STATE='", state, "'"),
    outFields = "COUNTY,OBJECTID,RISK_RATNG",
    f = "geojson",  # Request GeoJSON for easy mapping
    returnGeometry = "true",
    resultRecordCount = 1000
  ), encode = "form")
  geojson <- content(res, "text")
  sf::st_read(geojson, quiet = TRUE)  # Requires sf package
}

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

# get county attributes
get_county_attributes <- function(objectid) {
  res <- POST(paste0(layer_url, "/query"), body = list(
    where = paste0("OBJECTID=", objectid),
    outFields = "*",
    f = "json",
    returnGeometry = "false"
  ), encode = "form")
  txt <- content(res, "text")
  parsed <- fromJSON(txt)
  # Extract attributes from the first (and only) feature
  if (!is.null(parsed$features) && length(parsed$features) > 0) {
    return(parsed$features$attributes)
  } else {
    return(data.frame())
  }
}

# county color mapping
risk_colors <- c(
  "Very High" = "#C7445D",
  "Relatively High" = "#E07069",
  "Relatively Moderate" = "#F0D55D",
  "Relatively Low" = "#509BC7",
  "Very Low" = "#4D6DBD"
)


# app --------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("National Risk Index PDF Report Generator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select a State:", choices = state_list),
      textOutput("selected_county_name"),  
      downloadButton("downloadReport", "Download PDF Report")
    ),
    mainPanel(
      leafletOutput("county_map", height = 600)#,
      # DT::DTOutput("county_table")
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to store selected county OBJECTID
  selected_county <- reactiveVal(NULL)
  
  # Load county polygons when state is selected
  counties_sf <- reactive({
    req(input$state)
    get_county_geometries(layer_url, input$state)
  })
  
  # Render the leaflet map
  output$county_map <- renderLeaflet({
    counties <- counties_sf()
    req(counties)
    if (nrow(counties) == 0) return(NULL)
    sel_id <- selected_county()
    
    counties$selected <- if (!is.null(sel_id)) {
      as.character(counties$OBJECTID) == as.character(sel_id)
    } else {
      FALSE
    }
    
    # counties symbology
    risk_rating_vec <- as.character(counties$RISK_RATNG)
    idx <- match(risk_rating_vec, names(risk_colors))
    counties$fillColor <- risk_colors[idx]
    counties$fillColor[is.na(counties$fillColor)] <- "#cccccc"
    
    leaflet(counties) %>%
      addTiles() %>%
      addPolygons(
        layerId = ~OBJECTID,
        label = ~COUNTY,
        color = ~ifelse(selected, "#39ff14", "black"), # outline
        weight = ~ifelse(selected, 5, 1),
        fillOpacity = 1, # ~ifelse(selected, 0.7, 0.4)
        fillColor = ~fillColor
      )
  })
  
  # Observe map clicks
  observeEvent(input$county_map_shape_click, {
    click <- input$county_map_shape_click
    selected_county(click$id)  # Store the clicked OBJECTID
  })
  
  output$selected_county_name <- renderText({
    req(selected_county())
    # Get attributes for the selected county
    attrs <- get_county_attributes(selected_county())
    paste("Selected county:", attrs$COUNTY)
  })
  
  # PDF download handler uses selected_county()
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("county_report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      attrs <- get_county_attributes(selected_county())
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
# switch user selection to map
# format table into subtables
# add map to report
# then add census tract option
# then add webmap
# then get preview 
# then build comparison option

# # extracting county colors - save to reuse for census tracts 
# res <- POST(paste0(layer_url, "?f=json"))
# txt <- content(res, "text")
# parsed <- jsonlite::fromJSON(txt)
# renderer <- parsed$drawingInfo$renderer
# print(renderer)
# 
# arcgis_rgba_vec_to_hex <- function(rgba) {
#   rgb(rgba[1], rgba[2], rgba[3], maxColorValue = 255)
# }
# 
# # Build named vector: risk rating -> hex color
# risk_levels <- renderer$uniqueValueInfos$value
# color_list <- renderer$uniqueValueInfos$symbol$color  # This is a list of integer vectors
# 
# risk_colors <- sapply(color_list, arcgis_rgba_vec_to_hex)
# names(risk_colors) <- risk_levels
# print(risk_colors)


