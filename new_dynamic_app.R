# R Shiny application to visualize future climate and forest data

# App usage:
# - Navigate to root and run through terminal with: "Rscript app.R"

# Input files:
# - filtered_nuts2_areas.csv: NUTS surface area records
# - NUTS_RG_20M_2021_4326_LEVL_2.shp: Shapefile with NUTS2 area geometry
# - eu_land_cover_per_nuts2.csv: Land use percentages per NUTS2 area
# - general_nuts_info_final_output.csv: name, ID, forest surface area and
#   NUTS2 area surface areas 
# - deviation_added_TEST_30_year_combined_data_final_vars_fixed.csv: climatology
#   data for 30-year time periods. Temperature (bio1) and precipitaion (bio12)
# - *10_combined_with_difference_to_all_data_with_historical.csv: climatology
#   data for 10-year time periods. Temperature (bio1) and precipitaion (bio12)
#   * NOT BIAS CORRECTED DATA


# App structure:

# Libraries
# Input files
# UI + SERVER

# UI: Defines user interface
# - Options for 30-year and 10-year climate data, and forest data
# - Options to choose climate / forest model, scenario (ssp + rcp combination),
#   time period and climate and forest variables

# SERVER: Defines logic for input file usage and user inputs
# output$info_text: Defines info box. *Work in progress*
# filter_data: Filters input data according to user choice
# merged_data: Merges filtered data with other input data (nuts info etc.)
# pal: Chooses the color pallette depending on the user choice of data
# observeEvent: Updates variables when user changes something
# output$map (nested in observeEvent): Renders the map, adds information to UI

# shinyApp(ui, server): Runs the application, with the UI and SERVER


# App input data structure:
# /data/climate/: Contains both climate files (CHELSA data)
#  - 30-year data bias-corrected
#  - 10-year data ***NOT*** bias-corrected
# /data/forest/: Contains LPJ-GUESS demo data (created from output samples 9/2024)
# /data/land_use/: Contains data on NUTS areas and land use per NUTS-area
# /data/nuts/: Contains shapefiles for NUTS2 level
# /data/nuts/nuts_013: Contains shapefiles for NUTS0, NUTS1 and NUTS3 levels

# DEBUG List:
# - Units not done for forest side
# - Climate data for NUTS-1 level 
# - Color pallette numbers concistency
# - Zooming
# - output$info_text not utilized properly, specifically bad for climate data
# - "model average" does not work with "deviation from model mean"
# - 10-year climate: ssp119 - MPI-ESM1-2-HR combination does not have data
# - Probably something else

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)

# Define constants outside the server function for better performance
climate30_ssp <- list("Low Emissions (SSP1-2.6)" = "ssp126", "Medium Emissions (SSP3-7.0)" = "ssp370", "High Emissions (SSP5-8.5)" = "ssp585")
climate30_timepr <- list("2011-2040", "2041-2070", "2071-2100")

climate10_ssp <- list("Very Low Emissions (SSP1-1.9)" = "ssp119", "Low Emissions (SSP1-2.6)" = "ssp126", "Low-Medium Emissions (SSP2-4.5)" = "ssp245", "Medium Emissions (SSP3-7.0)" = "ssp370", "High Emissions (SSP5-8.5)" = "ssp585")
climate10_timepr <- list("2020-2030", "2030-2040", "2040-2050", "2050-2060", "2060-2070", "2070-2080", "2080-2090", "2090-2100")

# Load shapefile of NUTS area data
# Use the second line to exclude areas: Turkey, Svalbard and French Guyana excluded by default
shapefile <- st_read("data/nuts/NUTS_RG_20M_2021_4326_LEVL_2.shp") %>%
  filter(!grepl("^TR", NUTS_ID) & !NUTS_ID %in% c("NO0B", "FRY3"))

#shapefile <- st_read("data/nuts/nuts_013/NUTS_RG_20M_2021_4326_LEVL_2.shp")

land_use_data <- read_csv("data/land_use/eu_land_cover_per_nuts2.csv", show_col_types = FALSE)
surface_area_data <- read_delim("data/land_use/filtered_nuts2_surface_areas.csv", delim = ";", show_col_types = FALSE) %>% rename(surface_area_km2 = `2021`)
general_nuts_info <- read_csv("data/land_use/general_nuts_info_final_output.csv", show_col_types = FALSE)

# New Forest Data - Point data no grid yet
#forest_data <- read_csv("data/forest/lpj-guess_demo_averages_per_nuts2.csv", show_col_types = FALSE)
#forest_data <- read_csv("data/forest/nuts_weighted_averages_cpool.csv", show_col_types = FALSE)
forest_data <- read_csv("data/forest/modified_nuts_weighted_averages_cpool.csv", show_col_types = FALSE)
#forest_data <- read_csv("data/forest/lpj-guess_demo_averages_per_nuts2.csv", show_col_types = FALSE)

forest_data <- read_csv("data/forest/processed_MSA_EUROPE.csv", show_col_types = FALSE)


# Bias corrected
# With deviation test
preprocessed_climate_data_30yr <- read.csv("data/climate/30-year_climatological_data.csv")
# NOT Bias corrected
preprocessed_climate_data_10yr <- read.csv("data/climate/10-year_climatological_data.csv")

# UI
ui <- fluidPage(
  titlePanel("Climate and Forest Data"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("data_type", "Select Data Type:", choices = c(
        "Climate Data - Climatologies" = "climate",
        "Forest Data" = "forest"
      )),
      conditionalPanel(
        condition = "input.data_type == 'climate10'",
        selectInput("variable", "Select Variable:", choices = c("Mean Annual Air Temperature" = "bio1", "Annual Precipitation" = "bio12", "Temperature: Difference to Historical period (1981 - 2010)" = "difference_to_historical_data_bio1", "Precipitation: Difference to Historical period (1981 - 2010)" = "difference_to_historical_data_bio12")),
        selectInput("scenario", "Select Scenario:", choices = climate10_ssp),
        selectInput("model", "Select Model:", choices = c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")),
        selectInput("year", "Select Year Range:", choices = climate10_timepr)
      ),
      conditionalPanel(
        condition = "input.data_type == 'climate'",
        selectInput("variable", "Select Variable:", choices = c("Mean Annual Air Temperature" = "bio1", "Annual Precipitation" = "bio12", "Temperature: Difference to Historical period (1981 - 2010)" = "difference_to_historical_data_bio1", "Precipitation: Difference to Historical period (1981 - 2010)" = "difference_to_historical_data_bio12", "Temperature: Deviation from Model Mean" = "deviation_from_model_mean_bio1", "Precipitation: Deviation from Model Mean" = "deviation_from_model_mean_bio12")),
        selectInput("scenario", "Select Scenario:", choices = climate30_ssp),
        #selectInput("model", "Select Model:", choices = c("Average of all Climate Models" == "model_average", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")),
        selectInput("model", "Select Model:", choices = c(
          "Average of all Climate Models" = "model_average", 
          "GFDL-ESM4" = "GFDL-ESM4", 
          "IPSL-CM6A-LR" = "IPSL-CM6A-LR", 
          "MPI-ESM1-2-HR" = "MPI-ESM1-2-HR", 
          "MRI-ESM2-0" = "MRI-ESM2-0", 
          "UKESM1-0-LL" = "UKESM1-0-LL"
        )),
        
        selectInput("year", "Select Year Range:", choices = climate30_timepr)
      ),
      conditionalPanel(
        condition = "input.data_type == 'forest'",
        selectInput("forest_model", "Select Forest Model:", choices = unique(forest_data$forest_model)),
        selectInput("forest_variable", "Select Forest Variable:", choices = unique(forest_data$variable)),
        #selectInput("forest_variable", "Select Forest Variable:", choices = c(
        #  "Carbon Flux from Fire" = "cflux_Fire",
        #  "Carbon Flux from Harvest" = "cflux_Harvest",
        #  "Net Ecosystem Exchange of Carbon" = "cflux_NEE",
        #  "Vegetation Carbon Flux" = "cflux_Veg",
        #  "Bark Beetle Killed Carbon Mass (Total)" = "cmass_dist_barkbeetle_killed_sts_Total",
        #  "Wind Killed Carbon Mass (Total)" = "cmass_dist_wind_killed_sts_Total",
        #  "Carbon Mass in Managed Forests (Total)" = "cmass_out_Forest_sum",
        #  "Carbon Mass in Natural Forests (Total)" = "cmass_out_Natural_sum",
        #  "Total Carbon Mass" = "cmass_out_Total",
        #  "Fake carbon mass based gamma tree species diversity" = "cmass_out_shannon_index",
        #  "Total Carbon Pool" = "cpool_Total",
        #  "Percentage of Large trees (diam > 50) (ba_forest)" = "diamstruct_ba_forest_ratio_all_to_large",
        #  "All Trees Sum (ba_forest)" = "diamstruct_ba_forest_sum_all_trees",
        #  "Large Trees Sum (diam > 50) (ba_forest)" = "diamstruct_ba_forest_sum_large_trees"
        #)),
        selectInput("forest_year", "Select Year:", choices = unique(forest_data$year)),
        selectInput("scenario", "Select Exploratory Case:", choices = unique(forest_data$scenario))
      ),
      tags$div(id = "info_box", style = "background-color: white; padding: 10px; border: 1px solid #ddd; margin-top: 10px;", HTML("<strong>Additional Information on Data:</strong><br>"), uiOutput("info_text"))
    ),
    mainPanel(
      leafletOutput("map", width = "100%", height = "800px"),
      textOutput("loading_message")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Info text based on selected data type and variable
  output$info_text <- renderUI({
    selected_variable <- if (input$data_type == "climate" || input$data_type == "climate10") input$variable else input$forest_variable
    variable_long_name <- if (input$data_type == "climate" || input$data_type == "climate10") {
      preprocessed_climate_data_30yr$variable_long_name[preprocessed_climate_data_30yr$variable == selected_variable][1]
    } else {
      forest_data$variable_long_name[forest_data$variable == selected_variable][1]
    }
    #HTML(paste("<strong>Selected Variable:</strong> ", selected_variable, "<br>", "<strong>Description:</strong> ", variable_long_name))
    HTML(paste("<strong>Selected Variable:</strong> ", selected_variable, "<br>", "<strong>Description:</strong> ", "Total Soil Carbon Stock"))
  })
  
  # Data filtering logic based on user input
  filter_data <- reactive({
    req(input$data_type)
    data_cache <- switch(input$data_type,
                         "climate" = preprocessed_climate_data_30yr,
                         "climate10" = preprocessed_climate_data_10yr,
                         "forest" = forest_data)
    filtered_data <- data_cache %>%
      filter(if (input$data_type == "climate" || input$data_type == "climate10") {
        variable == input$variable & scenario == input$scenario & model == input$model & year == input$year
      } else {
        variable == input$forest_variable & year == input$forest_year
      })
    if (nrow(filtered_data) == 0) return(NULL)
    filtered_data
  })
  
  # Merging shapefile with filtered data
  merged_data <- reactive({
    req(filter_data())
    merged <- shapefile %>%
      left_join(filter_data(), by = "NUTS_ID") %>%
      left_join(land_use_data, by = "NUTS_ID") %>%
      left_join(surface_area_data, by = "NUTS_ID") %>%
      left_join(general_nuts_info, by = "NUTS_ID") %>%
      select(NUTS_ID, NUTS_NAME, value, forest, surface_area_km2, NUTS_surface_area, forest_surface_area_km2, geometry)
    merged
  })
  
  # Color palette for the map
  pal <- reactive({
    req(merged_data())
    
    data_values <- merged_data()$value
    value_range <- range(data_values, na.rm = TRUE)
    
    # Handle case where all values might be zero
    if (all(data_values == 0, na.rm = TRUE)) {
      value_range <- c(0, 1)  # Set a default range for visualization
    }
    
    #print(value_range)
    colorNumeric(palette = if (input$data_type == "forest") "Greens" else if (input$variable == "bio1" || input$variable == "difference_to_historical_data_bio1" || input$variable == "deviation_from_model_mean_bio1") "YlOrRd" else "Blues", domain = value_range)
  })
  
  observeEvent(input$data_type, {
    #print("Data Type Observed")
    if (input$data_type == "climate10") {
      updateSelectInput(session, "variable", choices = list(
        "Mean Annual Air Temperature" = "bio1",
        "Annual Precipitation" = "bio12",
        "Temperature: Difference to Historical period (1981 - 2010)" = "difference_to_historical_data_bio1",
        "Precipitation: Difference to Historical period (1981 - 2010)" = "difference_to_historical_data_bio12"
      ), selected = "bio1")
      updateSelectInput(session, "scenario", choices = climate10_ssp)
      updateSelectInput(session, "model", choices = c(
        "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
      ))
      updateSelectInput(session, "year", choices = climate10_timepr)
    } else if (input$data_type == "climate") {
      updateSelectInput(session, "variable", choices = list(
        "Mean Annual Air Temperature" = "bio1",
        "Annual Precipitation" = "bio12",
        "Temperature: Difference to Historical period (1981 - 2010)" = "difference_to_historical_data_bio1",
        "Precipitation: Difference to Historical period (1981 - 2010)" = "difference_to_historical_data_bio12",
        "Temperature: Deviation from Model Mean" = "deviation_from_model_mean_bio1", 
        "Precipitation: Deviation from Model Mean" = "deviation_from_model_mean_bio12"
      ), selected = "bio1")
      updateSelectInput(session, "scenario", choices = climate30_ssp)
      # updateSelectInput(session, "model", choices = c(
      #   "Average of all Climate Models" = "model_average", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
      # ))
      updateSelectInput(session, "model", choices = c(
        "Average of all Climate Models" = "model_average", 
        "GFDL-ESM4" = "GFDL-ESM4", 
        "IPSL-CM6A-LR" = "IPSL-CM6A-LR", 
        "MPI-ESM1-2-HR" = "MPI-ESM1-2-HR", 
        "MRI-ESM2-0" = "MRI-ESM2-0", 
        "UKESM1-0-LL" = "UKESM1-0-LL"
      ))
      updateSelectInput(session, "year", choices = climate30_timepr)
      #print(paste(input$model, input$scenario))
    } #else if (input$data_type == "climate") {
    
    #}
    
    # Trigger re-render of the map after the dropdowns are updated
    output$map <- renderLeaflet({
      req(input$data_type)
      
      data_merged <- merged_data()
      #print(head(data_merged))
      
      leaflet(data = data_merged) %>%
        addProviderTiles("OpenStreetMap") %>%
        addPolygons(
          fillColor = ~pal()(value),
          weight = 2,
          opacity = 1,
          color = 'white',
          dashArray = '3',
          fillOpacity = 0.7,
          popup = ~{
            value_formatted <- if (input$data_type == "climate") {
              if (input$variable %in% c("bio12", "difference_to_historical_data_bio12", "deviation_from_model_mean_bio12")) {
                sprintf("%.2f kg m⁻² year⁻¹", value)
              } else if (input$variable %in% c("bio1", "difference_to_historical_data_bio1", "deviation_from_model_mean_bio1")) {
                sprintf("%.2f °C", value)
              }
            } else if (input$data_type == "climate10") {
              if (input$variable %in% c("bio12", "difference_to_historical_data_bio12")) {
                sprintf("%.2f kg m⁻² year⁻¹", value)
              } else if (input$variable %in% c("bio1", "difference_to_historical_data_bio1")) {
                sprintf("%.2f °C", value)
              }
            } else {
              sprintf("%.2f kg C m^-2", value)
            }
            # value_formatted <- if (input$data_type == "climate" || input$data_type == "climate10") {
            #   if (input$variable %in% c("bio12", "difference_to_historical_data_bio12", "deviation_from_model_mean_bio12")) {
            #     sprintf("%.2f kg m⁻² year⁻¹", value)
            #   } else if (input$variable %in% c("bio1", "difference_to_historical_data_bio1", "deviation_from_model_mean_bio1")) {
            #     sprintf("%.2f °C", value)
            #   }
            # } else {
            #   sprintf("%.2f units", value)
            # }
            paste0(
              "NUTS ID: ", NUTS_ID, "<br>",
              "Value: ", value_formatted, "<br>",
              #"Forest Area: ", sprintf("%.2f", forest), "%<br>",
              "Forest Surface Area: ", sprintf("%.2f", forest_surface_area_km2),  "km²", "<br>",
              #"Surface Area: ", sprintf("%.0f", surface_area_km2), " km²", "<br>",
              "NUTS-area Surface Area: ", sprintf("%.0f", NUTS_surface_area), " km²", "<br>",
              "NUTS Area Name: ", NUTS_NAME, "<br>"
            )
          },
          group = "Data"
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal(),
          values = ~value,
          title = "Legend"
        )
    })
  })
}

# Run the app
shinyApp(ui, server)
