library(sf)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(leafgl)



# Read data ---------------------------------------------------------------

points_sf <- read_sf("projects/Greater Melbourne Distribution Center Landscape/resources/SA1_vic_dotcounts/points_sf.shp")
fullfillmentCenter_sf <- read_sf("projects/Greater Melbourne Distribution Center Landscape/resources/fullfillmentCenter/fullfillmentCenter_sf.shp")


# plot --------------------------------------------------------------------

# color_map <- c("Inhouse" = "red", "3PL" = "navy")
# Create leaflet ma


m <- leaflet() %>%
  addProviderTiles("OpenStreetMap") %>%
  fitBounds(
    lng1 = 144.72903, lat1 = -38.05242,
    lng2 = 145.23470, lat2 = -37.57800
  ) |>
  leafgl::addGlPoints(
    data = points_sf,
    fillColor = "orange",
    fillOpacity = 0.5,
    radius = 3,
    group = "Population Density Dots"
  ) |>
  addCircleMarkers(
    data = fullfillmentCenter_sf,
    group = "Post-pandemic",
    radius = ~dplyr::case_when(
      Area == "Small" ~ 4,
      Area == "Medium" ~ 6,
      Area == "Large" ~ 8,
      TRUE ~ 10
    ),
    color = ~dplyr::case_when(
      Type == "inhouse" ~ "red",
      Type == "3PL" ~ "navy",
      TRUE ~ "grey"
    ),
    stroke = TRUE,
    weight = 0.1,
    fillOpacity = 0.5,
    label = ~Name,
    popup = ~paste0(
      "<b>Name:</b> ", Name, "<br>",
      "<b>Size:</b> ", Area, "<br>",
      "<b>Since:</b> ", Since, "<br>",
      "<b>Note:</b> ", Note, "<br>"
    )
  ) |>
  addCircleMarkers(
    data = fullfillmentCenter_sf %>% dplyr::filter(Since < 2020),
    group = "Pre-pandemic",
    radius = ~dplyr::case_when(
      Area == "Small" ~ 4,
      Area == "Medium" ~ 6,
      Area == "Large" ~ 8,
      TRUE ~ 10
    ),
    color = ~dplyr::case_when(
      Type == "inhouse" ~ "red",
      Type == "3PL" ~ "navy",
      TRUE ~ "grey"
    ),
    stroke = TRUE,
    weight = 0.1,
    fillOpacity = 0.5,
    label = ~Name,
    popup = ~paste0(
      "<b>Name:</b> ", Name, "<br>",
      "<b>Size:</b> ", Area, "<br>",
      "<b>Since:</b> ", Since, "<br>",
      "<b>Note:</b> ", Note, "<br>"
    )
  ) |>
  addLegend(
    position = "bottomright",
    colors = c("Inhouse" = "red", "3PL" = "navy"),
    labels = c("Inhouse", "3PL"),
    title = "Warehouse Type"
  ) |>
  addControl(
    html = "<div id='dotKey' style='background:white; padding:4px; font-size:12px; border-radius:4px; display:none;'>
              <span style='display:inline-block; width:10px; height:10px; background:orange; border-radius:50%; margin-right:4px;'></span> = 100 persons
            </div>",
    position = "bottomleft"
  ) |>
  addLayersControl(
    baseGroups = c("Post-pandemic", "Pre-pandemic"),
    overlayGroups = c("Population Density Dots"),
    options = layersControlOptions(collapsed = FALSE)
  )

m <- htmlwidgets::onRender(m, "
  function(el, x) {
    var map = this;
    map.on('overlayadd', function(e) {
      if (e.name === 'Population Density Dots') {
        document.getElementById('dotKey').style.display = 'block';
      }
    });
    map.on('overlayremove', function(e) {
      if (e.name === 'Population Density Dots') {
        document.getElementById('dotKey').style.display = 'none';
      }
    });
  }
")


# saveWidget(m, "projects/Greater Melbourne Distribution Center Landscape/fig/fulfillmentCenterLandscape.html", selfcontained = TRUE)
