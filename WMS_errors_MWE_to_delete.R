
# Loading EMODnet WMS Tiles works -----------------------------------------

library(leaflet)
library(dplyr)


leaflet::leaflet() %>% 
  addTiles() %>%
  addWMSTiles(
    baseUrl = "https://geo.vliz.be/geoserver/MarineRegions/wms",
    layers = "eez",   # replace with your layer name
    options = WMSTileOptions(
      format = "image/png",
      styles = "Polygons_greyoutline",
      transparent = T
    )
  )



leaflet::leaflet() %>% 
  addTiles() %>%
  setView(3, 51.5, zoom = 8) %>%
  addWMSTiles(
    baseUrl = "https://geo.vliz.be/geoserver/Kustportaal/wms",
    layers = "Kustportaal:scheepswrakken_20180604",
    options = WMSTileOptions(
      format = "image/png",
      transparent = T
    )
  )


# Loading VLIZ geoserver WMS Tiles does not work --------------------------


library(leaflet)
library(dplyr)


legend_url <- paste0(
  "https://ows.emodnet-humanactivities.eu/wms",
  "?SERVICE=WMS&REQUEST=GetLegendGraphic",
  "&FORMAT=image/png",
  "&LAYER=", "windfarmspoly",
  "&VERSION=1.1.1"
)

#legend_url
#browseURL(legend_url) 


leaflet::leaflet() %>% 
  setView(3, 51.5, zoom = 8) %>%
  addTiles() %>%
  addWMSTiles(
    baseUrl = "https://ows.emodnet-humanactivities.eu/wms",
    layers = "windfarmspoly",
    options = WMSTileOptions(
      format = "image/png",
      transparent = T
    )
  ) %>%
  addControl(
    html = paste0(
      '<div style="background:none;padding:0px;border-radius:0px;">',
      '<div style="font-weight:600;margin-bottom:0px;">Wind farms</div>',
      '<img src="', legend_url, '" />',
      '</div>'
    ),
    position = "bottomright"
  )

# Loading RBINS geoserver WMS Tiles works --------------------------


library(leaflet)
library(dplyr)


leaflet::leaflet() %>% 
  setView(3, 51.5, zoom = 8) %>%
  addTiles() %>%
  addWMSTiles(
    baseUrl = "https://spatial.naturalsciences.be/geoserver/od_nature/wms",
    layers = "od_nature:Habitat_BPNS_Seafloor",
    options = WMSTileOptions(
      format = "image/png",
      transparent = T
    )
  )
# 
# legend_url <- paste0(
#   "https://spatial.naturalsciences.be/geoserver/od_nature/wms",
#   "?SERVICE=WMS&REQUEST=GetLegendGraphic",
#   "&FORMAT=image/png",
#   "&LAYER=", "od_nature:Habitat_BPNS_Seafloor",
#   "&VERSION=1.1.1"
# )
# 
# legend_url
# browseURL(legend_url)
