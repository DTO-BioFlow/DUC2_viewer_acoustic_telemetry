# TODO: fill with real acoustic data map
make_base_map <- function(lng = 3, lat = 51.5, zoom = 8,
                          arrow_src = "north_arrow.png") {
  
  
  north_arrow <-
    "<img src='https://www.clipartbest.com/cliparts/yTo/Lgr/yToLgryGc.png' style='width:35px;height:45px;'>"
  
  # ## TODO: change to file in .www/, not working for the moment
  # north_arrow <- sprintf(
  #   "<img src='%s' style='width:35px;height:45px;'>",
  #   arrow_src
  # )
  
  leaflet::leaflet() %>%
    leaflet::setView(lng, lat, zoom = zoom) %>%
    leaflet::addMapPane("basePane", zIndex = 100) %>%
    leaflet::addTiles(group = "Open Street Map",
                      options = leaflet::tileOptions(pane = "basePane")) %>%
    leaflet::addTiles(urlTemplate = "https://tiles.emodnet-bathymetry.eu/2020/baselayer/web_mercator/{z}/{x}/{y}.png",
                      group = "EMODnet Bathymetry",
                      options = leaflet::tileOptions(pane = "basePane")) %>%
    leaflet::addProviderTiles("CartoDB.Positron",
                              group = "CartoDB.Positron",
                              options = leaflet::tileOptions(pane = "basePane")) %>%
    leafem::addMouseCoordinates() %>%
    leaflet.extras::addFullscreenControl() %>%
    leaflet::addScaleBar(position = "bottomleft",
                         options = leaflet::scaleBarOptions(maxWidth = 150, imperial = FALSE)) %>%
    leaflet::addControl(html = north_arrow,
                        position = "topleft",
                        className = "fieldset {border: 0;}") %>%
    leaflet::addLayersControl(
      baseGroups = c("CartoDB.Positron", "Open Street Map", "EMODnet Bathymetry"),
      options = layersControlOptions(collapsed = FALSE),
      position = "bottomleft"
    ) 
}
