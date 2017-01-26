library(leaflet)
library(htmltools)
library(htmlwidgets)
library(shiny)

######################

source("gsg_grid/create_GSG.r")
source("sampling/clustersampling.r")
source("io/import.r")
##########################


df=data.frame()
r=1

country_code="DEU"
adm_level="0"

pdist.list=c(seq(from=400, to=500, by=100))# in m
gdist.list=c(seq(from=50, to=60, by=10)) # in km
germany=readRDS("countries/DEU_adm0.rds") 
boundary.geo = spTransform(germany, CRS("+init=epsg:4326"))
GSG=generateGSG(distance=40,landpoly.geo=boundary.geo)

# This tells htmlwidgets about our plugin name, version, and
# where to find the script. (There's also a stylesheet argument
# if the plugin comes with CSS files.)
bingPlugin <- htmlDependency("leaflet.plugins", "2.0.0",
                            # src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/leaflet-plugins/2.0.0/layer/tile/"),
                             src = normalizePath('webapp/'),
                             script = "Bing.min.js"
)


# A function that takes a plugin htmlDependency object and adds
# it to the map. This ensures that however or whenever the map
# gets rendered, the plugin will be loaded into the browser.
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
)

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      registerPlugin(bingPlugin) %>%
      onRender("function(el, x) { 
    var imagerySet = 'Aerial';
    var bing = new L.BingLayer('LfO3DMI9S6GnXD7d0WGs~bq2DRVkmIAzSOFdodzZLvw~Arx8dclDxmZA0Y38tHIJlJfnMbGq5GXeYmrGOUIbS2VLFzRKCK0Yv_bAl6oe-DOc',
         {type: imagerySet});
     this.addLayer(bing);
 }")%>%
      addMarkers(data = GSG@points)
  })
}

shinyApp(ui, server)



#######################
#Method for importing wfs services
#library("osmar")
#library(gdalUtils)
#library(rgdal)

#dsn <- "WFS:http://efforts-webgis.uni-goettingen.de/geoserver/wfs?VERSION=1.0.0&SERVICE=WFS&REQUEST=GetCapabilities"
#ogrinfo(dsn, so=TRUE)
#ogr2ogr(dsn, "sic.shp", "admin_jambi_2011")
#sic <- readOGR("sic.shp", "sic", stringsAsFactors=FALSE)
#plot(sic)
