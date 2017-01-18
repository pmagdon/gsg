library(leaflet)
library(htmltools)
library(htmlwidgets)
library(shiny)

# This tells htmlwidgets about our plugin name, version, and
# where to find the script. (There's also a stylesheet argument
# if the plugin comes with CSS files.)
bingPlugin <- htmlDependency("leaflet.plugins", "2.0.0",
                            # src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/leaflet-plugins/2.0.0/layer/tile/"),
                             src = normalizePath('/media/data/tmp/'),
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
      addMarkers(data = points())
  })
}

shinyApp(ui, server)