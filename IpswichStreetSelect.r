#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(ozmaps)
library(sf)
library(rvest)
library(mapview)
library(osmdata)
library(remotes)
library(showtext)
library(ggmap)

library("rnaturalearth")
library("rnaturalearthdata")
library("maps")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)

#filtered <- read.table("/Users/Bram/Desktop/Weather Data Chermside - Sheet1.csv", header = TRUE, sep = ",",
#                     colClasses = c("NULL", "NULL", "NULL","character","NULL", "character", "character","NULL", "NULL","NULL", "NULL","NULL", "NULL","NULL", "NULL","NULL", "NULL","NULL", "NULL","NULL", "NULL"))
#filtered <- filtered[139:163,]
#view(filtered)

nc <- ozmaps::ozmap_states %>% filter(NAME=="Queensland")
states <- cbind(nc, st_coordinates(st_centroid(nc)))
nd <- st_cast(nc, "POLYGON")
nd <- cbind(nd, st_coordinates(st_centroid(nd)))

mapinfo <- ozmaps::abs_lga
mapinfo <- cbind(mapinfo, st_coordinates(st_centroid(mapinfo)))
qldLGA <- st_crop(mapinfo$geometry, nc$geometry)

available_tags("highway")
getbb("Ipswich Queensland")
big_streets <- getbb("Ipswich Queensland")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()

med_streets <- getbb("Ipswich Queensland")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

small_streets <- getbb("Ipswich Queensland")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street","unclassified","service", "footway")) %>%
  osmdata_sf()

rose1 <- filter(med_streets[["osm_lines"]], name=="Rosewood Warrill View Road")
rose2 <- filter(big_streets[["osm_lines"]], name=="Old toowoomba road")
rose3 <- filter(big_streets[["osm_lines"]], name=="Brisbane Street")
rose4 <- filter(med_streets[["osm_lines"]], name=="Lobb Street")
rose5 <- filter(small_streets[["osm_lines"]], name=="Keogh Street")
rose6 <- filter(med_streets[["osm_lines"]], name=="Sydney Street")
rose7 <- filter(med_streets[["osm_lines"]], name=="Blackall Street")
rose8 <- filter(small_streets[["osm_lines"]], name=="Milford Street")
rose9 <- filter(med_streets[["osm_lines"]], name=="Bremer Street")
rose10 <- filter(med_streets[["osm_lines"]], name=="Olga Street")
rose11 <- filter(small_streets[["osm_lines"]], name=="East Owen Street")
rose12 <- filter(small_streets[["osm_lines"]], name=="Melbourne Street")

war1 <- filter(med_streets[["osm_lines"]], name=="Karrabin Rosewood Road")
war2 <- filter(med_streets[["osm_lines"]], name=="Ipswich Rosewood Road")
war3 <- filter(small_streets[["osm_lines"]], name=="Hodgsons Road")
war4 <- filter(small_streets[["osm_lines"]], name=="Bell Road")
war5 <- filter(med_streets[["osm_lines"]], name=="Roseberry Road")
war6 <- filter(med_streets[["osm_lines"]], name=="MacGregor Road")

warq11 <- filter(big_streets[["osm_lines"]], name=="Cunningham")
warq12 <- filter(med_streets[["osm_lines"]], name=="Middle Road")
warq13 <- filter(small_streets[["osm_lines"]], name=="Hall Street")
warq1 <- filter(small_streets[["osm_lines"]], name=="Bowen Street")
warq2 <- filter(small_streets[["osm_lines"]], name=="Berry Street")
warq3 <- filter(big_streets[["osm_lines"]], name=="Limestone Street")
warq4 <- filter(med_streets[["osm_lines"]], name=="Nelson Street")
warq5 <- filter(small_streets[["osm_lines"]], name=="Agnes Street")
warq6 <- filter(big_streets[["osm_lines"]], name=="Brisbane Road")
warq7 <- filter(med_streets[["osm_lines"]], name=="Brisbane Road")
warq8 <- filter(small_streets[["osm_lines"]], name=="Brisbane Road")
warq9 <- filter(med_streets[["osm_lines"]], name=="Clare Road")
warq10 <- filter(med_streets[["osm_lines"]], name=="Collingwood Drive")


warw1 <- filter(big_streets[["osm_lines"]], name=="Warwick Road")
warw2 <- filter(big_streets[["osm_lines"]], name=="Old toowoomba Road")
warw3 <- filter(small_streets[["osm_lines"]], name=="Lawrence Street")
warw4 <- filter(small_streets[["osm_lines"]], name=="Pelican Street")
warw5 <- filter(small_streets[["osm_lines"]], name=="Bank Street")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Road closure due to flooding"),
  textInput(inputId = "name",
            label = "Place:",
            value = "",
            placeholder = "Ipswich"),
  # Sidebar with a slider input for number of bins 
  tags$head(tags$script(src = "message-handler.js")),
  titlePanel("Level of Rainfall"),
  actionButton("do1", "Green"),
  actionButton("do2", "Yellow"),
  actionButton("do3", "Orange"),
  actionButton("do4", "Red"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Rain fall (mm):",
                  min = 0,
                  max = 250,
                  value = 50)
    ),

    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distmap")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  v1 <- reactiveValues(data = NULL)
  v2 <- reactiveValues(data = NULL)
  v3 <- reactiveValues(data = NULL)
  v4 <- reactiveValues(data = NULL)
  observeEvent(input$do1, {
    v1$data <- "green"
  })
  observeEvent(input$do2, {
    v2$data <- "yellow"
  })
  observeEvent(input$do3, {
    v3$data <- "orange"
  })
  observeEvent(input$do4, {
    v4$data <- "red"
  })
  output$distmap <- renderPlot({
    if (is.null(v1$data))return()
    if (is.null(v2$data))return()
    if (is.null(v3$data))return()
    if (is.null(v4$data))return()
    ggplot(data = world) +
      geom_sf(data = med_streets$osm_lines,
              inherit.aes = FALSE,
              color = "black",
              size = .3,
              alpha = .5) +
      geom_sf(data = small_streets$osm_lines,
              inherit.aes = FALSE,
              color = "#666666",
              size = .2,
              alpha = .3) +
      geom_sf(data = big_streets$osm_lines,
              inherit.aes = FALSE,
              color = "black",
              size = .5,
              alpha = .6) +
      geom_sf(data = rose1,inherit.aes = FALSE,color = v1$data,size = 1,alpha = 1) +
      geom_sf(data = rose2,inherit.aes = FALSE,color = v1$data,size = 1,alpha = 1) +
      geom_sf(data = rose3,inherit.aes = FALSE,color = v1$data,size = 1,alpha = 1) +
      geom_sf(data = rose4,inherit.aes = FALSE,color = v1$data,size = 1,alpha = 1) +
      geom_sf(data = rose5,inherit.aes = FALSE,color = v1$data,size = 1,alpha = 1) +
      geom_sf(data = rose6,inherit.aes = FALSE,color = v1$data,size = 1,alpha = 1) +
      geom_sf(data = rose7,inherit.aes = FALSE,color = v1$data,size = 1,alpha = 1) +
      geom_sf(data = rose8,inherit.aes = FALSE,color = v1$data,size = 1,alpha = 1) +
      geom_sf(data = rose9,inherit.aes = FALSE,color = v1$data,size = 1,alpha = 1) +
      geom_sf(data = rose10,inherit.aes = FALSE,color = v1$data,size = 1,alpha = 1) +
      geom_sf(data = rose11,inherit.aes = FALSE,color = v1$data,size = 1,alpha = 1) +
      geom_sf(data = rose12,inherit.aes = FALSE,color = v1$data,size = 1,alpha = 1) +
      geom_sf(data = war1,inherit.aes = FALSE,color = v2$data,size = 1,alpha = 1) +
      geom_sf(data = war2,inherit.aes = FALSE,color = v2$data,size = 1,alpha = 1) +
      geom_sf(data = war3,inherit.aes = FALSE,color = v2$data,size = 1,alpha = 1) +
      geom_sf(data = war4,inherit.aes = FALSE,color = v2$data,size = 1,alpha = 1) +
      geom_sf(data = war5,inherit.aes = FALSE,color = v2$data,size = 1,alpha = 1) +
      geom_sf(data = war6,inherit.aes = FALSE,color = v2$data,size = 1,alpha = 1) +
      geom_sf(data = warq1,inherit.aes = FALSE,color = v3$data,size = 1,alpha = 1) +
      geom_sf(data = warq2,inherit.aes = FALSE,color = v3$data,size = 1,alpha = 1) +
      geom_sf(data = warq3,inherit.aes = FALSE,color = v3$data,size = 1,alpha = 1) +
      geom_sf(data = warq4,inherit.aes = FALSE,color = v3$data,size = 1,alpha = 1) +
      geom_sf(data = warq5,inherit.aes = FALSE,color = v3$data,size = 1,alpha = 1) +
      geom_sf(data = warq6,inherit.aes = FALSE,color = v3$data,size = 1,alpha = 1) +
      geom_sf(data = warq7,inherit.aes = FALSE,color = v3$data,size = 1,alpha = 1) +
      geom_sf(data = warq8,inherit.aes = FALSE,color = v3$data,size = 1,alpha = 1) +
      geom_sf(data = warq9,inherit.aes = FALSE,color = v3$data,size = 1,alpha = 1) +
      geom_sf(data = warq10,inherit.aes = FALSE,color = v3$data,size = 1,alpha = 1) +
      geom_sf(data = warq11,inherit.aes = FALSE,color = v3$data,size = 1,alpha = 1) +
      geom_sf(data = warq12,inherit.aes = FALSE,color = v3$data,size = 1,alpha = 1) +
      geom_sf(data = warq13,inherit.aes = FALSE,color = v3$data,size = 1,alpha = 1) +
      geom_sf(data = warw1,inherit.aes = FALSE,color = v4$data,size = 1,alpha = 1) +
      geom_sf(data = warw2,inherit.aes = FALSE,color = v4$data,size = 1,alpha = 1) +
      geom_sf(data = warw3,inherit.aes = FALSE,color = v4$data,size = 1,alpha = 1) +
      geom_sf(data = warw4,inherit.aes = FALSE,color = v4$data,size = 1,alpha = 1) +
      geom_sf(data = warw5,inherit.aes = FALSE,color = v4$data,size = 1,alpha = 1) +
      geom_sf(data = qldLGA, colour = "black", size = .5, fill = "NA") +
      coord_sf(xlim = c(152.38272, 152.94088), ylim = c(-27.86547, -27.49033), expand = FALSE)+
    theme(plot.title = element_text(size = 20, family = "lato", face="bold", hjust=.5),
          plot.subtitle = element_text(family = "lato", size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) +
      labs(title = "Green Classification Rainfall", subtitle = "0-100mm")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
