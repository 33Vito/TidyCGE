
#--------------------loading packages-----------------------
# setwd("C:/Users/Tony/Google Drive/shinyapp/CGE dashboard/TidyCGE_v0.2")
# setwd("~/Google Drive/shinyapp/CGE dashboard/TidyCGE_v0.2")
source("TL gogogo.R")

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(plotly)

options(shiny.maxRequestSize=30*1024^2, 
        scipen=999, expressions=50000, 
        DT.options = list(pageLength = 15,
                          scrollX = TRUE,
                          dom = 'Bfrtip',
                          rowReorder = TRUE, 
                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), 
                          autoWidth = TRUE))

start_year <- 2016

#--------------------loading Shapefiles-----------------------
library(leaflet)

# load image data currently
load("SYD_LGA_state.RData")

# library(sp)
# library(rgdal)
# library(rgeos)

# AU_LGA_2011 <- rgdal::readOGR(dsn = "./NSW 2011/LGA11aAust.shp",
#                               layer = "LGA11aAust",
#                               GDAL1_integer64_policy = T)

SYD_LGA <- readxl::read_excel("ABS SYD LGA.xlsx", skip = 9)
SYD_LGA <- SYD_LGA$LGA[SYD_LGA$X__1 > 0][1:44]

# AU_LGA_2011$group <- ifelse(AU_LGA_2011$LGA_NAME11 %in% SYD_LGA, "10", AU_LGA_2011$STATE_CODE)
# 
# LGA_state <- gUnaryUnion(AU_LGA_2011, id = AU_LGA_2011@data$group)
# LGA_state <- rgeos::gSimplify(LGA_state, tol=0.1, topologyPreserve = T)
# row.names(LGA_state) <- as.character(1:length(LGA_state))
# LGA_state <- SpatialPolygonsDataFrame(LGA_state,
#                                       data.frame(
#                                         group = unique(AU_LGA_2011$group),
#                                         state = c("NSW", "ShockReg",
#                                                   "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", NA)
#                                       ))
# rm(AU_LGA_2011)

# load module
source("analysis module.R")

#--------------------plotly misc-----------------------

trade_bar_chart_types <- list(
  type = "buttons",
  direction = "left",
  xanchor = 'center',
  yanchor = "top",
  pad = list('r'= 0, 't'= 10, 'b' = 10),
  x = 0,
  y = 1.3,
  buttons = list(
    
    list(method = "restyle",
         args = list("type", "bar"),
         label = "Bar"),
    
    list(method = "restyle",
         args = list("type", "scatter"),
         label = "Line")
  ))




