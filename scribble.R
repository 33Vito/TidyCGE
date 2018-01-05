
setwd("C:/Users/Tony/Google Drive/shinyapp/CGE dashboard/TidyCGE_v0.2")
# setwd("~/Google Drive/shinyapp/CGE dashboard/TidyCGE_v0.2")
source("TL gogogo.R")

options(scipen=999, expressions=50000, 
        DT.options = list(pageLength = 8,
                          scrollX = TRUE,
                          dom = 'Bfrtip',
                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), 
                          autoWidth = TRUE))


data1 <- read_csv("BAUB-ssy copy.csv") %>% 
  select(Solution, matches(".*\\-[0-9]{4}")) %>% 
  separate(Solution, into = c("v1","v2","d1", "d2", "d3"), remove = F) %>% 
  mutate(d3 = ifelse(str_count(Solution, "_") == 0, d2, d3)) %>% 
  mutate(d2 = ifelse(str_count(Solution, "_") == 0, d1, d2)) %>% 
  mutate(d1 = ifelse(str_count(Solution, "_") == 0, v2, d1)) %>% 
  mutate(v2 = ifelse(str_count(Solution, "_") == 0, NA, v2))

data1 %>% 
  # head() %>% 
  datatable(extensions = "Buttons", filter = "top", 
            class = "compact hover display")

data1 %>% 
  mutate(group = ifelse(str_count(Solution, "_") == 0, v1, paste(v1,v2, sep="_"))) %>% 
  group_by(group) %>% 
  nest()

data1 %>% 
  filter(v1 == "gdp", v2 == "r") %>% 
  filter(d1 %in% c("SYD", "RON","ROA")) %>% 
  select(d1, matches(".*\\-[0-9]{4}")) %>% 
  gather(year, value, -d1) %>%
  mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>%
  ggplot(aes(x = year, y = value/100, fill = d1)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~d1) +
  ggy(percent, "GDP") + 
  ggl("none")

library(plotly)
library(rCharts)
library(xts)
library(dygraphs)
library(googleVis)
library(ggvis)

g1 <- data1 %>% 
  filter(v1 == "c", v2 == "GDP") %>%
  filter(d2 %in% c("SYD", "RON","ROA")) %>% 
  select(d2, matches(".*\\-[0-9]{4}")) %>% 
  gather(year, value, -d2) %>%
  mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>%
  
  ggplot(aes(x = year, y = value, fill = d2)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~d2) +
  ggy(comma, "") +
  ggx(identity, "") +
  ggf() + 
  ggl("none")

ggplotly(g1, tooltip = c("year", "value")) %>% 
  highlight("plotly_hover") %>% 
  layout(margin = list(l=60))
  
  # ggvis(~year, ~value, stroke = ~d2) %>% 
  # layer_lines() %>% 
  # add_tooltip(function(x) format(x), "hover")
  
  # plot_ly(x=~year, y=~value, color=~d2, type = 'scatter', mode = 'lines') %>%
  # layout(title = 'GDP',
  #        xaxis = list(title = ''),
  #        yaxis = list (title = ''))
  
  # nPlot(value~year, group="d2", type = "bar", data = ., dom = "GDP")
  
  # spread(d2, value) %>%
  # mutate(year = ymd(paste0(year, "-7-1"))) %>%
  # tidyquant::as_xts(year) %>%
  # dygraph() %>% 
  # dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = F)

  # gvisLineChart("year", "d2") %>%
  # plot()

qva <- data1 %>% 
  filter(v1 == "qva") %>% 
  filter(d2 == "SYD") %>% 
  select(d1, matches(".*\\-[0-9]{4}")) %>% 
  gather(year, value, -d1) %>% 
  mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>% 
  mutate(d1 = fct_inorder(d1)) %>% 
  rename(qva = value)

qVAind <- data1 %>% 
  filter(v1 == "c", v2 == "qVAind") %>% 
  filter(d2 == "SYD") %>% 
  select(d1, matches(".*\\-[0-9]{4}")) %>% 
  gather(year, value, -d1) %>% 
  mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>% 
  mutate(d1 = fct_inorder(d1)) %>% 
  rename(qVAind = value)

g1 <- inner_join(qva, qVAind, by = c("d1", "year")) %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(x=qVAind, y=qva, col=d1)) + 
  geom_point(alpha = .1) + 
  geom_point(aes(frame = year, ids = d1)) +
  # scale_y_log10() + 
  ggl("right")

ggplotly(g1) %>% 
  animation_opts(500, redraw = FALSE)

qVAind %>% 
  plot_ly(x=~year, y=~qVAind, color=~d1, type = "bar") %>%
  layout(barmode="stack") %>% 
  highlight()

dd <- qVAind %>% 
  left_join(qva, by = c("year", "d1")) %>% 
  mutate(d1 = fct_reorder(d1, qVAind))

p1 <- dd %>% 
  mutate(zeros = 0) %>% 
  select(-qva) %>% 
  gather(key, value, -d1, -year) %>% 
  plot_ly(y=~d1, x=~value, color=~d1, frame=~year, 
          type = "scatter", mode = "lines+markers", 
          line = list(dash = 'dot'), 
          showlegend = F) %>%
  layout(margin = list(l=60), 
         yaxis = list(title = "",
                      showgrid = F, showline = F, showticklabels = T),
         xaxis = list(title = "qVAind", showline = T, 
                      range = c(0, 1.1*max(dd$qVAind))))

p2 <- dd %>% 
  arrange(d1) %>% 
  plot_ly(x=~qva, y=~as.numeric(d1), frame=~year, 
          type = "scatter", mode = "lines+markers", 
          line = list(color = DC[1]), showlegend = F) %>%
  layout(margin = list(l=60, t=40), 
         yaxis = list(title = "",
                      showgrid = F, showline = T, showticklabels = F), 
         xaxis = list(title = "qva", 
                      side = 'top', tickprefix = '%', 
                      range = c(0, 1.1*max(dd$qva))))

subplot(p1, p2, shareY = TRUE, titleX = TRUE) %>% 
  animation_opts(500, redraw = T)

# library(sp)
# library(rgdal)
# library(rgeos)
# library(ggmap)
# 
# SYD_LGA <- readxl::read_excel("ABS SYD LGA.xlsx", skip = 9)
# SYD_LGA <- SYD_LGA$LGA[SYD_LGA$X__1 > 0][1:44]
# 
# AU_LGA_2011 <- rgdal::readOGR(dsn = "./NSW 2011/LGA11aAust.shp", 
#                               layer = "LGA11aAust", 
#                               GDAL1_integer64_policy = T)
# 
# AU_LGA_2011$group <- ifelse(AU_LGA_2011$LGA_NAME11 %in% SYD_LGA, "10", AU_LGA_2011$STATE_CODE)
# 
# LGA_state <- gUnaryUnion(AU_LGA_2011, id = AU_LGA_2011@data$group)
# LGA_state <- rgeos::gSimplify(LGA_state, tol=0.1, topologyPreserve = T)
# row.names(LGA_state) <- as.character(1:length(LGA_state))
# LGA_state <- SpatialPolygonsDataFrame(LGA_state, data.frame(group = unique(AU_LGA_2011$group), 
#                                                             state = c("NSW", "ShockReg", 
#                                                             "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", NA)))

library(leaflet)
load("SYD_LGA_state.RData")

leaflet(LGA_state) %>%
  addTiles() %>%
  # addProviderTiles("Stamen.TonerLite") %>%
  # addPolygons()
  addPolygons(
    # fillColor = ~pal(density),
    fillColor = ifelse(LGA_state$group == "10", DC[1], DC[2]),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 2,
      color = "black",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ), 
    label = sprintf("<strong>%s</strong><br/>",LGA_state$group) %>% 
      lapply(htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  )


library(chorddiag)
library(d3heatmap)

data1 %>% 
  filter(v1 == "qex") %>% 
  rename(value = "baub-2017") %>% 
  select(d1, d2, value) %>% 
  spread(d2, value) %>% 
  as.data.frame() %>% 
  `rownames<-`(NULL) %>% 
  column_to_rownames("d1") %>% 
  select("SYD", "RON", "ROA", "NZ", "CHN", "IND") %>% 
  as.matrix() %>% 
  
  d3heatmap(colors = "Blues", dendrogram = "none", scale = "none")
 
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
         # args = list(list("type", "scatter"), list("mode", "lines+markers")),
         label = "Line")
  ))

data1 %>% 
  filter(v1 == "qex") %>% 
  select(-Solution, -v1, -v2, -d3) %>%
  mutate(d1 = fct_inorder(d1)) %>% 
  gather(year, value, -d1, -d2) %>% 
  mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>% 
  
  filter(year >= start_year) %>% 
  filter(d2 %in% c("SYD", "RON", "ROA")) %>%
  spread(d2, value) %>% 
  
  # plot_ly(type = "scatter", mode = "markers+lines") %>% 
  plot_ly(type = "bar") %>% 
  add_trace(x=~year, y=~ROA, frame = ~d1, name = "ROA", color = I(DC[1]), 
          visible = "legendonly") %>% 
  add_trace(x=~year, y=~SYD, frame = ~d1, name = "SYD", color = I(DC[2]), visible = T) %>% 
  add_trace(x=~year, y=~RON, frame = ~d1, name = "RON", color = I(DC[3]), visible = T) %>% 
  # add_trace(x=~d1, y=~ROA, frame = ~year, name = "ROA", color = I(DC[1]), visible = "legendonly") %>% 
  
  layout(updatemenus = list(trade_bar_chart_types), 
         legend = list(x = 0.9, y = 1), 
         xaxis = list(title = "", dtick = 2),
         yaxis = list(title = "Aggregated export")) %>% 
  
  animation_opts(1000, easing = "elastic", redraw = T) %>%
  
  animation_slider(
    currentvalue = list(prefix = "Commodity ", font = list(color="black"))
  )


















