# TidyCGE
# Module to produce the whole dashboardbody
# Tony Liu
# Date: Jan, 2018

# analysis module.R
# module functions

analysisUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(id = ns("upload_box"),
        box(width = 5, 
            div(style="display: inline-block;vertical-align:top;",
                fileInput(ns("input_csv"), "upload csv file", width = "360px",
                          accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))), 
            div(style="display: inline-block;vertical-align:top; padding-top: 26px;",
                actionButton(ns("input_toggle"), "Run")),
            div(style="display: inline-block;vertical-align:top; padding-top: 26px;",
                actionButton(ns("input_hide"), "Hide")),
            div(style="display: inline-block;vertical-align:top; padding-top: 26px;",
                actionButton(ns("input_demo"), "demo"))
        ), 
        box(width = 7, height = "120px", 
            div(style="display: inline-block;vertical-align:top;",
            checkboxGroupInput(ns("download_list"),
                             label = "List of variables to download",
                             choices = c("GDP", "FTE", "GVA", "import", "export", "tax"),
                             selected = c("GDP", "FTE", "GVA"), 
                             inline = T,
                             width = "400px")), 
            div(style="display: inline-block;vertical-align:top;",
                uiOutput(ns("download_reg"), width = "400px")), 
            actionButton(ns("download_button"), "download (work in progress)", width = "750px")
          )
        # box(width = 2, height = "123px", 
        # div(style="display:center-align; vertical-align:top; padding-top:10px; padding-left:50px;",
        #     # style="display:center-align",
        #     tags$img(src = "data-science-v2.gif", width = "150px", height = "100px")
        #     )
        # ) # End of box
    ), # End of div
    
    shinyjs::hidden(
    div(id = ns("analysis_div"), 
        fluidPage(
          title = "Analysis",
          fluidRow(column(width = 12,
                          valueBoxOutput(ns("GDP_reg_vb"), width = 3),
                          valueBoxOutput(ns("GDP_state_vb"), width = 3),
                          valueBoxOutput(ns("GDP_roa_vb"), width = 3),
                          valueBoxOutput(ns("GDP_au_vb"), width = 3)
                          ) # end of column,
          ),
          # end of row
          
          fluidRow(column(
            width = 4,
            box(
              title = "Region map",
              status = "primary",
              width = 12,
              solidHeader = FALSE,
              collapsible = TRUE,
              leafletOutput(ns("map_AU"),width="100%",height="440px") %>% withSpinner(type=1, color = DC[2])
            ),
            box(
              title = "Grouped data dimensions",
              status = "primary",
              width = 12,
              solidHeader = FALSE,
              collapsible = TRUE,
              verbatimTextOutput(ns("gd_print")) %>% withSpinner(type=1, color = DC[2])
            ) # end of box
          ), # end of column
          
          column(
            width = 8,
            box(
              title = "GDP (change)",
              status = "primary",
              width = 12,
              # height = 300,
              solidHeader = FALSE,
              collapsible = TRUE,
              tabsetPanel(
                tabPanel("Value", plotlyOutput(ns("GDP_bar"), height = 400) %>% withSpinner(type=1, color = DC[2])),
                tabPanel("Growth", plotlyOutput(ns("GDP_g_bar"), height = 400) %>% withSpinner(type=1, color = DC[2]))
              )
            ),
            box(
              title = "FTE (change)",
              status = "primary",
              width = 12,
              # height = 300,
              solidHeader = FALSE,
              collapsible = TRUE,
              # collapsed = T,
              tabsetPanel(
                tabPanel("Value", plotlyOutput(ns("FTE_bar"), height = 400) %>% withSpinner(type=1, color = DC[2])),
                tabPanel("Growth", plotlyOutput(ns("FTE_g_bar"), height = 400) %>% withSpinner(type=1, color = DC[2]))
              )
            ) # End of Box
          ), # End of column
          
          column(
            width = 12, 
            box(
              title = "GVA (change)",
              status = "primary",
              width = 12,
              # height = 600,
              solidHeader = FALSE,
              collapsible = TRUE,
              
              uiOutput(ns("gvaRegName")), 
              
              tabsetPanel(
                tabPanel("Ordered bar", plotlyOutput(ns("qva_bar_o"), height = 600) %>% withSpinner(type=1, color = DC[2])), 
                tabPanel("Scatter", plotlyOutput(ns("qva_scat"), height = 600) %>% withSpinner(type=1, color = DC[2])), 
                tabPanel("qva_All", plotlyOutput(ns("qva_all"), height = 450) %>% withSpinner(type=1, color = DC[2])), 
                tabPanel("qVAind_All", plotlyOutput(ns("qVAind_all"), height = 450) %>% withSpinner(type=1, color = DC[2])), 
                tabPanel("Stacked bar", plotlyOutput(ns("qva_bar"), height = 600) %>% withSpinner(type=1, color = DC[2])), 
                tabPanel("Filled bar", plotlyOutput(ns("qva_bar_f"), height = 600) %>% withSpinner(type=1, color = DC[2]))
              )
            ) # End of Box
          ) # End of column
          ),
          
          fluidRow(
            box(
              title = "Export",
              status = "primary",
              width = 6,
              # height = 300,
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = FALSE,
              plotlyOutput(ns("qex_ind_bar")) %>% withSpinner(type=1, color = DC[2])
            ), 
            
            box(
              title = "Import",
              status = "primary",
              width = 6,
              # height = 300,
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = FALSE,
              plotlyOutput(ns("qimp_ind_bar")) %>% withSpinner(type=1, color = DC[2])
          ) # End of box
          ), # End of fluidRow
          
          fluidRow(
            box(
              title = "Raw data",
              status = "primary",
              width = 12,
              # height = 700,
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              DT::dataTableOutput(ns("data1_dt")) %>% withSpinner(type=1, color = DC[2])
            ) # End of Box
          ), 
          
          fluidRow(column(width = 12,
                          valueBoxOutput(ns("dim_reg_vb"), width = 3),
                          valueBoxOutput(ns("dim_commo_vb"), width = 3),
                          valueBoxOutput(ns("dim_year_vb"), width = 3), 
                          valueBoxOutput(ns("dim_var_vb"), width = 3)
          ) # end of column,
          ) # End of Fluid Row
        ) # End of fluidPage
    ) # End of div
    ) # End of shinyjs::hidden
  ) # End of tagList
}

analysis <- function(input, output, session, Year, RegName, StateName, Tab) {
  #--------------------------Load csv------------------------------------------
  data1 <- reactive({
    inFile <- input$input_csv
    
    if (input$input_demo) input_file <- ifelse(Tab() == "BAU", "BAUB-ssy simple.csv", 
                                              "BAUB-RUNR-AG1P-devc simple.csv")
    else input_file <- inFile$datapath
    
    dd <- read_csv(input_file, col_types = "cdddddddddddddddddddddddddddddddddddddddd")
    names(dd)[1] <- "Solution"
    
    dd <- dd %>% 
      select(Solution, matches(".*\\-[0-9]{4}")) %>% 
      separate(Solution, into = c("var","d1", "d2", "d3"), remove = F)
    #   mutate(d3 = ifelse(str_count(Solution, "_") == 0, d2, d3)) %>% 
    #   mutate(d2 = ifelse(str_count(Solution, "_") == 0, d1, d2)) %>% 
    #   mutate(d1 = ifelse(str_count(Solution, "_") == 0, v2, d1)) %>% 
    #   mutate(v2 = ifelse(str_count(Solution, "_") == 0, NA, v2))
    # 
    # dd[str_detect(dd$Solution, "c_"), 7:ncol(dd)] <- 1.14*dd[str_detect(dd$Solution, "c_"), 7:ncol(dd)]
    dd
  })
  
  shinyjs::onclick("input_hide",shinyjs::hide(id = "upload_box", anim = TRUE))
  shinyjs::onclick("input_toggle",shinyjs::show(id = "analysis_div", anim = TRUE))
  shinyjs::onclick("input_demo",shinyjs::show(id = "analysis_div", anim = TRUE))
  
  output$gd_print <- renderPrint({
    data1() %>% 
      group_by(var) %>% 
      nest() %>% 
      print(n = 100)
  })
  
  output$data1_dt <- DT::renderDataTable({
    data1()
  }, 
  extensions = c("Buttons","Scroller"), 
  style = "Bootstrap", 
  filter = "top", 
  class = "compact hover display")
  
  #--------------------------UI output------------------------------------------
  output$download_reg <- renderUI({
    checkboxGroupInput(session$ns("download_reg"), "List of regions to download", 
                       c(RegName(), StateName(), "ROA", 
                         "NZ", "CHN", "IND"), 
                       selected = c(RegName(), StateName(), "ROA"), 
                       inline = T)
  })
  
  output$gvaRegName <- renderUI({
    Regname <- ifelse(RegName() == "", StateName(), RegName())
    selectizeInput(session$ns("gvaRegName"), "Select region: ", 
                   c(Regname, StateName(), "ROA"))
  })
  
  #--------------------------Leaflet map--------------------------------------
  output$map_AU <- renderLeaflet({
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
        label = sprintf("<strong>%s</strong><br/>",LGA_state$state) %>% 
          lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      )
  })
  
  #--------------------------ValueBox------------------------------------------
  prefix_year <- reactive({
    ifelse(Tab() == "BAU", "baub-", "baub-runr-")
  })
  
  output$GDP_reg_vb <- renderValueBox({
    data1() %>% 
      filter(var == "GDP2") %>%
      filter(d1 == RegName()) %>% 
      .[[paste0(prefix_year(), Year())]] %>% 
      comma() %>% 
      paste0("$", .) %>% 
      valueBox(paste0("GDP in ", RegName(), ", ", Year()), 
               icon = icon("money"), color = "green")
  })
  
  output$GDP_state_vb <- renderValueBox({
    data1() %>% 
      filter(var == "GDP2") %>%
      filter(d1 == StateName()) %>% 
      .[[paste0(prefix_year(), Year())]] %>% 
      comma() %>% 
      paste0("$", .) %>% 
      valueBox(paste0("GDP in ", StateName(), ", ", Year()), 
               icon = icon("money"), color = "blue")
  })
  
  output$GDP_roa_vb <- renderValueBox({
    data1() %>% 
      filter(var == "GDP2") %>%
      filter(d1 == "ROA") %>% 
      .[[paste0(prefix_year(), Year())]] %>% 
      comma() %>% 
      paste0("$", .) %>% 
      valueBox(paste0("GDP in ", "ROA", ", ", Year()), 
               icon = icon("money"), color = "purple")
  })
  
  output$GDP_au_vb <- renderValueBox({
    data1() %>% 
      filter(var == "GDP2") %>%
      filter(d2 %in% c(RegName(),StateName(),"ROA")) %>% 
      select(matches(".*\\-[0-9]{4}")) %>% 
      map_df(sum) %>% 
      .[[paste0(prefix_year(), Year())]] %>% 
      comma() %>% 
      paste0("$", .) %>% 
      valueBox(paste0("GDP in AU, ", Year()), 
               icon = icon("money"), color = "navy")
  })
  
  output$dim_reg_vb <- renderValueBox({
    valueBox(n_distinct(data1()$d2), "Regions", 
             icon = icon("map"), color = "olive")
  })
  
  output$dim_commo_vb <- renderValueBox({
    valueBox(n_distinct(data1()$d1) - n_distinct(data1()$d2), 
             "Commodities", 
             icon = icon("bar-chart"), color = "aqua")
  })
  
  output$dim_year_vb <- renderValueBox({
    valueBox(ncol(data1()) - 4, "Years", 
             icon = icon("line-chart"), color = "orange")
  })
  
  output$dim_var_vb <- renderValueBox({
    nvar <- data1() %>% 
      group_by(var) %>% 
      nest() %>% 
      nrow() 
    
    valueBox(nvar, "Variables", 
             icon = icon("list-ul"), color = "maroon")
  })
  
  #--------------------------GDP--------------------------------------
  output$GDP_bar <- renderPlotly({
    pp <- data1() %>% 
      filter(var == "GDP2") %>%
      filter(d1 %in% c(RegName(),StateName(),"ROA")) %>% 
      select(d1, matches(".*\\-[0-9]{4}")) %>% 
      gather(year, value, -d1) %>%
      mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>%
      # mutate(d1 = fct_relevel(d1, c(RegName(),StateName(),"ROA"))) %>%
      
      group_by(d1) %>% 
      mutate(growth = value/lag(value)-1) %>% 
      
      filter(year >= start_year) %>% 
      spread(d1, value) %>% 
      
      plot_ly(type = "bar") %>% 
      add_trace(x=~year, y=~get(StateName()), name = StateName(), 
                color = I(DC[3]), visible = T) %>% 
      layout(updatemenus = list(trade_bar_chart_types), 
             # legend = list(x = 0.9, y = 1), 
             xaxis = list(title = "", dtick = 2),
             yaxis = list(title = "GDP"))
    
    if (RegName() != "") {
      pp %>% 
        add_trace(x=~year, y=~get(RegName()), name = RegName(), 
                  color = I(DC[2]), visible = T) %>% 
        add_trace(x=~year, y=~ROA, name = "ROA", color = I(DC[1]), 
                  visible = "legendonly")
    } else {
      pp %>% 
        add_trace(x=~year, y=~ROA, name = "ROA", color = I(DC[1]), 
                  visible = T)
    }
  })
  
  output$GDP_g_bar <- renderPlotly({
    pp <- data1() %>% 
      filter(var == "GDP2") %>%
      filter(d1 %in% c(RegName(),StateName(),"ROA")) %>% 
      select(d1, matches(".*\\-[0-9]{4}")) %>% 
      gather(year, value, -d1) %>%
      mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>%
      # mutate(d1 = fct_relevel(d1, c(RegName(),StateName(),"ROA"))) %>%
      
      arrange(year) %>% 
      group_by(d1) %>% 
      mutate(value = value/lag(value)-1) %>% 
      
      filter(year >= start_year) %>% 
      spread(d1, value) %>% 
      
      plot_ly(type = "bar") %>% 
      add_trace(x=~year, y=~get(StateName()), name = StateName(), 
                color = I(DC[3]), visible = T) %>% 
      layout(updatemenus = list(trade_bar_chart_types), 
             # legend = list(x = 0.9, y = 1), 
             xaxis = list(title = "", dtick = 2),
             yaxis = list(title = "GDP growth", tickformat = "%"))
    
    if (RegName() != "") {
      pp %>% 
        add_trace(x=~year, y=~get(RegName()), name = RegName(), 
                  color = I(DC[2]), visible = T) %>% 
        add_trace(x=~year, y=~ROA, name = "ROA", color = I(DC[1]), 
                  visible = "legendonly")
    } else {
      pp %>% 
        add_trace(x=~year, y=~ROA, name = "ROA", color = I(DC[1]), 
                  visible = T)
    }
  })
  
  #--------------------------GVA--------------------------------------
  GIND2 <- reactive({
    req(input$gvaRegName)
    
    data1() %>% 
      filter(var == "GIND2") %>% 
      filter(d2 == input$gvaRegName) %>% 
      select(d1, matches(".*\\-[0-9]{4}")) %>% 
      gather(year, value, -d1) %>% 
      mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>% 
      mutate(d1 = fct_inorder(d1)) %>% 
      mutate(d1 = fct_rev(d1)) %>% 
      
      arrange(year) %>% 
      group_by(d1) %>% 
      mutate(growth = value/lag(value)-1) %>% 
      
      filter(year >= start_year) %>% 
      ungroup()
  })
  
  output$qva_scat <- renderPlotly({
    
    g1 <- GIND2() %>% 
      ggplot(aes(x=value, y=growth, col=d1)) + 
      geom_point(alpha = .1) + 
      geom_point(aes(frame = year, ids = d1)) +
      scale_color_manual(values = colorRampPalette(brewer.pal(8, "Set2"))(19)) + 
      ggy(percent, "Growth rate") + 
      ggx(comma, "Growth value") +
      ggl("right")
    
    ggplotly(g1) %>% 
      animation_opts(500, redraw = FALSE)
    
  })
  
  output$qva_bar <- renderPlotly({
    GIND2() %>%  
      plot_ly(x=~year, y=~value, color=~d1, type = "bar") %>%
      layout(barmode = 'stack')
  })
  
  output$qva_bar_f <- renderPlotly({
    GIND2() %>% 
      group_by(year) %>% 
      mutate(value = value/sum(value)) %>% 
      
      plot_ly(x=~year, y=~value, color=~d1, type = "bar") %>%
      layout(barmode = 'stack', yaxis = list(tickformat = "%"), margin = list(l=60))
  })
  
  output$qva_bar_o <- renderPlotly({
    dd <- GIND2() %>% 
      mutate(d1 = fct_reorder(d1, value, function(x) mean(x, na.rm=T))) %>% 
      # mutate(d1 = fct_relevel(d1, c("FF", "LSTK", "CROPS", 
      #                               "COG", "IRON", "DWE", 
      #                               "ISR", 
      #                               "ELY", "CMN", "REC", 
      #                               "LMAN", "FMAN", "HMAN", 
      #                               "TRN", "CNS", 
      #                               "TRD", "OFI", "OBS", "OSG")))
      filter(year > min(year))
    
    p1 <- dd %>% 
      mutate(zeros = 0) %>% 
      select(-growth) %>% 
      gather(key, value, -d1, -year) %>% 
      plot_ly(y=~d1, x=~value, color=~d1, frame=~year, 
              type = "scatter", mode = "lines+markers", 
              line = list(dash = 'dot'), 
              showlegend = F) %>%
      layout(margin = list(l=60, t=40), 
             yaxis = list(title = "",
                          showgrid = F, showline = F, showticklabels = T),
             xaxis = list(title = "Growth value", 
                          side = 'top', showline = T, 
                          range = c(min(0, min(dd$value)-100), max(dd$value)+100)))
    
    p2 <- dd %>% 
      arrange(d1) %>% 
      plot_ly(x=~growth, y=~as.numeric(d1)-1, #Note: plotly convert factor start from 0
              frame=~year,  text=~d1, 
              type = "scatter", mode = "lines+markers", 
              line = list(color = DC[1]), showlegend = F) %>%
      layout(margin = list(l=60, t=40), 
             yaxis = list(title = "", zeroline = F, 
                          # ticktext = levels(dd$d1), 
                          showgrid = F, showline = T, showticklabels = F), 
             xaxis = list(title = "Growth rate", zeroline = F, 
                          showline = T, side = 'top', tickprefix = '%', 
                          range = c(min(0, min(dd$growth)-.01), max(dd$growth)+.01)))
    
    subplot(p1, p2, titleX = T, shareY = F, shareX = F) %>% 
      animation_opts(500, redraw = F)
  })
  
  output$qva_all <- renderPlotly({
    g1 <- data1() %>% 
      filter(var == "GIND2") %>% 
      filter(d2 %in% c(RegName(), StateName(), "ROA")) %>% 
      select(d1, d2, matches(".*\\-[0-9]{4}")) %>% 
      gather(year, value, -d1, -d2) %>% 
      mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>% 
      mutate(d2 = fct_relevel(d2, c(RegName(),StateName(),"ROA"))) %>% 
      
      group_by(d1, d2) %>% 
      arrange(year) %>% 
      mutate(growth = value/lag(value) - 1) %>% 
      
      filter(year >= start_year) %>%
      
      ggplot(aes(x= year, y=growth, group=d1)) + 
      geom_line(alpha=.1) + 
      geom_line(aes(frame=as.character(d1)), col=DC[1]) + 
      facet_wrap(~d2) + 
      ggx(comma, "") + 
      ggy(percent, "Growth rate") + 
      ggl("none")
    
    ggplotly(g1) %>% 
      animation_slider(currentvalue = list(prefix = "Commodity: ", 
                                           font = list(color = DC[1])))
  })
  
  output$qVAind_all <- renderPlotly({
    g1 <- data1() %>% 
      filter(var == "GIND2") %>% 
      filter(d2 %in% c(RegName(), StateName(), "ROA")) %>% 
      select(d1, d2, matches(".*\\-[0-9]{4}")) %>% 
      gather(year, value, -d1, -d2) %>% 
      mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>% 
      mutate(d2 = fct_relevel(d2, c(RegName(),StateName(),"ROA"))) %>% 
      
      filter(year >= start_year) %>%
      
      ggplot(aes(x= year, y=value, group=d1)) + 
      geom_line(alpha=.1) + 
      geom_line(aes(frame=as.character(d1)), col=DC[2]) + 
      facet_wrap(~d2) + 
      ggx(comma, "") + 
      ggy(comma, "Growth value") + 
      ggl("none")
    
    ggplotly(g1) %>% 
      animation_slider(currentvalue = list(prefix = "Commodity: ",
                                           font = list(color = DC[2])))
  })
  
  #--------------------------FTE--------------------------------------
  output$FTE_bar <- renderPlotly({
    pp <- data1() %>% 
      filter(var == "EMP2") %>%
      filter(d1 %in% c(RegName(),StateName(),"ROA")) %>% 
      select(d1, matches(".*\\-[0-9]{4}")) %>% 
      gather(year, value, -d1) %>%
      mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>%
      # mutate(d1 = fct_relevel(d1, c(RegName(),StateName(),"ROA"))) %>%
      
      group_by(d1) %>% 
      mutate(growth = value/lag(value)-1) %>% 
      
      filter(year >= start_year) %>% 
      spread(d1, value) %>% 
      
      plot_ly(type = "bar") %>% 
      add_trace(x=~year, y=~get(StateName()), name = StateName(), 
                color = I(DC[3]), visible = T) %>% 
      layout(updatemenus = list(trade_bar_chart_types), 
             # legend = list(x = 0.9, y = 1), 
             xaxis = list(title = "", dtick = 2),
             yaxis = list(title = "FTE"))
    
    if (RegName() != "") {
      pp %>% 
        add_trace(x=~year, y=~get(RegName()), name = RegName(), 
                  color = I(DC[2]), visible = T) %>% 
        add_trace(x=~year, y=~ROA, name = "ROA", color = I(DC[1]), 
                  visible = "legendonly")
    } else {
      pp %>% 
        add_trace(x=~year, y=~ROA, name = "ROA", color = I(DC[1]), 
                  visible = T)
    }
  })
  
  output$FTE_g_bar <- renderPlotly({
    pp <- data1() %>% 
      filter(var == "EMP2") %>%
      filter(d1 %in% c(RegName(),StateName(),"ROA")) %>% 
      select(d1, matches(".*\\-[0-9]{4}")) %>% 
      gather(year, value, -d1) %>%
      mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>%
      # mutate(d1 = fct_relevel(d1, c(RegName(),StateName(),"ROA"))) %>%
      
      arrange(year) %>% 
      group_by(d1) %>% 
      mutate(value = value/lag(value)-1) %>% 
      
      filter(year >= start_year) %>% 
      spread(d1, value) %>% 
      
      plot_ly(type = "bar") %>% 
      add_trace(x=~year, y=~get(StateName()), name = StateName(), 
                color = I(DC[3]), visible = T) %>% 
      layout(updatemenus = list(trade_bar_chart_types), 
             # legend = list(x = 0.9, y = 1), 
             xaxis = list(title = "", dtick = 2),
             yaxis = list(title = "FTE growth", tickformat = "%"))
    
    if (RegName() != "") {
      pp %>% 
        add_trace(x=~year, y=~get(RegName()), name = RegName(), 
                  color = I(DC[2]), visible = T) %>% 
        add_trace(x=~year, y=~ROA, name = "ROA", color = I(DC[1]), 
                  visible = "legendonly")
    } else {
      pp %>% 
        add_trace(x=~year, y=~ROA, name = "ROA", color = I(DC[1]), 
                  visible = T)
    }
  })
  
  #--------------------------Trade--------------------------------------
  output$qex_ind_bar <- renderPlotly({
    pp <- data1() %>% 
      filter(var == "EXPORTS1") %>% 
      select(-Solution, -var, -d3) %>%
      mutate(d1 = fct_inorder(d1)) %>% 
      gather(year, value, -d1, -d2) %>% 
      mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>% 
      
      filter(year >= start_year) %>% 
      filter(d2 %in% c(RegName(), StateName(), "ROA")) %>%
      spread(d2, value) %>% 
      
      # plot_ly(type = "scatter", mode = "lines+markers") %>% 
      plot_ly(type = "bar") %>% 
      add_trace(x=~year, y=~get(StateName()), frame = ~d1, name = StateName(), 
                color = I(DC[3]), visible = T) %>% 
      
      layout(updatemenus = list(trade_bar_chart_types), 
             # legend = list(x = 0.9, y = 1), 
             xaxis = list(title = "", dtick = 2),
             yaxis = list(title = "Exports"))
    
    if (RegName() == "") {
      pp %>% 
        add_trace(x=~year, y=~ROA, frame = ~d1, name = "ROA", color = I(DC[1]), 
                  visible = T) %>% 
        animation_slider(
          currentvalue = list(prefix = "Commodity ", font = list(color="red"))
        )
    } else {
      pp %>% 
        add_trace(x=~year, y=~get(RegName()), frame = ~d1, name = RegName(), 
                  color = I(DC[2]), visible = T) %>%
        add_trace(x=~year, y=~ROA, frame = ~d1, name = "ROA", color = I(DC[1]), 
                  visible = "legendonly") %>% 
        animation_slider(
          currentvalue = list(prefix = "Commodity ", font = list(color="red"))
        )
    }
  })
  
  output$qimp_ind_bar <- renderPlotly({
    
    pp <- data1() %>% 
      filter(var == "IMPORTS1") %>% 
      select(-Solution, -var, -d3) %>%
      mutate(d1 = fct_inorder(d1)) %>% 
      gather(year, value, -d1, -d2) %>% 
      mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>% 
      
      filter(year >= start_year) %>% 
      filter(d2 %in% c(RegName(), StateName(), "ROA")) %>%
      spread(d2, value) %>% 
      
      # plot_ly(type = "scatter", mode = "lines+markers") %>% 
      plot_ly(type = "bar") %>% 
      add_trace(x=~year, y=~get(StateName()), frame = ~d1, name = StateName(), 
                color = I(DC[3]), visible = T) %>% 
      
      layout(updatemenus = list(trade_bar_chart_types), 
             # legend = list(x = 0.9, y = 1), 
             xaxis = list(title = "", dtick = 2),
             yaxis = list(title = "Imports"))
    
    if (RegName() == "") {
      pp %>% 
        add_trace(x=~year, y=~ROA, frame = ~d1, name = "ROA", color = I(DC[1]), 
                  visible = T) %>% 
        animation_slider(
          currentvalue = list(prefix = "Commodity ", font = list(color="red"))
        )
    } else {
      pp %>% 
        add_trace(x=~year, y=~get(RegName()), frame = ~d1, name = RegName(), 
                  color = I(DC[2]), visible = T) %>%
        add_trace(x=~year, y=~ROA, frame = ~d1, name = "ROA", color = I(DC[1]), 
                  visible = "legendonly") %>% 
        animation_slider(
          currentvalue = list(prefix = "Commodity ", font = list(color="red"))
        )
    }
  })
}









