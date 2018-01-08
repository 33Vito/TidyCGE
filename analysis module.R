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
                tabPanel("Growth", plotlyOutput(ns("GDP_g_bar"), height = 500) %>% withSpinner(type=1, color = DC[2]))
              )
            ),
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
            ),
            box(
              title = "FTE (change)",
              status = "primary",
              width = 12,
              # height = 300,
              solidHeader = FALSE,
              collapsible = TRUE,
              # collapsed = T,
              tabsetPanel(# tabPanel("Value", plotlyOutput()),
                tabPanel("Growth", plotlyOutput(ns("FTE_bar"), height = 500) %>% withSpinner(type=1, color = DC[2])))
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
              # tabsetPanel(
              # tabPanel("Export", plotlyOutput(ns("qex_ind_bar")) %>% withSpinner(type=1, color = DC[2])),
              # tabPanel("Import", plotlyOutput(ns("qimp_ind_bar")) %>% withSpinner(type=1, color = DC[2]))
              # )
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
    
    if (input$input_demo) input_file <- ifelse(Tab() == "BAU", "BAUB-ssy copy.csv", 
                                              "BAUB-AG1P-devc copy.csv")
    else input_file <- inFile$datapath
    
    dd <- read_csv(input_file, col_types = "cdddddddddddddddddddddddddddddddddddddddd")
    names(dd)[1] <- "Solution"
    
    dd <- dd %>% 
      select(Solution, matches(".*\\-[0-9]{4}")) %>% 
      separate(Solution, into = c("v1","v2","d1", "d2", "d3"), remove = F) %>% 
      mutate(d3 = ifelse(str_count(Solution, "_") == 0, d2, d3)) %>% 
      mutate(d2 = ifelse(str_count(Solution, "_") == 0, d1, d2)) %>% 
      mutate(d1 = ifelse(str_count(Solution, "_") == 0, v2, d1)) %>% 
      mutate(v2 = ifelse(str_count(Solution, "_") == 0, NA, v2))
    
    dd[str_detect(dd$Solution, "c_"), 7:ncol(dd)] <- 1.14*dd[str_detect(dd$Solution, "c_"), 7:ncol(dd)]
    dd
  })
  
  shinyjs::onclick("input_hide",shinyjs::hide(id = "upload_box", anim = TRUE))
  shinyjs::onclick("input_toggle",shinyjs::show(id = "analysis_div", anim = TRUE))
  shinyjs::onclick("input_demo",shinyjs::show(id = "analysis_div", anim = TRUE))
  
  output$gd_print <- renderPrint({
    data1() %>% 
      mutate(group = ifelse(str_count(Solution, "_") == 0, v1, paste(v1,v2, sep="_"))) %>% 
      group_by(group) %>% 
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
    selectizeInput(session$ns("gvaRegName"), "Select region: ", 
                   c(RegName(), StateName(), "ROA"))
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
    c <- data1() %>% 
      filter(v1 == "c", v2 == "GDP") %>%
      filter(d2 == RegName()) %>% 
      .[paste0(prefix_year(), Year())]
    
    r <- data1() %>% 
      filter(v1 == "gdp", v2 == "r") %>%
      filter(d1 == RegName()) %>% 
      .[paste0(prefix_year(), Year())]/100
    
    round(c/r) %>% 
      comma() %>% 
      paste0("$", .) %>% 
      valueBox(paste0("GDP in ", RegName(), ", ", Year()), 
               icon = icon("money"), color = "green")
  })
  
  output$GDP_state_vb <- renderValueBox({
    c <- data1() %>% 
      filter(v1 == "c", v2 == "GDP") %>%
      filter(d2 == StateName()) %>% 
      .[paste0(prefix_year(), Year())]
    
    r <- data1() %>% 
      filter(v1 == "gdp", v2 == "r") %>%
      filter(d1 == StateName()) %>% 
      .[paste0(prefix_year(), Year())]/100
    
    round(c/r) %>% 
      comma() %>% 
      paste0("$", .) %>% 
      valueBox(paste0("GDP in ", StateName(), ", ", Year()), 
               icon = icon("money"), color = "blue")
  })
  
  output$GDP_roa_vb <- renderValueBox({
    c <- data1() %>% 
      filter(v1 == "c", v2 == "GDP") %>%
      filter(d2 == "ROA") %>% 
      .[paste0(prefix_year(), Year())]
    
    r <- data1() %>% 
      filter(v1 == "gdp", v2 == "r") %>%
      filter(d1 == "ROA") %>% 
      .[paste0(prefix_year(), Year())]/100
    
    round(c/r) %>% 
      comma() %>% 
      paste0("$", .) %>% 
      valueBox(paste0("GDP in ", "ROA", ", ", Year()), 
               icon = icon("money"), color = "purple")
  })
  
  output$GDP_au_vb <- renderValueBox({
    c <- data1() %>% 
      filter(v1 == "c", v2 == "GDP") %>%
      filter(d2 %in% c(RegName(),StateName(),"ROA")) %>% 
      select(matches(".*\\-[0-9]{4}")) %>% 
      map_df(sum) %>% 
      .[paste0(prefix_year(), Year())]
    
    r <- data1() %>% 
      filter(v1 == "gdp", v2 == "r") %>%
      filter(d1 == "AUS") %>% 
      .[paste0(prefix_year(), Year())]/100
    
    round(c/r) %>% 
      comma() %>% 
      paste0("$", .) %>% 
      valueBox(paste0("GDP in AU, ", Year()), 
               icon = icon("money"), color = "navy")
  })
  
  output$dim_reg_vb <- renderValueBox({
    valueBox(n_distinct(data1()$d3) - 3, "Regions", 
             icon = icon("map"), color = "olive")
  })
  
  output$dim_commo_vb <- renderValueBox({
    valueBox(n_distinct(data1()$d2) - n_distinct(data1()$d3) - 2, 
             "Commodities", 
             icon = icon("bar-chart"), color = "aqua")
  })
  
  output$dim_year_vb <- renderValueBox({
    valueBox(ncol(data1()) - 6, "Years", 
             icon = icon("line-chart"), color = "orange")
  })
  
  output$dim_var_vb <- renderValueBox({
    nvar <- data1() %>% 
      mutate(group = ifelse(str_count(Solution, "_") == 0, 
                            v1, paste(v1,v2, sep="_"))) %>% 
      group_by(group) %>% 
      nest() %>% 
      nrow() 
    
    valueBox(nvar, "Variables", 
             icon = icon("list-ul"), color = "maroon")
  })
  
  #--------------------------GDP--------------------------------------
  output$GDP_bar <- renderPlotly({
    data1() %>% 
      filter(v1 == "c", v2 == "GDP") %>%
      filter(d2 %in% c(RegName(),StateName(),"ROA")) %>% 
      select(d2, matches(".*\\-[0-9]{4}")) %>% 
      gather(year, value, -d2) %>%
      mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>%
      mutate(d2 = fct_relevel(d2, c(RegName(),StateName(),"ROA"))) %>% 
      
      filter(year >= start_year) %>% 
      spread(d2, value) %>% 
      
      # plot_ly(type = "scatter", mode = "lines+markers") %>% 
      plot_ly(type = "bar") %>% 
      add_trace(x=~year, y=~ROA, name = "ROA", color = I(DC[1]), 
                visible = "legendonly") %>% 
      add_trace(x=~year, y=~get(RegName()), name = RegName(), 
                color = I(DC[2]), visible = T) %>% 
      add_trace(x=~year, y=~get(StateName()), name = StateName(), 
                color = I(DC[3]), visible = T) %>% 
      
      layout(updatemenus = list(trade_bar_chart_types), 
             # legend = list(x = 0.9, y = 1), 
             bargap = 0.15, bargroupgap = 0.1, 
             xaxis = list(title = "", dtick = 2),
             yaxis = list(title = "c_GDP"))
      
      # ggplot(aes(x = year, y = value, fill = d2)) +
      # geom_col(position = position_dodge()) +
      # facet_wrap(~d2) +
      # ggy(comma, "") +
      # ggx(identity, "") +
      # ggf() + 
      # ggl("none")
    
    # ggplotly(g1, tooltip = c("year", "value")) %>% 
    #   layout(margin = list(l = 60))
  })
  
  output$GDP_g_bar <- renderPlotly({
    g1 <- data1() %>% 
      filter(v1 == "gdp", v2 == "r") %>% 
      filter(d1 %in% c(RegName(),StateName(),"ROA", "NZ", "CHN", "IND")) %>% 
      select(d1, matches(".*\\-[0-9]{4}")) %>% 
      gather(year, value, -d1) %>%
      mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>%
      mutate(d1 = fct_relevel(d1, c(RegName(),StateName(),"ROA"))) %>% 
      
      ggplot(aes(x = year, y = value/100, fill = d1, col = d1)) +
      geom_line() + 
      geom_point() + 
      facet_wrap(~d1) +
      ggy(percent, "") + 
      ggx(identity, "") +
      ggf() + 
      ggc() + 
      ggl("none")
    
    ggplotly(g1, tooltip = c("year", "value/100")) %>% 
      layout(margin = list(l = 60))
  })
  
  #--------------------------GVA--------------------------------------
  BAU_qva <- reactive({
    req(input$gvaRegName)
    
    data1() %>% 
      filter(v1 == "qva") %>% 
      filter(d2 == input$gvaRegName) %>% 
      select(d1, matches(".*\\-[0-9]{4}")) %>% 
      gather(year, value, -d1) %>% 
      mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>% 
      mutate(d1 = fct_inorder(d1)) %>% 
      rename(qva = value) %>% 
      
      filter(year >= start_year)
  })
  
  BAU_qVAind <- reactive({
    req(input$gvaRegName)
    
    data1() %>% 
      filter(v1 == "c", v2 == "qVAind") %>% 
      filter(d2 == input$gvaRegName) %>% 
      select(d1, matches(".*\\-[0-9]{4}")) %>% 
      gather(year, value, -d1) %>% 
      mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>% 
      mutate(d1 = fct_inorder(d1)) %>% 
      rename(qVAind = value) %>% 
      
      filter(year >= start_year)
  })
  
  output$qva_scat <- renderPlotly({
    
    g1 <- inner_join(BAU_qva(), BAU_qVAind(), by = c("d1", "year")) %>% 
      ggplot(aes(x=qVAind, y=qva/100, col=d1)) + 
      geom_point(alpha = .1) + 
      geom_point(aes(frame = year, ids = d1)) +
      scale_color_manual(values = colorRampPalette(brewer.pal(8, "Set2"))(19)) + 
      ggy(percent, "qva") + 
      ggx(comma) +
      ggl("right")
    
    ggplotly(g1) %>% 
      animation_opts(500, redraw = FALSE)
    
  })
  
  output$qva_bar <- renderPlotly({
    BAU_qVAind() %>% 
      plot_ly(x=~year, y=~qVAind, color=~d1, type = "bar") %>%
      layout(barmode = 'stack')
  })
  
  output$qva_bar_f <- renderPlotly({
    BAU_qVAind() %>% 
      group_by(year) %>% 
      mutate(qVAind = qVAind/sum(qVAind)) %>% 
      
      plot_ly(x=~year, y=~qVAind, color=~d1, type = "bar") %>%
      layout(barmode = 'stack', yaxis = list(tickformat = "%"), margin = list(l=60))
  })
  
  output$qva_bar_o <- renderPlotly({
    dd <- BAU_qVAind() %>% 
      left_join(BAU_qva(), by = c("year", "d1")) %>% 
      mutate(d1 = fct_reorder(d1, qVAind, function(x) mean(x, na.rm=T)))
      # mutate(d1 = fct_relevel(d1, c("FF", "LSTK", "CROPS", 
      #                               "COG", "IRON", "DWE", 
      #                               "ISR", 
      #                               "ELY", "CMN", "REC", 
      #                               "LMAN", "FMAN", "HMAN", 
      #                               "TRN", "CNS", 
      #                               "TRD", "OFI", "OBS", "OSG")))
    
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
             xaxis = list(title = "qVAind", 
                          side = 'top', showline = T, 
                          range = c(min(0, min(dd$qVAind)-100), max(dd$qVAind)+100)))
    
    p2 <- dd %>% 
      arrange(d1) %>% 
      plot_ly(x=~qva, y=~as.numeric(d1)-1, #Note: plotly convert factor start from 0
              frame=~year,  text=~d1, 
              type = "scatter", mode = "lines+markers", 
              line = list(color = DC[1]), showlegend = F) %>%
      layout(margin = list(l=60, t=40), 
             yaxis = list(title = "", zeroline = F, 
                          # ticktext = levels(dd$d1), 
                          showgrid = F, showline = T, showticklabels = F), 
             xaxis = list(title = "qva", zeroline = F, 
                          showline = T, side = 'top', tickprefix = '%', 
                          range = c(min(0, min(dd$qva)-.01), max(dd$qva)+.01)))
    
    subplot(p1, p2, titleX = T, shareY = F, shareX = F) %>% 
      animation_opts(500, redraw = F)
  })
  
  output$qva_all <- renderPlotly({
    g1 <- data1() %>% 
      # filter(v1 == "c", v2 == "qVAind") %>% 
      filter(v1 == "qva") %>% 
      filter(d2 %in% c(RegName(), StateName(), "ROA")) %>% 
      select(d1, d2, matches(".*\\-[0-9]{4}")) %>% 
      gather(year, qva, -d1, -d2) %>% 
      mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>% 
      mutate(d2 = fct_relevel(d2, c(RegName(),StateName(),"ROA"))) %>% 
      
      filter(year >= start_year) %>%
      
      ggplot(aes(x= year, y=qva/100, group=d1)) + 
      geom_line(alpha=.1) + 
      geom_line(aes(frame=as.character(d1)), col=DC[1]) + 
      facet_wrap(~d2) + 
      xlab("") + 
      ggy(percent, "qva") + 
      ggl("none")
    
    ggplotly(g1) %>% 
      animation_slider(currentvalue = list(prefix = "Commodity: ", 
                                           font = list(color = DC[1])))
  })
  
  output$qVAind_all <- renderPlotly({
    g1 <- data1() %>% 
      filter(v1 == "c", v2 == "qVAind") %>%
      # filter(v1 == "qva") %>% 
      filter(d2 %in% c(RegName(), StateName(), "ROA")) %>% 
      select(d1, d2, matches(".*\\-[0-9]{4}")) %>% 
      gather(year, qVAind, -d1, -d2) %>% 
      mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>% 
      mutate(d2 = fct_relevel(d2, c(RegName(),StateName(),"ROA"))) %>% 
      
      filter(year >= start_year) %>%
      
      ggplot(aes(x= year, y=qVAind, group=d1)) + 
      geom_line(alpha=.1) + 
      geom_line(aes(frame=as.character(d1)), col=DC[2]) + 
      facet_wrap(~d2) + 
      xlab("") + 
      ggy(comma, "qVAind") + 
      ggl("none")
    
    ggplotly(g1) %>% 
      animation_slider(currentvalue = list(prefix = "Commodity: ",
                                           font = list(color = DC[2])))
  })
  
  #--------------------------FTE--------------------------------------
  output$FTE_bar <- renderPlotly({
    g1 <- data1() %>% 
      filter(v1 == "empl") %>% 
      filter(d1 %in% c(RegName(), StateName(), "ROA", "NZ", "CHN", "IND")) %>% 
      select(d1, matches(".*\\-[0-9]{4}")) %>% 
      gather(year, value, -d1) %>%
      mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>%
      mutate(d1 = fct_relevel(d1, c(RegName(),StateName(),"ROA"))) %>% 
      
      ggplot(aes(x = year, y = value/100, fill = d1, col = d1)) +
      # geom_col(position = position_dodge()) +
      geom_line() + 
      geom_point() + 
      facet_wrap(~d1) +
      ggy(percent, "") +
      ggx(identity, "") +
      ggc() + 
      ggf() + 
      ggl("none")
    
    ggplotly(g1, tooltip = c("year", "value/100")) %>% 
      layout(margin = list(l = 60))
    
  })
  
  #--------------------------Trade--------------------------------------
  output$qex_ind_bar <- renderPlotly({
    data1() %>% 
      filter(v1 == "qex") %>% 
      select(-Solution, -v1, -v2, -d3) %>%
      mutate(d1 = fct_inorder(d1)) %>% 
      gather(year, value, -d1, -d2) %>% 
      mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>% 
      
      filter(year >= start_year) %>% 
      filter(d2 %in% c(RegName(), StateName(), "ROA")) %>%
      spread(d2, value) %>% 
      
      # plot_ly(type = "scatter", mode = "lines+markers") %>% 
      plot_ly(type = "bar") %>% 
      add_trace(x=~year, y=~ROA, frame = ~d1, name = "ROA", color = I(DC[1]), 
              visible = "legendonly") %>% 
      add_trace(x=~year, y=~get(RegName()), frame = ~d1, name = RegName(), 
                color = I(DC[2]), visible = T) %>% 
      add_trace(x=~year, y=~get(StateName()), frame = ~d1, name = StateName(), 
                color = I(DC[3]), visible = T) %>% 
      
      layout(updatemenus = list(trade_bar_chart_types), 
             # legend = list(x = 0.9, y = 1), 
             bargap = 0.15, bargroupgap = 0.1, 
             xaxis = list(title = "", dtick = 2),
             yaxis = list(title = "qex")) %>% 
      
      animation_slider(
        currentvalue = list(prefix = "Commodity ", font = list(color="red"))
      )
    
  })
  
  output$qimp_ind_bar <- renderPlotly({
    
    data1() %>% 
      filter(v1 == "qimp") %>% 
      select(-Solution, -v1, -v2, -d3) %>%
      mutate(d1 = fct_inorder(d1)) %>% 
      gather(year, value, -d1, -d2) %>% 
      mutate(year = as.numeric(str_extract_all(year, "(?<=\\-)[0-9]{4}"))) %>% 
      
      filter(year >= start_year) %>% 
      filter(d2 %in% c(RegName(), StateName(), "ROA")) %>%
      spread(d2, value) %>% 
      
      # plot_ly(type = "scatter", mode = "lines+markers") %>% 
      plot_ly(type = "bar") %>% 
      add_trace(x=~year, y=~ROA, frame = ~d1, name = "ROA", color = I(DC[1]), 
              visible = "legendonly") %>% 
      add_trace(x=~year, y=~get(RegName()), frame = ~d1, name = RegName(), 
                color = I(DC[2]), visible = T) %>% 
      add_trace(x=~year, y=~get(StateName()), frame = ~d1, name = StateName(), 
                color = I(DC[3]), visible = T) %>% 
      
      layout(updatemenus = list(trade_bar_chart_types), 
             # legend = list(x = 0.9, y = 1), 
             bargap = 0.15, bargroupgap = 0.1, 
             xaxis = list(title = "", dtick = 2),
             yaxis = list(title = "qimp")) %>% 
      
      animation_slider(
        currentvalue = list(prefix = "Commodity ", font = list(color="red"))
      )
  })
}









