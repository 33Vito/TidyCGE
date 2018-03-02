# TidyCGE
# Exploring the CGE results in a tidy data format
# Tony Liu
# Date: Mar, 2018

# server.R
# modularised

shinyServer(function(input, output, session) {
  output$LGA_list <- renderPrint({
    SYD_LGA
  })
  
  #--------------------reactive global inputs-----------------------
  Tab <- reactive({input$sbm})
  Year <- reactive({input$Year})
  RegName <- reactive({input$RegName})
  StateName <- reactive({input$StateName})
  
  #--------------------call analysis module-------------------------
  callModule(analysis, "BAU", Year, RegName, StateName, Tab)
  callModule(analysis, "DEV1", Year, RegName, StateName, Tab)
  callModule(analysis, "DEV2", Year, RegName, StateName, Tab)
  callModule(analysis, "DEV3", Year, RegName, StateName, Tab)
})