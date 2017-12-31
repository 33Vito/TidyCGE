# TidyCGE
# Exploring the CGE results in a tidy data format
# Tony Liu
# Date: Jan, 2018

# server.R
# modularised

shinyServer(function(input, output, session) {
  output$LGA_list <- renderPrint({
    SYD_LGA
  })
  
  #--------------------reactive global inputs-----------------------
  Year <- reactive({input$Year})
  RegName <- reactive({input$RegName})
  StateName <- reactive({input$StateName})
  
  #--------------------call analysis module-------------------------
  callModule(analysis, "BAU", Year, RegName, StateName)
  callModule(analysis, "DEV1", Year, RegName, StateName)
})