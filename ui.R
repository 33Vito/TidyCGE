# TidyCGE
# Exploring the CGE results in a tidy data format
# Tony Liu
# Date: Jan, 2018

# ui.R
# modularised

dashboardPage(
  #skin = "green",
  #--------------------Header---------------------------
  dashboardHeader(title = "TidyCGE v0.3 alpha"),
  #--------------------Sidebar---------------------------
  dashboardSidebar(
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    # ),
    sidebarMenu(
      id = "sbm",
      div(align = "center",
          h5("Tony Liu, Jan 2018")),
      
      numericInput("Year", "Year", value = 2017), 
      textInput("RegName", "Shock Region", value = "SYD"),
      textInput("StateName", "Shock State", value = "RON"),
      
      menuItem("LGA", tabName = "LGA", icon = icon("map-marker")), 
      menuItem("BAU", tabName = "BAU", icon = icon("database")),
      menuItem("DEV1", tabName = "DEV1", icon = icon("dashboard")),
      menuItem("DEV2", tabName = "DEV2", icon = icon("dashboard")),
      menuItem("DEV3", tabName = "DEV3", icon = icon("dashboard")),

      menuItem(
        "Help",
        tabName = "help",
        icon = icon("question-circle"),
        menuSubItem("Admin", icon = icon("user"), tabName = "helpAdmin"),
        menuSubItem("Social", icon = icon("coffee"), tabName = "helpSocial"),
        menuSubItem("Control", icon = icon("gear"), tabName = "helpControl")
      ),
      br(),
      withTags({
        div(align = "center",
            a(
              href = "http://www.deloitteaccesseconomics.com.au/",
              img(
                src = 'DEL_PRI_Access_Econ_RGB-01.jpg',
                class = 'logo',
                height = '65',
                width = '180'
              )
            ))
      }),
      br()
      
    ) # end of sidebarMenu
  ), # end of dashboardSidebar
  
  dashboardBody(
    shinyjs::useShinyjs(),
    includeCSS("www/style.css"),
    
    tabItems(
      #--------------------BAU---------------------------
      tabItem(
        tabName = "BAU",
        analysisUI("BAU")
        ), # End of tabItem
      #--------------------DEV1---------------------------
      tabItem(
        tabName = "DEV1",
        analysisUI("DEV1")
      ), # End of tabItem
      #--------------------DEV2---------------------------
      tabItem(
        tabName = "DEV2", 
        analysisUI("DEV2")),
      #--------------------DEV3---------------------------
      tabItem(
        tabName = "DEV3", 
        analysisUI("DEV3")),
      #--------------------LGA---------------------------
      tabItem(
        tabName = "LGA", 
        fileInput("LGA_csv", "upload LGA list", width = "260px",
                  accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
        verbatimTextOutput("LGA_list")
      ),
      #--------------------Help---------------------------
      tabItem(tabName = "helpAdmin"),
      tabItem(tabName = "helpSocial"),
      tabItem(tabName = "helpControl")
    )# End of tabItems
  )# end of dashboard body
)# end of dashboard page









