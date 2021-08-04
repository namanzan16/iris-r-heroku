library(shiny)
library(shinydashboard)
library(rsconnect)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(dplyr)
library(shinyWidgets)
library(readxl)
library(DT)
library(shinyjs)
library(dashboardthemes)
library(ggiraph)
library(ggrepel)
library(RColorBrewer)
library(dygraphs)
library(xts)
library(shinycssloaders)
library(waiter)
library(shinyalert)
library(shinythemes)
library(spsComps)
library(stringr)
library(data.table)
library(tidyverse)
library(stringr)
library(gridExtra)
library(ggplot2)
library(wesanderson)
library(RColorBrewer)
library(gridExtra)
library(zoo)
library(lubridate)
library(forecast)
library(prophet)
library(MLmetrics)
library(smooth)
library(astsa)
library(reshape)
library(sweep)
library(timetk)
library(nnfor)

customTheme <- shinyDashboardThemeDIY(
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "#000000"
  ,primaryFontColor = "#000000"
  ,infoFontColor = "#000000"
  ,successFontColor = "#000000"
  ,warningFontColor = "#0F0F0F"
  ,dangerFontColor = "#000000"
  ,bodyBackColor = "#FFFFFF"
  
  ### header
  ,logoBackColor = "#D1D3F0"
  
  ,headerButtonBackColor = "#B4B7F0"
  ,headerButtonIconColor = "#E6F7F1"
  ,headerButtonBackColorHover = "#6266D6"
  ,headerButtonIconColorHover = "#3C3C3C"
  
  ,headerBackColor = "#D1D3F0"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = "#CCEBD6"
  ,sidebarPadding = "0"
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = "0"
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = "#000000"
  
  ,sidebarSearchBackColor = "#F0F0F0"
  ,sidebarSearchIconColor = "#646464"
  ,sidebarSearchBorderColor = "#DCDCDC"
  
  ,sidebarTabTextColor = "#000000"
  ,sidebarTabTextSize = "14"
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = "0"
  
  ,sidebarTabBackColorSelected = "#E6E6E6"
  ,sidebarTabTextColorSelected = "#000000"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = "#F5F5F5"
  ,sidebarTabTextColorHover = "#000000"
  ,sidebarTabBorderStyleHover = "none solid none none"
  ,sidebarTabBorderColorHover = "#C8C8C8"
  ,sidebarTabBorderWidthHover = "4"
  ,sidebarTabRadiusHover = "0px"
  
  ### boxes
  ,boxBackColor = "#F5F5F5"
  ,boxBorderRadius = "5"
  ,boxShadowSize = "none"
  ,boxShadowColor = ""
  ,boxTitleSize = "18"
  ,boxDefaultColor = "#F0F0F0"
  ,boxPrimaryColor = "#5F9BD5"
  ,boxInfoColor = "#B4B4B4"
  ,boxSuccessColor = "#70AD47"
  ,boxWarningColor = "#ED7D31"
  ,boxDangerColor = "#E84C22"
  
  ,tabBoxTabColor = "#F8F8F8"
  ,tabBoxTabTextSize = "14"
  ,tabBoxTabTextColor = "#646464"
  ,tabBoxTabTextColorSelected = "#2D2D2D"
  ,tabBoxBackColor = "#F8F8F8"
  ,tabBoxHighlightColor = "#C8C8C8"
  ,tabBoxBorderRadius = "5"
  
  ### inputs
  ,buttonBackColor = "#FFFFFF"
  ,buttonTextColor = "#000000"
  ,buttonBorderColor = "#969696"
  ,buttonBorderRadius = "5"
  
  ,buttonBackColorHover = "#BEBEBE"
  ,buttonTextColorHover = "#000000"
  ,buttonBorderColorHover = "#969696"
  
  ,textboxBackColor = "#FFFFFF"
  ,textboxBorderColor = "#767676"
  ,textboxBorderRadius = "5"
  ,textboxBackColorSelect = "#F5F5F5"
  ,textboxBorderColorSelect = "#6C6C6C"
  
  ### tables
  ,tableBackColor = "#F8F8F8"
  ,tableBorderColor = "#EEEEEE"
  ,tableBorderTopSize = "1"
  ,tableBorderRowSize = "1"
)
#############################################################################################################


ui<-dashboardPage(
  dashboardHeader(title = title <- tags$a(
    icon("shopify"),
    span("Retail Dashboard",
         style = "color: black; font-size: 20px"), target="_blank" ), titleWidth = 230,
                  dropdownMenu(type = "messages", badgeStatus = "success",
                               messageItem("Support Team",
                                           "This is the content of a message.",
                                           time = "3 mins"
                               )
                  ),
                  
                  # Dropdown menu for notifications
                  dropdownMenu(type = "notifications", badgeStatus = "warning",
                               notificationItem(icon = icon("flag"), status = "info",
                                                "Sample Notification"
                               )
                               
                  )),
  shinydashboard::dashboardSidebar(
    sidebarMenu(
      menuItem("Sales Analytical Report", tabName = "info", icon = icon("dashboard"),
               menuSubItem('Home Page',
                           tabName = 'homepage',
                           icon = icon('th')),
               menuSubItem('Sales Overview',
                           tabName = 'sales',
                           icon = icon('line-chart')),
               menuSubItem('Sales Distribution',
                           tabName = 'distribution',
                           icon = icon('line-chart')),
               menuSubItem('Daily Sales Forecasting',
                           tabName = 'forecast',
                           icon = icon('cart-arrow-down')),
               menuSubItem('Monthly Sales Forecasting',
                           tabName = 'forecast_monthly',
                           icon = icon('cart-arrow-down'))
      )
    )
  ),
  dashboardBody(
    
    
    customTheme,
    useShinyjs(),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    tags$head(tags$script(HTML("$(function() { $('a.sidebar-toggle').mouseover(function(e) { $(this).click()})});"))),
    tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),
    
    
    tabItems(
      # First tab content
      tabItem(tabName = "homepage",
              # img(src="sale.jpg",  height="100%", width="100%"),
              # h3("This Dashboard will contain some basic details about sales data of
              # 50 stores"),
              
              fluidRow(
                
                valueBox("9,13,000", "DATAPOINTS", icon = icon("database"),color = "purple"),
                valueBox("4.7 cr", "TOTAL SALES", icon = icon("chart-line"),color = "green"),
                valueBox(5, "YEARS DATA", icon = icon("calendar-alt"),color = "teal"),
                valueBox(10, "STORES", icon = icon("store"),color = "yellow"),
                
                valueBox(50, "ITEMS", icon = icon("shopping-cart"),color = "blue"),
                valueBox(4, "REGIONS", icon = icon("compass"),color = "red"))
              
              
              
              
              
              
      ),
      tabItem(tabName = "sales",
              column(3,
                     shinydashboard::box(
                       pickerInput("select_time", "Time Range:",
                                   choices = list("Date" = 1,
                                                  "Month" = 2,
                                                  "Year" = 3),
                                   selected = 3,
                                   options = list(`live-search` = TRUE,`actions-box` = TRUE)),
                       width = 12, height = 430)
              ),
              column(9,
                     fluidRow(
                       box(dygraphOutput("sale_plot")%>% withSpinner(), width = 12)),
              )),
      
      tabItem(tabName = "distribution",
              column(3,
                     shinydashboard::box(
                       pickerInput("select_distribution", "Distribution type",
                                   choices = list("Sales"=1, 
                                                  "Sales by Store"=2,
                                                  "Sales by item"=3
                                   ),
                                   options = list(`live-search` = TRUE,`actions-box` = TRUE), multiple = F),
                       
                       pickerInput("select_store", "Store:",
                                   choices = seq(1,10,by=1),
                                   options = list(`live-search` = TRUE,`actions-box` = TRUE,
                                                  tickIcon = T), 
                                   multiple = T),
                       pickerInput("select_item", "Item:",
                                   choices = seq(1,50,by=1),
                                   options = list(`live-search` = TRUE,`actions-box` = TRUE,
                                                  tickIcon = T), 
                                   multiple = T),
                       width = 12, height = 425)
              ),
              column(9,
                     fluidRow(
                       box(plotOutput("dist_plot")%>% withSpinner(), width = 12)),
                     
              )),
      
      tabItem(tabName = "forecast",
              
              
              column(3,
                     shinydashboard::box(
                       pickerInput("select_region", "Select a region for forecast",
                                   choices = list("North"="north", 
                                                  "East"="east",
                                                  "South"="south",
                                                  "West"="west"
                                   ),
                                   selected = 1,
                                   options = list(`live-search` = TRUE,`actions-box` = TRUE,
                                                  tickIcon = T),
                                   multiple = F),
                       
                       pickerInput("select_store1", "Forecast for Store:",
                                   choices = seq(1,10,by=1),
                                   selected = NULL,
                                   options = pickerOptions(`live-search` = TRUE,`actions-box` = F,
                                                           tickIcon = T, maxOptions = 1 ),
                                   multiple = T),
                       pickerInput("select_item1", "Forecast for Item:",
                                   choices = seq(1,50,by=1),
                                   selected = NULL,
                                   options = pickerOptions(`live-search` = TRUE,`actions-box` = F,
                                                           tickIcon = T, maxOptions = 1),
                                   multiple = T),
                       pickerInput("select_model", "Select Forecast Model:",
                                   choices = list("Prophet"=1, 
                                                  "TBATS"=2,
                                                  "Naive"=3,
                                                  "NNETAR"=4,
                                                  "ETS"=5
                                   ),
                                   selected = 3,
                                   options = list(`live-search` = TRUE,`actions-box` = TRUE), multiple = T),
                       br(),
                       br(),
                       column(3,),
                       column(9,actionButton("gobutton","Forecast", icon = icon("chart-area"))),
                       width = 12, height = 900)
                     
              ),
              column(9,
                     fluidRow(
                       box(dygraphOutput("forecast_plot")%>% withSpinner(), width = 12)),
                     
                     tabsetPanel(
                       tabPanel("Accuracy Chart",
                                # fluidRow(...)
                                girafeOutput("accuracy_plot")%>% withSpinner()
                                # plotOutput("plot2")
                       ),
                       tabPanel("Forecasted Values",
                                # fluidRow(...)
                                dataTableOutput("forecast_table")%>% withSpinner()
                                # plotOutput("plot2")
                       )
                     )
                     
                     
              ),
              
      ),
      
      tabItem(tabName = "forecast_monthly",
              
              
              column(3,
                     shinydashboard::box(
                       pickerInput("select_m_region", "Select a region for forecast",
                                   choices = list("North"="north", 
                                                  "East"="east",
                                                  "South"="south",
                                                  "West"="west"
                                   ),
                                   selected = 1,
                                   options = list(`live-search` = TRUE,`actions-box` = TRUE,
                                                  tickIcon = T),
                                   multiple = F),
                       
                       pickerInput("select_m_store1", "Forecast for Store:",
                                   choices = seq(1,10,by=1),
                                   selected = NULL,
                                   options = pickerOptions(`live-search` = TRUE,`actions-box` = F,
                                                           tickIcon = T, maxOptions = 1 ),
                                   multiple = T),
                       pickerInput("select_m_item1", "Forecast for Item:",
                                   choices = seq(1,50,by=1),
                                   selected = NULL,
                                   options = pickerOptions(`live-search` = TRUE,`actions-box` = F,
                                                           tickIcon = T, maxOptions = 1),
                                   multiple = T),
                       pickerInput("select_m_model", "Select Forecast Model:",
                                   choices = list("ARIMA"=1, 
                                                  "TBATS"=2,
                                                  "Naive"=3,
                                                  "NNETAR"=4,
                                                  "ETS"=5
                                   ),
                                   selected = 3,
                                   options = list(`live-search` = TRUE,`actions-box` = TRUE), multiple = T),
                       br(),
                       br(),
                       column(3,),
                       column(9,actionButton("gobutton1","Forecast", icon = icon("chart-area"))),
                       width = 12, height = 900)
                     
              ),
              column(9,
                     fluidRow(
                       box(dygraphOutput("forecast_m_plot")%>% withSpinner(), width = 12)),
                     
                     tabsetPanel(
                       tabPanel("Accuracy Chart",
                                # fluidRow(...)
                                girafeOutput("accuracy_m_plot")%>% withSpinner()
                                # plotOutput("plot2")
                       ),
                       tabPanel("Forecasted Values",
                                # fluidRow(...)
                                dataTableOutput("forecast_m_table")%>% withSpinner()
                                # plotOutput("plot2")
                       )
                     )
                     
                     
              ),
      )
    ),
    
    
    
    
  )
)