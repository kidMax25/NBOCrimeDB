library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinydashboardPlus)
library(billboarder)
library(summaryBox)
library(DT)
library(leaflet)
library(highcharter)

theme <- bslib::bs_theme(version = 5)

# Define UI
ui <- shinyUI(
  dashboardPage(
    title = "Nairobi Crime Report",
    dashboardHeader(
      title = "Crime Report",
      titleWidth = NULL,
      dropdownMenu(type = "notifications", icon = icon("coins"))
    ),
    dashboardSidebar(
      sidebarMenu(
        sidebarSearchForm("searchText", "buttonSearch", "Search"),
        menuItem(
          "General Report",
          tabName = "dashboard",
          icon = icon("pie-chart"),
          badgeLabel = "RPT",
          badgeColor = "red"
        ),
        menuItem(
          "Crime Tables",
          tabName = "CrimeTables",
          icon = icon("list", class = NULL, lib = "font-awesome"),
          badgeLabel = "TBL",
          badgeColor = "green"
        ),
        menuItem(
          "Modelling & Prediction",
          tabName = "ModellingPrediction",
          icon = icon("virus", class = NULL, lib = "font-awesome"),
          badgeLabel = "TSA",
          badgeColor = "orange"
        )
      )
    ),
    dashboardBody(
      tags$head(tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
      ),
      
      tags$style(
        HTML(
          "
          body { background-color: ghostwhite}
          .my-box {border-radius: 1000px; padding-top: 10px;}"
        )
      )),
      tags$style(
        HTML(
          "
        #averageMonthlyCases {
          font-weight: bold;
          text-align: center;
          font-size: 20px;
        }
      "
        )
      ),
      tags$style(
        HTML(
          "
        #crimeRate {
          font-weight: bold;
          text-align: center;
          font-size: 20px;
        }
      "
        )
      ),
      # tags$style(
      #   HTML(
      #     "
      #   #projectedCases {
      #     font-weight: bold;
      #     text-align: center;
      #     font-size: 20px;
      #   }
      # "
      #   )
      # ),
      
      tabItems(
        tabItem(
          tabName = "dashboard",
          infoBox(
            tags$style(HTML("
                border-radius: 100px;
                ")),
            title = h2(style= "margin-bottom: 0px;","Nairobi Crime Dashboard"),
            width = 12,
            subtitle = div(style = "padding-left:5px;","Exploring Trends in Crime in Nairobi"),
            icon = icon("neos", lib = "font-awesome"),
            color = "aqua",
            fill = FALSE
          ),
          
          tags$section(id = "mypanels",div(style = "padding-top: 3rem;",
            fixedRow(
              valueBoxOutput("kpi_valuebox", width = 3),
              valueBoxOutput("Avg_crime", width = 3),
              valueBox(
                value = sum(total4$Total) ,
                subtitle = "Total Reported Crime",
                color = "orange",
                width = 3,
                icon = icon("medal")
              ),
              valueBox(
                value = round(sum(rnorm(50, 3, 20)), 0),
                subtitle = "Projected Cases",
                color = "teal",
                width = 3,
                icon = icon("java")
              )
              
            )
          )),
          fluidRow(column(
            width = 12,
            tabBox(
              id = "pieChart",
              side = "left",
              title = h3("Prevalent Crimes in Nairobi"),
              width = 6,
              tabPanel(title = "Categories",
                       billboarderOutput("hpie")),
              tabPanel(title = "Subcategories",
                       billboarderOutput("hpie2"))
            ),
            tabBox(
              id = "barGraph",
              title = h3("Yearly Distribution of Crime in Nairobi"),
              width = 6,
              tabPanel(title = "Yearly",
                       billboarderOutput("stackedChart")),
              tabPanel(title = "Trend",
                       highchartOutput("bubble"))
                       # billboarderOutput("hbar2"))
            )
          ))
        ),
        tabItem(tabName = "CrimeTables",
                
                tabsetPanel(
                  header = NULL,
                  type = "hidden",
                  tabPanel("Crime Tables",
                           fillRow(fluidRow(
                             box(
                               title = "Crime Tables",
                               width = 12,
                               status = "info",
                               solidHeader = T ,
                               DTOutput("crime_table")
                             ),
                             box(
                               title = "Descriptive Tables",
                               width = 12,
                               status = "info",
                               solidHeader = T,
                               selectInput(
                                 "selected_offence",
                                 "Select Offence:",
                                 choices = c(
                                   "Human_offences",
                                   "Property_offences",
                                   "Drug_offences",
                                   "Other_offences"
                                 ),
                                 selected = "Human_offences"
                               ),
                               DTOutput("summary_table")
                             )
                           )))
                )),
        
        
        tabItem(
          tags$style(HTML("
            opacity: 4;
            ")),
          tabName = "ModellingPrediction",
          fluidPage(
            tags$style(
              type = "text/css",
              "body {opacity: 4;}",
              "#map {height: calc(100vh - 10px) !important;z-index: 0; position:absolute;
                       top: 0; left: 0; right: 0; bottom: 0;
                       background-color: #f0f0f0;}",
              ".leaflet-container {z-index: 1;}"
            ),
            leafletOutput("map"),
            div(
              id = "abs1",
              class = "myabs1",
              absolutePanel(
                top = 0,
                left = 2,
                right = 20,
                height = 1000,
                width = 1100,
                div(tabBox(
                  id = "tsa",
                  side = "left",
                  title = NULL,
                  width = 12,
                  tabPanel(title = "Components",
                           div(
                             boxPad(
                               id = "box1" ,
                               solidHeader = FALSE,
                               title = NULL,
                               width = 12,
                               footer = NULL,
                               collapsible = TRUE,
                               div(
                                 id = "set1",
                                 class = "mybuttons1",
                                 actionButton("obs", "Observed Values", class = "btn-primary"),
                                 actionButton("tsdata", "Get Time Series Data", class = "btn-primary"),
                                 actionButton("trnd", "Trend Component", class = "btn-primary"),
                                 actionButton("szn", "Seasonal Component", class = "btn-primary"),
                                 actionButton("ertc", "Erractic Component", class = "btn-primary")
                               )
                             )
                           )),
                  
                  tabPanel(title = "Decomposition",
                           div(
                             boxPad(
                               id = "box2" ,
                               solidHeader = FALSE,
                               title = NULL,
                               width = 12,
                               footer = NULL,
                               collapsible = TRUE,
                               div(
                                 id = "set2",
                                 class = "mybuttons2",
                                 actionButton("dtr", "Detrend", class = "btn-primary"),
                                 actionButton("dszn", "Deseasonalize", class = "btn-primary"),
                                 actionButton("dns", "Denoise", class = "btn-primary"),
                                 actionButton("elmt", "View Elements", class = "btn-primary")
                               )
                             )
                           )),
                  tabPanel(title = "Unit Root Test",
                           div(
                             boxPad(
                               id = "box3" ,
                               solidHeader = FALSE,
                               title = NULL,
                               width = 12,
                               footer = NULL,
                               collapsible = TRUE,
                               div(
                                 id = "set3",
                                 class = "mybuttons3",
                                 actionButton("adft", "ADF Test", class = "btn-primary"),
                                 actionButton("lbt", "Ljung-Box Test", class = "btn-primary"),
                                 actionButton("dwt", "Durbin-Watson Test", class = "btn-primary"),
                                 actionButton("cmprsn", "Comparison", class = "btn-primary")
                               )
                             )
                           )),
                  tabPanel(title = "Parameter Estimation",
                           div(
                             boxPad(
                               id = "box4" ,
                               solidHeader = FALSE,
                               title = NULL,
                               width = 12,
                               footer = NULL,
                               collapsible = TRUE,
                               div(
                                 id = "set4",
                                 class = "mybuttons4",
                                 actionButton("mdl", "Model selection", class = "btn-primary"),
                                 actionButton("acfpacf", "ACF and PACF", class = "btn-primary"),
                                 actionButton("cfts", "Model Equation", class = "btn-primary")
                               )
                             )
                           )),
                  tabPanel(title = "Model Diagnostics",
                           div(
                             boxPad(
                               id = "box5" ,
                               solidHeader = FALSE,
                               title = NULL,
                               width = 12,
                               footer = NULL,
                               collapsible = TRUE,
                               div(
                                 id = "set5",
                                 class = "mybuttons5",
                                 actionButton("fvo", "Fitting Vs Observed Values", class = "btn-primary"),
                                 actionButton("tsdata", "Normality Plots", class = "btn-primary"),
                                 actionButton("acc", "Accuracy Measures", class = "btn-primary"),
                               )
                             )
                           )),
                      tabPanel(title = "Forecasting",
                           div(
                             boxPad(
                               id = "box7" ,
                               solidHeader = FALSE,
                               title = NULL,
                               width = 12,
                               footer = NULL,
                               collapsible = TRUE,
                               div(
                                 id = "set7",
                                 class = "mybuttons7",
                                 actionButton("mnth6", "Next 6 Months", class = "btn-primary"),
                                 actionButton("mnth9", "Next 9 Months", class = "btn-primary"),
                                 actionButton("mnth12", "Next 12 Months", class = "btn-primary")
                               )
                             )
                           ))
                )),
                div(
                  style = "{
                  background: rgba(255, 255, 255, 0.34);
                  border-radius: 16px;
                  box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);
                  backdrop-filter: blur(15.3px);
                  -webkit-backdrop-filter: blur(15.3px);
                  border: 1px solid rgba(255, 255, 255, 0.61);
                  }",
                  mainPanel(tags$head(tags$style(
                  HTML("
                      #plot_area {
                      width: 100%
                      }
                      ")
                )), width = 12,
                highchartOutput("plot_area")))
              )
            )
          )
        )
      )
    )
  )
)

# Run the application
shinyApp(ui, server)
