library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinydashboardPlus)
library(billboarder)


# Define UI
ui <- shinyUI(
  dashboardPage(
    title = "Nairobi Crime Report",
    dashboardHeader(title = "Crime Report",
                    dropdownMenu(
                      type = "notifications", icon = icon("coins")
                    )),
    dashboardSidebar(
      skin = "green",
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
      theme = theme,
      tags$head(tags$style(
        HTML(
          "
          body { background-color: ghostwhite}
          .my-box {border-radius: 1000px; padding-top: 10px;}"
        )
      )),
      tags$style(
        HTML(
          "
        #totalCrimeCases {
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
      tags$style(
        HTML(
          "
        #projectedCases {
          font-weight: bold;
          text-align: center;
          font-size: 20px;
        }
      "
        )
      ),
      infoBox(
        title = h2("Nairobi Crime Dashboard"),
        width = 12,
        subtitle = "Exploring Trends in Crime in Nairobi",
        icon = icon("bar-chart", lib = "font-awesome"),
        color = "aqua",
        fill = FALSE
      )
      ,
      
      ##Break
      br(),
      br(),
      br(),
      hr(),
      
      column(12,
             fluidRow(column(
               width = 12,
               box(
                 title = "Total Crime Cases",
                 width = 3,
                 status = "warning",
                 solidHeader = TRUE,
                 textOutput("totalCrimeCases"),
                 style = "text-align: center;"
               ),
               box(
                 title = "Average Monthly Cases",
                 width = 3,
                 status = "info",
                 solidHeader = TRUE,
                 textOutput("averageMonthlyCases")
               ),
               box(
                 title = "Daily Crime Rate",
                 width = 3,
                 status = "info",
                 solidHeader = TRUE,
                 textOutput("crimeRate")
               ),
               box(
                 title = "Projected Cases",
                 width = 3,
                 status = "success",
                 solidHeader = TRUE,
                 textOutput("projectedCases")
               )
             ))),
      
      br(),
      hr(),
      fluidRow(column(
        width = 12,
        tabBox(
          id = "pieChart",
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
                   billboarderOutput("hbar2"))
        ),
        theme = "blue_gradient"
      ))
      
    )
  )
)

# Run the application
shinyApp(ui, server)
