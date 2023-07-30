library(shiny)
library(RMySQL)
library(tidyverse)
library(lubridate)
library(wesanderson)
library(plotly)
library(billboarder)
library(DT)
library(leaflet)
library(osmdata)
library(sp)
library(rgdal)

#Connection to MySQL and Connection Kill

  con <- dbConnect(
    MySQL(),
    user = "root",
    password = "Maxy2001",
    host = "localhost",
    port = 3306,
    dbname = "crimedb"
  )
  query <- "SELECT * FROM `kasarani report`"
  mydb <- dbGetQuery(con, query)

  ## Kill connections
  connections <- dbListConnections(drv = dbDriver("MySQL"))

  #iterate
  closeConnections <- function() {
    for (conn in connections) {
      dbDisconnect(conn)
    }
  }



#Data

{
  table <- mydb %>% mutate(
    Month,
    Murder = MURDER..ATTEMPTS.,
    Manslaughter = MANSLAUGHTER,
    Defilement = RAPE.ATTEMPTS..GANG.DEFILEMENTS,
    Assault = ASSAULTS.lNDECENT.ASSAULTS,
    Offence.Persons = OTHER.OFFENCES.AGAINST.PERSONS,
    Robbery = ROBBERY.WITH.VIOLENCE,
    Burglary = BREAKINGS.BORGLARY,
    Stock.Theft = THEFT.OF.STOCK,
    Stealing = GENERAL.STEALING,
    Carjacking = THEFT.OF.M.V,
    Car.Vandalism = THEFT.FROM.M.V.PARTS.,
    Log.Book.Offences = THEFT.FROM.M.V..ARTICLES.,
    MotorCycle.Theft = THEFT.OF.BICYCLES.MOTOR.CYCLE,
    Theft.By.Servants = THEFT.BY.SERVANTS,
    Drugs.Offence = DANGEROUS.DRUGS.OFFENCE,
    Theft.Accomplice = HANDLING_STOLEN_GOODS,
    Corruption = CORRUPTION.OFFENCES,
    Dangerous.Driving = CAUSING.DEATH.BY.DANGEROUS.DRIVING,
    Property.Offence = OFFENCES.AGAINST.PROPERTY,
    Misc.Cases = ALL.OTHER.PENAL.CODE.OFFENCES
  )
  table_t <-
    table %>% select(
      Month,
      Murder,
      Manslaughter,
      Defilement,
      Assault,
      Offence.Persons,
      Robbery,
      Burglary,
      Stock.Theft,
      Stealing,
      Carjacking,
      Car.Vandalism,
      Log.Book.Offences,
      MotorCycle.Theft,
      Theft.By.Servants,
      Drugs.Offence,
      Theft.Accomplice,
      Corruption,
      Dangerous.Driving,
      Property.Offence,
      Misc.Cases
    )
  
  
  
  #table_t <- t(table_t)
  total <- table_t %>%
    summarise(across(-Month, sum))
  total <- t(total)
  AllCrimes <- row.names(total)
  rownames(total) <- NULL
  total2 <- data.frame(AllCrimes, total)
  colnames(total2) <- c("AllCrimes", "Total")
  
  total3 <- table_t %>%
    mutate(
      Human_offences = Murder + Manslaughter + Defilement + Assault + Offence.Persons,
      Property_offences = Robbery + Burglary + Stock.Theft + Stealing + Carjacking +
        Car.Vandalism + Log.Book.Offences + MotorCycle.Theft + Theft.By.Servants,
      Drug_offences = Drugs.Offence,
      Other_offences = Theft.Accomplice + Corruption + Dangerous.Driving + Property.Offence + Misc.Cases
    ) %>%
    select(Month,
           Human_offences,
           Property_offences,
           Drug_offences,
           Other_offences)
  
  total3 <- total3 %>%
    mutate(Year = year(mdy(Month))) %>%
    group_by(Year) %>%
    filter(Year %in% c(2018, 2019, 2020, 2021, 2022))
  grouped_categories <- total3 %>% group_by(Year) %>%
    summarize(
      Human_offences = sum(Human_offences),
      Property_offences = sum(Property_offences),
      Drug_offences = sum(Drug_offences),
      Other_offences = sum(Drug_offences)
    )
  
}
shinyServer(
function(input, output) {
  
  simulated_dynamic_value <- reactive({
    runif(1, min = min(total3$Drug_offences), max = max(total3$Drug_offences))
  })
  
  output$kpi_valuebox <- renderValueBox({
    value <- simulated_dynamic_value()
    valueBox(
      value = round(value, 0),
      subtitle = "Drug offence",
      icon = icon("tachometer-alt"),
      color = ifelse(value>5, "red", "green")
    )
  })
  
  output$Avg_crime <- renderValueBox({
    avgcrime <- mean(total2$Total)
    valueBox(value = round(avgcrime,0),
             subtitle = "Average Crime Rate Monthly",
             icon = icon("gauge"))
  })
  
  # output$totalCrimeCases <- renderValueBox({
  #   totalCrime <- sum(total4$Total)
  #   valueBox(value = totalCrime,
  #            subtitle = "Total Reported Crime",
  #            icon = icon("bolt"))
  # })
  # 
  # output$projectedCases <- renderValueBox({
  #   sample <- rnorm(20,3,50)
  #   psum <- round(sum(sample),0)
  #   valueBox(psum, subtitle = "Projected Cases 2023", color = "teal",
  #            icon = icon("medal"))
  # })
  
  output$piePlot <- renderPlot({
    total2 <- total2 %>%
      mutate(Total = as.numeric(Total),
             AllCrimes = factor(AllCrimes))
    colors <- colorRampPalette(c("#27A4F2", "#CFEBFC"))(20)
    gg <- ggplot(total2, aes(x = "", y = Total, fill = AllCrimes)) +
      geom_bar(stat = "identity") +
      coord_polar("y") +
      scale_fill_manual(values = colors) +
      theme_void() +
      labs(fill = "Category")
    
  })
  
    output$hpie <- renderBillboarder({
    categoryTotal <- colSums(grouped_categories[,c("Human_offences", "Property_offences",
                                                   "Drug_offences", "Other_offences")])
    categoryTotal <- data.frame(categoryTotal)
    colnames(categoryTotal)[1] <- "Category"
    adius <- rownames(categoryTotal)
    rownames(categoryTotal) <- NULL
    categoryTotal2 <- data.frame(adius, categoryTotal)
    colnames(categoryTotal2) <- c("Category", "Total")
    
    billboarder() %>% bb_piechart(categoryTotal2) %>%
      bb_legend(position = "inset",
                inset = list(anchor = "top-right")) %>%
      bb_colors_manual(
        "Human_offences" = "#41AB5D",
        "Property_offences" = "#4292C6",
        "Drug_offences" = "#FEB24C",
        "Other_offences" = "#BEBEBE"
      ) %>% 
      bb_labs(title = "Crime Major Categories") 
    
  })
  
  output$hpie2 <- renderBillboarder({
    billboarder() %>% bb_piechart(total2, color = colors) %>%
      bb_legend(position = 'bottom', size = 10) %>% 
      bb_labs(title = "Crime SubCategories")
  })
  output$barPlot <- renderPlot({
    table_t$Month <- as.character.POSIXt(table_t$Month)
    year_group <- table_t %>%
      mutate(Year = year(mdy(Month))) %>%
      group_by(Year) %>%
      filter(Year %in% c(2018, 2019, 2020, 2021, 2022))
    year_group %>%
      mutate(Total = rowSums(across(where(is.numeric)))) %>%
      group_by(Year) %>%
      summarise(yearly_total = sum(Total)) %>%
      ggplot(aes(x = Year, y = yearly_total, fill = Year)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "#CFEBFC", high = "#27A4F2") +
      geom_line(lwd = 1.3, color = "skyblue") +
      xlab("Year") +
      ylab("Crime Total Yearly") +
      theme_minimal()
  })
  
  output$hbar <- renderBillboarder({
    y <- total3 %>% group_by(Year) %>%
      summarize(
        Human_offences = sum(Human_offences),
        Property_offences = sum(Property_offences),
        Drug_offences = sum(Drug_offences),
        Other_offences = sum(Drug_offences)
      )
    billboarder() %>%
      bb_stacked_bar(
        total3,
        x = Year,
        y = c(
          Human_offences,
          Property_offences,
          Drug_offences,
          Other_offences
        )
      ) %>%
      bb_legend(position = "right")
  })
  
  output$hbar2 <- renderBillboarder({
    total4 <- total3 %>% 
      select(Month, Human_offences,Property_offences, Drug_offences, Other_offences)
    total4 <- total4[,-1]
    total4 <- total4 %>% 
      mutate(Total = rowSums(across(where(is.numeric))))
    total4 <- total4 %>% 
      mutate(Month = mdy(Month)) %>% 
      select(Month, Total)
    
    billboarder() %>% 
      bb_linechart(
        data = total4[, c("Month", "Total")],
        type = "spline"
      ) %>% 
      bb_labs(
        title = "Crime Activity in Kasarani (2018-2022)",
        x = "Month",
        y = "Crime Activity Reported"
      ) %>% 
      bb_legend(position = "right")
    
    
      
  })
  output$totalCrimeCases <- renderText({
    totalCases <- sum(table1$Total)
    paste(totalCases)
  })
  output$sumCrime <- renderValueBox({
    valueBox(
      value = textOutput("totalCrimeCases"),
      subtitle = "Total Crime Reported",
      icon = icon("warning"),
      color = "aqua"
    )
  })
  
  output$stackedChart <- renderBillboarder({
    as_tibble(grouped_categories)
    grouped_categories <- grouped_categories %>%
      mutate(Year = factor(Year))
    billboarder() %>%
      bb_barchart(data = grouped_categories[, c(
        "Year",
        "Human_offences",
        "Property_offences",
        "Drug_offences",
        "Other_offences"
      )],
      stacked = TRUE) %>%
      bb_data(
        names = list(
          Human_offences = "Human.Offences",
          Property_offences = "Property.Offences",
          Drug_offences = "Drug.Offences",
          Other_offences = "Other.Offences"
        ),
        labels = TRUE
      ) %>%
      bb_colors_manual(
        "Human_offences" = "#41AB5D",
        "Property_offences" = "#4292C6",
        "Drug_offences" = "#FEB24C",
        "Other_offences" = "#BEBEBE"
      ) %>%
      bb_legend(position = "inset",
                inset = list(anchor = "top-right")) %>%
      bb_labs(title = "Crime Ofences by Year",
              x = "Year",
              y = "Total Cases")
  })
  
  output$bubble <- renderHighchart({
    scatter1 <- total4 %>% 
      hchart('scatter',hcaes(x = Month, y = Total, size = Total,
                             group = lubridate::year(Month)), maxSize = "15%") %>% 
      hc_yAxis(min = 0)
      
    
    scatter1
  })
  
  
  output$averageMonthlyCases <- renderText({
    averageMonthly <- round((sum(table1$Total) / 60), 0)
    averageMonthly
  })
  
  output$crimeRate <- renderText({
    totalCrimes <- sum(table1$Total)
    numDays <- 365 * 5
    crimeRate <- round((totalCrimes / numDays), 0)
    crimeRate
  })
  output$projectedCases <- renderText({
    projectedCases <- sample(100:200, 1)
    projectedCases
  })
  
  output$crime_table <- renderDT({
    datatable(
      total3,
      options = list(pageLength = 5),
      selection = "none"
    )
  })
 
  output$summary_table <- renderDT({
    selected_column <- input$selected_offence
    
    datatable(
      total3 %>% group_by(Year) %>% 
        summarize(
          Mean = round(mean(!!sym(selected_column)), 0),
          Median = round(median(!!sym(selected_column)), 0),
          Max = max(!!sym(selected_column)),
          Min = min(!!sym(selected_column)),
          SD = round(sd(!!sym(selected_column)), 2)
        ),
      class = 'cell-border stripe',
      rownames = FALSE,
      options = list(
        dom = 't'
      )
    )
  })
  
  map_data <- read.csv("Nairobi Crime Rates.csv")
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 36.8167, lat = -1.2833,zoom = 7) %>% 
      addMarkers(data = map_data, popup = ~paste("Latitude:", lat, "Longitude:", lng))
  })
  
  # observeEvent(input$obs, {
  #   output$plot_area <- renderPlotly(
  #     plot_ly(x = total4$Month, y = total4$Total, type = "scatter", mode = "lines") %>% 
  #       layout(title = "Observed Crime Patterns", xaxis = list(title = "Time"), yaxis = list(title = "Reported"))
  #   )
  # })
  
  observeEvent(input$obs, {
    output$plot_area <- renderHighchart({
      highchart() %>% 
        hc_chart(type = "line") %>% 
        hc_add_series(data = total4$Total, name = "Observed Crime Data") %>% 
        hc_title(text = "Observed Values") %>% 
        hc_xAxis(title = list(text = "Time")) %>% 
        hc_yAxis(title = list(text = "Value"))
    })
  })
  
  observeEvent(input$tsdata, {
    totalts <- ts(total4$Total, frequency = 12, start = c(2018,1))
    showNotification("Crimes total is converted into a time series object.")
  })
  
  output$tseries <- renderDT({
    totalts <- ts(total4$Total, frequency = 12, start = c(2018,1))
    datatable(totalts)
  })
  
})
      
  closeConnections()

