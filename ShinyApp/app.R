#library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(raster)
library(maptools)
library(sf)
library(shinythemes)


source("www/functions/switchMap.R")
source("www/functions/pollutantByStation.R")
source("www/functions/pollutantByMonth.R")
source("www/functions/pollutantByWeather.R")

options(warn=-1)

df <- read.csv("df.csv")

# 1. Add AQI in df

# Function to calculate AQI
calculate_aqi <- function(pm25, pm10) {
    
    pm25_breakpoints <- c(0, 12.1, 35.5, 55.5, 150.5, 250.5, 350.5, 500.5)
    pm25_i <- approx(pm25_breakpoints, c(0, 50, 100, 150, 200, 300, 400, 500), pm25)$y
    
    pm10_breakpoints <- c(0, 54, 154, 254, 354, 424, 504, 604)
    pm10_i <- approx(pm10_breakpoints, c(0, 50, 100, 150, 200, 300, 400, 500), pm10)$y
    
    aqi_pm25 <- max(pm25_i, na.rm = TRUE)
    aqi_pm10 <- max(pm10_i, na.rm = TRUE)
    
    aqi <- max(aqi_pm25, aqi_pm10)
    
    return(aqi)
}

# Calculate AQI for each row in the dataframe
aqi_values <- apply(df[, c("PM2.5", "PM10")], 1, function(x) calculate_aqi(x[1], x[2]))

# Add AQI values to the dataframe
df$aqi <- aqi_values

df$aqi_category <- cut(df$aqi,
                       breaks = c(0, 50, 100, 150, 200, 300, Inf), 
                       labels = c("Good", "Moderate", "Unhealthy for Sensitive Groups", 
                                  "Unhealthy", "Very Unhealthy", "Hazardous"))
############################################################################################
############################################################################################
ui <- navbarPage(
    "Air Quality in Beijing",
    theme = shinytheme("flatly"),
    
    # Add a tab for the first page
    tabPanel("Overview of Air Condition",
             sidebarPanel(
                 width = 3,
                 p("The dataset includes 6 types of common pollutants in Beijing from March 1st, 2013 to February 28th, 2017"),
                 radioButtons(
                     inputId = "colN1",
                     label = "Choose the pollutant type:",
                     choices = c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3"),
                     selected = "PM2.5"),
                 p("Data Source: https://archive.ics.uci.edu/ml/datasets/Beijing+Multi-Site+Air-Quality+Data#"),
                 
                 verbatimTextOutput("ccc")),
             mainPanel(
                 fluidRow(
                     column(width = 12, plotOutput("boxPlot"))
                 ),
                 fluidRow(
                     column(width = 6, plotOutput("linePlot")),
                     column(width = 6, plotOutput("vioPlot"))
                 )
             )
    ),
    
    # Add a tab for the second page
    tabPanel("AQI",
             sidebarPanel(
                 width = 3,
                 p("Based on the air quality index (AQI), we can classify air quality as good, moderate, unhealthy for sensitive groups, unhealthy, very unhealthy, and hazardous"),
                 dateInput("select_date",
                           label = "Choose the Date:",
                           format = "yyyy-mm-dd",
                           value="2013-03-01",
                           min="2013-03-01",
                           max="2017-02-28"),
                 sliderInput(
                     width = "100%",
                     inputId = "hour",
                     label = "Choose the Hour:",
                     min = 0,
                     max = 23,
                     value = 0,
                     step = 1,
                     animate = animationOptions(interval = 1000, loop = FALSE)
                 )),
             mainPanel(
                 plotOutput(
                     "plotChart",
                     width = "100%",
                     height = "500px"),
                 imageOutput("img"),
                 tags$div(class="header", checked=NA, align="center",
                          tags$a(href="https://img.freepik.com/free-vector/air-quality-index-template-with-sick-children-city_1308-38761.jpg?w=1800&t=st=1680910271~exp=1680910871~hmac=25074b6b3cec393db687a00e30157bb4339c037b2fe7bc0fde2496fc487d2b23", "Image by brgfx on Freepik")
                 )
             )
    ),
    
    # Add a tab for the third page
    tabPanel("Map by pollutants",
             sidebarPanel(
                 width = 3,
                 p("Please select date between March 1st, 2013 and February 28th, 2017"),
                 radioButtons(
                     inputId = "colN2",
                     label = "Choose the pollutant type:",
                     choices = c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3"),
                     selected = "PM2.5"
                 ),
                 sliderInput(
                     width = "100%",
                     inputId = "Year",
                     label = "Chhose the Year:",
                     min = 2013,
                     max = 2017,
                     value = 2013
                 ),
                 sliderInput(
                     width = "100%",
                     inputId = "Month",
                     label = "Choose the Month:",
                     min = 1,
                     max = 12,
                     value = 1
                 ),
                 sliderInput(
                     width = "100%",
                     inputId = "Day",
                     label = "Choose the Day:",
                     min = 1,
                     max = 31,
                     value = 1
                 ),
                 sliderInput(
                     width = "100%",
                     inputId = "Hour",
                     label = "Choose the Hour:",
                     min = 0,
                     max = 23,
                     value = 0
                 ),
                 verbatimTextOutput("bbb")),
             
             mainPanel(
                 h1("Map plot of air pollutants"),
                 p("Concentration of selected air pollutants at 12 nationally-controlled air-quality monitoring sites"),
                 plotOutput("mapPlot")
             )
    ),
    tabPanel("Overview of correlation of PM2.5",
             h3("For modeling part, we focus on PM2.5. So we focus on check the correlation between PM2.5 and other variables,"),
             
             fluidRow(
                 column(width = 6,
                        imageOutput("pm25_co", height = 400, width = 500)),
                 column(width = 6,
                        imageOutput("pm25_dewpoint",height = 400, width = 500))
             ),
             fluidRow(
                 column(width = 6,
                        imageOutput("pm25_temp", height = 400, width = 500)),
                 column(width = 6,
                        imageOutput("pm25_so2", height = 400, width = 500))
             )
             
    ),
    tabPanel("Linear Regression - set station as a factor",
             h2("We use linear regression model and set station as factor to build our model"),
             br(),
             h4("Based on the values in the Estimate column, we can write the fitted regression model:"),
             br(),
             h4("PM2.5 = -19.36 + 0.55(PM10) + 0.02(SO2) + 0.17(NO2) + 0.02(co) + 0.06(O3) + 1.75(Changping) + 9.89(Dingling) + 3.19(Dongsi) + 0.912(Guanyuan) - 4.55(Gucheng) + 6.43(HUairou) + 1.35(Nongzhanguan) - 1.04(Wanliu) - 0.54(Wanshouxigong)"),
             br(),
             imageOutput("intercept", height = 600, width = 600),
    ),
    
    tabPanel("Lasso Regression",
             h2("We use Lasso Regression model to build our model"),
             br(),
             h4("To test Mean-Squared Error with different lambda value, the lambda value that minimizes the test MSE is 0.21"),
             br(),
             h4("Final model: PM2.5 = -13.894 + 0.56(PM10) + 0.02(SO)2 + 0.11(NO2) + 0.02(CO) + 0.05(O3)"),
             h4("R-Squared = 0.825, the model was able to explain 82.5% of the variation"),
             imageOutput("lambda",height = 500, width = 800),
             br()
    ),
    tabPanel("Ridge Regression",
             h2("Ridge regression is a method we use to fit a regression model when multicollinearity is present in the data"),
             br(),
             h4("To test Mean-Squared Error with different lambda value, the lambda value that minimizes the test MSE is 7.04"),
             br(),
             h4("Final model: PM2.5 = -13.725 + 0.49(PM10) + 0.09(SO2) + 0.22(NO2) + 0.02(CO) + 0.07(O3)"),
             h4("R-Squared = 0.822, the model was able to explain 82.2% of the variation"),
             fluidRow(
                 column(width = 3,
                        imageOutput("intercept2", height = 300, width = 300)),
                 column(width = 9,
                        imageOutput("RidgeModel", height = 300, width = 500))
             )
    )
    
)


server <- function(input, output, session) {
    # Server code for first tabPanel
    defaultValues2 <- reactiveValues(
        colN = "PM2.5"
    )
    
    observe({
        defaultValues2$colN <- input$colN1
    })
    
    observeEvent(input$colN1, {
        defaultValues2$colN
    })
    
    output$boxPlot <- renderPlot({
        pollutantByStation(input$colN1)
    })
    
    output$linePlot <- renderPlot({
        pollutantByMonth((input$colN1))
    })
    
    output$vioPlot <- renderPlot({
        pollutantByWeather((input$colN1))
    })
    
    # Server code for second tabPanel
    values <- reactiveValues(date = NULL,
                             year = NULL,
                             month = NULL,
                             day = NULL,
                             hour = NULL)
    
    observeEvent(input$select_date, {
        values$date <- input$select_date
        values$year <- as.numeric(format(as.Date(values$date), "%Y"))
        values$month <- as.numeric(format(as.Date(values$date), "%m"))
        values$day <- as.numeric(format(as.Date(values$date), "%d"))
        
    })
    
    observeEvent(input$hour, {
        values$hour <- as.numeric(input$hour)
    })
    
    observe({
        output$plotChart <- renderPlot({
            color_vec <- c("Good" = "green", "Moderate" = "yellow", "Unhealthy for Sensitive Groups" = "orange", "Unhealthy" = "red", "Very Unhealthy" = "purple", "Hazardous" = "maroon")
            df %>% 
                filter(year == values$year & month == values$month & day == values$day & hour == values$hour) %>% 
                ggplot(aes(x=station, y=aqi, fill=aqi_category)) +
                geom_bar(stat = "identity") +
                scale_fill_manual(values = color_vec) +
                theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
                theme(legend.title = element_text(face = "bold"))+
                labs(x = "Station", y = "Air Quality Index", fill = "Air Quality Index Level")
        })
    })
    
    output$img <- renderImage({
        list(src = "www/functions/AQI.jpg",
             width = "50%",
             height = "100%"
        )
    }, deleteFile = F)
    
    
    # Server code for third tabPanel
    # Server code for third tabPanel
    defaultValues <- reactiveValues(
        colN = "PM10",
        Year = 2013,
        Month = 3,
        Day = 1,
        Hour = 0
    )
    
    # Set initial values of input elements
    observe({
        updateSelectInput(session, "colN", selected = defaultValues$colN)
        updateSliderInput(session, "Year", value = defaultValues$Year)
        updateSliderInput(session, "Month", value = defaultValues$Month)
        updateSliderInput(session, "Day", value = defaultValues$Day)
        updateSliderInput(session, "Hour", value = defaultValues$Hour)
    })
    
    observeEvent(input$colN2, {
        defaultValues$colN <- input$colN2
    })
    
    observeEvent(input$Year, {
        defaultValues$Year <- input$Year
    })
    
    observeEvent(input$Month, {
        defaultValues$Month <- input$Month
    })
    
    observeEvent(input$Day, {
        defaultValues$Day <- input$Day
    })
    
    observeEvent(input$Hour, {
        defaultValues$Hour <- input$Hour
    })
    
    output$mapPlot <- renderPlot({
        switchMap(input$colN2, input$Year, input$Month, input$Day, input$Hour)
    })
    
    # match the images for the modeling part
    output$pm25_co <- renderImage({
        list(src = "www/functions/PM25vsCO.jpg", height = 400, width = 500)
    }, deleteFile = FALSE)
    
    output$pm25_dewpoint <- renderImage({
        list(src = "www/functions/PM25vsDewPoint.jpg", height = 400, width = 500)
    }, deleteFile = FALSE)
    
    output$pm25_temp <- renderImage({
        list(src = "www/functions/PM25vsTemperature.jpg", height = 400, width = 500)
    }, deleteFile = FALSE)
    
    output$pm25_so2 <- renderImage({
        list(src = "www/functions/SO2vsPM25.jpg", height = 400, width = 500)
    }, deleteFile = FALSE)
    
    output$intercept <- renderImage({
        list(src = "www/functions/intercept.jpg", height = 600, width = 600)
    }, deleteFile = FALSE)
    
    output$lambda <- renderImage({
        list(src = "www/functions/lambda.jpg", height = 500, width = 800)
    }, deleteFile = FALSE)
    
    output$intercept2 <- renderImage({
        list(src = "www/functions/intercept2.jpg", height = 300, width = 300)
    }, deleteFile = FALSE)
    
    output$RidgeModel <- renderImage({
        list(src = "www/functions/RidgeModel.jpg", height = 300, width = 500)
    }, deleteFile = FALSE)
    
}


# Run the app ----

shinyApp(ui = ui, server = server)