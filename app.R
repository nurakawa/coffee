#####
# libraries
library(shiny)
library(leaflet)
library(readxl)
library(shinydashboard)
library(graphics)
library(googleVis)

#####
# source helper
source("scripts/app-helper.R")

#####
# ui
ui <- bootstrapPage(
  
  # include custom CSS
  includeCSS("style.css"),
  
  # style tag
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  # map output
  leafletOutput("camap", width = "100%", height = "100%"),
  
  # about this app
  absolutePanel(id="about",
                top=300,
                left=20,
                width=350,
                h3("About this App"),
                p("This app visualizes the distribution of Starbucks stores accross 
                  California. Each city is marked by a circle, whose size is proportional 
                  to its population and whose color is proportional to the number of Starbucks 
                  locations per capita. To see population and income information about a city, 
                  select or type its name in the above box. The code and data used to make this app 
                  are available at github.com/nurakawa/coffee.")),
  
  # title
  absolutePanel(id="title",
                #class = "panel panel-default",
                draggable=FALSE,
                top = 10,
                left = 80,
                width="auto",
                height = 10,
                h1("Starbucks Stores California")),
  
  # city stats
  absolutePanel(id = "stats", class = "panel panel-default", 
                fixed = TRUE,
                draggable = TRUE, 
                top = 100,
                left = 20,
                right = "auto",
                bottom = "auto",
                width = 300, height = "auto",
                selectInput("city", label = strong(h4("City Statistics")), 
                                                choices = c("", ca$city), 
                                                selected = "",
                                                width = "100%"),
                fluidRow(
                valueBoxOutput("popBox"),
                valueBoxOutput("incomeBox"),
                valueBoxOutput("starBox")
                )
  ))


##### 
# server
server <- function(input, output, session) {
  
  # Starbucks count
  output$starBox <- renderValueBox({
    df <- df_valueBox[df_valueBox$city == input$city,]
    valueBox(
      paste0(df$starbucks_count,"\n","(", 
             df$bin_star, ")"), "Starbucks Stores")
  })
  
  # Population
  output$popBox <- renderValueBox({
    df <- df_valueBox[df_valueBox$city == input$city,]
    valueBox(
      paste0(df$pop,"\n","(", df$bin_pop, ")"), "Population")
  })
  
  # Median Income
  output$incomeBox <- renderValueBox({
    df <- df_valueBox[df_valueBox$city == input$city,]
    valueBox(
      paste0(df$median_household_income,"\n",
             "(", df$bin_income, ")"), 
      "Median Household Income")
    })

  # california map
  output$camap <- renderLeaflet({
    
    # set zoom to somewhere in central CA
    leaflet(ca) %>% addTiles() %>% 
      setView(lng = ca[ca$city == "Fresno","long"], 
              lat = ca[ca$city == "Fresno","lat"], 
              zoom = 6) 
  })
  
  # add circle markers proportional to population
  leafletProxy("camap",data=ca) %>% 
    addCircleMarkers(data = ca,
                     fillOpacity = 0.6,
                     radius = ~(sqrt(pop)/30),
                     weight = 1,
                     label = ~city,
                     color = ~starbuckspal(make_bins(starbucks_count/pop))) %>%
    
    # legend: palette, starbucks pc
    addLegend("bottomleft", 
              colors= c("darkolivegreen1",
                        "darkgreen",
                        "black"),
              title = "Starbucks stores per Capita",
              labels = c("low",
                         "med",
                         "high"))
                  
  
  # popup based on selected city
  observeEvent(input$city, {
    
    if(input$city != "")
    {
      leafletProxy("camap") %>% 
        clearPopups()
      
      index = which(ca$city == input$city)
      
      leafletProxy("camap") %>% 
        addPopups(lng = as.numeric(ca$long[index]), 
                  lat = as.numeric(ca$lat[index]), 
                  popup = paste(ca$city[index], ": ", ca$starbucks_count[index],
                                "stores"))
    } else {
      leafletProxy("camap") %>% 
        clearPopups() 
    }
  })
    
}  

  
# run shiny app
shinyApp(ui, server)