#library(ddply)
library(shinythemes)
library(data.table) # A faster way to handle data frames in R 
library(ggplot2) # For more control on plots
library(ggthemes) # For prettier ggplot2 plot aesthetics and acessibility for color-blind palettes
library(knitr) # For pretty tables
library(lubridate) # For easy handling dates
library(scales) # To add more ticks and facilitate plot interpretation
library(tidyverse)
library(stringr)
#library(kableExtra)
library(DT)
library(cowplot)
# For Deadly Days Calendar Plot
library(lattice)
library(chron)
require(shiny)
require(ggplot2)
require(dplyr)
require(scales)
require(leaflet)
library(shinydashboard)

require(magrittr)
# Nicket Mines -> Pennsylvania
# Pennsburg ->Pennsylvania
# Washingto D.C to Washington
state_lat_long <-fread("Lat_Long_Data.csv")
list1<-state_lat_long$State
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Map", tabName = "map", icon = icon("map")),
    menuItem("Histogram", icon = icon("th"), tabName = "histogram"),
    menuItem("Race", icon = icon("th"), tabName = "race"),
    menuItem("Locations", icon = icon("th"), tabName = "locations"),
    menuItem("Causes", icon = icon("th"), tabName = "causes"),
    selectInput(choices =  list1 ,selected = list1[1] ,inputId = "State",label="State"),
    sliderInput("year", "Years:",min = 1960, max = 2017,value = 2017,step = 5)
  )
)
  
body <- dashboardBody( tags$head(tags$style(HTML('
        .shiny-text-output{ color: #111; sans-serif; font-size: 25px; font-weight: bold; letter-spacing: -1px; line-height: 1; text-align: center; }
                                   
                                                 '))),
  tabItems(
    tabItem(tabName = "map",
            fluidPage(
              box(title = strong("Mass Shooting in US"), status = "info", width = 12,
                                        "The number of mass shootings around the country in 2018 continues to climb.",br(),"
                          •According to data from the Gun Violence Archive, a total of 100 mass shooting incidents have occurred as of May 17.",br(),"
                          •Friday's deadly shooting at a high school in Texas marks the 101st mass shooting this year.",br(),"
                          •In 2017, the U.S. saw a total of 346 mass shootings.",br(),
                          "•The F.B.I. defines a mass killing as the killing of three or more people in a public place, but the federal agency also defines a mass murderer 
                            as someone who has killed four or more people in the same location.",br(),
                          "•The Gun Violence Archive lists itself as a not-for-profit organization that documents gun violence and gun crime nationally."),br(),textOutput('title_hist0'),br(),
                     leafletOutput('map',height = '600px'),br(),br(),
                     infoBoxOutput(outputId = "stat1"),
                     infoBoxOutput(outputId = "stat2"),
                     infoBoxOutput(outputId = "stat3")
                     )),
  tabItem(tabName = "histogram",fluidPage(box(title = strong("Histogram of The Mapped Data"), status = "info", width = 12,
                                              "• Seattle, Los Angles and Atlanta are the cities that are more likely to have a mass shootings.",br(),"
• Wisconsin and Texas have the most shootings than any other state.",br()," • During 2015 and 2016, mass shootings have risen dramatically"),renderText(br()),textOutput('title_hist1'),renderText(br()),plotOutput('shoot_hist1'),textOutput('title_hist2'),plotOutput('shoot_hist2'))),
  tabItem(tabName = "race",fluidPage(box(title = strong("Shooters Race"), status = "info", width = 12,
                                         "•The number of European American shooters is doubled than the African American shooters."),br(),textOutput('title_hist3'),plotOutput('race'))),
  tabItem(tabName = "locations",fluidPage(box(title = strong("Location Type"), status = "info", width = 12,
                                              "•The majority of these mass shootings happen in closed locations or spaces rather than open locations or spaces."),br(),textOutput('title_hist4'),plotOutput('locations'))),
  tabItem(tabName = "causes",fluidPage(box(title = strong("Causes"), status = "info", width = 12,
                                           "•Mental Health, Terrorism, Anger are the main causes of these mass-shootings."),br(),textOutput('title_hist5'),plotOutput('causes')))
  
  ))

ui <- dashboardPage(skin = 'red',
  dashboardHeader(title ="Mass Shootings in US from 1966 to 2017",dropdownMenuOutput("header_menu")),
  sidebar,body)

  
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  clean_shoot_data <- function (){
    shooting.events <- fread("Mass Shootings Dataset Ver 5.csv")
    shooting.events$Total_Victims <- shooting.events$`Total victims`
    shooting.events$OCL <- shooting.events$`Open/Close Location`
    victims.year <- shooting.events[,.(Date,Fatalities,Injured)]
    victims.year$Date <- mdy(victims.year$Date)
    nrow(victims.year[is.na(Date) | is.na(Fatalities) | is.na(Injured)])
    victims.year.long <- melt(victims.year,id.vars="Date",measure.vars = c("Fatalities","Injured"),variable.name="TypeOfVictim",value.name = "Number")
    fatalities.year.long <- melt(victims.year,id.vars="Date",measure.vars = c("Fatalities"),variable.name="Fatalities",value.name = "Number")
    head(state_lat_long)
    states <- {}
    shooting.events <- shooting.events[!(is.na(shooting.events$Location) | shooting.events$Location==""), ]
    loca <- shooting.events$Location
    for (i in 1:length(loca)){
      temp_state <- unlist(strsplit(loca[i],split = ","))
      temp_state
      if (length(temp_state)!=2){
        states[i]=unlist(temp_state)[1] %>% trimws()
      }
      else if (length(temp_state)==2){
        states[i]=unlist(temp_state)[2] %>% trimws()
      }
      else {
        print(loca[i])
      }
    }
    # Nicket Mines -> Pennsylvania
    # Pennsburg ->Pennsylvania
    # Washingto D.C to Washington
    shooting.events$'State'<-states
    head(shooting.events,1)
    state_lat <- {}
    state_long <- {}
    i=0
    nrow(shooting.events)
    for (i in 1:nrow(shooting.events)){
      
      if (length(unlist(state_lat_long[state_lat_long$State==shooting.events[i]$State]))==3){
        #print (shooting.events[i]$State)
        state_lat[i]<-unlist(state_lat_long[state_lat_long$State==shooting.events[i]$State])[2]
        state_long[i]<-unlist(state_lat_long[state_lat_long$State==shooting.events[i]$State])[3]
      }
      else{
        #print (shooting.events[i]$State)
      }
      
      #print (shooting.events[i])
      
    }
    # Converting to float
    shooting.events$'state_lat'<-as.numeric(state_lat)
    shooting.events$'state_long'<-as.numeric(state_long)
    # Updating Lat/Long values to State Lat /Long
    shooting.events$Latitude[is.na(shooting.events$Latitude)] <- as.numeric(shooting.events$state_lat[is.na(shooting.events$Latitude)])
    shooting.events$Longitude[is.na(shooting.events$Longitude)] <- as.numeric(shooting.events$state_long[is.na(shooting.events$Longitude)])
    
    # Cleaning Date and Creating Years
    date <- shooting.events$Date
    head(date)
    years <- {}
    length(date)
    for ( i in 1:length(date)){
      year <-unlist(strsplit(date[i],split = "/"))
      if (as.numeric(year[3])>100){
        years[i]<- as.numeric(year[3])
      }
      else {
        years[i]<- 2000+as.numeric(year[3])
      }
    }
        for (i in 1:length(years)){
      if ((years[i]) >2017){
        years[i] <- years[i]-100
      }
    }    
     shooting.events$'year'<- years
    return (shooting.events)
  }
  get_shoot_state <- function(location){
    shooting.events <- clean_shoot_data()
    shooting.events <- shooting.events[shooting.events$State==location]
    return (shooting.events)
  }
  get_shoot_year <- function(location,year_end){
    shooting.events <-get_shoot_state(location)
    shooting.events <- shooting.events[shooting.events$year <= year_end]
    return (shooting.events)
  }
  require(magrittr) 
  Location <- reactive(input$State)
  Year<-reactive(input$year)
    # Data Cleaning
  ############### Data Cleaning Done ### OUTPUTS
  state_name <- reactive({
    paste("State is ~",Location())
  })
  year_name <- reactive({
    paste("Till Year ~",Year())
  })
  output$title1 <- renderText({state_name()})
  output$title2 <- renderText({year_name()})
  output$title3 <- renderText({year_name()})
  output$map <- renderLeaflet({
    plot_data <-get_shoot_year(Location(),Year())
    head(plot_data)
    lati <-plot_data$state_lat[1]
    longi <-plot_data$state_long[1]
    
    leaflet(plot_data) %>% addTiles() %>% setView(longi,lati,zoom=5) %>%addCircles(~unlist(Longitude), ~unlist(Latitude), 
                 radius = ~sqrt(`Total_Victims`) * 8000,
                 popup=~as.character(`Summary`),weight = 1,color = 'red')
  })
  output$title_hist0 <- renderText(paste("Mass Shootings from 1966 to",Year(),"In State of",Location()))
  
  output$title_hist1 <- renderText(paste("Injuries from 1966 to",Year(),"In State of",Location()))
  output$title_hist2 <- renderText(paste("Fatatlities from 1966 to",Year(),"In State of",Location()))
  output$title_hist3 <- renderText(paste("Shooters Race from 1966 to ",Year(),"In State of",Location()))
  output$title_hist4 <- renderText(paste("Shooters V/S Location from 1966 to ",Year(),"In State of",Location()))
  output$title_hist5 <- renderText(paste("Shooters V/S Cause from 1966 to ",Year(),"In State of ",Location()))
  
  output$shoot_hist1 <- renderPlot({
    data <-get_shoot_year(Location(),Year())
    data <-data.table(data)
    theme_set(theme_bw())  # pre-set the bw theme
    summary_data<-data[,list('Injured'=sum(Injured),'Fatalities'=sum(Fatalities)),by=data$year]
    gg<-ggplot(summary_data, aes(data,Injured))+geom_histogram(fill='lightblue',stat = "Identity",na.rm = TRUE)+
      scale_x_continuous("data", labels = as.character(summary_data$data),breaks = summary_data$data)+
      labs(x='Years')+
      geom_text(label=(summary_data$Injured),size = 3)
    return(gg)
  })
  output$shoot_hist2 <- renderPlot({
    data <-get_shoot_year(Location(),Year())
    data <-data.table(data)
    summary_data<-data[,list('Injured'=sum(Injured),'Fatalities'=sum(Fatalities)),by=data$year]
    ggplot(summary_data, aes(data,Fatalities))+geom_histogram(fill='lightgreen',stat = "Identity",na.rm = TRUE)+scale_x_continuous("data", labels = as.character(summary_data$data),breaks = summary_data$data)+labs(x='Years')+geom_text(label=(summary_data$Fatalities),size = 3)
  })
  output$race <- renderPlot({
    data <-get_shoot_year(Location(),Year())
    shooting.events <-data.table(data)
    shooting.events$Race[shooting.events$Race == "Black"] <- "African American"
    shooting.events$Race[shooting.events$Race == "Black American or African American"] <- "African American"
    shooting.events$Race[shooting.events$Race == "black"] <- "African American"
    shooting.events$Race[shooting.events$Race == "White American or European American"] <- "European American"
    shooting.events$Race[shooting.events$Race == "white"] <- "European American"
    shooting.events$Race[shooting.events$Race == "White"] <- "European American"
    shooting.events$Race[shooting.events$Race == "Asian"] <- "Asian American"
    shooting.events$Race[shooting.events$Race == "Latino"] <- "Other"
    shooting.events$Race[shooting.events$Race == "Some other race"] <- "Other"
    shooting.events$Race[shooting.events$Race == "Native American or Alaska Native"] <- "Other"
    shooting.events$Race[shooting.events$Race == "White American or European American/Some other Race"] <- "Other"
    shooting.events$Race[shooting.events$Race == "Black American or African American/Unknown"] <- "Other"
    shooting.events$Race[shooting.events$Race == "Asian American/Some other race"] <- "Other"
    shooting.events$Race[shooting.events$Race == ""] <- "Other"
    shootings <- shooting.events
    shootings %>%
      filter(!is.na(shootings$Race)) %>%
      group_by(Race) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      ungroup() %>%
      mutate(Race = reorder(Race,Count)) %>%
      
      ggplot(aes(x = Race,y = Count)) +
      geom_bar(stat='identity',colour="white", fill = 'red') +
      geom_text(aes(x = Race, y = 1, label = paste0("(",Count,")",sep="")),
                hjust=0, vjust=.5, size = 4, colour = 'black',
                fontface = 'bold') +
      labs(x = 'Race', 
           y = 'No of Shootings'
         ) +
      coord_flip() +
      theme_bw() 
  })
  
  output$locations <- renderPlot({
    data <-get_shoot_year(Location(),Year())
    shooting.events <-data.table(data)
    shooting.events$OCL[shooting.events$OCL == ""] <- 'Unknown'
    shooting.events$OCL[shooting.events$OCL == "Open+Close"] <- 'Both'
    shooting.events$OCL[shooting.events$OCL == "Open+CLose"] <- 'Both'
    shootings <- shooting.events
    shootings %>%
      filter(!is.na(OCL)) %>%
      group_by(OCL) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      ungroup() %>%
      mutate(OCL = reorder(OCL,Count)) %>%
      ggplot(aes(x = OCL,y = Count)) +
      geom_bar(stat='identity',colour="white", fill = 'Orange') +
      geom_text(aes(x = OCL, y = 1, label = paste0("Count = ",Count,"",sep="")),
                hjust=0, vjust=.5, size = 4, colour = 'black',
                fontface = 'bold') +
      labs(x = 'Open/Close Location', 
           y = 'No of Shootings'
           ) +
      coord_flip() +
      theme_bw() 
    
    
  })
  
  output$causes <- renderPlot({
    data <-get_shoot_year(Location(),Year())
    shooting.events <-data.table(data)
    shooting.events$Cause[shooting.events$Cause == ""] <- NA
    shootings <- shooting.events
    shootings%>%
      filter(!is.na(shooting.events$Cause)) %>%
      group_by(Cause) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      ungroup() %>%
      mutate(Cause = reorder(Cause,Count)) %>%
      
      ggplot(aes(x = Cause,y = Count)) +
      geom_bar(stat='identity',colour="white", fill = 'lightblue') +
      geom_text(aes(x = Cause, y = 1, label = paste0("Count = ",Count,"",sep="")),
                hjust=0, vjust=.5, size = 4, colour = 'black',
                fontface = 'bold') +
      labs(x = 'Cause', 
           y = 'No of Shootings') +
      coord_flip() +
      theme_bw() 
    })
  
  output$stat1 <- renderInfoBox({
    data <- get_shoot_year(Location(),Year())
    infoBox(
      "Total Mass Shootings ", paste0(nrow(data)), icon = icon("bomb"),
      color = "blue"
    )})
  
  output$stat2 <- renderInfoBox({
    data <- get_shoot_year(Location(),Year())
    infoBox(
      "Injured ", paste0(sum(data$Injured)), icon = icon(lib ='glyphicon',"fire"),
      color = "yellow"
    )
  })
  output$stat3 <- renderInfoBox({
    data <- get_shoot_year(Location(),Year())
    infoBox(
      "Total Dead ", paste0(sum(data$Fatalities)), icon = icon("times"),
      color = "red"
    )
  })
  output$header_menu <- renderMenu({
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = 'messages',
                 notificationItem(
                   text = "Sarthak Kedia",
                   icon("user")
                 ),
                 notificationItem(
                   text = "28574400",
                   icon("graduation-cap"),
                   status = "success"
                 ),
                 notificationItem(
                   "Dated: 6-June-2018",
                   icon(lib = "glyphicon","calendar"),
                   status = "success"
                 ))
  })
}
  

# Run Shiny app ----
shinyApp(ui = ui, server = server)

