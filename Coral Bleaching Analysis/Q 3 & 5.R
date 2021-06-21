library(shiny)
library(leaflet)
library(ggplot2)

# Reading the data file
bleach <- read.csv("assignment-02-data-formated.csv")

# Conversion of bleaching % into numeric for better processing and visualisation
bleach$bleaching <- as.numeric(sub("%","",bleach$value))

# Ordering the sites based on latitude, rev is used to order the sites in desending order
bleach$location <- factor(bleach$location,levels = rev(unique(bleach$location[order(bleach$latitude)])))

# Creating the UI of app
ui <- fluidPage(
  
  # Title of UI app
  titlePanel("Bleaching of Corals"),
  
  # Creating the drop down menu for Coral Type and Smoothing function
  sidebarLayout(
    sidebarPanel(
      selectInput("type","Select Coral", 
                  c("Blue Corals" = "blue corals", "Soft Corals"="soft corals","Hard Corals" = "hard corals",
                    "Sea Fans" = "sea fans", "Sea Pens"="sea pens")
      ),
      selectInput("smooth","Select Smoothing function",
                  c("None"='No function',"Linear Model"='lm',"Generalised Linear Model"='glm')
                  )
    ),
    
    mainPanel(
      h2(textOutput("header")),
      plotOutput("plot")
    )
  ),
  # creating Leafmap
  leafletOutput("leafmap")
  
)

# Creating server of app (Since it's on same machine so it will be localhost)
server <- function(input, output) {
  
  # LeafMap output
  output$leafmap <- renderLeaflet(
    {
      leaflet(data = bleach) %>% addTiles() %>%
        addMarkers(~longitude, ~latitude, popup = ~as.character(location)) 
      
    }
  )
  
  # Header of the plot, paste function concatenates the string
  output$header <- renderText(paste(toupper(input$type),"bleaching at different Sites"))
  
  # Plotting the bleaching of corals for different sites
  output$plot <- reactivePlot(function(){
   
    # subset the data based on coral type choosen 
    bleach_new <- bleach[bleach$coralType == input$type,]
    
    
    # Condition, wheather user wants to select a smoothing function or not
    if(input$smooth != 'No function'){
      ggplot(data = bleach_new, aes(x=year, y=bleaching, color = location, size=bleaching)) +
        geom_jitter() + xlab("Years") + ylab("Bleaching Percentage") +
        facet_grid(~location) + ggtitle(toupper(input$type)) + geom_smooth(method = input$smooth , span = 0.5)
    }
    else{
      ggplot(data = bleach_new, aes(x=year, y=bleaching, color = location, size = bleaching)) +
        geom_jitter() + xlab("Years") + ylab("Bleaching Percentage") +
        facet_grid(~location) + ggtitle(toupper(input$type))
      
    }
  })
  

  
}

shinyApp(ui, server)