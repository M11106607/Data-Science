
# importing necessary libraries
library(shiny)
library(reshape2)
library(ggplot2)
library(plotly)
library(ECharts2Shiny)
library(shinyWidgets)

# Reading the data file
df <- read.csv("Movie_Data.csv")

# Setup for UI
ui <- fluidPage(
  # Background color for the application
  setBackgroundColor(
    color = c("#FFE4E1","#FFB6C1"),
    gradient = "linear",
    direction = "bottom"
  ), 
  
  # Application title
  titlePanel(span("Movie Data Analysis based on different parameters", style="color:firebrick")),
  
# Creating a navigation tab
navbarPage(strong(span("Navigation Bar", style="color:midnightblue")),
           # Tab 1 for Genre Types
           tabPanel( strong(span("Genre Types", style="color:midnightblue")),
                     
            # Loading ECharts2shiny 
             loadEChartsLibrary(),
             # Creating a div for paragraph
             tags$div(class="header", checked=NA, style="width:90%;height:100px;",
                     tags$p(strong("In this section of the application, I have visualized the different types of genres. The pie chart displays all types of genre and their proportional ratio. When you hover over the Pie Chart you will see the count of that specific genre and its proportion, respectively. The line graph below shows the Revenue Vs Budget for top five genres. Simply choose the genre type from the drop down above and you will see the respective Revenue Vs Budget output. It's really important to understand this relation as these relations shows us what type of genre audience prefer and by knowing the audience preferences production companies can invest accordingly."))
             ),
             
             tags$div(class="header", checked=NA, tags$h3(strong("Genre Pi Chart Representation"), align="left")),
             
            # Creating a div for pi chart
             tags$div(id="test", style="width:80%;height:500px;"),
             deliverChart(div_id = "test"),
             
            # Creating a div for paragraph
             tags$div(class="header", checked=NA, style="width:90%;height:50px;",
                      tags$p(strong(em(span("As we can see from above Pie Chart, genres such as Drama, Comedy, Action, and Horror share the major portion. Which depicts the prefernces what genre type people prefer more.", style = "color:brown"))))),
             
            # creating a side bar layout in tab 1 of the application
             sidebarLayout( position = "right",
               sidebarPanel(
                 # Crearting input variable and drop down widget for first tab
                 selectInput("movieGenre","Select Movie Genre", 
                             c("Comedy" = "Comedy", "Drama" = "Drama", "Action" = "Action",
                               "Horror" = "Horror", "Documentary" = "Documentary")),
                 tags$div(class="header", checked=NA,
                          tags$p(strong(em("Please chose the", span("Movie Genre",style="color:red"), "from the drop down. As per your selection the Revenue vs Budget graph on the left side will change. When hover over the graph you can see the Revenue and Budget for a specific year.")))
                 )),
               # naming output variable
               mainPanel(
                 h4(textOutput("header"), align = "center"),
                 plotlyOutput(outputId = "genre"),
                 plotlyOutput(outputId = "share")
                 )
               )
             ),
           # Tab 2 of navigation bar
           tabPanel(strong(span("Novel vs Non-Novel Movies", style="color:midnightblue")),
                    
                    tags$div(class="header", checked=NA, style="width:90%;height:200px;",
                             tags$p(strong(span("Movies Based on Novels:",style = "color:coral"), "After selecting option Novel, as we can see below that movies based on novel have increased over the period. Although numbers are not that much significant but since 1997 numbers have been doubled. If we look at the figure below. We can see novel based movies have better returns in
comparison to budget. Revenue is almost double compare to their budget. In this figure we can see that both budget and revenue has increased since 1997 because number of movies have been increased. This shows that production companies have little interest to produce a movie based on Novel. Although their returns are good."))
                             , tags$p(strong(span("Movies not Based on Novels:",style = "color:coral"), "After selecting option Non-Novel, as we can see below that Movies that are not based on Novel have been dominating. We can see exponential growth in number of movies released after Year 2000. It clearly shows that production companies have more confidence in their own creativity rather than to adapt others idea. Both total budget and total revenue have been exponentially increased after 1995. We can also conclude from the below that investing more in a film is yielding more revenue."))
                             ),
                    # Crearting input variable and drop down widget for second tab
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("Based","Movies Based On", 
                                    c("Novel" = "Is Novel", "Non-Novel" = "Not Novel")),
                        tags$div(class="header", checked=NA,
                                 tags$p(strong(em("Please chose one of the option from the drop down. As per your selection the Revenue vs Budget graph on the right side below will be updated. When hover over the graph you can see the Revenue and Budget for a specific year.")))
                        )),
                      # naming output variable
                      mainPanel(
                        plotlyOutput(outputId = "novel"),
                        plotlyOutput(outputId = "count")
                      ))),
           
           # Tab 3 of navigation bar
           tabPanel(strong(span("Production Country", style="color:midnightblue")),
                    tags$div(class="header", checked=NA, style="width:90%;height:50px;",
                             tags$p(strong("Every country has its own taste and culture. When it comes to entertainment majority of population have their preferences which makes popularity of genre different from country to country. In this section we are analysing that what genres are more preferred over the others for different countries. It can be determined by the number of movies released for each genre in a specific country. "))),
                    
                    tags$div(class="header", checked=NA, style="width:50%;height:20px;",
                            tags$p(strong(span("Analysis",style="color:red")))),
                    
                    # Creating list
                    tags$ul(
                      tags$li(strong("Bar graph", span("Total Movies Released for different countries", style="color:red"),"show total movies released from 1960 to 2017.")),
                      tags$li(strong("United States of America tops the list with more than 14K movies released in last 6 decades followed by United Kingdom.")),
                      tags$li(strong("Bar graph", span("Popular Genre for Country:", style="color:red"), "shows the popularity based on total movies released in last 6 decades for each genre.")),
                      tags$li(strong("From option", span("Chose Country",style="color:red"), "when we select a country it we produce a bar chart based on the popularity of genre in that Country."))
                    ),
                    # Crearting input variable and drop down widget for third tab
                    sidebarLayout(
                      
                      sidebarPanel(
                        selectInput("Country","Chose Country", 
                                    c("Canada"="Canada","France"="France","Germany"="Germany",
                                      "India"="India","Italy"="Italy","Japan"="Japan","Russia"="Russia",
                                      "Spain"="Spain","United Kingdom"="United Kingdom",
                                      "United States of America"="United States of America")),
                        tags$div(class="header", checked=NA,
                                 tags$p(strong(em("Please chose one of the", span("Production Country", style = "color:red"), "from the drop down. On right side below as per your selection we can classify the genres in descending order as per the popularity in that country.")))
                        )),
                      # naming output variable
                      mainPanel(
                        plotlyOutput(outputId = "country"),
                        plotlyOutput(outputId = "countrygenre")
                      ))),
           
           # Tab 4 of navigation bar
           tabPanel(strong(span("Production Companies", style="color:midnightblue")),
                    tags$div(class="header", checked=NA, style="width:90%;height:100px;",
                             tags$p(strong("Just like genre we have many different production companies. In this section we will compare top production companies (more than 300 movies released over the 6 decades).",
                                           span("Total Movies released by different Production Countries", style="color:red"),"Paramount Pictures top the list with more than 600 movies released.
                                           From the drop down we can chose the company name and it will generate a line graph which show releation between revenue and budget. That way we can analyze how to they have performed over the years in terms of profit."))),
                    
                    # Crearting input variable and drop down widget for fourth tab
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("Prod","Select Company", 
                                    c("Columbia Pictures"="Columbia Pictures", 
                                      "Paramount Pictures"="Paramount Pictures", 
                                      "Twentieth Century Fox Film Corporation"="Twentieth Century Fox Film Corporation", 
                                      "Universal Pictures"="Universal Pictures",
                                      "Warner Bros"="Warner Bros.")),
                        tags$div(class="header", checked=NA,
                                 tags$p(strong(em("Please chose one of the", span("Production Company", style = "color:red"), "from the drop down. As per your selection the Revenue vs Budget graph on the right side below will be updated. When hover over the graph you can see the Revenue and Budget for a specific year.")))
                        )),
                      # naming output variable
                      mainPanel(
                        plotlyOutput(outputId = "prodCount"),
                        plotlyOutput(outputId = "production")
                        
                      )))
           
           ),
)

server <- function(input, output) {

  # rendering plot for genre types
  output$genre <- renderPlotly( {
    
    # dataframe based on movie genre
    gen <- df[df$genres == input$movieGenre,]
    
    # aggregating to get sum of revenue and budget
    rev <- as.data.frame(aggregate(gen$revenue, by=list(gen$year), FUN=sum))
    bug <- as.data.frame(aggregate(gen$budget, by=list(gen$year), FUN=sum))
    
    # updating columns name
    colnames(rev) <- c("Year", "Revenue")
    colnames(bug) <- c("Year", "Budget")
    
    # merging revenue and budget dataframe in one
    genre_df <- merge(rev, bug, by = "Year")
    data_df <- melt(genre_df, id="Year")
    
    # ploting line graph using ggplot
    ggplot(data = data_df, aes(x = Year, y = value, color = variable)) + geom_line(size = 1.5) +
      xlab("Year") + ylab("Revenue vs Budget") +
      ggtitle(paste("Revenue generated by", input$movieGenre,"Movies vs Budget")) + 
      theme(plot.title = element_text(size = 15, face = "bold"))
    
  })
  # Rendering Pie chart for genre types
  genre <- c(as.character(df$genres))
  
  renderPieChart(div_id = "test",
                 data = genre)
  
  # Rendering interactive bar plot
  output$novel <- renderPlotly({
    
    # dataframe based on Novel and Non-novel movies
    novel <- df[df$based.on.novel == input$Based,]
    
    # Counting the number of movies based on novel and non novel movies
    type <- as.data.frame(table(novel$year))
    
    colnames(type) <- c("Year", "Count")
    col <- if(input$Based == "Is Novel"){"coral2"} else{"aquamarine2"}
    
    # Bar graph using ggplot
    ggplot(data = type, aes(x = Year, y = Count)) + geom_bar(stat="identity", fill = col) +
      xlab("Year") + ylab("Count") + theme(axis.text.x=element_text(angle=45, hjust=1)) +
      ggtitle(paste("Number of", if(input$Based == "Is Novel"){"Novel"} else{"Non-Novel"}, 
                    "movies released over the years")) + 
      theme(plot.title = element_text(size = 15, face = "bold"))
    
  })
  
  # Rendering interactive line graph 
  output$count <- renderPlotly({
    
    # dataframe based on Novel and Non-novel movies
    novel1 <- df[df$based.on.novel == input$Based,]
    
    # aggregating to get sum of revenue and budget
    type1 <- as.data.frame(aggregate(novel1$revenue, by=list(novel1$year), FUN=sum))
    
    colnames(type1) <- c("Year", "Revenue")
    col <- if(input$Based == "Is Novel"){"coral2"} else{"aquamarine2"}
    
    # line graph using ggplot
    ggplot(data = type1, aes(x = Year, y = Revenue)) + geom_line(color = col, size = 1.5) +
      xlab("Year") + ylab("Revenue") + theme(axis.text.x=element_text(angle=45, hjust=1)) + 
      ggtitle(paste("Revenue for", if(input$Based == "Is Novel"){"Novel"} else{"Non-Novel"}, 
                    "Movies")) + 
      theme(plot.title = element_text(size = 15, face = "bold"))
  }) 
  
  # Rendering interactive bar graph
 output$country <- renderPlotly({
   # dataframe based on countries, getting count of movies released
   country <- as.data.frame(table(df$production_countries))
   
   colnames(country) <- c("Country", "No.Of.Movies")
   
   count <- country[country$No.Of.Movies > 500,]
   
   # bar plot using ggplot for count movies
   ggplot(data = count, aes(x = reorder(Country, -No.Of.Movies), y = No.Of.Movies, 
                            group = 1, fill = Country)) + 
     geom_bar(stat="identity",width = 0.5) + xlab("Countries") + ylab("Total Movies Released") +
     theme(axis.text.x=element_text(angle=45, hjust=1)) +
     ggtitle("Total Movies released for different Countries") + 
     theme(plot.title = element_text(size = 15, face = "bold"))
   
   
 }) 
 # Rendering interactive bar graph
 output$countrygenre <- renderPlotly({
   # dataframe based on country
   count_genre <- df[df$production_countries == input$Country,]
   # dataframe getting count of genres
   gen <- as.data.frame(table(count_genre$genres))
   
   # renaming column name 
   colnames(gen) <- c("Genres", "Freq")
   
   # bar plot of genre based on country
   ggplot(data = gen, aes(x = reorder(Genres, -Freq), y = Freq, group = 1, fill = Genres)) + 
     geom_bar(stat="identity",width = 0.5) + 
     xlab("Genres") + ylab("Genres Count") +  theme(axis.text.x=element_text(angle=45, hjust=1)) +
     ggtitle(paste("Popular Genre for Country:", input$Country)) + 
     theme(plot.title = element_text(size = 15, face = "bold"))
 })
 
 # Rendering interactive bar graph
 output$prodCount <- renderPlotly({
   # dataframe based on production country
   prod <- as.data.frame(table(df$production_companies))
   # renaming column name
   colnames(prod) <- c("Production_House", "No.Of.Movies")
   # filtering number of movies
   prod <- prod[prod$No.Of.Movies > 300,]
   # bar plot of total movies released by different production country
   ggplot(data = prod, aes(x = reorder(Production_House, -No.Of.Movies), y = No.Of.Movies, 
                            group = 1, fill = Production_House)) + 
     geom_bar(stat="identity",width = 0.5) + xlab("Production Companies") + 
     ylab("Total Movies Released") +
     theme(axis.text.x=element_text(angle=45, hjust=1)) +
     ggtitle("Total Movies released by different Production Companies") + 
     theme(plot.title = element_text(size = 15, face = "bold"))
   
 })
 
 # Rendering interactive line graph
 output$production <- renderPlotly({
   # dataframe based on production country
   companies <- df[df$production_companies == input$Prod,]
   # taking total sum of revenu and budget
   com.rev <- as.data.frame(aggregate(companies$revenue, by=list(companies$year), FUN=sum))
   com.bug <- as.data.frame(aggregate(companies$budget, by=list(companies$year), FUN=sum))
   # rebaming column names
   colnames(com.rev) <- c("Year", "Revenue")
   colnames(com.bug) <- c("Year", "Budget")
   # merging revenue and budget data frame
   comp.rev.bug <- merge(com.rev, com.bug, by = "Year")
   # reshaping the data frame
   comp <- melt(comp.rev.bug, id="Year")
   # line plot for revenue and budget
   ggplot(data = comp, aes(x = Year, y = value, color = variable)) +   geom_line(size = 1.5) + 
     xlab("Year") + ylab("Revenue vs Budget") + 
     theme(axis.text.x=element_text(angle=45, hjust=1))+
     ggtitle(paste("Revenue vs Budget for production house:", input$Prod)) + 
     theme(plot.title = element_text(size = 15, face = "bold"))
 })
  
}

# rendering the application
shinyApp(ui = ui, server = server)
