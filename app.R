
library(shiny)
library(rvest)
library(shinythemes)

#Loops from 2017 to current year
for(Season in 2017:as.numeric(strsplit(as.character(Sys.Date()), "-")[[1]][1])){
  url <- paste('https://www.pro-football-reference.com/years/', Season,'/games.htm', sep = "")
  webpage<- read_html(url)
  
  ## Gets the spreadsheet, but in two parts
  fb_data_html_left <- html_nodes(webpage,'.left')
  
  ## Checks to see if website exists
  if(length(fb_data_html_left) > 0){
    fb_data_left <- html_text(fb_data_html_left)
    
    fb_data_html_right <- html_nodes(webpage,'.right')
    fb_data_right <- html_text(fb_data_html_right)
    
    ## Cleaning the mess
    ## Working with right
    Week <- 1:(length(fb_data_right)/9 - 12)
    for (i in 1:length(Week)) {
      Week[i] <- fb_data_right[1 + 9*(i-1)]
    }
    
    Time <- 1:(length(fb_data_right)/9 - 12)
    for (i in 1:length(Time)) {
      Time[i] <- fb_data_right[2 + 9*(i-1)]
    }
    
    At <- 1:(length(fb_data_right)/9 - 12)
    for (i in 1:length(At)) {
      At[i] <- fb_data_right[3 + 9*(i-1)]
    }
    
    At <- ifelse(At == "@", "@", "vs.")
    
    ## Working with left
    fb_data_left <- fb_data_left[fb_data_left != 'Day']
    
    Day <- 1:(length(fb_data_left)/4 - 12)
    for(i in 1:length(Day)) {
      Day[i] <- fb_data_left[1 + 4*(i-1)]
    }
    
    Date <- 1:(length(fb_data_left)/4 - 12)
    for(i in 1:length(Date)) {
      Date[i] <- fb_data_left[2 + 4*(i-1)]
    }
    
    Winner <- 1:(length(fb_data_left)/4 - 12)
    for(i in 1:length(Winner)) {
      Winner[i] <- fb_data_left[3 + 4*(i-1)]
    }
    
    Loser <- 1:(length(fb_data_left)/4 - 12)
    for(i in 1:length(Loser)) {
      Loser[i] <- fb_data_left[4 + 4*(i-1)]
    }
    
    #Saves all the pieces to a dummy data set
    football <- data.frame(Season, Week, Day, Date, Time, Winner, At, Loser)
    
    #If the master dataset doesn't exist, make one. If it does, add on to it.
    ifelse(!exists("football_data"), football_data <- football, football_data <- rbind(football_data, football))
  }
}



ui <- navbarPage("Tabs",
   # Theme
   theme = shinytheme("slate"),
   tabPanel("Results"),
   tabPanel("Select Teams",
           
   # Application title
   titlePanel("Football Pool"),
   
   # Sidebar 
   sidebarLayout(
      sidebarPanel(
         selectInput("season", h4("Select a Season"),
                    choices = list("2017", "2018")),
         selectInput("week", h4("Select a Week"),
                     choices = list("Week 1", "Week 2", "Week 3", "Week 4",
                                    "Week 5", "Week 6", "Week 7", "Week 8",
                                    "Week 9", "Week 10", "Week 11", "Week 12",
                                    "Week 13", "Week 14", "Week 15", "Week 16",
                                    "Week 17"))
      ),
      
      # Displays output in Main Panel
      mainPanel(
        h3(textOutput("selected_week")),
        uiOutput("submissions")
      )
   ))
)


server <- function(input, output) {
  
   output$selected_week <- renderText({
     paste("Pick the teams you believe will win during ", input$week, ".", sep = "")
   })
   
   output$submissions <- renderUI({
     user_season <- as.integer(input$season)
     user_week <- as.numeric(strsplit(input$week, " ")[[1]][2])
     
     conditionalPanel(
       condition = paste("input.week == '", input$week, "' & input.season == '",input$season,"'", sep = ""),
       
       textInput("name", "Enter Name"),
       lapply(which(football_data$Week == user_week & football_data$Season == user_season), function(i){
         selectInput(
           paste("gamewinner",i, sep = ""),
           label = h4(paste(football_data[i,6], football_data[i,7], football_data[i,8])),
           choices = list(paste(football_data[i,6]), paste(football_data[i,8]))
         )
       }),
       actionButton("submit", "Submit")
     )
   }) 
   
}

# Run the application 
shinyApp(ui = ui, server = server)

