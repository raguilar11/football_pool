#Loops from 2017 to current year
for(Season in 2016:as.numeric(strsplit(as.character(Sys.Date()), "-")[[1]][1])){
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
    
    #If the master dataset doesn't extist, make one. If it does, add on to it.
    ifelse(!exists("football_data"), football_data <- football, football_data <- rbind(football_data, football))
  }
}
