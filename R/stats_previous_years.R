#' Get Prior Year LPGA Stats Links
#'
#'This function gets the links for pages that have LPGA statistics for previous years.
#'Currently only gets previous year pages for statistics that are on the main page
#'of the statistics section of LPGA.com.
#'
#'@param year Numeric.  This is the previous year that you want to collect the statistics for.
#'@export

lpga_getlinks <- function(year = 2016){
  front_page <- read_html('http://www.lpga.com/statistics/')
  links <- front_page %>%
    rvest::html_nodes('a.button') %>%
    rvest::html_attr('href')

  # keep just the links that go to a statistics page
  statIndex <- grep("statistics", links)
  links <- links[statIndex]

  # add the front part of the URL
  links <- paste0("http://www.lpga.com", links)
  links16 <- paste0(links,"?year=",year)
  return(links16)

}


#' Scrape LPGA Stats Pages
#'
#' This function takes a list of website pages from LPGA.com for current year or prior years statistics
#' and pulls the data table. The list can be the output from the lpga_getlinks() function.
#'  A data frame is created from the table from each page  and added to a list of data frames.
#'  Output is a list of dataframes.
#'
#' @param links  A list of website URLs
#' @export

lpga_scrape <- function(links = NULL){
  listofdfs <- list()
  for(i in 1:length(links)){
    page <- read_html(links[i])
    table <- page %>%
      rvest::html_node('table.table') %>%
      rvest::html_table()
    listofdfs[[i]]<- table
  }
return(listofdfs)
}



#' Get LPGA Statistics Names
#'
#' This function is used by other functions in the package.  It extracts the name of the
#' statistic from the url for all the links gathered using the lpga_getlinks() function.
#' Could also be used for any list of statistics URLs from the LPGA.com site.
#'
#' @param links A list of website URLs
#' @export

stat_names<- function(links = NULL, year = NULL){
  stat_names<- stringr::str_extract(links, "[^/]+(?=/$|$)")
  stat_names <- gsub("-","_",stat_names)
  stat_names <- gsub(paste0("[?]year=",year),"", stat_names)
  return(stat_names)
}


#' Clean Rolex PoY Dataframe
#'
#' Select the correct table from the Rolex Player of the Year statistics site
#' on LPGA.com.
#'
#' @param year The year of the statistic pulled from the LPGA.com site.
#' @export

rolex_clean <- function(year = 2016){
  rolex_link <- paste0("http://www.lpga.com/statistics/points/rolex-player-of-the-year","?year=",year)
  rolex <- read_html(rolex_link)
  rol_table <- rolex %>%
    rvest::html_nodes('table.table') %>%
    rvest::html_table()
  rol_table<- rol_table[[2]]
  rolex_player_of_the_year <- rol_table
  return(rolex_player_of_the_year)
}


#' Clean LPGA Statistics Dataframes
#'
#' Cleans up the dataframes created from lpga_scrape() function.  Returns a list of dataframes.
#' Input dataframes must have specific names and variable names, created with the stat_names() function
#' in ladygolfstats.
#'
#' @export

clean_ladygolf <- function(){
  average_driving_distance <- dplyr::rename(average_driving_distance, Rank_Avg_Drive_Dist = Rank, Avg_Drive_Dist = "Average Driving Distance")
  birdies <- dplyr::rename(birdies, Rank_Birdies = Rank, Official_Rounds = "Official Rounds", Avg_Num_Birdies = Average)
  driving_accuracy <- dplyr::rename(driving_accuracy, Rank_Drive_Accuracy = Rank, Possible_Fairways = "Possible Fairways", Percent_Fairways = Percentage)
  eagles <- dplyr::rename(eagles, Rank_Eagles = Rank, Official_Rounds = "Official Rounds", Avg_Num_Eagles = Average)
  greens_in_reg <- dplyr::rename(greens_in_reg, Rank_Greens_in_Reg = Rank, Percent_Greens = Greens)
  holes_in_one <- dplyr::rename(holes_in_one, Rank_Holes_in_One = Rank)
  louise_suggs_rolex_rookie_of_the_year <- dplyr::rename(louise_suggs_rolex_rookie_of_the_year, Rank_RoY = Rank, RoY_Points = Points)
  official_money <- dplyr::rename(official_money, Rank_Money = Rank, Official_Money = "Official Money", Events = "Events Played")
  putting_average <- dplyr::rename(putting_average, Rank_Putting_Avg = Rank, Total_Putts = "Total Putts", Total_Rounds = "Total Rounds", Putts_Avg = "Putts Average")
  race_to_cme_globe_final <- dplyr::rename(race_to_cme_globe_final, Position_CME_Final = Position, CME_Reset_Points = "Reset Points",
                                    CME_Final_Points = "Final Points", Top_10s = "Top Tens")
  race_to_cme_globe_season <- dplyr::rename(race_to_cme_globe_season, CME_Rank_This_Week = "Rank THIS Week", CME_Rank_Last_Week = "Rank LAST Week",
                                     CME_Globe_Points = "Globe Points", Top_10s = "Top Tens", CME_Points_Behind = "Points Behind")
  scoring_average <- dplyr::rename(scoring_average, Rank_Scoring_Avg = Rank, Total_Strokes = "Total Strokes", Total_Rounds = "Total Rounds",
                            Avg_Strokes_Per_Round = Average)
  solheim_cup <- dplyr::rename(solheim_cup, Rank_Solheim_Cup = Rank, Solheim_Cup_Points = "Solheim Cup Points")
  top_10_finishes_percentage <- dplyr::rename(top_10_finishes_percentage, Rank_Top_10s = Rank, Top10_Finishes = "Top 10 Finishes",
                                       Events = "Events Played", Percent_Top10s = Percentage)
  rolex_player_of_the_year <- dplyr::rename(rolex_player_of_the_year, Rank_PoY = Rank, PoY_Points = Points)

  mylist <- list(average_driving_distance = average_driving_distance, birdies = birdies, driving_accuracy=driving_accuracy, eagles = eagles,
                 greens_in_reg=greens_in_reg, holes_in_one=holes_in_one,louise_suggs_rolex_rookie_of_the_year=louise_suggs_rolex_rookie_of_the_year,
                 official_money=official_money, putting_average=putting_average, race_to_cme_globe_final = race_to_cme_globe_final,
                 race_to_cme_globe_season=race_to_cme_globe_season, scoring_average = scoring_average, solheim_cup=solheim_cup,
                 top_10_finishes_percentage=top_10_finishes_percentage, rolex_player_of_the_year = rolex_player_of_the_year)
  return(mylist)
}


#' Create By Name Statistics Dataframe
#'
#' This function joins the dataframes created from the lpga_scrape() function into
#' one dataframe that provides statistics for the year by name of player.  It requires
#' that the dataframes be in the Environment and named based on the output of the stat_names()
#' function.
#'
#' @export

stats_join <- function(){
  average_driving_distance <- average_driving_distance[c(2,1,3)]
  stats_byname <- dplyr::full_join(average_driving_distance, birdies, by = "Name")
  stats_byname <- dplyr::full_join(stats_byname, driving_accuracy, by = "Name")
  stats_byname <- dplyr::full_join(stats_byname, eagles, by = "Name")
  stats_byname <- dplyr::full_join(stats_byname, greens_in_reg, by = "Name")
  stats_byname <- dplyr::full_join(stats_byname, holes_in_one, by = "Name")
  stats_byname <- dplyr::full_join(stats_byname, louise_suggs_rolex_rookie_of_the_year, by = "Name")
  stats_byname <- dplyr::full_join(stats_byname, official_money, by = "Name")
  stats_byname <- dplyr::full_join(stats_byname, putting_average, by = "Name")
  stats_byname <- dplyr::full_join(stats_byname, race_to_cme_globe_final, by = "Name")
  stats_byname <- dplyr::full_join(stats_byname, race_to_cme_globe_season, by = "Name")
  stats_byname <- dplyr::full_join(stats_byname, rolex_player_of_the_year, by = "Name")
  stats_byname <- dplyr::full_join(stats_byname, solheim_cup, by = "Name")
  stats_byname <- dplyr::full_join(stats_byname, top_10_finishes_percentage, by = "Name")
  return(stats_byname)
}


#' Clean Joined LPGA Stats
#'
#' This function cleans the dataframe created with the stats_join() function.
#'
#'@param df  Dataframe.  Created using the stats_join() function.
#'@export
#'
clean_joined_stats <- function(df = NULL){
  stats_byname <- df
  # Wins.y has a number for all 209 players so remove Wins.x
  stats_byname$Wins.x <- NULL
  stats_byname <- dplyr::rename(stats_byname, Wins = Wins.y)

  # Official_Rounds.x has a number for all 209 players so remeove Official_Rounds.y
  stats_byname$Official_Rounds.y <- NULL
  stats_byname <- dplyr::rename(stats_byname, Official_Rounds = Official_Rounds.x)

  # Top_10s.y has a number for all 209 players to remove Top_10s.x
  stats_byname$Top_10s.x <- NULL
  stats_byname <- dplyr::rename(stats_byname, Top10s = Top_10s.y)

  # Events.y.y has a number for all 209 players so remove other Events
  stats_byname$Events <- stats_byname$Events.x <- stats_byname$Events.x.x <- stats_byname$Events.y <- NULL
  stats_byname <- dplyr::rename(stats_byname, Events = Events.y.y)

  # Fix data types
  stats_byname$RoY_Points <- as.numeric(gsub("[,]","",stats_byname$RoY_Points))
  stats_byname$CME_Final_Points <- as.numeric(gsub("[,]","",stats_byname$CME_Final_Points))
  stats_byname$CME_Reset_Points <- as.numeric(gsub("[,]","",stats_byname$CME_Reset_Points))
  stats_byname$CME_Globe_Points <- as.numeric(gsub("[,]","",stats_byname$CME_Globe_Points))
  stats_byname$CME_Points_Behind <- as.numeric(gsub("[,]","",stats_byname$CME_Points_Behind))

  stats_byname$Percent_Top10s <- as.numeric(gsub(" [%]","",stats_byname$Percent_Top10s))
  stats_byname$Percent_Fairways <- as.numeric(gsub(" [%]","", stats_byname$Percent_Fairways))
  stats_byname$Percent_Greens <- as.numeric(gsub(" [%]","", stats_byname$Percent_Greens))
  stats_byname$Official_Money <- as.numeric(gsub('[$,]', '', stats_byname$Official_Money))

  return(stats_byname)
}







