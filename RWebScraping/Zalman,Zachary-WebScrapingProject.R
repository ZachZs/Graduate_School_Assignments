
# Load in the packages
library(rvest)
library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
library(RSelenium)
library(purrr)

# create blank lists for all of the categories
director <- list()
imdb_score <- list()
metascore <- list()
title <- list()
rank <- list()
rating <- list()
runtime <- list()
year <- list()

# read the imdb top 500 list webpage
imdb_page <- read_html("https://www.imdb.com/list/ls062911411/")

# start the for loop for the number of pages
for (i in 1:5) {
  
  # NOTE: all of the categories use the purrr lmap function to check if the node is present. If the
  # node does not exist for that particular movie for that category, then the lsit is given an NA
  # value. This is to maintain consistent list lengths between categories
  
  # get the ranking of the movies, read the ranks as numeric values, then append to the main list
  movie_rank <- imdb_page %>%
    html_nodes("h3.lister-item-header") %>%
    lmap(~list(rank = html_node(.x, "span.lister-item-index") %>% 
                 html_text() %>% 
                 {if(length(.) == 0) NA else .})) %>% 
    as.numeric
  rank <- c(rank, movie_rank)
  
  # get the names of the movies, then append to the main list
  movie_names <- imdb_page %>%
    html_nodes("h3.lister-item-header") %>%
    lmap(~list(rank = html_node(.x, "a") %>% 
                 html_text() %>% 
                 {if(length(.) == 0) NA else .}))
  title <- c(title, movie_names)
  
  # get the movie year and strip the parentheses, and concert to a number (some movies have an 
  # (I) as part of the year, so I trimmed the whitespace and removd the I to get the actual 
  # year), then append to the main list
  movie_year <- imdb_page %>%
    html_nodes("h3.lister-item-header") %>%
    lmap(~list(rank = html_node(.x, "span.lister-item-year") %>% 
                 html_text() %>% 
                 {if(length(.) == 0) NA else .})) %>% 
    str_trim() %>% 
    str_remove_all("[()I]") %>%
    as.numeric
  year <- c(year, movie_year)
  
  # get the movie rating (this takes all the a nodes from the three different nodes, so after
  # pulling the data, i trimmed it to just the ratings by selecting every third value, starting 
  # with the 1st position), then append to the main list
  movie_rating <- imdb_page %>%
    html_nodes("p.text-muted.text-small") %>%
    lmap(~list(rank = html_node(.x, "span.certificate") %>% 
                 html_text() %>% 
                 {if(length(.) == 0) NA else .}))
  movie_rating <- movie_rating[seq(1, length(movie_rating), 3)]
  rating <- c(rating, movie_rating)
  
  # get the movie runtime, remove the whitespace and the min, then turn into a number, then 
  # append to the main list. (this takes all the a nodes from the three different nodes, so after
  # pulling the data, i trimmed it to just the runtime by selecting every third value, starting 
  # with the 1st position), then append to the main list
  movie_runtime <- imdb_page %>%
    html_nodes("p.text-muted.text-small") %>%
    lmap(~list(rank = html_node(.x, "span.runtime") %>% 
                 html_text() %>% 
                 {if(length(.) == 0) NA else .})) %>% 
    str_remove_all(" ") %>%
    str_remove_all("[min]") %>%
    as.numeric
  movie_runtime <- movie_runtime[seq(1, length(movie_runtime), 3)]
  runtime <- c(runtime, movie_runtime)
  
  # get the movie rating as a number, then append to the main list - there are multiple html 
  # tages of the same class, so I limited it by narrowing in on the imdb score widget, and 
  # selecting only the first node
  movie_imdb_score <- imdb_page %>%
    html_nodes("div.lister-item-content") %>%
    html_node("div.ipl-rating-widget") %>%
    lmap(~list(rank = html_node(.x, "span.ipl-rating-star__rating") %>% 
                 html_text() %>% 
                 {if(length(.) == 0) NA else .})) %>% 
    as.numeric
  imdb_score <- c(imdb_score, movie_imdb_score)
  
  # get the metascore score, then append to the main list
  movie_metascore_score <- imdb_page %>%
    html_nodes("div.lister-item-content") %>%
    html_node("div.inline-block.ratings-metascore") %>%
    lmap(~list(rank = html_node(.x, "span.metascore.favorable") %>% 
                 html_text() %>% 
                 {if(length(.) == 0) NA else .})) %>%
    as.numeric
  metascore <- c(metascore, movie_metascore_score)
  
  # select the directors (this takes all the a nodes from the three different nodes, so after
  # pulling the data, i trimmed it to just the directors by selecting every third value, starting 
  # with the 2nd position), then append to the main list
  movie_director <- imdb_page %>%
    html_nodes("div.lister-item-content") %>%
    html_nodes("p.text-muted.text-small") %>%
    lmap(~list(rank = html_node(.x, "a") %>% 
                 html_text() %>% 
                 {if(length(.) == 0) NA else .}))
  movie_director <- movie_director[seq(2, length(movie_director), 3)]
  director <- c(director, movie_director)
  
  # get the end of the url of the next page in the list, which is in the next button on the
  # bottom of the page
  new_page <- imdb_page %>%
    html_nodes("div.list-pagination") %>%
    html_node("a.flat-button.lister-page-next.next-page") %>% 
    html_attr("href")
  
  # join the first part of the list url with the new_page segment and trim the space between
  # the two strings
  imdb_page <-  str_remove_all(paste("https://imdb.com", new_page), " ")
  
  # read the new page with rvest
  imdb_page <- read_html(imdb_page)

}
                  
# turn the lists of categories into vectors
rank <- unlist(rank)
title <- unlist(title)
year <- unlist(year)
rating <- unlist(rating)
runtime <- unlist(runtime)
director <- unlist(director)
imdb_score <- unlist(imdb_score)
metascore <- unlist(metascore)

# combine all the vectors into a matrix
movie_matrix <- cbind(rank, title, year, rating, runtime, director, imdb_score, metascore)

# turn the matrix into a dataframe
imdb_df <- as.data.frame(movie_matrix)

# export the dataframe to a csv
# write.csv(imdb_df, file = "IMDBTop500.csv", row.names=FALSE)

# Change all of the columns in the df into their appropriate types
imdb_df$rank <- as.numeric(as.character(imdb_df$rank))
imdb_df$title <- as.character(imdb_df$title)
imdb_df$year <- as.numeric(as.character(imdb_df$year))
imdb_df$rating <- as.character(imdb_df$rating)
imdb_df$runtime <- as.numeric(as.character(imdb_df$runtime))
imdb_df$director <- as.character(imdb_df$director)
imdb_df$imdb_score <- as.numeric(as.character(imdb_df$imdb_score))
imdb_df$metascore <- as.numeric(as.character(imdb_df$metascore))

##################################################################################################
### VISUALIZATIONS  - GRAPHS                                                                   ###
##################################################################################################

# create a plot of the movies by year
theme_set(theme_minimal(base_size = 15))
p_1 <- ggplot(imdb_df, aes(x=year)) + 
  geom_histogram(color="black", fill="#f3ce13", stat='count') +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Number of Movies in the IMDB Top 500 by Year")
p_1

# get the average imdb and metascore scores per year into a dataframe
score_df <- imdb_df %>% 
  group_by(year) %>% 
  summarize(mean_imdb_score = mean(imdb_score, na.rm=TRUE), mean_metascore = mean(metascore, na.rm=TRUE),
            mean_runtime = mean(runtime, na.rm=TRUE))

# scale the imdb scores so they are out of 100 (to match metascore)
score_df['new_imdb_score'] = score_df['mean_imdb_score'] * 10

# plot the scores combined
theme_set(theme_minimal(base_size = 15))
p_2 <- ggplot(score_df, aes(x=year)) + 
  geom_line(aes(y = new_imdb_score, color='IMDB')) +
  geom_line(aes(y = mean_metascore, color='Metacritic')) +
  scale_colour_manual("", values = c("#f3ce13", "black")) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.justification = c(1, 0.8), legend.position = c(1, 1)) +
  ggtitle("Average Score per Year") +
  xlab("Year") +
  ylab("Score")
p_2

# plot just the IMDB score
theme_set(theme_minimal(base_size = 15))
p_3 <- ggplot(score_df, aes(x=year)) + 
  geom_line(aes(y = mean_imdb_score), color='#f3ce13') +
theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Average IMDB Score per Year") +
  xlab("Year") +
  ylab("Score")
p_3

# plot the average runime
theme_set(theme_minimal(base_size = 15))
p_4 <- ggplot(score_df, aes(x=year)) + 
  geom_line(aes(y = mean_runtime), color='#f3ce13') +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Average Runtime per Year") +
  xlab("Year") +
  ylab("Runtime")
p_4

##################################################################################################
### VISUALIZATIONS  - TABLES                                                                   ###
##################################################################################################

# the the most popular directors in the top 500
top_directors <- imdb_df %>% 
  group_by(director) %>% 
  summarise(number_of_movies = sum(n())) %>% 
  arrange(desc(number_of_movies))
top_directors <- head(top_directors,5)

top_directors

##################################################################################################
### SELENIUM                                                                                   ###
##################################################################################################

# make a list of the directors names from the top 5 list
directors_to_search <- top_directors$director

# initialize empty years active list
years_active <- c()

# Access the FireFox browser, and initialize it
driver <- rsDriver(browser = c("firefox"))
ff <- driver[["client"]]
ff$open()

# loop through the top five directors
for (i in 1:5) {
  
  # open wikipedia
  ff$navigate("https://en.wikipedia.org/wiki/Main_Page")
  
  # pause the webpage for 1 second
  Sys.sleep(1)
  
  # set up the error catch (tryCatch) with the variable for the years_active list -> the first {} will be the
  # regular code chunk -> this is to circumvent errors where selenium cant find an HTML element
  years_active <- tryCatch( {
    
    # find the search bar
    search_bar <- ff$findElement(using = 'name', value = 'search')
    
    # enter the first value from the directors list, and hit enter
    search_bar$sendKeysToElement(list(directors_to_search[i], key = "enter"))
    
    # pause the webpage for 1 second
    Sys.sleep(1)
    
    # find the html for the biography card
    output <- ff$findElement(using = "class", value="infobox")
    
    # scrape the biography card -> puts everything into a list of 1 element
    biography <- output$getElementText()
    
    # break up the single list item into sections on \n, turn it into a df, and rename the col
    biography_split <- unlist(strsplit(biography[[1]], "\n"))
    biography_split <- as.data.frame(biography_split, stringsAsFactors=FALSE)
    colnames(biography_split) <- "info"
    
    # get the years active information
    # use grepl to return T/F for the row containing the string, then subset based on rows that are true
    a <- grepl("Years active", biography_split$info)
    x <- biography_split[a == TRUE, ]
    
    # trim the years active statement
    x <- str_sub(x,14,-1)
    
    # append the years active statement to the list of years active
    years_active <- c(years_active, x)
    
  },
  
  # following the regular code, the following two {} will cover warnings and errors, and add "No Information"
  # to the the years active list
  warning=function(e){years_active <- c(years_active, "No Information")},
  
  error=function(e){years_active <- c(years_active, "No Information")}
  
  )
  # end of tryCatch
  
  
  # pause for 1 second
  Sys.sleep(1)
  
}

# add a years active col to the directors top 5 dataframe
top_directors$years_active <- years_active

# end the session
driver$server$stop()


top_directors
