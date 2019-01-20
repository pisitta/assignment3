install.packages("RCurl")
library(tidyverse)
library(xml2)
library(RCurl)
base_url <- "https://www.cia.gov/library/publications/the-world-factbook/"


#' Question 1: Get Population Ranking
#'
#' @return
#' @export
#'
#' @examples
get_population_ranking <- function(){
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  url = str_c(base_url, "fields/335rank.html")
  #download url and execute all XPath queries which will each return a column for a data_frame
  
  #make the necessary adjustments to the data frame as given by the assignment
}


##Create dataframe with four elements
##Note: not sure why I couldn't install RCurl package hence could not use the function you've given. 
##so I'm using Rvest package instead, which also turns dataframe nicely

library(rvest)

page <- read_html("https://www.cia.gov/library/publications/the-world-factbook/fields/335rank.html")
population_rank <- page %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table()

population_rank <- population_rank[[1]]

##Renaming
colnames(population_rank)[1] <- "rank.population"
colnames(population_rank)[3] <- "population"

##Extract links
page %>%
  html_nodes(xpath = "//td/a") %>% 
  html_attr("href")


#' Question 2: Retrieve Land Area
#'
#' @param country_link A character vector of one or more country_link urls
#'
#' @return
#' @export
#'
#' @examples
get_land_area <- function(country_link){
  xpath <- str_c("//div[@id='","field-area","']/div[",2,"]/span[2]")
  #download the file from country_link and execute the xpath query
}


##Again I could not get your given function to work
##so this would be my best guess after some research

library(purrr)

countrylist <- read_html("https://www.cia.gov/library/publications/the-world-factbook/fields/335rank.html")
links <- countrylist %>%
  html_nodes(".region") %>%
  html_nodes("a") %>%
  html_attr("href") %>% 
  xml2::url_absolute("https://www.cia.gov/library/publications/the-world-factbook")

pages <- links %>% map(read_html)

land_area_attr <- pages %>%
  html_nodes(".category_data subfield numeric")
land_area <- do.call(rbind, lapply(land_area_attr, function(node) {
  label <- node %>%
    select_text(".subfield-name")
  data_value <- node %>%
    select_text(".subfield-number")
  data_extra <- node %>%
    select_text(".subfield-note") %>%
    replace_if_empty(NA)
  tibble(label = label, value = data_value, extra = data_extra)
}))


#' Question 3: Get Population Density
#'
#' @return
#' @export
#'
#' @examples
get_population_density <- function(){
  
}

##given that I couldn't get the given function to work, I'm at my wit's end at coming up with
##alternative options to complete the task for this one


#' Question 4: Get All Provided Rankings
#'
#' @return
#' @export
#'
#' @examples

##
get_rankings <- function(){
  url <- "https://www.cia.gov/library/publications/the-world-factbook/docs/rankorderguide.html"
  xpath <- c("characteristic" = "//div[@class='field_label']/strong/a",
             "characteristic_link" = "//div[@class='field_label']/strong/a/@href")
  #...
}

##for this question, I gave the xpath approach a try, and here's my attempt at it:

rankingpg <- "https://www.cia.gov/library/publications/the-world-factbook/docs/rankorderguide.html"

data_frame(
  characteristic = html_text(html_nodes(rankingpg, xpath="//div[@class='field_label']/strong/a")),
  characteristic_link = html_attr(html_nodes(rankingpg, xpath="//div[@class='field_label']/strong/a/@href"))
) -> rank_tab



#' Question 5 - Part 1: Get Ranking

##note: for this question it's not clear to me from the instruction 
##what I'm required to accomplish.

#'
#' @param url The url of the ranking
#' @param characteristic What this ranking is about
#'
#' @return
#' @export
#'
#' @examples
get_ranking <- function(url = "fields/335rank.html", characteristic = "population"){
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  #...
}

#' Question 5 - Part 2: Get Country Characteristic
#'
#' @param country_link 
#' @param xpath_field_id 
#' @param item 
#'
#' @return
#' @export
#'
#' @examples
get_country_characteristic <- function(country_link, xpath_field_id = "field-area", item = 2){
  #update the xpath and use similar code other than that
}



#' Question 6: Combine Rankings
#'
#' @param rankings Rankings from get_rankings (or a selection thereof)
#'
#' @return
#' @export
#'
#' @examples
combine_rankings <- function(rankings){
  
}

##For this question, I'm trying a slightly different approach than above
##I would first get all the links 
library(dplyr)

pg <- read_html("https://www.cia.gov/library/publications/the-world-factbook/docs/rankorderguide.html")

comr <- html_nodes(pg, "table.region reference > div > strong > a[href*='fields']")

country_name <- html_text(comr)

## then donwload all the links and return the table 
##although (I'm not entirely sure how to return the table properly in this case)

combined_ranking <- pbsapply(html_attr(comr , "href"), function(URL) {
  comr_pg <- read_html(URL)
  html_text(html_nodes(comr_pg, xpath="//*[@id="rankOrder"]/div/table[1]"))
}, html_table(fill = TRUE))


##As you might notice, I find this last assignment to be quite challenging 
##although I know I might have not mastered this webscraping task, 
##I did put a lot of effort looking at class materials, 
##as well as a lot of doing further research online to answer the questions.
##I'd love to know how my codes could be improve, and what the correct codes are.
##However if my performance here doesn't satisfy you, please let me know 
##if there're an extra assingment that I can do. While I've indeed learned a lot from your course
## and already implemented some of the lesson for my research, 
##I'd also hope that I can pass the course as well. 
## Thank you!