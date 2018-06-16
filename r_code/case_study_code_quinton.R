#setwd("~/Documents/@Grad School/@SMU/@Term 1 Courses/Doing Data Science/Week5/CaseStudy_2_2_2")

library(tidyverse)

# read in data
beers <- read.csv("Beers.csv", stringsAsFactors = FALSE)
breweries <- read.csv("Breweries.csv", stringsAsFactors = FALSE)

# calculate breweries by state
breweries_by_state <- breweries %>% count(State) %>% arrange(desc(n))
colnames(breweries_by_state) <- c("state", "brewery_count")

# merge beers and breweries
beers_by_brewery <- merge(beers, breweries, by.x = "Brewery_id", by.y = "Brew_ID")
colnames(beers_by_brewery) <- c("brewery_id", "beer_name", "beer_id", "abv",
                                "ibu", "stye", "ounces", "brewery_name", 
                                "brewery_city", "brewery_state")
head(beers_by_brewery, 6)
tail(beers_by_brewery, 6)

# count NAs
count_of_brewery_id_nas <- sum(is.na(beers_by_brewery$brewery_id))
count_of_beer_name_nas <- sum(is.na(beers_by_brewery$beer_name))
count_of_beer_id_nas <- sum(is.na(beers_by_brewery$beer_id))
count_of_abv_nas <- sum(is.na(beers_by_brewery$abv))
count_of_ibu_nas <- sum(is.na(beers_by_brewery$ibu))
count_of_stye_nas <- sum(is.na(beers_by_brewery$stye))
count_of_ounces_nas <- sum(is.na(beers_by_brewery$ounces))
count_of_brewery_name_nas <- sum(is.na(beers_by_brewery$brewery_name))
count_of_brewery_city_nas <- sum(is.na(beers_by_brewery$brewery_city))
count_of_brewery_state_nas <- sum(is.na(beers_by_brewery$brewery_state))

# calculate median abv and ibu by state and make barplots
median_abv_by_state <- beers_by_brewery %>% 
  group_by(brewery_state) %>% summarise(median_abv = median(abv, na.rm = TRUE)) 

median_ibu_by_state <- beers_by_brewery %>% 
  group_by(brewery_state) %>% summarise(median_ibu = median(ibu, na.rm = TRUE))

barplot(median_abv_by_state$median_abv, horiz = TRUE, cex.names=0.2, 
        names.arg = median_abv_by_state$brewery_state, xlim = c(0, .075))

barplot(median_ibu_by_state$median_ibu, horiz = TRUE, cex.names=0.2, 
        names.arg = median_ibu_by_state$brewery_state, xlim = c(0, 70))

# find which states have the max abv and ibu
max_abv <- max(beers_by_brewery$abv, na.rm = TRUE)
max_abv_beer <- subset(beers_by_brewery, beers_by_brewery$abv == max_abv)
max_abv_state <- max_abv_beer$brewery_state

max_ibu <- max(beers_by_brewery$ibu, na.rm = TRUE)
max_ibu_beer <- subset(beers_by_brewery, beers_by_brewery$ibu == max_ibu)
max_ibu_state <- max_ibu_beer$brewery_state

# summary of abv column
abv_summary <- summary(beers_by_brewery$abv)
       
# bitterness vs. abv plot
plot(beers_by_brewery$ibu, beers_by_brewery$abv)

