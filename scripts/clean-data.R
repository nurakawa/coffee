# clean-data.R: cleans and combines data for shiny app

#####
# read data

# dataset of california cities, towns and counties
# source: http://www.downloadexcelfiles.com/us_en/download-excel-file-list-cities-california-state#.WjGbJFWnHIU
ca_cities <- read.csv("../data/ca-cities.csv", stringsAsFactors = FALSE)

# starbucks data
# source: https://github.com/ali-ce/datasets/tree/master/Starbucks
starbucks <- read.csv("../data/ca-starbucks.csv",stringsAsFactors = FALSE)


#####
# extract city names from peets data, get peets count

source(scrape-peets.R) # gets peets data

for(i in 1:length(peets_california_stores)){
  peets_california_stores[i] = paste(strsplit(peets_california_stores[i], " ")[[1]][1:5], collapse=" ")
}

cities_vector <- ca_cities$Name
cities_vector <- cities_vector[cities_vector != ""] # remove blanks
cities_vector <- cities_vector[1:(length(cities_vector)-2)] # remove extra two lines from bottom

city_freq <- numeric(length(cities_vector)); names(city_freq) = cities_vector

for(i in 1:length(city_freq)){
  city_freq[i] = sum(grepl(cities_vector[i], peets_california_stores))
}

peets_count = city_freq

#####
# clean starbucks data and get count
# dataframe of city and frequency

starbucks_cities <- starbucks %>% select(City, Number.of.Starbucks)
starbucks_count <- numeric(length(cities_vector)); names(starbucks_count) = cities_vector

for(city in cities_vector){
  index <- which(starbucks_cities$City == city)
  if(length(index)==0){starbucks_count[city]=0}
  else{starbucks_count[city] = starbucks_cities$Number.of.Starbucks[index]}
}

#####
# create data frame

# function that aids in joining starbucks count to ca cities
get_city_info <- function(city, column){
  if (!(city %in% starbucks_cities$City)){return(NA)}
  else{return(starbucks[which(starbucks$City == city),column])}}

# dataframe for shiny app
california_coffee = data.frame(
  "city" = cities_vector,
  "starbucks_count" = starbucks_count,
  "peets_count" = peets_count,
  "median_age" = as.numeric(sapply(cities_vector, get_city_info, colnames(starbucks)[7])),
  "median_household_income" = sapply(cities_vector, get_city_info, colnames(starbucks)[8]),
  "2010_population" = as.numeric(sapply(cities_vector, get_city_info, colnames(starbucks)[9])),
  "p_white" = as.numeric(sapply(cities_vector, get_city_info, colnames(starbucks)[10])),
  "land_area" = as.numeric(sapply(cities_vector, get_city_info, colnames(starbucks)[11]))
)

#####
# cleaning column median_household_income

# make median household income type numeric
library(stringr)
rm_comma <- str_replace_all(california_coffee$median_household_income,",","")

for(i in 1:length(rm_comma)){
  rm_comma[i] = as.numeric(paste0(strsplit(rm_comma[i],
                                           "")[[1]][-1],collapse = ""))
}
rm_comma <- as.numeric(rm_comma)
california_coffee$median_household_income <- rm_comma
#####
# remove excess items from global environment
rm(list=setdiff(ls(), "california_coffee"))

# writing as a .csv
write.csv(california_coffee, file = "../data/clean/peets-starbucks-ca.csv")

