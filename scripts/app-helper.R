# code to support shiny app

##### 
# load libraries
library(maps)
library(leaflet)
library(dplyr)
library(shiny)

#####
# read in data
# coffeeshop data
ca <- read.csv("data/clean/peets-starbucks-ca.csv", stringsAsFactors = FALSE); ca$X <- NULL

# california cities coordinates
cities <- read.csv("data/ca-city-coords.csv")[,1:3]
colnames(cities) = c("name", "lat", "long")

#####
# combine the two datasets
lat = integer(nrow(ca)); long = lat
names(lat) = ca$city; names(long) = ca$city

# combine
for(city in ca$city){
  if (city %in% cities$name){
    index <- which(cities$name == city)
    lat[city] = cities[index,"lat"]
    long[city] = cities[index, "long"]
  }
  else{
    lat[city] = NA
    long[city] = NA
  }
}
ca[,"lat"] = lat; ca["long"] = long
colnames(ca)[6] <- "pop"

ca <- ca[ca$starbucks_count>0,]
ca <- ca[rowSums(is.na(ca))==0,]


#####
# color palettes

starbuckspal <- colorFactor(c("darkolivegreen1",
                              "darkgreen",
                              "black"),
                    domain = c("low", "med","high"))

#####
# bins 

make_bins <- function(shop_count){
  binned_count=cut(shop_count, 
                   breaks=(c(0,quantile(shop_count, c(0.33,0.66)),Inf)), 
                   labels=c("low", "med", "high"))
  return(binned_count)
}

##### 
# data for valuebox

df_valueBox <- data.frame(
  "city" = ca$city,
  "pop" = ca$pop,
  "starbucks_count" = ca$starbucks_count,
  "median_household_income" = ca$median_household_income,
  "median_age" = ca$median_age,
  "bin_income" = make_bins(ca$median_household_income),
  "bin_pop" = make_bins(ca$pop),
  "bin_star" = make_bins(ca$starbucks_count)
)

