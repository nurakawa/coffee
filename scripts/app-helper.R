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
# add in starbucks and peets counts

#starbucks_ca <- ca %>% filter(starbucks_count>0) 
#peets_ca <- ca %>% filter(peets_count>0)

#shop_data <- list("starbucks" = starbucks_ca,
#                  "peets" = peets_ca)


#####
# color palettes

starbuckspal <- colorFactor(c("darkolivegreen1",
                              "darkgreen",
                              "black"),
                    domain = c("low", "med","high"))

peetspal <- colorNumeric(c("tan3",
                           "tan4",
                           "chocolate3",
                           "chocolate4",
                           "black"),
                         domain = 1:max(ca$peets_count))

#####
# for the app
# shop_pallete <- list("starbucks" = starbuckspal,
#                      "peets" = peetspal)
# 
# shop_label <- list("starbucks" = shop_data$starbucks$starbucks_count,
#                    "peets" = shop_data$peets$peets_count)
#####
# radius bins 

make_bins <- function(shop_count){
  binned_count=cut(shop_count, 
                   breaks=(c(0,quantile(shop_count, c(0.33,0.66)),Inf)), 
                   labels=c("low", "med", "high"))
  return(binned_count)
}

  
make_bins(ca$starbucks_count)
#ca$peets_count <= 1

##### 
# data for pie chart
shops <- data.frame(
  "city" = ca$city,
  "shops" = ca$starbucks_count+ca$peets_count,
  "starbucks" = ca$starbucks_count,
  "peets" = ca$peets_count
)

# bubble <- data.frame(
#   "city" = ca$city,
#   "median_income_per_capita" = ca$median_household_income/ca$pop,
#   "median_age" = ca$median_age,
#   peets = ca$peets_count,
#   #"peets" = as.numeric(set_radius(ca$peets_count)),
#   "starbucks" = as.numeric(set_radius(ca$starbucks_count))
# )
# 
# 

bar <- data.frame(
  "city" = ca$city,
  "stbks_pc" = round(ca$starbucks_count/ca$pop,5),
  "rank" = rank(ca$starbucks_count/ca$pop)
  #"starbucks_p_ft" = ca$starbucks_count/ca$land_area
  #"med_age" = ca$median_age,
  #"med_income" = ca$median_household_income/ca$pop
)

bar# <- bar %>% arrange(stbks_pc) %>% tail(5)

plot(scale(ca$median_age),
     scale(ca$starbucks_count/ca$pop))

plot(make_bins(ca$median_household_income),
     make_bins(ca$starbucks_count/ca$pop))

plot(make_bins(ca$median_age),
     make_bins(ca$starbucks_count/ca$pop),
     xlab = "Median Age",
     ylab = "#Starbucks per capita")

plot(make_bins(ca$land_area),
     make_bins(ca$starbucks_count/ca$pop),
     xlab = "Median Household Income",
     ylab = "#Starbucks per capita")


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



