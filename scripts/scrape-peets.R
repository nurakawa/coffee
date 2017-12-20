# load libraries
require(rvest)
require(dplyr)

# scrape peets stores data
url <- "https://www.peets.com/stores/peets-stores/stores-store-list.html"
download.file(url, destfile = "scrapedpage.html", quiet=TRUE)

peets_stores <- read_html("scrapedpage.html")

california_stores <- peets_stores %>% 
  html_nodes("#storeListCali .storeItem") %>%
  html_text

# remove non-califoria entries
is_cali <- function(x){
  sum(grepl("CA", strsplit(x, " ")[[1]])) == 1}

peets_california_stores <- california_stores[unlist(lapply(california_stores, is_cali))]

# remove everything except peets_california_stores
rm(list=setdiff(ls(), "peets_california_stores"))