library(tidyverse)
library(rvest)

get_html2 <- function(url){
  Sys.sleep(2)
  
  page <- read_html(url) #read the data
  
  prices_ten_thousands <- page %>% #extract the price in 10,000 jpy
    html_elements(".detail-box") %>%
    html_elements(".detail-inner") %>%
    html_elements(".num") %>%
    html_text2() %>%
    as.numeric()
  prices <- prices_ten_thousands * 10000 #transform to jpy
  
  layouts <- page %>% #get info to structure and size (they are sitting in the same box)
    html_elements(".detail-box") %>% 
    html_elements(".detail-inner") %>%
    html_elements(".layout") %>% 
    html_text2() %>%
    str_split_fixed("\n", n = 2) #split them into columns of structure and size
  sizes_sq_m <- layouts[,2] %>% str_remove_all("㎡") %>% as.numeric() #only taking the size column
  
  text <- page %>% #get address and bunch of extra information
    html_elements(".summary-txtArea") %>% #class = main-txt
    html_elements(".l-table") %>% 
    html_elements("tr") %>%
    html_elements("td") %>%
    html_text2()
  
  address <- text[1 + 5 * 0:99] %>% #get addresses
    str_remove_all("\\n\\n周辺地図")
  
  location <- str_extract(address, "都(.*?)区") %>% #get location
    str_replace_all("都", "")
  
  building_names <- page %>% #extract the building name
    html_elements(".js-detailLinkUrl") %>%
    html_text2() %>% 
    str_remove_all("\r |\r")
  
  tibble(location, address, building_name = building_names, price_JPY = prices, sizes_sq_m)
}

i = 1:239
tokyo_all <- paste0("https://www.able.co.jp/list/?prefkey=tokyo&cf=0&ct=0&sf=0&st=0&b=1&b=2&b=3&k=1&jks=0&n=B&g=13101&g=13102&g=13103&g=13104&g=13105&g=13106&g=13107&g=13108&g=13109&g=13110&g=13111&g=13112&g=13113&g=13114&g=13115&g=13116&g=13117&g=13118&g=13119&g=13120&g=13121&g=13122&g=13123&o=&p=10&i=", i)
all_data <- map_df(tokyo_all, get_html2)
filename = paste0("tokyo_rent_", Sys.Date(), ".csv")
write_csv(all_data, filename)
