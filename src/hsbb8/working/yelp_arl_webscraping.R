# library(RCurl)
# library(XML)
# library(rvest)
#
# yelp_arl_df <- data.frame(stringsAsFactors = FALSE)
# colnames(yelp_arl_df) <- c("name", "time", "place", "type", "year")
#
# num = as.integer(28440)
# j = as.integer(1)
#
# for(k in 1:1)
# {
#   site <- paste("https://www.yelp.com/events/arlington-va-us/browse?end_date=20170613&sort_by=upcoming&start=", num, sep = "")
#   yelp_arl <- read_html(site)
#
#   names <- yelp_arl %>% html_nodes('.card_content-title--linked span') %>% html_text()
#   times <- yelp_arl %>% html_nodes('meta+ .u-space-b1') %>% html_text()
#   places <- yelp_arl %>% html_nodes('.u-space-b1+ .u-space-b1') %>% html_text()
#   types <- yelp_arl %>% html_nodes('.card_footer a') %>% html_text()
#
#   for(i in 1:length(yelp_arl %>% html_nodes('.card_content-title--linked span')))
#   {
#     if(grepl("Arlington", places[16 - i]))
#     {
#       yelp_arl_df[j, 1] <- names[16 - i]
#       yelp_arl_df[j, 2] <- times[16 - i]
#       yelp_arl_df[j, 3] <- places[16 - i]
#       yelp_arl_df[j, 4] <- types[16 - i]
#       yelp_arl_df[j, 5] <- as.integer(2007)
#       j <- j + 1
#
#     }
#   }
#   num <- num - 15
# }


