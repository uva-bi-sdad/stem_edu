# library(RCurl)
# library(rvest)
# library(XML)
#
# github_df <- data.frame(NA, NA, NA, stringsAsFactors = FALSE)
#
# jekyll_members_url <- read_html("https://github.com/jekyll/jekyll/network/members")
# jekyll_members <- jekyll_members_url %>% html_nodes('.gravatar+ a') %>% html_text()
#
# l = 1
# for (a in 1:1)
# {
#   user_site <- paste0("https://github.com/", jekyll_members[a])
#   user_site <- read_html(user_site)
#   if(is.null(user_site %>% html_nodes('.css-truncate-target') %>% html_text()))
#   {
#     github_df[l, 1] <- NA
#   }
#   else((user_site %>% html_nodes('.css-truncate-target') %>% html_text()) != NULL)
#   {
#     github_df[l, 1] <- user_site %>% html_nodes('.css-truncate-target') %>% html_text()
#   }
#   #github_df[l, 1] <- user_site %>% html_nodes('.pr-3') %>% html_text()
#   #github_df[l, 2] <- user_site %>% html_nodes('.css-truncate-target') %>% html_text()
#   #github_df[l, 3] <- user_site %>% html_nodes('.css-truncate-target+ .css-truncate-target') %>% html_text()
#   l <- l + 1
# }

library(httr)
library(jsonlite)
library(httpuv)

oauth_endpoints("github")
hsb_app <- oauth_app(appname = "hannah_brinkley_summer_17", key = "c2cfaf2646b3042fdc7b", secret = "830f88aaa75c3c348d4a1036ed0eea2cf017b44b")

gt <- oauth2.0_token(oauth_endpoints("github"), hsb_app)
gt2 <- config(token = gt)

