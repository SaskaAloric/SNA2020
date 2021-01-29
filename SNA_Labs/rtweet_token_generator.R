library(rtweet)
library(httpuv)

tweets <- search_tweets(q = "#rtweet", include_rts = FALSE)

setwd("SNA_Labs")
saveRDS(get_token(),  "rtweet_token.rds")
