library(rtweet)
library(httpuv)

tweets <- search_tweets(q = "#rtweet", include_rts = FALSE)
saveRDS(get_token(),  "rtweet_token.rds")
