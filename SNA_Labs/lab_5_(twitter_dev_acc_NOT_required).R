##############################################################
# 
# LAB 5
#
# The primary objective of this lab is to show how to use the
# *rtweet* R package to collect data about tweets and Twitter 
# users, and how to use the collected data to create graphs 
# (social networks) of Twitter users in igraph.
# The secondary objective is to demonstrate further the use 
# of graph visualization R packages such as *visNetwork*.
#
##############################################################


##
# 1. SETUP
##

# To be able to collect data from Twitter through the Twitter API
# (https://developer.twitter.com/en/docs/twitter-api/v1)
# you'll need to do the following three things:
# 1) Set up a Twitter account, if you don’t already have one
# (you do not need to be active on Twitter, just to have an account)
# 2) Install and load the following R packages: rtweet, httpuv, and tidyr
# 
# rtweets GitHub repository with lots of useful information on the front page:
# https://github.com/ropensci/rtweet

# Install and load rtweet, httpuv, and tidyr R packages
# install.packages('rtweet')
# install.packages('httpuv')
# install.packages('tidyr')
library(rtweet)
library(httpuv)
library(tidyr)

# We'll also install and load some additional 
# R packages that we'll need for this lab:
# - dplyr - for various data manipulation tasks (not specific to SNA)
# - visNetwork - for creating interactive graph visualizations
#
# install.packages('visNetwork')
# install.packages('dplyr')
library(dplyr)
library(visNetwork)
library(igraph)


###
# 2. SEARCH TWITTER AND COLLECT DATA FOR SETTING UP A NETWORK
###

?search_tweets
tweets <- search_tweets(q = "#ParisClimateAgreement OR #ParisClimateAccord", lang="en",
                        type = 'recent', n = 15000, include_rts = FALSE)

# Note: to more easily create complex queries, that is queries enabled by 
# Twitter advanced search (https://twitter.com/search-advanced),
# you can follow these guidelines:
# https://help.twitter.com/en/using-twitter/twitter-advanced-search

# (optionally) save the results
setwd("SNA_Labs")
saveRDS(tweets, 'data/ParisClimateAgreement_28-01-2021.RData')
# load the saved data (for offline work)
# tweets <- readRDS("data/ParisClimateAgreement_28-01-2021.RData")

# Get a glimpse into the obtained dataset
glimpse(tweets)

# For a detailed description of all the variables in the result set,
# see Twitter documentation for the Tweet object:
# https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/tweet 

# We will create "mention networks", that is networks with Twitter
# users as nodes and connections (edges) established based on one
# user mentioning another in his/her tweet. Mentioning can take the
# form of direct replies or 'simple' mentions anywhere in the tweet's text.
# So, we will have 2 networks: reply_to and mentioned networks. 

# So, we will select only those columns (variables) that are relevant 
# for creating "mention networks". We will also rename the selected 
# columns, for the sake of having shorter variable names
tweets_users <- tweets %>%
  select(screen_name, reply_to_screen_name, mentions_screen_name) %>%
  rename(sender=screen_name, replied_to=reply_to_screen_name, mentioned=mentions_screen_name)

glimpse(tweets_users) 
# Note that the mentioned column is a list; we will deal with it later.

# Note also that many entries have missing values (NA) for the 'replied_to' and 'mentioned'
# variables. Check how many such cases we have:
sum(is.na(tweets_users$replied_to))
sum(is.na(tweets_users$mentioned))
# Keep only those tweets where the sender mentioned or replied to at least 
# one other Twitter user
tweets_users <- tweets_users %>%
  filter( (is.na(replied_to)==FALSE) | (is.na(mentioned)==FALSE) )

nrow(tweets_users)
View(tweets_users[1:10,])

# Next, we will unnest the 'mentioned' column. 
# Unnesting is the transformation of a list-column, so that each 
# element of the list gets its own row.
?unnest
tweets_users <- unnest(tweets_users, mentioned) 
View(tweets_users[1:20,])

# To create graphs, we need to transform our tweets_users data frame
# into the format required by igraph's functions for graph construction.
# In particular, we will transform it into 2 edge lists: one for the  
# 'reply_to' relation, and the other for the 'mentioned' relation.

# First, create edge list for the 'reply_to' relation:
replied_to_edgelist <- tweets_users %>%
  select(-mentioned) %>%              # remove the 'mentioned' column
  filter(complete.cases(.)) %>%       # keep only complete rows, that is, rows without NAs
  group_by(sender, replied_to) %>%    # group the rows based on the sender, replied_to combination
  summarise(weight = n()) %>%         # compute the size of each group (i.e. number of connections 
  ungroup()                           # between each sender - replied_to pair) and assign it to the 
                                      # 'weight' variable
  

head(replied_to_edgelist, n=10)
# check the pairs with the most intensive communication: 
replied_to_edgelist %>%
  arrange(desc(weight)) %>%
  head(n=10)

# In a similar way, create an edge list based on 'mentioned' relation
mentioned_edgelist <- tweets_users %>%
  select(-replied_to) %>%
  filter(complete.cases(.)) %>%
  group_by(sender, mentioned) %>%
  summarise(weight = n()) %>%
  ungroup()

nrow(mentioned_edgelist)
head(mentioned_edgelist, n=10)
# examine most frequent mentions:
mentioned_edgelist %>%
  arrange(desc(weight)) %>%
  head(n=10)


# Get the unique users in each edge list.
# Those will be nodes in the corresponding networks.

# Unique users in replied_to edgelist
reply_to_unique <- union(replied_to_edgelist$sender, replied_to_edgelist$replied_to)
# the the number of unique users (nodes in the network) 
length(reply_to_unique)

# Unique users in mentioned edgelist
mention_unique <- union(mentioned_edgelist$sender, mentioned_edgelist$mentioned)
length(mention_unique)

# Considering the large number of unique users, that is, the number of
# nodes in the prospective networks, it is worth examining options for 
# filtering out some of them, in order to:
# - reduce the 'noise' in the data,
# - make the graphs easier to manipulate and analyse.

# To decide on how to do the filtering, we'll examine the frequency 
# of connections
summary(replied_to_edgelist$weight)
summary(mentioned_edgelist$weight)

# As there are too many users with loose connections 
# (mentioned / replied_to just once),
# keep only connections that are above the median
# First for the mentioned edgelist:
mentioned_edgelist_reduced <- mentioned_edgelist %>%
  filter(weight > median(mentioned_edgelist$weight))
# Note a huge reduction in the size of the edgelist
nrow(mentioned_edgelist_reduced)

# Check again the number of unique users 
mention_unique <- union(mentioned_edgelist_reduced$sender, 
                         mentioned_edgelist_reduced$mentioned)
# It is also significantly reduced
length(mention_unique)

# Now, for the reply_to edge list:
replied_to_edgelist_reduced <- replied_to_edgelist %>%
  filter(weight > median(replied_to_edgelist$weight))
# Again, a notable reduction
nrow(replied_to_edgelist_reduced)

# Check again the number of unique users 
reply_to_unique <- union(replied_to_edgelist_reduced$sender, 
                         replied_to_edgelist_reduced$replied_to)
# It is reduced but not as much as in the case of the 
# mentioned relation (as expected)
length(reply_to_unique)

# Next, we'll collect users' data. These data can be used to describe nodes 
# in networks, that is, to associate attributes to nodes.

# First, we will collect data about the senders, since these data are available 
# in the dataset we have already collected from Twitter (tweets data frame)
glimpse(tweets)
# Extract the data for all the senders, regardless of the type of relation
# they established with alters (mentioned, replied_to). 
all_senders <- union(mentioned_edgelist_reduced$sender, 
                     replied_to_edgelist_reduced$sender)
senders_data <- tweets %>%
  filter(screen_name %in% all_senders) %>%  
  users_data() %>%                          # rtweet's function that pulls user data from a dataset of tweets
  distinct(user_id, .keep_all = TRUE)       # keep just distinct users (distinguish users based on user_id) 

# check the kind of user data that is available
glimpse(senders_data)


# Now, collect data for alters (mentioned / replied_to) - these are not necessarily
# available in the collected tweets data, but have to be obtained separately.

# First, identify the alters (users) for whom the data are not available
no_data_alters <- setdiff(union(mentioned_edgelist_reduced$mentioned, 
                                replied_to_edgelist_reduced$replied_to), all_senders)
length(no_data_alters)
# Collect data for these users (no_data_alters) from Twitter. 
# To that end, we will use the lookup_users function from rtweet package
?lookup_users
alters_data <- lookup_users(no_data_alters)

# save the data
saveRDS(alters_data, "data/raw_alters_data_28-01-2021.RData")
# load the saved data (for offline work)
# alters_data <- readRDS("data/raw_alters_data_28-01-2021.RData")

glimpse(alters_data)
# In addition to user data, the lookup_users() f. also returned users' tweets.
# However, we need only user data:
alters_data <- users_data(alters_data)

# We should also check if we managed to retrieve data for all the users that 
# we were interested in (the service may not return all the requested data)
missing_alter <- setdiff(no_data_alters, alters_data$screen_name)
length(missing_alter)
# In case of missing user data, drop those users from the edge lists 
# before saving those edge lists to files (see below).

# Since we have done a lot of relevant processing steps, it would be wise to save 
# the created edge lists and user attributes, so that we do not have to repeat the
# processing steps.
# Before saving edge lists, rename columns to the typical names used in edge lists
mentioned_edgelist_reduced %>%
  filter(!mentioned %in% missing_alter) %>%
  rename(ego=sender, alter=mentioned, mention_tie=weight) %>%
  saveRDS(file = "data/mentions_edgelist_28-01-2020.RData")

replied_to_edgelist_reduced %>%
  filter(!replied_to %in% missing_alter) %>%
  rename(ego=sender, alter=replied_to, reply_to_tie=weight) %>%
  saveRDS(file = "data/replied_to_edgelist_28-01-2020.RData")

# Merge all available user data and store it in a file
senders_data %>%
  rbind(alters_data) %>%
  saveRDS("data/user_data_28-01-2020.RData")

# Do the clean up, that is, remove all objects from the environment, 
# we won't need them any more
remove(list = ls())


##
# 3. CREATE NETWORKS OF TWITTER USERS
##

# Load data (edge list) for creating a network based on the 'mentioned' relation
mention_edgelist <- readRDS("data/mentions_edgelist_20-01-2020.RData")
glimpse(mention_edgelist)

# Create a directed network
mention_net <- graph_from_data_frame(mention_edgelist)
summary(mention_net)
# Obviously very sparse network; let's check its density
edge_density(mention_net)

# We can try to plot the graph, but it is overly large 
# and the plot will be messy
plot(mention_net, 
     layout = layout_with_graphopt(mention_net),
     edge.arrow.size=0.3,
     vertex.size = 5,
     vertex.label = NA)
# We will get back to the plotting task a bit later.

# Let's add attributes to nodes, and use these to better understand the network.
# For example, we can include the number of followers, friends, and tweets.
# To that end, we will create a function that receives: 
# 1) Twitter user data as returned by the rtweet's users_data() function, 
# 2) a vector of screen names of those users we are interested in.
# The function returns a data frame with four variables (columns):
# screen_name, followers_count, friends_count, and statuses_count
get_user_attrs <- function(twitter_user_data, users_screen_names) {
  twitter_user_data %>%
    filter(screen_name %in% users_screen_names) %>%
    select(screen_name, followers_count, friends_count, statuses_count)
}

# Load user data; it will be used to add attributes to the nodes
user_data <- readRDS("data/user_data_28-01-2020.RData")
# Extract the set of attributes we are interested in 
node_attrs <- get_user_attrs(user_data, V(mention_net)$name)
# Sort based on the username, to match the order of vertex name attribute 
node_attrs <- arrange(node_attrs, screen_name)                          

head(node_attrs, n=10)
summary(node_attrs[,-1])

# Create graph with node attributes
mention_net <- graph_from_data_frame(mention_edgelist, 
                                     vertices = node_attrs)
summary(mention_net)


# Let's now make use of the vertex attributes to better understand the graph.

# We'll make the size of the nodes proportional to the number of followers and 
# use color to reflect the number of posted tweets (statuses) 
# Create gradient color vector based on the number of friends:
source('SNA_custom_functions.R')
posts_for_color <- attr_based_color_gradient(log1p(V(mention_net)$statuses_count), 
                                               c('grey100','midnightblue'))
# Create a vector for node sizes, based on the number of followers
followers_for_size <- log1p(V(mention_net)$followers_count)
# Note that we've used logged values of both the friend_count and followers_count 
# vectors due to the very uneven distribution of values of these two attributes 
# (plot the density functions of these attributes to see the distribution)

# Now, draw a plot
plot(mention_net, 
     layout = layout_with_dh(mention_net),
     edge.arrow.size=0.3, 
     vertex.label = NA,
     vertex.size = followers_for_size,
     vertex.color = posts_for_color,
     main = "Twitter-based mention network",
     sub= "Node color denotes the number of tweets, node size reflects the number of followers")

###
# TASK: Follow the above procedure to create and visualise a graph 
# based on the reply_to connection
###


# If the overall graph is overly large for meaningful visualization,
# we can take its giant component and create its visualization.

# To find the giant component, we start by identifying components (subgraphs) in the graph. 
# Due to the sparsity of edges, we won't be able to identify 'strong' components, 
# so, we will opt for 'weak' components (reminder: for 'strong' components, the 
# directionality of edges is considered; for detection of 'weak' components, the 
# direction of edges is disregarded)
m_net_comp <- components(mention_net, mode = 'weak')
str(m_net_comp)
# Identify the largest (gigantic) component:
giant_comp_size <- max(m_net_comp$csize)
giant_comp_index <- which(m_net_comp$csize == giant_comp_size)
# Get ids of nodes that belong to the gigantic component:
nodes_in_gc <- which(m_net_comp$membership==giant_comp_index)
# Next, extract the giant component from the mention_net graph. 
# To that end, we will use induced_subgraph function
?induced_subgraph
giant_comp <- induced_subgraph(mention_net, vids = nodes_in_gc)

summary(giant_comp)
is_connected(giant_comp, mode = 'weak')

# Now, plot the giant component using igraph's plotting features. 
# Use the same kind of mapping between nodes' attributes 
# (statuses_count and followers_count) and their visual representation 
# (color and size, respectively)
gc_colors <- attr_based_color_gradient(log1p(V(giant_comp)$statuses_count), 
                                       c('grey100','midnightblue'))
gc_size <- log1p(V(giant_comp)$followers_count)
set.seed(2501)
plot(giant_comp, 
     layout = layout_with_dh(giant_comp),
     edge.arrow.size=0.3, 
     vertex.label = NA,
     vertex.size = gc_size,
     vertex.color = gc_colors,
     main = "Giant component of the Twitter-based mention network",
     sub="Node color denotes the number of tweets, node size reflects the number of followers")

# Better than the previous graph, but still not sufficiently clear. 
# To try to get a better insight into the network, we will use interactive plots of the
# *visNetwork* R package.


##
# 4. VISUALISE NETWORKS OF TWITTER USERS USING VISNETWORK
##

# Note: for a tutorial on visNetwork and examples of use, see:
# - Introduction to visNetwork, at:
#   https://cran.r-project.org/web/packages/visNetwork/vignettes/Introduction-to-visNetwork.html 
# - visNetwork documentation:
#   http://datastorm-open.github.io/visNetwork/ 
# - Section 6.2 of the 'Network visualization with R' tutorial, available at:
#   http://kateto.net/network-visualization


# As a minimum for graph visualization, visNetwork requires:
# - nodes data frame, with 'id' column (plus any additional columns with node attributes)
# - edges data.frame, with 'from' and 'to' columns (plus any additional columns with edge attributes)

# Let's create those two (minimal) data frames:
# Note: if your network is overly large for a meaningful visualisation, visualise the
# gigantic component (giant_comp) instead
nodes_df <- data.frame(id=V(mention_net)$name, stringsAsFactors = FALSE)
head(nodes_df)
edges_df <- data.frame(as_edgelist(mention_net), stringsAsFactors = FALSE)
colnames(edges_df) <- c('from', 'to')
head(edges_df)
# Now, we can create the simplest visualisation with visNetwork
visNetwork(nodes = nodes_df, 
           edges = edges_df, 
           main="Twitter mention network")


# Extend the nodes data frame with columns defining node color and size
# Note: see visNodes for column naming rules
nodes_df$color <- posts_for_color
nodes_df$size <- followers_for_size * 2
head(nodes_df)

# Extend the edges data frame with columns defining edge width, color, 
# if it is curvy or not, and if / how arrows are displayed.
# Note: see visEdges for column naming and available options
edges_df$width <- 1 + (E(mention_net)$mention_tie / 2)
edges_df$color <- 'slategray3'
edges_df$smooth <- TRUE # should the edges be curved?
edges_df$arrows <- 'to'
head(edges_df)

visNetwork(nodes = nodes_df, 
           edges = edges_df, 
           main="Twitter mention network",
           footer = "Node color denotes the number of tweets, node size reflects the number of followers")


# Let's add a few additional details for node display:
nodes_df$title  <- nodes_df$id  # text to be displayed on mouse over 
nodes_df$borderWidth <- 1.5     # node border width

# Instead of one color for a node, we can specify different colors 
# for different parts of a node (main body and border) and different interaction states.
# First, remove the existing color attribute
nodes_df <- nodes_df %>% select(-color)
# Then, add new color-related attributes
nodes_df$color.background <- posts_for_color
nodes_df$color.border <- "black"
nodes_df$color.highlight.background <- "orange" # color of the main body of a node when clicked
nodes_df$color.highlight.border <- "darkred"    # color of the border when a node is clicked

# Run the visualization with the new parameters set
visnet <- visNetwork(nodes = nodes_df, 
                     edges = edges_df, 
                     main="Twitter-based mention network",
                     footer = "Node color denotes the number of tweets, 
                                node size reflects the number of followers")
visnet

# To see the various node attributes that can be used, check:
?visNodes

# Similarly, for the recognised edge-related attributes, see
?visEdges


# visNetwork offers a number of other options available through 
# the visOptions() function. For instance, we can highlight all 
# direct neighbors and those 2 hops away:
visnet2 <- visOptions(visnet, 
                      highlightNearest = list(enabled = TRUE, degree = 2))
visnet2

# explore other visOptions
?visOptions

# To examine the available layout options
?visLayout

visLayout(visnet, randomSeed = 2021, improvedLayout = TRUE)

###
# TASK: Follow the above procedure to create and visualise - using visNetwork - 
# giant component of the graph based on the reply_to connection
###
