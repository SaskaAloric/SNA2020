####################################################################
# This R script is partially based on the 'LAB 4: Centrality' from  
# the Social Network Analysis course at Stanford University
# (available at: https://sna.stanford.edu/lab.php?l=4)
####################################################################



##############################################################
# 
# LAB 2
#
# The objective of this lab is to introduce graph centrality 
# measures, to examine how they are interrelated, and to learn 
# how to interpret their meaning
#
##############################################################


### 
# 1. SET UP THE WORKSPACE
###

# Install and load the required libraries
# install.packages('igraph')
library(igraph)
# install.packages('ggplot2')
library(ggplot2)
# install.packages('viridis')
library(viridis)
# Note: a package providing access to professionally created
# colour palettes: https://github.com/sjmgarnier/viridis
# install.packages('tidyr')
library(tidyr)
# Note: tidyr is a new package; we will need it for some tasks 
# associated with manipulating data frames

# Load the data, that is, the networks we created in Lab 1

getwd()
# If current working directory is not the 'SNA_Labs' folder, 
# set it to SNA_Labs
setwd("SNA_Labs")

data_dir = "output/lab1/"
paste0(data_dir, "krack_advice.RData")

krack_advice <- readRDS(paste0(data_dir, "krack_advice.RData"))
summary(krack_advice)

krack_friendship <- readRDS(paste0(data_dir, "krack_friendship.RData"))
summary(krack_friendship)

krack_reports_to <- readRDS(paste0(data_dir, "krack_reports_to.RData"))
summary(krack_reports_to)


### 
# 2. NODE-LEVEL STATISTICS: CENTRALITY MEASURES
###

#
# 2.1 Degree centrality
#

# Compute indegree and outdegree for each node, 
# first in the advice network
?degree
advice_deg_in <- degree(krack_advice, mode="in") 
advice_deg_in
table(advice_deg_in)

advice_deg_out <- degree(krack_advice, mode="out") 
advice_deg_out
table(advice_deg_out)

# To better appreciate the computed values, we will visualise them

# First, create a data frame that integrates the computed 
# in and out degree measures for all the nodes
deg_advice_df <- data.frame(node_id=as.integer(V(krack_advice)$name), 
                            in_degree=advice_deg_in,
                            out_degree=advice_deg_out)
head(deg_advice_df)

# Then, transform the data frame from wide to long format suitable
# for plotting; to that end, we'll use the pivot_longer() f. from 
# the tidyr package
?pivot_longer
deg_advice_df_long <- pivot_longer(data = deg_advice_df, 
                                 cols = in_degree:out_degree,
                                 names_to = 'degree_type', 
                                 names_ptypes = list(degree_type = factor()),
                                 values_to = 'degree_value')
head(deg_advice_df_long)
tail(deg_advice_df_long)

# Finally, create a barplot plot 
ggplot(data = deg_advice_df_long, 
       mapping = aes(x=node_id, y=degree_value, fill=degree_type)) +
  geom_col(position = 'dodge') +
  labs(x = "Node ID", y = "In- and Out-Degrees") +
  scale_fill_viridis(name = "Degree type",
                     labels = c("In-degree", "Out-degree"),
                     discrete = TRUE) +
  scale_x_continuous(breaks = seq(0,21,1)) +
  theme_minimal()

# We can also make a plot (histogram) to examine degree distribution
max_degree <- max(deg_advice_df_long$degree_value)

ggplot(data = deg_advice_df_long, 
       mapping = aes(x = degree_value, fill = degree_type)) +
  geom_histogram(bins = 10, position = 'identity', alpha=0.85) +
  labs(x = "Degree value", title = "Degree distribution for the Advice network") +
  scale_fill_viridis(name = "Degree type",
                     labels = c("In-degree", "Out-degree"),
                     discrete = TRUE) +
  scale_x_continuous(breaks = seq(0,max_degree,1)) +
  scale_y_continuous(breaks = seq(0,6)) +
  theme_minimal()


# Another way to visualise and interpet the computed metrics is to plot a
# network as a graph and scale nodes' size and color based on the computed
# in- and out-degree values. 
# So, let's plot the advice network as a graph with in-degree determining the 
# node color and out-degree represented through the node size.

# To create a color palette based on the computed in-degree values, we will use 
# the attr_based_color_gradient() function from the SNA_custom_functions R script
source('SNA_custom_functions.R')
# The function creates a gradient color vector with as many color gradients as 
# there are different values in the given attribute 
viridis_pal_ends = c('#FDE725FF', '#440154FF')
in_degree_colors = attr_based_color_gradient(g_attr = deg_advice_df$in_degree, 
                                             pal_end_points = viridis_pal_ends)
in_degree_colors
# Note: 
# to select colors using the viridis and similar colour blind safe pallets, consult this post:
# https://www.thinkingondata.com/something-about-viridis-library/
# or use this R Colors cheatsheet:
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf

# Now, we can make a graph plot as we did in Lab 1
plot(krack_advice, 
     layout=layout_with_kk(krack_advice), 
     vertex.color = in_degree_colors, 
     vertex.size = deg_advice_df$out_degree * 1.5,
     # vertex.label = V(krack_advice)$LEVEL,
     vertex.label.cex	= deg_advice_df$out_degree * 0.15,
     edge.arrow.size = 0.3,
     main="Advice network",
     sub="Node color denotes in-degree (lighter color smaller value),\n size denotes out-degree")


#########
# TASK 1:
#
# Compute indegree and outdegree for the nodes in the other two networks 
# (friendship and reports-to) and visualise the computed values (as done above).
# Also, create visualizations for the degree distribution for those two networks. 
# Compare the degree distribution of the three networks: advice, friendship and 
# reports-to. Note down your observations.
#
#########


#
# 2.2 Closeness centrality 
#

# We'll start with an undirected network, as it is somewhat easier to deal with
# when it comes to the closeness centrality.
# Let's consider the friendship network and transform it into an undirected network.
# But before doing so, we'll add weight attribute (equal to 1) to each edge, so that
# after the transformation into undirected network we can differentiate between 
# reciprocal ties (their weight will be 2) and unidirected ties (weight will remain 1)
E(krack_friendship)$weight = 1
summary(krack_friendship)
krack_friendship_undirect <- as.undirected(krack_friendship, 
                                           mode = "collapse",
                                           edge.attr.comb = list(weight='sum'))
# Note that we are summing edge weights - see the documentation for attribute combination
# for more options

summary(krack_friendship_undirect)
table(E(krack_friendship_undirect)$weight)

# Before computing closeness, let's check if the network is connected.
# A network is connected if there is a path between any pair of nodes
# in the network.
is_connected(krack_friendship_undirect)

?closeness
closeness_friend_undirect <- closeness(krack_friendship_undirect)
summary(closeness_friend_undirect)

# Note: 
# whenever you want to compare a metric across networks (of different sizes),
# that metric should be normalised (by setting normalized = TRUE), to account
# for the difference in the network sizes.


# We can also include edge attributes (weights) in the calculation of closeness.
# It is important to note that edge "weights are used for calculating weighted 
# shortest paths, so they are interpreted as distances".
# In our case higher weights mean closer relations, that is, lower distance. 
# So, to appropriately calculate weighted closeness, it is 
# better to take reciprocal value of the weight attribute:
cl_weighted_friend_undirect <- closeness(krack_friendship_undirect, 
                                         weights = 1/E(krack_friendship_undirect)$weight)
summary(cl_weighted_friend_undirect)

# To better appreciate the computed metrics, let's visualise them.
# In particular, let's plot a graph with closeness determining the node color 
# and degree represented through the node size.
# First, create color palettes
closeness_colors = attr_based_color_gradient(g_attr = closeness_friend_undirect, 
                                             pal_end_points = viridis_pal_ends)
cl_weighted_colors = attr_based_color_gradient(g_attr = cl_weighted_friend_undirect, 
                                             pal_end_points = viridis_pal_ends)

# Now, make a plot
plot(krack_friendship_undirect, 
     layout=layout_nicely(krack_friendship_undirect), 
     vertex.color = closeness_colors, #cl_weighted_colors,
     vertex.size = 1.5 * degree(krack_friendship_undirect),
     vertex.label.cex	= 0.15 * degree(krack_friendship_undirect),
     main="Uniderected friendship network",
     sub="Node colour denotes closeness (lighter colour, lower closeness),\n size denotes degree")


# Now, we move (go back) to directed network and compute in-closeness and 
# out-closeness centrality.
# You can think of in-closeness centrality as the average number of steps 
# one would have to make to get TO a given node FROM all other reachable
# nodes in the network. Out-closeness centrality, not surprisingly, 
# measures the same thing with the directionality reversed: the average
# number of steps FROM the given node TO any other reachable node in the 
# network.

# First, check if the network is connected.
# Note: in directed networks, we need to differentiate between two modes
# of connectedness: 'weak' and 'strong'. The 'weak' form does not consider
# edge direction, whereas the 'strong' mode does consider the direction of
# edges 
is.connected(krack_friendship, mode='strong')
# not connected -> cannot compute closeness; so, we need to find the 
# giant component (= the largest connected component in the graph)
fr_components <- components(krack_friendship, mode = 'strong')
str(fr_components)
fr_components$membership
# Observe components (and their members) by plotting the graph 
plot(krack_friendship, 
     layout=layout_nicely(krack_friendship), 
     vertex.color = fr_components$membership,
     edge.arrow.size = 0.3,
     palette=viridis(3, direction = -1))

# two nodes outside the giant component; get ids of those nodes
not_in_gc <- which(fr_components$membership != 1)
# create the giant component by removing these two nodes
friendship_gc <- delete.vertices(krack_friendship, not_in_gc)
summary(friendship_gc)
is.connected(friendship_gc, mode='strong')
# Now that we have a connected friendship (sub)graph, we can compute in- and
# out-closeness
friendship_closeness = data.frame(node_id=as.integer(V(friendship_gc)$name),
                                  in_cl=closeness(friendship_gc, mode = 'in'),
                                  out_cl=closeness(friendship_gc, mode = 'out'))
str(friendship_closeness)

# Let's visualise these measures using node and label size to represent in-closeness,
# and node color to represent out-closeness
out_closeness_colors = attr_based_color_gradient(friendship_closeness$out_cl, viridis_pal_ends)
plot(friendship_gc, 
     layout=layout_nicely(friendship_gc), 
     vertex.color=out_closeness_colors, 
     vertex.size=friendship_closeness$in_cl*700, # in-closeness is multiplied by 700 since in-closeness values are very small 
     vertex.label.cex=friendship_closeness$in_cl*50, 
     edge.arrow.size=.20,
     main="Giant component of the Friendship network",
     sub="Node color denotes out-closeness (lighter colour, smaller values),\n size denotes in-closeness")

# It seems that those with high in-closeness have low out-closeness and vice versa;
# We will check that later by computing correlations of centrality measures.

#########
# TASK 2:  
# 
# Do the same kinds of computations and visualizations for the advice network
# and compare it to the results obtained for the friendship network. 
# Note down your observations.
#
#########


#
# 2.3 Betweenness centrality
#

# Betweenness centrality measures the number of shortest paths 
# beetween node pairs that go through a specific vertex.
?betweenness
summary(betweenness(krack_friendship))

# Compute betweeness for all the networks and store them in a data frame
krack_betweenness_df = data.frame(node_id=as.integer(V(krack_advice)$name),
                                  advice=betweenness(krack_advice),
                                  friendship=betweenness(krack_friendship),
                                  reports_to=betweenness(krack_reports_to))
krack_betweenness_df

# Identify the node with the highest betweenness in the advice network:
max(krack_betweenness_df$advice)
which(krack_betweenness_df$advice == max(krack_betweenness_df$advice))

# Identify nodes with the highest betweenness in each network:
apply(krack_betweenness_df[,-1], 2, function(x) which(x==max(x)))
# Interestingly, no overlap across the networks

# Let's visualise betwenees across the networks
# First, transform the df from wide to long format
krack_betweenness_long <- pivot_longer(data = krack_betweenness_df,
                                       cols = 2:4,
                                       names_to = "tie_type",
                                       names_ptypes = list(tie_type=factor()),
                                       values_to = "betweenness")
head(krack_betweenness_long)
# Then, make a bar plot showing for each actor their betweenness value in each 
# of the 3 networks
ggplot(data = krack_betweenness_long,
       mapping = aes(x = node_id, y = betweenness, fill = tie_type)) +
  geom_col(position = 'dodge') +
  labs(x = "Node ID", y = "Betweenness") +
  scale_fill_discrete(name = "Tie type") +
  scale_x_continuous(breaks = 1:21) +
  theme_bw() + 
  coord_flip()

# We can also examine the distribution of betwenness values across the 3 networks
ggplot(data = krack_betweenness_long,
       mapping = aes(x = tie_type, y = betweenness, fill = tie_type)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.65, alpha=0.9) +
  scale_fill_viridis(name="Tie type", discrete = TRUE, alpha=0.75) +
  labs(x = "\nNetwork", y = "Betweenness") +
  theme_minimal()


#########
# TASK 3:  
# 
# Visualize the three networks (advice, friendship, and reports-to) by
# ploting them as graphs where the node color represents betweenness 
# whereas the node size represents in-degree (try also in-closeness). 
#
#########


#
# 2.4 Eigen vector centrality
# 

# Eigen vector centrality gives higher scores to nodes the more they 
# are connected to other highly connected nodes. It reflects the notion
# that it is not important how many connections one has, but how important
# one's connections are. Hence, it is often interpreted as a measure of 
# a node's network importance.

?eigen_centrality

# Let's compute Eigenvector centrality for the friendship network
eigen_friend <- eigen_centrality(krack_friendship, directed = TRUE)
str(eigen_friend)
# Note that we are only interested in the $vector element of the object
# returned by the eigen_centrality() function
eigen_friend <- eigen_friend$vector
summary(eigen_friend)

# We can also compute weighted Eigenvector centrality.
# Note that in this case, weights are interpreted as the reflection of the 
# connection strength: "higher weights spread the centrality better"
# This is fully suitable for the friendship network.
# We will compute this measure on undirected friendship network since
# it is a weighted network (the directed one does not have weights).
w_eigen_friend <- eigen_centrality(krack_friendship_undirect, 
                                    weights = E(krack_friendship_undirect)$weight)
w_eigen_friend <- w_eigen_friend$vector
summary(w_eigen_friend)


# Plot the friendship graph with eigenvector centrality represented 
# through the nodes' color, and degree centrality through the nodes' size
eigen_col = attr_based_color_gradient(eigen_friend, viridis_pal_ends)

plot(krack_friendship, 
     layout=layout_nicely(krack_friendship), 
     vertex.color=eigen_col, 
     vertex.size=degree(krack_friendship, normalized = TRUE) * 25,
     vertex.label.cex=degree(krack_friendship, normalized = TRUE) * 2.5,
     edge.arrow.size=0.3,
     main="Friendship network",
    sub="Node colour denotes eigenvector centrality,\n size reflects degree")

# Note that actors with the highest number of connections (that is, highest degree)
# are not at the same time the most important according to Eigen vector centrality 
      


##########
# TASK 4:  
# 
# Do the same kinds of computations and visualizations for the other two networks 
# (advice and reports-to) and compare them to the results obtained for the 
# friendship network. Note down your observations.
#
##########


#
# 2.5 Summary of centrality metrics
#

# We'll use the friendship network as an example to compare and contrast all 
# examined centrality measures on one network.
# To that end, we'll first construct a data frame with the vertices as rows 
# and the centrality scores as columns:
friendship_centrality_all <- data.frame(node_id=as.integer(V(krack_friendship)$name),
                                        in_degree=degree(krack_friendship, mode = 'in'),
                                        out_degree=degree(krack_friendship, mode = 'out'),
                                        betweenness=krack_betweenness_df$friendship,
                                        eigen=eigen_friend)
View(friendship_centrality_all)
# Recall that to compute in- and out-closeness we had to remove a few vertices,
# so, we need to add these scores in a separate step, using the merge() function
# which allows for merging two data frames based on the given "key"
?merge
friendship_centrality_all <- merge(x = friendship_centrality_all, 
                                   y = friendship_closeness,
                                   # by = 'node_id',
                                   all.x = TRUE, all.y = TRUE)
str(friendship_centrality_all)
View(friendship_centrality_all)

# Now we can sort the data frame to find the most central actors 
# according to different centrality measures we have computed.

# Sort by betwenness
View(friendship_centrality_all[order(friendship_centrality_all$betweenness, decreasing = TRUE),])
# altrnative way (requires dplyr package):
friendship_centrality_all %>% arrange(desc(betweenness)) %>% View()
# Note that betweenness seems to be correlated with out-degree and out-closeness

# Sort by eigen
View(friendship_centrality_all[order(friendship_centrality_all$eigen, decreasing = TRUE),])
# altrnative way:
friendship_centrality_all %>% arrange(desc(eigen)) %>% View()
# Note that Eigenvector c. seems to be correlated with in-degree and in-closeness

##########
# TASK 5: 
#
# Try to interpret these results - how can we explain the observed correlations?
# Make the same computation and comparison for the advice network - do you observe
# the same relations between the metrics?
#
##########


###
# 3. CORRELATIONS BETWEEN CENTRALITY MEASURES
###

# Now we'll more thoroughly examine correlations between the centrality metrics 
# to determine how closely these measures are interrelated.To that end, we'll 
# use the data frame with centrality metrics for the friendship network.

# We need to compute pairwise correlations, that is, to generate a table
# with correlation values for each centrality measures pair.
# To determine how to compute these correlations, we need to check if the 
# assumption of normal distribution applies to our centrality measures:
apply(friendship_centrality_all[,-1], 2, shapiro.test)
apply(friendship_centrality_all[,-1], 2, qqnorm)
# Not all metrics are normally distributed (betweenness and out_degree). 
# So, better compute Spearman correlation coefficient
centrality_corr <- cor(friendship_centrality_all[,-1], 
                       use='complete.obs', # has to be set as we have a few NAs
                       method = 'spearman')
centrality_corr
# Not that easy to read and follow...
# We will use the corrpolot() function from the *corrplot* R package 
# to visually represent the computed correlations table
# install.packages('corrplot')
library(corrplot)
corrplot(corr = centrality_corr, 
         type = "upper", 
         diag = FALSE,
         addCoef.col = "black")

# Note:
# To examine the plotting options of the corrplot function
# check, for example, this page:
# https://rpubs.com/melike/corrplot


# Interpretation:
# In-degree and out-degree have low positive correlation (rho = 0.24),
# indicating that naming someone a friend is not that often reciprocated
# (e.g., naming many people as friends does not mean that the person will
# be recognised as a friend by many others)
#
# In-degree is highly positively correlated with in-closeness, eigenvector 
# centrality (EC), and betweenness. This suggests that those who have been
# named by many others as friends have good network position that is often
# associated with having power and influence in the network. 
#
# Out-degree is highly correlated with out-closeness, and to a lesser 
# extent with betweeness. This implies that those who have named many alters
# as friends have higher chances to act as intermediaries in the network
# (betweenness) and can reach many others through outbound edges.
#
# There is almost perfect positive correlation between in-closeness and
# eigenvector centrality. It suggests that those who are close to many others 
# by being named a friend are also those who have well connected friends 
# (eigenvector). Both metrics are also highly correlated with in-degree. 

##########
# TASK 6: 
#
# Do the same kind of analysis - computation of centrality measures 
# and interpretation of their correlations for the advice network.
#
##########
