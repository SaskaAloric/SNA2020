###############################################################################
# This R script is partially based on the 'LAB 3: Clusters, Factions and Cores' 
# script from the Social Network Analysis course at Stanford  University
# (available at: https://sna.stanford.edu/lab.php?l=3)
###############################################################################


#######################################################
# 
# Lab 4 
#
# The purpose of this lab is to introduce different 
# community detection (clustering) methods that allow 
# for identifying communities (groups, clusters) in a 
# social network.
#
######################################################


###
# 1. SET UP A SESSION
###

#
# 1.1 Load the required R packages
#

# For this lab, in addition to igraph, we'll use four new R packages:
# - cluster: a package for general purpose cluster-analysis with  
#   applications beyond the social-network context
# - factoextra: a package for seamless visualisation of clustering results
#   (and many other things)
# - proxy: we'll use it to compute cosine similarity - a similarity measure 
#   required in the clustering process
# - animation: used, not surprisingly, for producing animations
# - NetData: a package with all datasets used in the Stanford's SNA R labs
#   To see what datasets are available in the package, check:
#   https://cran.r-project.org/web/packages/NetData/NetData.pdf

# Reminder: installation of packages is to be done just once; 
# loading of packages has to be done in every session they are to be used
# install.packages(c('cluster', 'proxy', 'NetData', 'factoextra'))
# install.packages("animation", 
#                  repos = "http://cran.cnr.berkeley.edu/",
#                  dependencies = TRUE)

library(igraph)
library(cluster)
library(proxy)
library(animation)
library(NetData)

#
# 1.2 Set the seed to be used in random processes
#
seed <- 612


###
# 2. LOADING the DATA and CREATING GRAPHS
###

# We'll use a dataset from the NetData package:
data(studentnets.M182, package = "NetData")

# Out of the 4 loaded data frames (df), we will use 3 dfs 
# with data about 3 student networks:
# - friend_df: self-reported friendship ties
#   (2 = best friend, 1 = friend, 0 = not friend)
# - social_df: observed social interactions
# - task_df: observed task interactions
# Note that all 3 networks represent the same 
# group of students, related via 3 different kinds 
# of relationships

str(friend_df)
table(friend_df$friend_tie)

# All three dfs have the same structure:
# - the first column is the ego, 
# - the second column is the alter, 
# - the third column is an integer or a floating-point number
#   greater than or equal to zero representing the strength 
#   of the association for the given two vertices (ego, alter).
#
# There is also the 4th df - m182_full_data_frame - that
# integrates data for the 3 abovementioned networks. 
str(m182_full_data_frame)
# As we won't need it for this lab, we'll remove it:
remove(m182_full_data_frame)

# Create graphs for the 3 different types of ties

View(head(friend_df, 20))
# Notice many zeros (at least half tie values are zero); 
# so, we'll first select only those rows of the df 
# where ego and alter are connected through friend tie.
# If we do not do this, we would get a graph where all node 
# pairs would be connected and we would have to rely on the 
# connection (edge) weight to determine if a connection exists
# or not (0 - not connected, >0 - connected)
friend_net_nonzero_edges <- subset(friend_df, friend_tie > 0)
# Now, create a network
friend_net <- graph_from_data_frame(friend_net_nonzero_edges)
summary(friend_net)
V(friend_net)$name
# Note that 2 nodes are missing (4 and 16) - that is because
# those students didn't report friendship tie with any other student


# Do the same for the other two dfs
View(head(social_df, 20))
# Note: the weight in this network is the estimated number of 
# social interactions per hour.
social_net <- graph_from_data_frame(subset(social_df, social_tie > 0))
summary(social_net)
V(social_net)$name # all students present

View(head(task_df, 20))
# Note: the weight in this network is the estimated number of 
# task-related interactions per hour.
task_net <- graph_from_data_frame(subset(task_df, task_tie > 0))
summary(task_net)
V(task_net)$name # all students present


# To get an idea of what the three student networks look like, 
# let's create a function that will allow us to quickly plot graphs.
# The edge_weights argument will allow us to plot edge thickness 
# proportional to the intensity of the ties between actor pairs
plot_graph <- function(g, connection_type, edge_weights) {
  set.seed(seed)
  plot(g, 
       layout=layout_with_kk(g),
       vertex.color='gold', 
       vertex.frame.color='steelblue4',
       edge.arrow.size=.3,
       edge.width=edge_weights,
       edge.color='steelblue3', 
       main=paste(connection_type, "network"))
}

# Use the function to plot the friendship network
friend_tie_weights <- E(friend_net)$friend_tie * 1.35
plot_graph(friend_net, 'Friendship', friend_tie_weights)

# Now, plot the social interactions network
plot_graph(social_net, 'Social interactions', 
           E(social_net)$social_tie * 0.5)

# And, the task interactions network
plot_graph(task_net, 'Task interactions', 
           E(task_net)$task_tie * 0.75)

# If curious about the weights of the thick edges, you can inspect
# those weights, for example, by viewing adjacency matrix
as_adjacency_matrix(task_net, attr = "task_tie")


###
# 3. COMMUNITY DETECTION
###

# We'll use the friend network as the basis for our exploration of 
# community detection methods. For clarity and simplicity, we'll 
# set the network to undirected and assure it is connected. 
# Note: a large portion of the current community detection algorithms
# work only with undirected graphs.

friend_net_und <- as.undirected(friend_net, 
                                 mode='collapse',
                                 edge.attr.comb="max")
summary(friend_net_und)
# Note that the number of edges has decreased (62->42) as reciprocated 
# directed ties were consolidated into single undirected ties. We can check
# also tie weights:
table(E(friend_net_und)$friend_tie)

plot_graph(friend_net_und, 
           'Friendship (undirected)', 
           E(friend_net_und)$friend_tie)

# Check if it is connected 
is_connected(friend_net_und)
# it is connected


# There are many different ways to detect communities in a network. 
# A common characteristic of a large majority, if not all, of them
# is their reliance on a measure called Modularity to estimate the
# quality of the detected community structure.
# Modularity measures the strength of division of a network into 
# modules (clusters, communities). More precisely, it measures the
# density of links inside communities as compared to the density of 
# links between communities. It takes values between -1 and 1.
# Networks with high modularity have dense connections between the 
# nodes within modules (communities) but sparse connections between 
# nodes in different modules.

# Note: for more details about the modularity measure, listen / watch
# the explanation of modularity given in the (video) lecture: 
# Network Analysis. Lecture 8. Network communities:
# https://www.youtube.com/watch?v=lU1QEUH0nNc around 1h 5min 

# In this lab, we'll examine the following algorithms for 
# community detection: 
# - Edge-betweenness
# - Louvain (Multi-level optimization of modularity)
# - Walktrap
# - General clustering
# and compare their performance using the Modularity metric.

# For the sake of quantitative comparison of different community detection
# algorithms, we'll keep a list of modularity scores produced by the algorithms:
modularity_scores <- list()


###
# 3A. COMMUNITY DETECTION: THE EDGE BETWEENNESS METHOD 
###

# The edge-betweenness (EB) score of an edge measures the proportion of shortest 
# paths between any pair of vertices in the graph that go through that edge. 
# The EB community detection method is a hierarchical graph decomposition process 
# where edges are removed in the decreasing order of their EB scores.
# The method is motivated by the assumption that edges connecting different 
# communities are more likely to be part of multiple shortest paths, that is, have
# high EB score, simply because in many cases they are the only option to go 
# from one community to another. 

?cluster_edge_betweenness
friend_comm_eb <- cluster_edge_betweenness(friend_net_und, directed = FALSE)
friend_comm_eb
# we got 3 groups, with 5, 4, and 5 vertices

# We can plot the graph with color coded community membership to better
# estimate the partitioning
plot(friend_comm_eb,  friend_net_und,
     main="Communities detected in undirected friendship network")

# We can also identify edges that connect two different communities; these
# are also referred to as bridges.
# The crossing() f. returns a logical vector, with one value for each 
# edge, ordered according to the edge ids. The value is TRUE if the 
# edge connects two different communities
crossing(friend_comm_eb, friend_net_und)
cross_community_edges <- 
  which(crossing(friend_comm_eb, friend_net_und)==TRUE)
cross_community_edges


# We can establish a connection here with the concept of brokers that
# we discussed in the context of betweenness centrality: 
# brokers are nodes with high betweenness centrality that often 
# connect two or more communities.

# We can detect brokers by visualizing a network using:
# - node size to denote betweenness centrality, 
# - node color to mark community membership, and
# - edge color to mark cross community edges

edge_color <- rep('steelblue4', times=ecount(friend_net_und))
edge_color[cross_community_edges] <- 'firebrick4'

fnet_betweenness <- betweenness(friend_net_und, directed = FALSE)

fnet_copy <- friend_net_und
E(fnet_copy)$color <- edge_color
E(fnet_copy)$width <- 2
V(fnet_copy)$color <- membership(friend_comm_eb) # nodes are colored based on the community membership
V(fnet_copy)$size <- fnet_betweenness * 5
V(fnet_copy)$label.cex <- fnet_betweenness * 0.25
plot(fnet_copy, 
     layout=layout_nicely(fnet_copy),
     main="Brokers in undirected friendship network")

# We can observe, for example, that nodes 1 and 15 both have high betweenness centrality
# and are connecting the "green community" with the other two communities; thus, they 
# could be considered brokers in this network.

# If a network is large and visual representation does not allow for identifying brokers,
# the detection of brokers can be done computationally - in particular, we need to 
# identify nodes connected by bridging edges and sort those nodes based on their 
# betweenness value.
# First, find incident vertices of the graph bridges
?ends
bridge_ends <- ends(friend_net_und, cross_community_edges)
# Next, identify unique nodes at the bridge ends
candidate_brokers <- unique(c(bridge_ends[,1], bridge_ends[,2]))
# Compute betweenness for the identified nodes (broker candidates)
candidates_btwn <- betweenness(friend_net_und, v = candidate_brokers, directed = FALSE)
# Put together node ids and their betweenness in one df
candidate_brokers_df <- data.frame(node = candidate_brokers, 
                                   betweenness = candidates_btwn)
# sort the data frame based on the betweenness value
candidate_brokers_df[order(candidate_brokers_df$betweenness, decreasing = TRUE),]
# The obtained results correspond well to what we concluded based on the visual 
# inspection of the graph. 


# Note that all igraph's community detection functions return 
# a 'communities' object that contains many useful data about the 
# detected communities. So, it is worth checking the documentation
# for the communities object
?communities

# This community detection process creates a hierarchical 
# structure that can be visualized as a dendrogram.  
# Dendrogram is a tree-like structure or a hierarchical map that 
# is typically used for visual representation of clustering results
# when some form of hierarchical clustering is applied. In this case,
# leaves are vertices and the root of the tree represents 
# the whole graph; the tree structure indicates how the vertices are
# grouped into clusters (communities)
plot_dendrogram(friend_comm_eb)

# Modularity is used as the criterion for determining the number of 
# communities based on the dendrogram structure; namely modularity 
# scores of the partitions at each level of the dendrogram are 
# compared and the partition that results in the highest modularity 
# score is selected. 
# Let's get the modularity of the best partition and store it in 
# our modularity_scores list
modularity_scores$EB <- modularity(friend_comm_eb)


# Function animate_edge_betweenness() produces an animation of the EB
# community detection process applied to the given graph (g). 
# The function is defined in the 'SNA_custom_functions.R' script.
# To run it, you may need to install ImageMagick:
# http://www.imagemagick.org/script/binary-releases.php
# The result is a .gif file that will be saved under the name specified
# as the 2nd argument of the function.
setwd("SNA_Labs")
source('SNA_custom_functions.R')
animate_edge_betweenness(friend_net_und, 'friend_net_eb.gif')


###
# 3B. COMMUNITY DETECTION: THE LOUVAINE METHOD 
###

# The Louvaine community detection method is a hierarchical, bottom-up process, 
# based on modularity. It brief, it works as follows:
# - Initially, each vertex is assigned to a community on its own. 
# - In every step, vertices are re-assigned to communities as follows: 
#   - Each community is considered as a vertex on its own;
#   - Each vertex is moved to the community with which it achieves the highest 
#     contribution to modularity; 
#   - When no vertices can be reassigned, so that the reassignment further 
#     maximizes modularity, each community is considered a vertex, and the
#     process starts again with the merged communities (as new vertices). 
# - The process stops when there is only a single vertex left or when the 
#   modularity cannot be increased any more in a step.
# In a comprehensive comparative analysis of community detection algorithms 
# (Lancichinetti & Fortunato, 2009), this algorith proved to be the second 
# best algorithm (after InfoMap).

?cluster_louvain
friend_comm_louvain <- cluster_louvain(friend_net_und)
friend_comm_louvain
# now, we got two groups: one w/ 5, the other with 9 vertices

plot(friend_comm_louvain, friend_net_und)

# We can also plot the detected communities without shaded regions, using 
# node color to mark community membership and edge color to differentiate 
# edges within and between communities
cross_community_edges <- which(crossing(friend_comm_louvain, friend_net_und))
edge_colors <- rep('dodgerblue3', times=ecount(friend_net_und))
edge_colors[cross_community_edges] <- 'firebrick4'
plot(friend_net_und, 
     layout=layout_with_fr(friend_net_und),
     vertex.color=membership(friend_comm_louvain),
     edge.color = edge_colors,
     main="Communities detected in undirected friendship network\n using Louvain method")


# Retrieve the modularity score and add it to the modularity_scores list
modularity_scores$Louvain <- modularity(friend_comm_louvain)


# We can also examine the communities that would be obtained if edge weights are
# considered, that is, used by the Louvain algorithm
friend_comm_Louvain_w <- cluster_louvain(friend_net_und,
                                         weights = E(friend_net_und)$friend_tie)
friend_comm_Louvain_w
# with weights considered, we get 3 communities

# Let's plot it
cross_community_weighted <- crossing(friend_comm_Louvain_w, friend_net_und)
cross_community_weighted <- as.integer(cross_community_weighted)
edge_colors <- c('dodgerblue3', 'firebrick4')[cross_community_weighted + 1]

plot(friend_net_und, 
     layout = layout_with_fr(friend_net_und),
     vertex.color = membership(friend_comm_Louvain_w), 
     edge.color = edge_colors,
     edge.width = E(friend_net_und)$friend_tie * 1.5,
     main="Communities detected in undirected weighted friendship network\n using Louvain method")

# Add modularity of the weighted result the modularity_scores list
modularity_scores$Louvain_weighted <- modularity(friend_comm_Louvain_w)


###
# 3C. COMMUNITY DETECTION: WALKTRAP
###

# This algorithm detects communities in a bottom-up manner, through 
# a series of short random walks through the graph. 
# The idea is that the vertices encountered on any given random walk 
# are more likely to be within a community than not, since there are 
# typically only a few edges that lead outside a given community. 
# 
# In brief, the algorithm works as follows:
# Initially, it treats all nodes as communities of their own, then 
# it iteratively merges them into larger communities, and these into 
# still larger communities, and so on. In particular, in each iteration, 
# walktrap runs short random walks and uses the results of these random walks 
# to merge separate communities in a bottom-up manner. 

# The Walktrap algorithm requires from the user to specify the length 
# of random walks. Some researchers recommend walks of 4 or 5 steps, whereas
# others question the quality of thus obtained communities.
# A typical approach to estimating the quality of a particular graph 
# partitioning (i.e. a particular way of splitting the graph into communities) 
# is to compute the modularity score for the partitioning. The partitioning 
# that maximizes the modularity score is considered the best; such result is
# often referred to as the “maximum modularity partition”.

# Since the walktrap algorithm is based on random walks, we need to set 
# the seed to be able to replicate the results
set.seed(seed)
friend_comm_wt <- cluster_walktrap(friend_net_und, steps=4)
friend_comm_wt
# we got two groups: one w/ 5, the other w/ 9 vertices

# Plot the community structure
plot(friend_comm_wt, friend_net_und)

# It seems that we got the same result as with the Louvain algorithm.
# Let's verify that:
communities(friend_comm_louvain)
communities(friend_comm_wt)
# yes, they are equal

# Get the modularity score and add it to the modularity_scores list
modularity_scores$WT_4steps <- modularity(friend_comm_wt)


# We used 4 as the number of random steps, since that number of steps 
# was reported as giving good results in many examined cases (networks).
# If we want to more systematically determine the number of steps, we can
# choose a range of values for the number of steps, execute the algorithm
# for each value and eventually choose the one that results in the largest 
# modularity value.
wt_modularity <- list()
for (s in 2:8) {
  set.seed(seed)
  wt_result <- cluster_walktrap(friend_net_und, steps = s)
  wt_modularity[[paste0('s=',s)]] <- modularity(wt_result)
}
max_wt_modularity <- max(unlist(wt_modularity))
wt_modularity[wt_modularity == max_wt_modularity]
# We got the best score for 2 and 3 steps
# So, let's re-run the algorithm with, e.g., 3 steps
set.seed(seed)
friend_comm_wt_st3 <- cluster_walktrap(friend_net_und, steps=3)
friend_comm_wt_st3
plot(friend_comm_wt_st3, friend_net_und)
# Now, we have 3 communities, as we got with the edge betweenness algorithm.

# Add the modularity score for this solution to our modularity_scores list
modularity_scores$WT_3steps <- modularity(friend_comm_wt_st3)


# We can also apply the walktrap algorithm with edge weights included.
# To that end, we will repeat the above modularity-based process of finding 
# the optimal number of steps in the random walk. 
wt_weighted_modularity <- list()
for (s in 2:8) {
  set.seed(seed)
  wt_weighted_result <- cluster_walktrap(friend_net_und, steps = s, 
                                         weights = E(friend_net_und)$friend_tie)
  wt_weighted_modularity[[paste0('s=',s)]] <- modularity(wt_weighted_result)
}
max_mod_wt_weighted <- max(unlist(wt_weighted_modularity))
wt_weighted_modularity[wt_weighted_modularity == max_mod_wt_weighted]
# Again, we got the best score for 2 and 3 steps
# Let's re-run the algorithm again with 3 steps
set.seed(seed)
friend_comm_wt_weighted <- cluster_walktrap(friend_net_und, steps=3,
                                            weights = E(friend_net_und)$friend_tie)
friend_comm_wt_weighted

# Plot the result using a custom color scheme
weighted_fr_layout <- layout_with_fr(friend_net_und, 
                                     weights = E(friend_net_und)$friend_tie)

cross_community_edges <- crossing(friend_comm_wt_weighted, friend_net_und)
edge_colors <- c('midnightblue', 'firebrick4')[cross_community_edges + 1]

node_colors <- c('orangered2', 'limegreen', 'gold')[membership(friend_comm_wt_weighted)]

plot(friend_net_und,
     layout = weighted_fr_layout,
     vertex.color = node_colors,
     edge.width = E(friend_net_und)$friend_tie * 1.75,
     edge.color = edge_colors,
     main="Communities detected in undirected weighted friendship network\n using Walktrap method") 

# Add the modularity score for this solution to our modularity_scores list
modularity_scores$WT_weighted <- modularity(friend_comm_wt_weighted)


###
# 3D. AGGLOMERATIVE HIERARCHICAL CLUSTERING (UNDIRECTED, WEIGHTED GRAPH)
###

# We will now apply hierarchical clustering as a general clustering method, 
# that is, a method that is not specifically designed for community 
# detection in social networks but can be applied in various domains

# Step 1: get the adjacency matrix of the undirected friendship graph
friend_und_adj_mat <- as_adjacency_matrix(friend_net_und, 
                                         attr = 'friend_tie',
                                         sparse=FALSE)
friend_und_adj_mat

# Step 2: compute cosine similarity between rows of the adjacency matrix
# 
# Note: cosine similarity is a frequently used measure for estimating 
# similarity of numeric vectors. Other typically used similarity / distance
# measures include: Euclidean distance, Manhattan distance, and 
# Pearson correlation coefficient.

# To compute cosine similarity, we will use the simil() function
# from the *proxy* R package:
friend_und_cos_sim <- simil(friend_und_adj_mat, method = "cosine")
# set 1 on the diagonal as that is the maximum Cosine similarity value
friend_und_sim_mat <- as.matrix(friend_und_cos_sim, diag=1)
View(friend_und_sim_mat)

# We can use corrplot to get an idea about the grouping of nodes
# (based on the similarity of their network connections) 
library(corrplot)
corrplot(friend_und_sim_mat, is.corr = FALSE, type = 'upper', 
         diag = FALSE, order = 'hclust')

# Step 3: create distance matrix, required by the clustering function
friend_und_dist_mat <- 1-friend_und_sim_mat

# Step 4: do the clustering using the hclust function
?hclust
# The function offers different linkage methods, among which 'ward.D2' 
# and 'complete' linkage are often used
friend_und_hc <- hclust(as.dist(friend_und_dist_mat), 
                        method = "ward.D2")

# Step 5: plot dendrogram
library(factoextra)
fviz_dend(friend_und_hc)

# Step 6: Choose the best number of clusters by examining the structure 
# of the dendrogram and using modularity to choose between the candidate 
# solutions.
# 2 clusters:
modularity(friend_net_und, cutree(friend_und_hc, k = 2))
# 3 clusters:
modularity(friend_net_und, cutree(friend_und_hc, k = 3))

# Based on the modularity scores, 2 clusters is far better option.
# Color the clusters to better visualise them
fviz_dend(friend_und_hc, k=2)

# Step 7: get cluster assignments by cutting the dendrogram into 2 clusters
friend_und_clust <- cutree(friend_und_hc, k = 2)

# Plot graph with clusters
plot(friend_net_und, 
     vertex.color=c('gold', 'plum')[friend_und_clust], 
     layout = layout_with_fr(friend_net_und),
     edge.color = 'midnightblue',
     edge.width = E(friend_net_und)$friend_tie * 1.75,
     main="Communities detected in undirected friendship network\n using Agglomerative Hierarchical clustering")

# Add modularity to the modularity_scores list
modularity_scores$HC <- modularity(friend_net_und, friend_und_clust)


###
# 3E. AGGLOMERATIVE HIERARCHICAL CLUSTERING (DIRECTED, WEIGHTED GRAPH)
###

# We will now apply hierarchical clustering but using directed
# (and weighted) friendship graph

# Step 1: get the graph's adjacency matrix
friend_net_adj_mat = as_adjacency_matrix(friend_net, 
                                         sparse=FALSE, 
                                         attr = 'friend_tie')
friend_net_adj_mat


# Step 2: compute cosine similarity between nodes based on
# both incoming and outgoing connections
# Note: since simil() f. computes similarity between vectors 
# in rows of the given matrix, to compute:
# - similarity based on outgoing connections, we can use the matrix 
#   directly (as is) 
# - similarity based on incoming connections, we need to transpose 
#   the adj. matrix before using it as input for the simil f. 
friend_net_out_sim <- as.matrix(simil(friend_net_adj_mat, method = 'cosine'), diag=1)
friend_net_in_sim <- as.matrix(simil(t(friend_net_adj_mat), method = 'cosine'), diag=1)

# Combine the two similarity matrices by averaging them
# (other options - sum, prod, min, max - are also possible)
friend_net_sim_mat <- (friend_net_in_sim + friend_net_out_sim)/2

# Step 3: create distance matrix, required by the clustering function
friend_net_dist_mat <- 1 - friend_net_sim_mat

# Step 4: use the hclust f. to do the clustering
friend_net_hc <- hclust(as.dist(friend_net_dist_mat), method = "ward.D2")

# Step 5: plot the dendrogram
fviz_dend(friend_net_hc)

# Step 6: Choose the best number of clusters by examining the structure 
# of the dendrogram and modularity of the candidate solutions 
# Note: the modularity() function will disregard the direction of edges
# 2 clusters:
modularity(friend_net, cutree(friend_net_hc, k = 2))
# 3 clusters:
modularity(friend_net, cutree(friend_net_hc, k = 3))

# Based on the modularity scores, 3 clusters is the better option. 
# Use colors to visualise clusters
fviz_dend(friend_net_hc, k = 3)

# Step 7: get cluster assignments by cutting the dendrogram into 3 clusters
friend_net_clust = cutree(friend_net_hc, k = 3)

# Plot graph with clusters
plot(friend_net, 
     vertex.color=c('gold', 'steelblue2', 'plum')[friend_net_clust], 
     edge.width = E(friend_net)$friend_tie * 1.75,
     edge.color = 'midnightblue',
     edge.arrow.size = 0.3,
     main="Communities detected in directed weighted friendship network\n using Agglomerative Hierarchical clustering")

# Add modularity to the modularity_scores list
modularity_scores$HC_weighted <- modularity(friend_net, friend_net_clust)



###
# 3F. EDGE BETWEENNESS (EB) APPLIED TO DIRECTED GRAPHS 
###

# The EB community detection method is one of few that can be applied to directed graphs
# (though the implementation is far from perfect as you'll see in the warning messages).
# It also allows for the inclusion of edge weights. Here, it is important to note that
# "edges are interpreted as distances, not as connection strengths"
?cluster_edge_betweenness
friend_dir_eb <- cluster_edge_betweenness(friend_net, 
                                           directed = TRUE,
                                           weights = 1/E(friend_net)$friend_tie)
friend_dir_eb

cross_community_edges <- crossing(friend_dir_eb, friend_net)
edge_colors <- c('midnightblue', 'firebrick4')[cross_community_edges + 1]
plot(friend_net, 
     layout=layout_with_fr(friend_net),
     vertex.color=membership(friend_dir_eb),
     edge.color = edge_colors,
     edge.arrow.size = 0.3,
     main="Communities detected in directed weighted friendship network\n using EB method")

modularity_scores$EB_dir_wt <- modularity(friend_dir_eb)


###
# 3G. COMPARE THE PERFORMANCE OF THE USED COMMUNITY DETECTION ALGORITHMS 
###

# Let's examine the collected modularity scores:
modularity_scores

# Based on the modularity scores, for the undirected graph, the best solutions 
# are those generated by the Louvain and Walktrap algorithms
friend_comm_Louvain_w
friend_comm_wt_weighted
# For directed graph, the best result was obtained with the hierarchical clustering
friend_net_clust
# as all three algorithms lead to the same communities, we can assume that it is the
# the best community structure for the given (friendship) network


###
# TASK: 
# identify communities - using the above examined community detection algorithms - 
# in at least one of the other two networks: social_net and task_net.
###

