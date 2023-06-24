library(dplyr) # data manipulation
library(purrr) # data manipulation
library(cluster) # Kmeans and Hclustering
library(scales) # to do min/max scaling
# plot
library(ggplot2) 
library(plotrix) # for polar chart
library(fmsb) # for polar chart
library(plotly) # for polar chart
library(factoextra) # fpor PCA
library(patchwork) # to display subplots
library(ggsoccer) # to draw pitch
library(ggthemes) # for color-blindness
library(GGally) # for pairplot
library(corrplot) # for correlagram
# utilities
library(progress) # for progress in for loops
library(doParallel) # for parallel
# process in parallel
cl = makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)

# USE PRELOADED EVENTS FROM EventsAndFeatures.RData file
#-------------------------------------------------------------------------------------------

# DATVIZ
## function to plot the pitch
### 100x100 pitch
pitch = function(){ggplot() +
    annotate_pitch(fill = "white", colour = "#494B49", alpha = 0.7) +
    theme_pitch() +
    theme(panel.background = element_rect(fill = "#148923"))
}

pitch()
### all the points
all_points_plot = pitch() +
               # scatter
               #geom_point(data = events, mapping = aes(x = start_x, y = start_y), colour = "blue" ,alpha = 0.1)
               # heatmap
               geom_bin2d(data = events, mapping = aes(x = start_x, y = start_y), alpha = 0.4) +
               scale_fill_gradient(low = "white", high = "red")
all_points_plot

#players$true_role = players$role$name # create the true_role column
### 100x100 pitch
### copFeatures
cop_plot = pitch() +
        # scatter
        geom_point(data = copFeatures, mapping = aes(x = mean_x, y = mean_y), colour = "blue" ,alpha = 0.7)
        # heatmap
        #geom_bin2d(data = copFeatures, mapping = aes(x = mean_x, y = mean_y)) +
        #scale_fill_gradient(low = "white", high = "red")
cop_plot

# CORRELATION
cor_table<-cor(data_matrix) 

corrplot(cor_table, type = "upper",     #first corr plot
         tl.col = "black", tl.srt = 45, col = colorRampPalette(c("blue", "white", "orange"))(100))
cor(copFeatures$mean_y, copFeatures$Rfoot) # 47% of correlation between mean_y and Rfoot that means that if you are right footed you probably play on the left because a greater y means upper in a horizzontal pitch
# cor(features_matrix[,"mean_y"], features_matrix[,"Rfoot"]) # same 47% even using rescaled data

# PAIRPLOT
ggpairs(copFeatures, columns = c(3,4,7:length(copFeatures))) # useless

# SCALED DATA MATRIX
## Combine mean_x, mean_y and features columns into a matrix
features_matrix = as.matrix(copFeatures[, !(names(copFeatures) %in% c("Min","playerId","shortName","fullName"))])#))]) # with categorical Rfoot (,"Rfoot")
## Scale data
features_matrix = scale(features_matrix) # now no Rfoot # we also scale the Rfoot dummy to enhance the distance between left and right footed (from 0-1 to 1.67-0.59)

# PCA
pca<-prcomp(features_matrix, scale = FALSE) # data already scaled
summary(pca)
# select how many components:
fviz_eig(pca, addlabels = TRUE, xlab = "Number of Principal Components") +
  ggtitle(label ="Scree Plot of PCA", subtitle = "The first two principal components explain the 52.2% of the variance.")
# individual plot
fviz_pca_ind(pca,
             col.ind = "blue",
             alpha.ind = 0.5,
             repel = TRUE,           # Avoid text overlapping
             addEllipses = FALSE,   # Disable observation labels
             geom = "point") +      # Use points for individuals
             ggtitle("Players along the two PC")

# rotations plot
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = FALSE,
             repel = TRUE) +    # Avoid text overlapping
             ggtitle("Loadings of the variables")
# biplot
fviz_pca_biplot(pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969", # Individuals color
                addEllipses = FALSE,
                geom.ind = "point",
                alpha = 0.6) +
                ggtitle("Biplot: Players and Variables along the two PC")

# K-MEANS
# leave a 8-means on mean_x and mean_ y just to start
xy_matrix = as.matrix(copFeatures[, c("mean_x","mean_y")])
fullMatrix = scale(as.matrix(copFeatures[, !(names(copFeatures) %in% c("playerId","shortName","mean_x","mean_y","fullName","Min", "Rfoot", "predictedRole"))]))
kmeans8 = kmeans(fullMatrix, centers = 8, iter.max = 100000)
kmeans5 = kmeans(fullMatrix, centers = 5, iter.max = 100000)

pitch() +
  geom_point(data = copFeatures, aes(x = mean_x, y = mean_y, color = as.factor(kmeans5$cluster))) +
  scale_color_colorblind(name = "Cluster") +
  ggtitle("5-means clustering", subtitle = "Using K = 5, silhouette score improves but not the interpretation") + 
  geom_text(mapping = aes(x = 20, y = 90), label = paste("Silhouette score: ", round(ss_list[5],3), cex = 4))


## Kmeans on PCA
wss = list()
ss_list = list()
k_guesses = c(1:20)
for (k in k_guesses){
  #set.seed(29) # to get 8 clusters
  #kmeans_tune = kmeans(pca$x[,1:2], centers = k, iter.max = 100000000)
  kmeans_tune = kmeans(fullMatrix, centers = k, iter.max = 100000000)
  wss = append(as.numeric(wss), as.numeric(kmeans_tune$tot.withinss))
  
  # Silhouette score
  clusterK_assignment = kmeans_tune$cluster
  ssK = as.data.frame(silhouette(clusterK_assignment, dist(pca$x[,1:2]))) # ssK as df
  ss_list = append(as.numeric(ss_list), as.numeric(mean(ssK$sil_width)))
}
# Silhouette
ss_plot = ggplot(mapping = aes(x = k_guesses, y = ss_list)) +
  geom_line() +
  geom_text(aes(x = 5, y = 0.396 + 0.009), label = round(ss_list[5],3)) +
  geom_point(size=2) +
  labs(x = "Number of Clusters (k)", y = "Silhouette score")
# The clustering project makes 13 roles

# WSS
screeplot = ggplot(mapping = aes(x = k_guesses, y = wss)) +
  geom_line() +
  geom_point(size=2) +
  labs(x = "Number of Clusters (k)", y = "Within-Cluster Sum of Squares (WCSS)")
(screeplot / ss_plot) # BEST K = 5

# 5-means on pca
set.seed(42)
PCA_5means = kmeans(pca$x[,1:2], centers = 5, iter.max = 100000)

# HYBRIDS PLAYERS
ss5 = silhouette(PCA_5means$cluster, dist(pca$x[,1:2]))[,"sil_width"]
mean(ss5) # 0.3962002 ss | # 0.39499 without Rfoot
paste("Percentage of hybrids players with delta = 0.1:", round(sum(ss5 < 0.1)/length(ss5)*100,2),"%") # 8.71% of the observations are hybrids
# function to map clusters to names
cluster2role = function(row, silhouette_threshold = 0.1, cluster_names = c("Letal finisher","Handyman","Steal-and-Build","Funambulist","Warrior")){
  if (row["silhouetteScore"] < silhouette_threshold){
    return("Hybrid")
  } else {
    cluster_index <- row["cluster"]
    return(cluster_names[cluster_index])
  }
}
clusters_5_df = data.frame(cluster = PCA_5means$cluster, silhouetteScore = ss5, role = NA)
clusters_5_df$role = as.character(apply(clusters_5_df, 1,
                                        function(row) cluster2role(row))) # as.char needed to convert it into a character vector

# x,y display of kmeans on pca
pitch() +
  geom_point(data = copFeatures, aes(x = mean_x, y = mean_y, color = as.factor(kmeans5$cluster))) +
  scale_color_colorblind(name = "Cluster") +
  ggtitle("5-means clustering", subtitle = "Using K = 5, silhouette score improves but not the interpretation") + 
  geom_text(mapping = aes(x = 20, y = 90), label = paste("Silhouette score: ", round(ss_list[5],3), cex = 4))

# biplot
# Your fviz_pca_biplot code
biplot <- fviz_pca_biplot(pca, repel = TRUE,
                          col.var = "#CBCBCB", # Variables color
                          col.ind = as.factor(clusters_5_df$role), # Individuals color
                          addEllipses = FALSE,
                          geom.ind = "point",
                          alpha = 0.6) +
  ggtitle("Biplot: Players and Variables along the two PC", subtitle = "Player colored by role (5-means Clustering)")

# Customize colors and shapes
playersIndex = c(612,59,50,288,61,1047) # Hazard (Funambolist), Piqué (Warrior), Ronaldo(Letal finisher), Fellaini(Handyman), Busquets (Steal-and-Build), Chiesa (Hybrid)
customized_biplot <- biplot +
  scale_color_manual(values = c("red", "blue", "#969696", "orange", "green", "purple")) +
  scale_shape_manual(values = c(16, 16, 17, 16, 16, 16)) +
  #geom_text(data = copFeatures, aes(x = pca$x[,1], y = pca$x[,2], label = shortName), size = 1.9, vjust = -1)
  geom_text(data = copFeatures[playersIndex,], aes(x = pca$x[playersIndex,1], y = pca$x[playersIndex,2], label = shortName), size = 3.5, vjust = -1)
customized_biplot
#--------------------------polar

# Add the role to the main dataset
copFeatures$predictedRole = clusters_5_df$role
# minmax scale the whole df
MINMAXscaled_copfeatures = as.data.frame(lapply(copFeatures[,7:32], scales::rescale)) # leave out "playerId"      "shortName"     "mean_x"        "mean_y"        "fullName"      "Min" Rfoot"         "predictedRole"
# add the role to the new df
MINMAXscaled_copfeatures$predictedRole = clusters_5_df$role
# group by role
grouped_scaled_copFeatures = MINMAXscaled_copfeatures |>
                          group_by(predictedRole) |>
                          summarise(across(names(MINMAXscaled_copfeatures)[1:length(names(MINMAXscaled_copfeatures))-1], mean))
# grouped into clusters and means
grouped_variables_for_polar = grouped_scaled_copFeatures[,c("Passes_cmp","Touches", "TakeOns_succ","SCA", "Shots_oT","xG", "Ycard", "Interceptions")]
# all players scaled 0-1 values
variables_for_polar = MINMAXscaled_copfeatures[,c("Passes_cmp","Touches", "TakeOns_succ","SCA", "Shots_oT","xG", "Ycard", "Interceptions")]

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
grouped_variables_for_polar <- rbind(rep(1,8) , rep(0,8) , grouped_variables_for_polar)
# i don't need the min max values for players because i rnind them to the grouped
# variables_for_polar <- rbind(rep(1,8) , rep(0,8) , variables_for_polar)
# 
# Hazard (Funambolist), Piqué (Warrior), Ronaldo(Letal finisher), Fellaini(Handyman), Busquets (Steal-and-Build), Chiesa (Hybrid)
# radar charts
radarchart(
  rbind(grouped_variables_for_polar[c(1,2,5),],variables_for_polar[playersIndex[6],]),
    pfcol = c(rgb(0, 0.2, 0.6, alpha = 0.3), rgb(0.5, 0.5, 0.5, alpha = 0.2)),
    plty = c(1,1),
    plwd = 2,
    cglcol = rgb(0.3, 0.3, 0.3, alpha = 0.2),
    cglty = 1
  ) +
title(main = "Hybrid", sub = paste("The red chart is", copFeatures$fullName[playersIndex[6]]))
# HEATMAPS FOR SINGLE PLAYERS
#playerId shortName  
# 25707 E. Hazard
# 3341 Piqué    
# 3322 Cristian…
# 8249 M. Fella…
# 3346 Sergio B…
# 447804 F. Chiesa
pitch() +
  # heatmap
  geom_bin2d(data = events[events$playerId == 3322,], mapping = aes(x = start_x, y = start_y), alpha = 0.5, bins = 35) +
  scale_fill_gradient(low = "#B8DBBD", high = "blue") +
  ggtitle("Cristiano Ronaldo's heatmap", subtitle = "Positions were He performed an on-the-ball event")

# HIERARCHICAL CLUSTERING 

#define linkage methods
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# Function to compute agglomerative coefficient
ac <- function(x) {
  agnes(features_matrix, method = x)$ac
}
# Calculate agglomerative coefficient for each clustering linkage method
sapply(m, ac)
# hierarchical_clustering = agnes(pca$x[,1:2], method = "ward") # not a great result
hierarchical_clustering = agnes(features_matrix, method = "ward")
#produce dendrogram
pltree(hierarchical_clustering, cex = 0.6, hang = -1, main = "Dendrogram", xlab = "Hierarchical clustering with Ward's linkage") 
# choice of k = number of clusters
#calculate gap statistic for each number of clusters (up to 10 clusters)
gapStatistic = clusGap(features_matrix, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gapStatistic) # gap statistic from Tibshirani

groups <- cutree(hierarchical_clustering, k=8)
#find number of observations in each cluster
table(PCA_5means$cluster,groups)

# assign clusters
hierarc_copFeatures = cbind(copFeatures, cluster = groups)
hierarc_cop_plot = pitch() +
  # scatter
  geom_point(data = hierarc_copFeatures, mapping = aes(x = mean_x, y = mean_y), colour = hierarc_copFeatures$cluster, alpha = 0.7) +
  scale_color_colorblind(name = "Cluster") +
  ggtitle("Hierarchical clustering on PCA scores with 8 clusters",
          subtitle =
"Players, displayed by mean positions, show a very different clustering shape
w.r.t. Cintia and Pappalardo's work. Taking into accounts performance features changes things.")
hierarc_cop_plot



# TODO:
## table(kmeans,hierarchical) to compare the results of the two clustering methods (see ex. at oecd_data_analysis.R)
## in the table output --> when comparing hcluster and kmeans i have to check that the columns and the rows contains all 0 and one number (that is the number of observations in the cluster), that would be the best solution
## table doesn't have to be squared --> clusters number can be different