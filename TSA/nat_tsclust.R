### Install & Load Packages #####
#install.packages(c("dtwclust"))
library(dtwclust)
library(cluster)
library(clue)
library(dendextend)
library(tictoc)
library(ggplot2)

##### Read CSV File #####
nat <- read.csv("nat_region_country_2011_2020.csv")

##### Pre-Processing #####
# Convert from dataframe to list
nat <- as.list(nat[-1])
nat <- t(nat)
# Country
country <- nat[,7:205]

##### COUNTRY #####
#### Hierarchical Clustering ####
### Distance: Shape Based Distance (SBD) ###
## Conditions: Agglomerative (Average), 2 Clusters ##
tic("Clustering") # Measure CPU
hc_sbd_c <- tsclust(country, type="h", k=2L, preproc=zscore, seed=899,
                    distance="sbd", centroid=shape_extraction,
                    control=hierarchical_control(method="average"))
toc() # 0.1 sec elapsed

# Plotting dendrogram
par(mar=c(5.5, 4.1, 4.1, 2.1)) # Adjust bottom margin
as.dendrogram(hc_sbd_c) %>% set("branches_k_color", k=2) %>%
  set("labels_cex", 0.4) %>% plot(main="SBD Average Country Dendrogram",
                                  ylab="Height")
# View centroid (prototype) and series
plot(hc_sbd_c, type="sc")
# View centroids only
plot_sbd_c <- plot(hc_sbd_c, type="c") + labs(title="SBD Average Prototypes")
# Identify cluster to region
sbd_agg_list <- data.frame(hc_sbd_c@cluster)
# Table of clusters
table(sbd_agg_list)

## Conditions: Divisive (DIANA), 2 Clusters ##
tic("Clustering") # Measure CPU
hc_sbd_diana_c <- tsclust(country, type="h", k=2L, preproc=zscore, seed=899,
                          distance="sbd", centroid=shape_extraction,
                          control=hierarchical_control(method=diana))
toc() # 0.15 sec elapsed

# Plotting dendrogram
par(mar=c(5.5, 4.1, 4.1, 2.1)) # Adjust bottom margin
as.dendrogram(hc_sbd_diana_c) %>% set("branches_k_color", k=2) %>%
  set("labels_cex", 0.4) %>% plot(main="SBD Divisive Country Dendrogram",
                                  ylab="Height")
# View centroid (prototype) and series
plot(hc_sbd_diana_c, type="sc")
# View centroids only
plot_sbd_diana_c <- plot(hc_sbd_diana_c, type="c") + labs(title="SBD Divisive Prototypes")
# Identify cluster to region
sbd_diana_list <- data.frame(hc_sbd_diana_c@cluster)
# Table of clusters
table(sbd_diana_list)

## Cluster Evaluation ##
# Average linkage: The average distance between the elements in cluster 1 and the elements in cluster 2.
# Centroid linkage: The distance between the centroid for cluster 1 (a mean vector of length p variables) and the centroid for cluster 2.
# Link: https://www.datanovia.com/en/lessons/agglomerative-hierarchical-clustering/
# Consolidate average & diana results
hc_sbd_all_c <- c(list(hc_sbd_c), list(hc_sbd_diana_c))
names(hc_sbd_all_c) <- c("average", "diana")
# Group the clusters for analysis
hc_sbd_all_c <- cl_ensemble(list=hc_sbd_all_c)
# Compare all the clusters
cl_dissimilarity(hc_sbd_all_c) # Clusters are not the same

### Distance: Dynamic Time Warping (DTW) ###
## Conditions: Agglomerative (Average), 2 Clusters ##
tic("Clustering") # Measure CPU
hc_dtw_c <- tsclust(country, type="h", k=2L, preproc=zscore, seed=899,
                    distance="dtw", centroid=dba,
                    control=hierarchical_control(method="average"))
toc() # 26 sec elapsed

# Plotting dendrogram
par(mar=c(5.5, 4.1, 4.1, 2.1)) # Adjust bottom margin
as.dendrogram(hc_dtw_c) %>% set("branches_k_color", k=2) %>%
  set("labels_cex", 0.4) %>% plot(main="DTW Average Country Dendrogram",
                                  ylab="Height")
# View centroid (prototype) and series
plot(hc_dtw_c, type="sc")
# View centroids only
plot_dtw_c <- plot(hc_dtw_c, type="c") + labs(title="DTW Average Prototypes")
# Identify cluster to region
dtw_agg_list <- data.frame(hc_dtw_c@cluster)
# Table of clusters
table(dtw_agg_list)

## Conditions: Divisive (DIANA), 2 Clusters ##
tic("Clustering") # Measure CPU
hc_dtw_diana_c <- tsclust(country, type="h", k=2L, preproc=zscore, seed=899,
                          distance="dtw", centroid=dba,
                          control=hierarchical_control(method=diana))
toc() # 26 sec elapsed

# Plotting dendrogram
par(mar=c(5.5, 4.1, 4.1, 2.1)) # Adjust bottom margin
as.dendrogram(hc_dtw_diana_c) %>% set("branches_k_color", k=2) %>%
  set("labels_cex", 0.4) %>% plot(main="DTW Divisive Country Dendrogram",
                                  ylab="Height")
# View centroid (prototype) and series
plot(hc_dtw_diana_c, type="sc")
# View centroids only
plot_dtw_diana_c <- plot(hc_dtw_diana_c, type="c") + labs(title="DTW Divisive Prototypes")
# Identify cluster to region
dtw_diana_list <- data.frame(hc_dtw_diana_c@cluster)
# Table of clusters
table(dtw_diana_list)

## Cluster Evaluation ##
# Apply DIANA method to the agglomerate methods
hc_dtw_all_c <- c(list(hc_dtw_c), list(hc_dtw_diana_c))
names(hc_dtw_all_c) <- c("average", "diana")
# Group the clusters for analysis
hc_dtw_all_c <- cl_ensemble(list=hc_dtw_all_c)
# Compare all the clusters
cl_dissimilarity(hc_dtw_all_c) # Clusters are not the same

### Total Cluster Evaluation ###
# Combine SBD (Average) and DTW (Average)
hc_avg_all_c <- c(hc_sbd_all_c,hc_dtw_all_c)
names(hc_avg_all_c) <- c("sbd_average", "sbd_diana",
                         "dtw_average", "dtw_diana")
# Group the clusters for analysis
hc_avg_all_c <- cl_ensemble(list=hc_avg_all_c)
# Compare all the clusters
cl_dissimilarity(hc_avg_all_c)
# SUMMARY: SBD and DTW are not the same clusters

## Dendrogram ##
par(mfrow=c(2,2))
# SBD Average
as.dendrogram(hc_sbd_c) %>% set("branches_k_color", k=2) %>%
  set("labels_cex", 0.4) %>% plot(main="SBD Average Country Dendrogram",
                                  ylab="Height")
# SBD DIANA
as.dendrogram(hc_sbd_diana_c) %>% set("branches_k_color", k=2) %>%
  set("labels_cex", 0.4) %>% plot(main="SBD Divisive Country Dendrogram",
                                  ylab="Height")
# DTW Average
as.dendrogram(hc_dtw_c) %>% set("branches_k_color", k=2) %>%
  set("labels_cex", 0.4) %>% plot(main="DTW Average Country Dendrogram",
                                  ylab="Height")
# DTW DIANA
as.dendrogram(hc_dtw_diana_c) %>% set("branches_k_color", k=2) %>%
  set("labels_cex", 0.4) %>% plot(main="DTW Divisive Country Dendrogram",
                                  ylab="Height")

## Prototypes ##
plot_grid(plot_sbd_c, plot_sbd_diana_c, plot_dtw_c, plot_dtw_diana_c)

## Cluster Identification ##
clust_sbd <- merge(sbd_agg_list, sbd_diana_list, by='row.names', all=TRUE)
clust_dtw <- merge(dtw_agg_list, dtw_diana_list, by='row.names', all=TRUE)
clust_id <- merge(clust_sbd, clust_dtw, by='Row.names', all=TRUE)
# Save into a CSV file
write.csv(clust_id, 'nat_cluster_id.csv')

## FOCUS: Mexico & Northern Triangle ##
# Define variables
Year <- c(2011:2020)
Mexico <- country$Mexico
Guatemala <- country$Guatemala
Honduras <- country$Honduras
El_Salvador <- country$El_Salvador

# Create database
focus <- data.frame(Year, Mexico, Guatemala, Honduras, El_Salvador)

# Time series plot
par(mfrow=c(1,1))
nat.plot <- ggplot(focus, aes(x=Year)) +
  scale_x_discrete(limits=c(2011,2015,2019)) +
  geom_line(aes(y=Mexico, color="Mexico")) +
  geom_line(aes(y=Guatemala, color="Guatemala")) +
  geom_line(aes(y=Honduras, color="Honduras")) +
  geom_line(aes(y=El_Salvador, color="El Salvador")) +
  labs(title="Naturalization Time Series", x="Year", y="Number", col="Country")
# Plot of all the datasets in a grid
#plot_grid(app.plot, lpr.plot, nat.plot, non.plot)

##### NOTES #####
# k=3: Showed a small group for one cluster.
# LINK: https://rdrr.io/cran/dtwclust/man/tsclust.html
# LINK: https://uc-r.github.io/hc_clustering
