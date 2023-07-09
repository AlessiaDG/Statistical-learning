library(BBmisc)
library(cluster)
library(factoextra)

#UL: CLUSTERING
summary(scale(df_num))
df_num <-df[, -c(1,2,13)]
df_num2 <- df_num[,-c(1)]

df_scaled <- as.data.frame(scale(df_num2))
#serach for the optimal number of clusters
wssplot <- function(data, nc=0, seed=1234){
  wss <- (nrow(df_scaled)-1)*sum(apply(df_scaled,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(df_scaled, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

fviz_nbclust(df_scaled, kmeans, method = "wss")
#elbow method: 3
#calculate gap statistic based on number of clusters
gap_stat <- clusGap(df_scaled,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

#perform k-means clustering with k = 3 clusters
km <- kmeans(df_scaled, centers = 3, nstart = 25)

#view results
km

fviz_cluster(km, data = df_scaled)
# Aggiungi l'etichetta del cluster a df_num2
df_num$cluster <- km$cluster



# Calcola le statistiche di Fertility per ciascun cluster
fertility_stats <- aggregate(Fertility ~ cluster, data = df_num, FUN = mean)
fertility_stats


round(aggregate(df_num, by=list(cluster=km$cluster), mean), 2)

km$cluster
df <- df[,-c(13)]
cluster_data_list <- split(df, factor(km$cluster))
cluster_data_list

cluster_1_data <- cluster_data_list[[1]]
cluster_2_data <- cluster_data_list[[2]]
cluster_3_data <- cluster_data_list[[3]]

cluster_1_data$Country
#Le nazioni menzionate sono principalmente appartenenti all'Africa, con alcune rappresentanti dell'Asia e dell'Oceania e America (Haiti).
#sono caratterizzate da un alto tasso di fertilità, bassa life expectancy e anche anni di scuola e uso di contraccettivi
#possiamo pensare che siano meno sviluppate 


cluster_2_data$Country
#Le nazioni menzionate sono principalmente appartenenti all'Asia e all'America, con alcune rappresentanti dell'Africa e dell'Oceania.
#il tasso di fertilità scende a 2.3 e anche le altre variabili sembrano aumentare
cluster_3_data$Country
#sono principalmente appartenenti all'Europa, con alcune rappresentanti dell'America, dell'Asia e dell'Oceania.
#possiamo definirli come i paesi più sviluppati dove il well being condiziona anche il tasso di fertilità, che appare molto basso

religion_freq1 <- table(cluster_1_data$Religion)
religion_freq1 #Cristianesimo
religion_freq2 <- table(cluster_2_data$Religion)
religion_freq2
religion_freq3 <- table(cluster_3_data$Religion)
religion_freq3 #Cristianesimo e Islamismo


ris3 <- eclust(df_scaled, "kmeans", k=3)
fviz_silhouette(ris3)
sil3 <- ris3$silinfo$widths

# Create separate dataframes for each cluster
cluster1_dataF <- subset(df_num, cluster == 1, select = c(Fertility))
cluster2_dataF <- subset(df_num, cluster == 2, select = c(Fertility))
cluster3_dataF <- subset(df_num, cluster == 3, select = c(Fertility))

# Create a list of dataframes for each cluster
cluster_dataF <- list(cluster1_dataF, cluster2_dataF, cluster3_dataF)

# Plot boxplots for each cluster
par(mfrow = c(1, 3))  # Set the layout of subplots
for (i in 1:3) {
  boxplot(cluster_dataF[[i]], main = paste("Cluster", i), ylab = "Fertility Rate")
}


#CLUSTERING WITH MIXED DATA TYPE
df_numeriche <- df[,-c(1,2, 3)]
norm = normalize(df_numeriche, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")
summary(norm)
df_religion <- as.data.frame(df[,c(2)])
hc.c=cbind(df_religion, norm)
colnames(hc.c)
colnames(hc.c)[colnames(hc.c) == "df[, c(2)]"] <- "Religion"
hc.c$Religion <-as.factor(hc.c$Religion)
head(hc.c)
gower_dist <- daisy(hc.c, metric = "gower", type = list(logratio=3))
summary(gower_dist)
gower_mat <- as.matrix(gower_dist)

# Output most similar pair
df[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Output most dissimilar pair
df[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

Esgower <- numeric(10)  
#PAM = partitioning and medoids
for(i in 2:10){
  pames <- pam(gower_dist, diss=TRUE, k=i)
  Esgower[i] <- pames$silinfo$avg.width}
plot(1:10, Esgower, type="b", ylab="Silhouette", xlab ="Number of Clusters")


pam_fit<- pam(gower_dist, diss=TRUE, k =2)
fviz_silhouette(pam_fit)

pam_results <- df %>%
  dplyr::select(-Country) %>%
  mutate(cluster=pam_fit$clustering)%>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary
# Access the cluster assignments
cluster_assignments <- pam_fit$clustering

# Add cluster assignments to the original dataframe
df_with_clusters <- df
df_with_clusters$Cluster <- cluster_assignments

# Create separate dataframes for each cluster
cluster1F <- subset(df_with_clusters, Cluster == 1, select = c(Fertility))
cluster2F <- subset(df_with_clusters, Cluster == 2, select = c(Fertility))


# Create a list of dataframes for each cluster
clusterF <- list(cluster1F, cluster2F)

# Plot boxplots for each cluster
par(mfrow = c(1, 2))  # Set the layout of subplots
for (i in 1:2) {
  boxplot(clusterF[[i]], main = paste("Cluster", i), ylab = "Fertility Rate")
}


# Print the countries in each cluster
for (cluster in unique(cluster_assignments)) {
  countries_in_cluster <- df_with_clusters$Country[df_with_clusters$Cluster == cluster]
  print(paste("Cluster", cluster, ":", countries_in_cluster))
}

hc <- hclust(gower_dist, method = "complete")
plot(hc)
groups <- cutree(hc, k=2)
rect.hclust(hc, k=2, border="red")
