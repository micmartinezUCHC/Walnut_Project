#The purpose of this script is to test different clustering methods to see how to 
#best stratify the urolithin data

#-----Load Libraries
library(dplyr)
library(ggplot2)
library(ggfortify)
library(stats)
library(factoextra)

#-----Set working directory
setwd("/Users/mikemartinez/Desktop/Urolithin_Stratification")

#-----Read in the data
uro <- read.csv("/users/mikemartinez/Desktop/UroStrat_103123.csv", header = FALSE, sep = ",")

#-----Tidy up the raw data so it is more R friendly
uro <- uro[-1,]
colnames(uro) <- uro[1,]
uro <- uro[-1,]
rownames(uro) <- uro$`Sample ID`
uro$`Sample ID` <- NULL




v1df <- as.data.frame(lapply(v1, as.numeric))
urodf <- as.data.frame(lapply(uro[,1:9], as.numeric))
urodf[is.na(urodf)] <- 0
#-----Let's run PCA on these to see how they cluster
v1pca <- prcomp(v1df, scale = TRUE)
uropca <- prcomp(urodf, scale = TRUE)

#-----Scree plot/scatter plot for visit 1
uroscree <- fviz_eig(uropca)
uroscatter <- fviz_pca_ind(uropca, repel = TRUE, habillage = uro$Visit, addEllipses = TRUE,
                           title = "PCA: Visits 1 -3")
urobiplot <- fviz_pca_biplot(uropca, repel = TRUE, habillage = uro$Visit, addEllipses = TRUE)
urobiplot <- urobiplot + ggtitle("PCA Biplot: Urolithin Metabolite, Visits 1-3") +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        title = element_text(size = 20))

uroloadings <- fviz_pca_var(uropca)


#-----Let's run the analysis on visit 3 since this is the timepoint we will be looking at 
v3df <- as.data.frame(lapply(v3, as.numeric))
v3df[is.na(v3df)] <- 0

#-----Let's run PCA on these to see how they cluster
v3pca <- prcomp(v3df, scale = TRUE)
v3pca <- prcomp(v3df, scale = TRUE)
v3data <- as.data.frame(v3pca$x)

#-----Scree plot/scatter plot for visit 1
v3scree <- fviz_eig(v3pca, title = "Visit 3 Scree Plot")
v3scatter <- fviz_pca_ind(v3pca, repel = TRUE, addEllipses = FALSE,
                           title = "PCA: Visit 3")
v3scatter <- v3scatter + geom_density2d()
v3biplot <- fviz_pca_biplot(v3pca, repel = TRUE, title = "Visit 3 BiPlot")
urobiplot <- urobiplot + ggtitle("PCA Biplot: Urolithin Metabolite, Visits 1-3") +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        title = element_text(size = 20))

v3loadings <- fviz_pca_var(v3pca)


#-----Stratify based on urolithinA and isourolithinA
v3uroA <- v3df[,1:2]

#-----Calculate K-means with 3 clusters
KM <- kmeans(v3uroA, 3)
clusterNames <- c("High", "Low", "Med")
clusterDesignation <- KM[["cluster"]]
v3uroA$Cluster <- clusterDesignation
v3uroA$group <- ifelse(v3uroA$Cluster == "1", "Med", 
                       ifelse(v3uroA$Cluster == "2", "Low", "High"))

#-----Save these results as a csv
write.csv(v3uroA, file = "Stratified_Visit3_basedOnUroA_isoA_Kmeans.csv")

#-----IMPORTANT TO REMEMBER
#CLUSTER 2 = LOW
#CLUSTER 1 = MED
#CLUSTER 3 = HIGH


#-----Let's view the clustering and see how the samples fall
v3uroA$group <- factor(v3uroA$group, levels = c("Low", "Med", "High"))
v3uroApca <- prcomp(v3uroA[,1:2], scale = TRUE)
v3uorAclusterScatter <- fviz_pca_ind(v3uroApca, repel = TRUE, habillage = v3uroA$group, addEllipses = FALSE,
                                title = "K-means Clustering of Visit3 based on UroA and IsoA")






test <- v3df

#-----Calculate K-means with 3 clusters
KM <- kmeans(test, 3)
clusterNames <- c("High", "Low", "Med")
clusterDesignation <- KM[["cluster"]]
test$Cluster <- clusterDesignation
test$group <- ifelse(test$Cluster == "1", "Med", 
                       ifelse(test$Cluster == "2", "Low", "High"))

test$group <- factor(test$group, levels = c("Low", "Med", "High"))
testpca <- prcomp(test[,1:9], scale = TRUE)
testclusterScatter <- fviz_pca_ind(testpca, repel = TRUE, habillage = test$group, addEllipses = FALSE,
                                     title = "K-means Clustering of Visit3 based on full Uro profile")

#-----Save these results as a csv

cowplot::plot_grid(v3uorAclusterScatter, testclusterScatter)


urodata <- read.csv("/Users/mikemartinez/Desktop/UroStrat_103123.csv", header = TRUE, sep = ",")

# Load the necessary packages if you haven't already
library(dplyr)
library(tidyr)

# Assuming your dataframe is named "df"
visit3 <- urodata[urodata$Visit == "Visit_3",]
rownames(visit3) <- visit3$Sample.ID
visit3$Sample.ID <- NULL
visit3$Visit <- NULL
visit2 <- urodata[urodata$Visit == "Visit_2",]
rownames(visit2) <- visit2$Sample.ID
visit2$Sample.ID <- NULL
visit2$Visit <- NULL
delta <- visit3 - visit2

delta$SampleID <- paste(rownames(delta), "delta", sep = "_")
delta$SampleID <- sub("V3", "", delta$SampleID)
rownames(delta) <- delta$SampleID
delta$SampleID <- NULL


#-----PCA on the delta
#-----Let's run PCA on these to see how they cluster
deltaPCA<- prcomp(delta, scale = TRUE)
deltaScatter <- fviz_pca_ind(deltaPCA, repel = TRUE, addEllipses = FALSE,
                           title = "Urolithin Delta Values (V3-V2)")
deltaLoadings <- fviz_pca_var(deltaPCA)
deltaBiplot <- fviz_pca_biplot(deltaPCA, repel = TRUE, title = "Urolithin Delta Values (V3-V2) BiPlot")

#-----Calculate K-means with 3 clusters
KM <- kmeans(delta[,1:9], 3)
clusterNames <- c("High", "Low", "Med")
clusterDesignation <- KM[["cluster"]]
delta$Cluster <- clusterDesignation
delta$group <- ifelse(delta$Cluster == "1", "Med", 
                       ifelse(delta$Cluster == "2", "Low", "High"))


#-----Redo PCA, visualize with clusters
deltaClusteredPCA <- prcomp(delta[,1:9], scale = TRUE)
dPCA <- as.data.frame(deltaClusteredPCA[["x"]])
deltaScatter <- fviz_pca_ind(deltaPCA, repel = TRUE, addEllipses = FALSE, habillage = delta$group,
                             title = "Urolithin Delta Values (V3-V2) Stratified on Full Uro Panel")

delta$Cluster <- NULL
delta$Stratification <- delta$group
delta$group <- NULL
delta$Stratification <- factor(delta$Stratification, levels = c("Low", "Med", "High"))

write.csv(delta, file = "Urolithin_Delta_Stratification.csv")


autoplot(KM, delta)

labels <- rownames(dPCA)
dPCA$group <- delta$group
dPCA$group <- factor(dPCA$group, levels = c("Low", "Med", "High"))

deltaPCAplot <- ggplot(dPCA, aes(x=PC1, y=PC2, color = group)) +
  geom_point() +
  geom_text_repel(aes(label = labels)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        title = element_text(size = 14)) +
  labs(title = "Full Urolithin Panel Delta Values",
       subtitle = "Kmeans-Clustered, K=3") +
  ylab("PC2 (0.09%)") +
  xlab("PC1 (99.85%)")
ggsave("Delta_Stratification_KMeans_PCA.pdf", deltaPCAplot)








