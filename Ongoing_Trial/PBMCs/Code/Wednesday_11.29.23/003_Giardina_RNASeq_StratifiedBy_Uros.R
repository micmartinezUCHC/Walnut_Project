# Set working directory
setwd("/users/mikemartinez/Desktop/")

library(pheatmap)
library(ggplot2)

# Read in the data
data <- read.csv("Giardina_RNASeq_Stratified.csv", header = TRUE, sep = ",")
rownames(data) <- data$Sample
data$Sample <- NULL

stratification <- data$Delta_Stratification
names(stratification) <- rownames(data)
stratification <- as.data.frame(stratification)
data$Delta_Stratification <- NULL
stratification$stratification <- factor(stratification$stratification, levels = c("Low", "Med", "High"))



plot <- pheatmap(data,
                 scale = "column",
                 annotation_row = stratification,
                 display_numbers = TRUE,
                 main = "Z-Scaled Fold Changes",
                 fontsize = 20)
ggsave("Giardina_Heatmap_Urolithins.png", plot, width = 8, height = 10)