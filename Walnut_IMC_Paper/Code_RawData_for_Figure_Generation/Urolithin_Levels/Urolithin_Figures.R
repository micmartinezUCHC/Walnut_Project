#The purpose of this script is to generate figures for the urolithin metabolomics paper

#Set working directory
setwd("/Users/mikemartinez/Desktop/Walnut_Study/Metabolomics_data/")

#Load libraries
library("ggplot2")
library("ggh4x")
library("ggrepel")
library("tidyverse")
library("dplyr")
library("ggpubr")


#Timepoint Urolithin A plot
#Reload in the data just for Urolithin A
uro_data <- read.csv("uro_data_long.csv", header = TRUE, sep = ",")

log2Uro <- log2(uro_data$ng.mg + 0.1)
uro_data$ng.mg <- log2Uro

time_order <- c("Before",
                "After")
uro_data <- uro_data %>%
  mutate(Timepoint = factor(Timepoint, levels = time_order))

group_order <- c("Low", "Med", "High")
uro_data<- uro_data %>%
  mutate(Group = factor(Group, levels =  group_order))

urolithins_paired <- ggpaired(uro_data, x = "Timepoint", y = "ng.mg",
                  fill = "Timepoint", line.color = "black", line.size = 0.1) +
  labs(y = "Log2 creatinine-normalized urolithin levels (ng/mg)",
       x = "Walnut Supplementation") +
  theme(legend.position = "right") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "right")+
  facet_wrap(~Metabolite) +
  stat_compare_means(method = "wilcox", paired = TRUE, label = "p.format", vjust = 0.5)
urolithins_paired

isaA <- uro_data[uro_data$Metabolite == "Isourolithin A",]
log2isaA <- log2(isaA$ng.mg + 0.1)
isaA$ng.mg <- log2isaA

isaA <- isaA %>%
  mutate(Timepoint = factor(Timepoint, levels = time_order))


isoA_paired <- ggpaired(isaA, x = "Timepoint", y = "ng.mg",
                              fill = "Timepoint", line.color = "gray", line.size = 0.4) +
  labs(y = "Log2 creatinine-normalized isourolithin A levels (ng/mg",
       x = "Walnut Supplementation") +
  theme(legend.position = "right") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")+
  ggtitle("Isourolithin A")
isoA_paired

uroB <- uro_data[uro_data$Metabolite == "Urolithin B",]
log2uroB <- log2(uroB$ng.mg + 0.1)
uroB$ng.mg <- log2uroB

uroB <- uroB %>%
  mutate(Timepoint = factor(Timepoint, levels = time_order))

uroB_paired <- ggpaired(uroB, x = "Timepoint", y = "ng.mg",
                        fill = "Timepoint", line.color = "gray", line.size = 0.4) +
  labs(y = "Log2 creatinine-normalized urolithin B levels (ng/mg",
       x = "Walnut Supplementation") +
  theme(legend.position = "right") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")+
  ggtitle("Urolithin B")
uroB_paired


uroC <- uro_data[uro_data$Metabolite == "Urolithin C",]
log2uroC <- log2(uroC$ng.mg + 0.1)
uroC$ng.mg <- log2uroC

uroC <- uroC %>%
  mutate(Timepoint = factor(Timepoint, levels = time_order))

uroC_paired <- ggpaired(uroB, x = "Timepoint", y = "ng.mg",
                        fill = "Timepoint", line.color = "gray", line.size = 0.4) +
  labs(y = "Log2 creatinine-normalized urolithin C levels (ng/mg",
       x = "Walnut Supplementation") +
  theme(legend.position = "right") +
  stat_compare_means(method = "wilcox")+
  scale_fill_brewer(palette = "Set3") +
  ggtitle("Urolithin C")
uroC_paired

uroD <- uro_data[uro_data$Metabolite == "Urolithin D",]
log2uroD <- log2(uroD$ng.mg + 0.1)
uroD$ng.mg <- log2uroD

uroD <- uroD %>%
  mutate(Timepoint = factor(Timepoint, levels = time_order))

uroD_paired <- ggpaired(uroD, x = "Timepoint", y = "ng.mg",
                        fill = "Timepoint", line.color = "gray", line.size = 0.4) +
  labs(y = "Log2 creatinine-normalized urolithin D levels (ng/mg",
       x = "Walnut Supplementation") +
  theme(legend.position = "right") +
  stat_compare_means(method = "wilcox")+
  scale_fill_brewer(palette = "Set3") +
  ggtitle("Urolithin D")
uroD_paired

uroE <- uro_data[uro_data$Metabolite == "Urolithin E",]
log2uroE <- log2(uroE$ng.mg + 0.1)
uroE$ng.mg <- log2uroE

uroE <- uroE %>%
  mutate(Timepoint = factor(Timepoint, levels = time_order))

uroE_paired <- ggpaired(uroE, x = "Timepoint", y = "ng.mg",
                        fill = "Timepoint", line.color = "gray", line.size = 0.4) +
  labs(y = "Log2 creatinine-normalized urolithin E levels (ng/mg",
       x = "Walnut Supplementation") +
  theme(legend.position = "right") +
  stat_compare_means(method = "wilcox")+
  scale_fill_brewer(palette = "Set3") +
  ggtitle("Urolithin E")
uroE_paired

uroM5 <- uro_data[uro_data$Metabolite == "Urolithin M5",]
log2uroM5 <- log2(uroM5$ng.mg + 0.1)
uroM5$ng.mg <- log2uroM5

uroM5 <- uroM5 %>%
  mutate(Timepoint = factor(Timepoint, levels = time_order))

uroM5_paired <- ggpaired(uroM5, x = "Timepoint", y = "ng.mg",
                        fill = "Timepoint", line.color = "gray", line.size = 0.4) +
  labs(y = "Log2 creatinine-normalized urolithin M5 levels (ng/mg",
       x = "Walnut Supplementation") +
  theme(legend.position = "right") +
  stat_compare_means(method = "wilcox")+
  scale_fill_brewer(palette = "Set3") +
  ggtitle("Urolithin M5")
uroM5_paired

uroM6 <- uro_data[uro_data$Metabolite == "Urolithin M6",]
log2uroM6 <- log2(uroM6$ng.mg + 0.1)
uroM6$ng.mg <- log2uroM6

uroM6 <- uroM6 %>%
  mutate(Timepoint = factor(Timepoint, levels = time_order))

uroM6_paired <- ggpaired(uroM6, x = "Timepoint", y = "ng.mg",
                         fill = "Timepoint", line.color = "gray", line.size = 0.4) +
  labs(y = "Log2 creatinine-normalized urolithin M6 levels (ng/mg",
       x = "Walnut Supplementation") +
  theme(legend.position = "right") +
  stat_compare_means(method = "wilcox")+
  scale_fill_brewer(palette = "Set3") +
  ggtitle("Urolithin M6")
uroM6_paired

uroM7 <- uro_data[uro_data$Metabolite == "Urolithin M7",]
log2uroM7 <- log2(uroM7$ng.mg + 0.1)
uroM7$ng.mg <- log2uroM7

uroM7 <- uroM7 %>%
  mutate(Timepoint = factor(Timepoint, levels = time_order))

uroM7_paired <- ggpaired(uroM7, x = "Timepoint", y = "ng.mg",
                         fill = "Timepoint", line.color = "gray", line.size = 0.4) +
  labs(y = "Log2 creatinine-normalized urolithin M7 levels (ng/mg",
       x = "Walnut Supplementation") +
  theme(legend.position = "right") +
  stat_compare_means(method = "wilcox")+
  scale_fill_brewer(palette = "Set3") +
  ggtitle("Urolithin M7")
uroM7_paired









