#Load libraries
library("ggplot2")
library("ggh4x")
library("dplyr")
library("tidyverse")

#Set working directory
setwd("/Users/mikemartinez/Desktop/Marmar_Emails/")

#Read in the all vimentin data
vimentin <- read.csv("violinDat_vim_widthnorm_allROIs.csv", header = TRUE, sep = ",")

#Add faceting column
vimentin <- vimentin %>%
  mutate(Low_or_High = case_when(
    ClusterID >= 1 & ClusterID <= 10 ~ "Low Urolithin A",
    ClusterID >= 11 & ClusterID <= 23 ~ "High Urolithin A",
    TRUE ~ "Other"
  ))

#Set factor order
vimentin$Low_or_High <- factor(vimentin$Low_or_High,
                               levels = c("Low Urolithin A", "High Urolithin A"))

#Plot width scaled violin plot
ggplot(vimentin, aes(x = factor(as.integer(vimentin$ClusterID)),
                      y = as.numeric(vimentin$XprValue),
                      fill = factor(as.integer(vimentin$ClusterID))
)) +
  theme_bw() +
  theme(axis.text=element_text(size=15),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold")) +
  geom_violin(#position = "dodge",
    trim = FALSE, scale = "width" ,adjust = 1, na.rm = TRUE
    ,draw_quantiles = TRUE  ) +
  facet_wrap(~Low_or_High, scales = "free_x") +
  geom_boxplot(outlier.shape = 19, outlier.size = 0.1, na.rm = TRUE, varwidth = TRUE, width=0.05)+
  xlab("ROI") + ylab("Vimentin Expression Density (Log10-Transformed)") +
  guides(fill = guide_legend(ncol = 1, title = "ROI"))



#Vimentin cluster 3 ROIs
#Read in the all vimentin data
c3 <- read.csv("violinDat_vim_widthnorm_C3HighVim.csv", header = TRUE, sep = ",")

#Add faceting column
c3 <- c3 %>%
  mutate(Low_or_High = case_when(
    ClusterID >= 1 & ClusterID <= 10 ~ "Low Urolithin A",
    ClusterID >= 11 & ClusterID <= 23 ~ "High Urolithin A",
    TRUE ~ "Other"
  ))

#Set factor order
c3$Low_or_High <- factor(c3$Low_or_High,
                               levels = c("Low Urolithin A", "High Urolithin A"))

#Plot width scaled violin plot
ggplot(c3, aes(x = factor(as.integer(c3$ClusterID)),
                     y = as.numeric(c3$XprValue),
                     fill = factor(as.integer(c3$ClusterID))
)) +
  theme_bw() +
  theme(axis.text=element_text(size=15),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold")) +
  geom_violin(#position = "dodge",
    trim = FALSE, scale = "width" ,adjust = 1, na.rm = TRUE
    ,draw_quantiles = TRUE  ) +
  facet_wrap(~Low_or_High, scales = "free_x") +
  geom_boxplot(outlier.shape = 19, outlier.size = 0.1, na.rm = TRUE, varwidth = TRUE, width=0.05)+
  xlab("ROI") + ylab("Vimentin Expression Density (Log10-Transformed)") +
  guides(fill = guide_legend(ncol = 1, title = "ROI"))




