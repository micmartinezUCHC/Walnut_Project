library(ggplot2)
library(ggpubr)

featData<-read.csv("violinDat_GLP1.csv", row.names = 1, header = TRUE)
violinDat<-as.data.frame(featData, stringsAsFactors = FALSE)
str(violinDat)
#colnames(violinDat)<-c("GeneName","XprValue","ClusterID")
groupLabels <-c("HighHigh", "HighLow", "LowHigh","LowLow")

ylim = NULL
# if(fixed_ylim == TRUE)
#   ylim = c(-0.5, 2.5) #c(-2.5, 5)

ggplot(violinDat, aes(x = factor(as.integer(violinDat$ClusterID)),
                      y = as.numeric(violinDat$XprValue),
                      fill = factor(as.integer(violinDat$ClusterID))
)) +
  scale_x_discrete(labels= groupLabels)+
  ggtitle(paste0(as.character(factor(violinDat$GeneName))," ", tools::toTitleCase("area"), "- Scaled Violin Plot")) +
  scale_fill_manual(values = c("#FF0000BF", "#A6CEE3" ,  "#FF8000BF" ,"#1F78B4") #use.palette#(rainbow(maxNumClust))
                    ,labels = groupLabels )+
  theme_bw()  +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title = element_text(size=15,face="bold"))+
  coord_cartesian(ylim = ylim#NULL#c(-2.5, 5)
  ) +
  geom_violin(#position = "dodge",
    trim = FALSE, scale = "area" ,adjust = 1, na.rm = TRUE
    ,draw_quantiles = TRUE  ) +
  stat_compare_means(method = "wilcox.test", comparisons = list(c("1", "3")))+
  geom_boxplot(outlier.shape = 19, outlier.size = 0.1, na.rm = TRUE, varwidth = TRUE, width=0.05)+
  xlab("Group by Cluster/Library") + ylab("Gene Expression Density (Log10-transformed Expression)") +
  guides(fill = guide_legend(title = "Cluster/Library"))
