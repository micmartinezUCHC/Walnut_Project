library("ggplot2")
library("ggpubr")

#Set working directory
setwd("/Users/mikemartinez/Desktop/Walnut_Study/Violins/")

CRPData<-read.csv("violinDat_CRP.csv", row.names = 1, header = TRUE)
CRP <-as.data.frame(CRPData, stringsAsFactors = FALSE)
str(CRP)
#colnames(violinDat)<-c("GeneName","XprValue","ClusterID")
groupLabels <-c("High Urolithin \nHigh BMI", "High Urolithin \nLow BMI", "Low Urolithin \nHigh BMI","Low Urolithin \nLow BMI")



ENA78Data<-read.csv("violinDat_ENA78.csv", row.names = 1, header = TRUE)
ENA78 <-as.data.frame(ENA78Data, stringsAsFactors = FALSE)
str(ENA78)
#colnames(violinDat)<-c("GeneName","XprValue","ClusterID")
groupLabels <-c("High Urolithin \nHigh BMI", "High Urolithin \nLow BMI", "Low Urolithin \nHigh BMI","Low Urolithin \nLow BMI")

ylim = NULL
# if(fixed_ylim == TRUE)
#   ylim = c(-0.5, 2.5) #c(-2.5, 5)

ENA78plot <- ggplot(ENA78, aes(x = factor(as.integer(ClusterID)),
                       y = as.numeric(XprValue),
                       fill = factor(as.integer(ClusterID))
)) +
  scale_x_discrete(labels= groupLabels)+
  ggtitle(paste0(as.character(factor(ENA78$GeneName)))) +
  scale_fill_manual(values = c("#FF0000BF", "#A6CEE3" ,  "#FF8000BF" ,"#1F78B4") #use.palette#(rainbow(maxNumClust))
                    ,labels = groupLabels )+
  theme_bw()  +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(size=15,face="bold"))+
  coord_cartesian(ylim = ylim#NULL#c(-2.5, 5)
  ) +
  geom_violin(#position = "dodge",
    trim = FALSE, scale = "area" ,adjust = 1, na.rm = TRUE
    ,draw_quantiles = TRUE  ) +
  geom_boxplot(outlier.shape = 19, outlier.size = 0.1, na.rm = TRUE, varwidth = TRUE, width=0.05) +
  ylab("Gene Expression Density (Log10-transformed Expression)") +
  xlab("") +
  theme(axis.text = element_text(size = 8)) +
  guides(fill = guide_legend(title = "Cluster/Library"))

ENA78plot

xpos <- c(1, 3)
ypos <- max(ENA78$XprValue) + 0.6


GLP1Data<-read.csv("violinDat_GLP1.csv", row.names = 1, header = TRUE)
GLP1 <-as.data.frame(GLP1Data, stringsAsFactors = FALSE)
str(GLP1)
#colnames(violinDat)<-c("GeneName","XprValue","ClusterID")
groupLabels <-c("High Urolithin \nHigh BMI", "High Urolithin \nLow BMI", "Low Urolithin \nHigh BMI","Low Urolithin \nLow BMI")

ylim = NULL
# if(fixed_ylim == TRUE)
#   ylim = c(-0.5, 2.5) #c(-2.5, 5)

GLP1plot <- ggplot(GLP1, aes(x = factor(as.integer(ClusterID)),
                               y = as.numeric(XprValue),
                               fill = factor(as.integer(ClusterID))
)) +
  scale_x_discrete(labels= groupLabels)+
  ggtitle("GLP-1 (Active)") +
  scale_fill_manual(values = c("#FF0000BF", "#A6CEE3" ,  "#FF8000BF" ,"#1F78B4") #use.palette#(rainbow(maxNumClust))
                    ,labels = groupLabels )+
  theme_bw()  +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(size=15,face="bold"))+
  coord_cartesian(ylim = ylim#NULL#c(-2.5, 5)
  ) +
  geom_violin(#position = "dodge",
    trim = FALSE, scale = "area" ,adjust = 1, na.rm = TRUE
    ,draw_quantiles = TRUE  ) +
  geom_boxplot(outlier.shape = 19, outlier.size = 0.1, na.rm = TRUE, varwidth = TRUE, width=0.05) +
  ylab("Gene Expression Density (Log10-transformed Expression)") +
  xlab("") +
  theme(axis.text = element_text(size = 8)) +
  guides(fill = guide_legend(title = "Cluster/Library"))

GLP1plot
# Calculate position for bracket and text
xpos <- c(1, 3)
ypos <- max(GLP1$XprValue) - 0.2

######################
testdata<-read.csv("test.csv", header = TRUE)
test <-as.data.frame(testdata, stringsAsFactors = FALSE)
str(test)
#colnames(violinDat)<-c("GeneName","XprValue","ClusterID")
groupLabels <-c("High Urolithin \nHigh BMI", "High Urolithin \nLow BMI", "Low Urolithin \nHigh BMI","Low Urolithin \nLow BMI")

ylim = NULL
# if(fixed_ylim == TRUE)
#   ylim = c(-0.5, 2.5) #c(-2.5, 5)

testplot <- ggplot(test, aes(x = factor(as.integer(ClusterID)),
                             y = as.numeric(XprValue),
                             fill = factor(as.integer(ClusterID))
)) +
  scale_x_discrete(labels= groupLabels)+
  ggtitle("Biomarkers") +
  scale_fill_manual(values = c("#FF0000BF", "#A6CEE3" ,  "#FF8000BF" ,"#1F78B4") #use.palette#(rainbow(maxNumClust))
                    ,labels = groupLabels )+
  theme_bw()  +
  facet_wrap(~GeneName) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(size=15,face="bold"))+
  coord_cartesian(ylim = ylim#NULL#c(-2.5, 5)
  ) +
  geom_violin(#position = "dodge",
    trim = FALSE, scale = "area" ,adjust = 1, na.rm = TRUE
    ,draw_quantiles = TRUE  ) +
  geom_boxplot(outlier.shape = 19, outlier.size = 0.1, na.rm = TRUE, varwidth = TRUE, width=0.05) +
  ylab("Gene Expression Density (Log10-transformed Expression)") +
  xlab("") +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Cluster"))

testplot
# Calculate position for bracket and text
xpos <- c(1, 3)
ypos <- max(CRP$XprValue) - 0.2

test_with_bracket <- testplot +
  annotate("segment", x = xpos, xend = xpos, y = ypos, yend = ypos + 0.1) +
  annotate("segment", x = xpos[1], xend = xpos[2], y = ypos + 0.1, yend = ypos + 0.1, color = "black") +
  annotate("text", x = mean(xpos), y = ypos + 0.2, label = "p < 0.051", size = 3)
test_with_bracket


cowplot::plot_grid(CRP_with_bracket, ENA78_with_bracket, GLP1_with_bracket)

p_values <- c("ns", "p < 0.00014", "p < 0.051", "ns", "p < 0.024", "p < 0.0033")
gene_names <- unique(test$GeneName)

p_Values_df <- data.frame(GeneName = gene_names, p_value = p_values)

#Merge the p-values with the original dataframe
test_with_pvalues <- merge(test, p_Values_df, by = "GeneName", all.x = TRUE)

leptin <- read.csv("violinDat_LEPTIN.csv", header = TRUE)
MIP <- read.csv("violinDat_MIP1D.csv", header = TRUE)
SICAM <- read.csv("violinDat_SICAM1.csv", header = TRUE)


# Calculate position for bracket and text
xpos <- c(1, 3)
yposCRP <- max(CRP$XprValue) + 0.6
yposENA78 <- max(GLP1$XprValue) + 0.6
leptin <- max(leptin$XprValue) + 0.6
MIP <- max(MIP$XprValue) + 0.6
SICAm <- max(SICAM$XprValue) + 0.6


max_values <- test_with_pvalues %>%
  group_by(GeneName) %>%
  summarize(Max_XprValue = max(XprValue, na.rm = TRUE))

ypos <- max(CRP$XprValue) + 0.6
test_with_pvalues <- test_with_pvalues %>%
  left_join(max_values, by = "GeneName")

testplot <- ggplot(test_with_pvalues, aes(x = factor(as.integer(ClusterID)),
                             y = as.numeric(XprValue),
                             fill = factor(as.integer(ClusterID))
)) +
  scale_x_discrete(labels= groupLabels)+
  scale_fill_manual(values = c("#FF0000BF", "#A6CEE3" ,  "#FF8000BF" ,"#1F78B4") #use.palette#(rainbow(maxNumClust))
                    ,labels = groupLabels )+
  theme_bw()  +
  facet_wrap(.~GeneName ~., scale = "free_y") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(size=15,face="bold"))+
  scale_y_continuous() +
  geom_violin(#position = "dodge",
    trim = FALSE, scale = "area" ,adjust = 1, na.rm = TRUE
    ,draw_quantiles = TRUE) +
  geom_boxplot(outlier.shape = 19, outlier.size = 0.1, na.rm = TRUE, varwidth = TRUE, width=0.05) +
  ylab("Gene Expression Density (Log10-transformed Expression)") +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 14)) +
  xlab("") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  annotate("segment", x = xpos, xend = xpos, y = test_with_pvalues$Max, yend = test_with_pvalues$Max + 0.1) +
  annotate("segment", x = xpos[1], xend = xpos[2], y = Max + 0.1, yend = Max + 0.1, color = "black") +
  geom_text(aes(x = mean(xpos), y = Max + 0.3, label = paste("", p_value)),
            size = 3.5) +
  guides(fill = guide_legend(title = "Cluster"))
testplot

testplot <- testplot + 
  annotate("segment", x = xpos, xend = xpos, y = ypos, yend = ypos + 0.1) +
  annotate("segment", x = xpos[1], xend = xpos[2], y = max_values + 0.1, yend = max_values + 0.1, color = "black") +
  geom_text(aes(x = mean(xpos), y = ypos + 0.3, label = paste("", p_value)),
            size = 3.5)
            
testplot 

library(rstatix)

anno <- read.csv("/Users/mikemartinez/Desktop/anno.txt", header = TRUE, sep = ",")


plot2 <- ggplot(test_with_pvalues, aes(x = factor(as.integer(ClusterID)),
                                          y = as.numeric(XprValue),
                                          fill = factor(as.integer(ClusterID))
)) +
  scale_x_discrete(labels= groupLabels)+
  scale_fill_manual(values = c("#FF0000BF", "#A6CEE3" ,  "#FF8000BF" ,"#1F78B4") #use.palette#(rainbow(maxNumClust))
                    ,labels = groupLabels )+
  theme_bw()  +
  facet_wrap2(vars(GeneName), scales = "free_y") +
  stat_compare_means(comparisons = c(1,3), method = "wilcox.test") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(size=15,face="bold"))+
  scale_y_continuous() +
  geom_violin(#position = "dodge",
    trim = FALSE, scale = "area" ,adjust = 1, na.rm = TRUE
    ,draw_quantiles = TRUE) +
  geom_boxplot(outlier.shape = 19, outlier.size = 0.1, na.rm = TRUE, varwidth = TRUE, width=0.05) +
  ylab("Gene Expression Density (Log10-transformed Expression)") +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 14)) +
  xlab("") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Cluster"))
plot2








################
xpos <- c(1,3)
ypos <- max(CRP$XprValue) + 0.6
CRP$pvalue <- "p = 0.09"


CRPplot <- ggplot(CRP, aes(x = factor(as.integer(ClusterID)),
                                          y = as.numeric(XprValue),
                                          fill = factor(as.integer(ClusterID))
)) +
  scale_x_discrete(labels= groupLabels)+
  scale_fill_manual(values = c("#FF0000BF", "#A6CEE3" ,  "#FF8000BF" ,"#1F78B4") #use.palette#(rainbow(maxNumClust))
                    ,labels = groupLabels )+
  theme_bw()  +
  facet_wrap(~pvalue) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(size=15,face="bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank())+
  scale_y_continuous() +
  labs(title = "CRP") +
  geom_violin(#position = "dodge",
    trim = FALSE, scale = "area" ,adjust = 1, na.rm = TRUE
    ,draw_quantiles = TRUE) +
  geom_boxplot(outlier.shape = 19, outlier.size = 0.1, na.rm = TRUE, varwidth = TRUE, width=0.05) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 14)) +
  xlab("") +
  ylab("") +
  #theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  annotate("segment", x = xpos, xend = xpos, y = ypos, yend = ypos + 0.1) +
  annotate("segment", x = xpos[1], xend = xpos[2], y = ypos + 0.1, yend = ypos + 0.1, color = "black") +
  #geom_text(aes(x = mean(xpos), y = ypos + 0.3, label = paste("", pvalue)),
  #size = 3.5) +
  theme(legend.position = "none")
CRPplot


ypos <- max(ENA78$XprValue) + 0.6
ENA78$pvalue <- "p < 0.00014"


ENA78plot <- ggplot(ENA78, aes(x = factor(as.integer(ClusterID)),
                           y = as.numeric(XprValue),
                           fill = factor(as.integer(ClusterID))
)) +
  scale_x_discrete(labels= groupLabels)+
  scale_fill_manual(values = c("#FF0000BF", "#A6CEE3" ,  "#FF8000BF" ,"#1F78B4") #use.palette#(rainbow(maxNumClust))
                    ,labels = groupLabels )+
  theme_bw()  +
  facet_wrap(~pvalue) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(size=15,face="bold"),axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank())+
  scale_y_continuous() +
  labs(title = "ENA78") +
  geom_violin(#position = "dodge",
    trim = FALSE, scale = "area" ,adjust = 1, na.rm = TRUE
    ,draw_quantiles = TRUE) +
  geom_boxplot(outlier.shape = 19, outlier.size = 0.1, na.rm = TRUE, varwidth = TRUE, width=0.05) +
  ylab("Gene Expression Density (Log10-transformed Expression)") +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 14)) +
  xlab("") +
  ylab("") +
  #theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  annotate("segment", x = xpos, xend = xpos, y = ypos, yend = ypos + 0.1) +
  annotate("segment", x = xpos[1], xend = xpos[2], y = ypos + 0.1, yend = ypos + 0.1, color = "black") +
  #geom_text(aes(x = mean(xpos), y = ypos + 0.3, label = paste("", pvalue)),
            #size = 3.5) +
  theme(legend.position = "none")
ENA78plot



ypos <- max(GLP1$XprValue) + 0.1
GLP1$pvalue <- "p < 0.051"


GLP1plot <- ggplot(GLP1, aes(x = factor(as.integer(ClusterID)),
                               y = as.numeric(XprValue),
                               fill = factor(as.integer(ClusterID))
)) +
  scale_x_discrete(labels= groupLabels)+
  scale_fill_manual(values = c("#FF0000BF", "#A6CEE3" ,  "#FF8000BF" ,"#1F78B4") #use.palette#(rainbow(maxNumClust))
                    ,labels = groupLabels )+
  theme_bw()  +
  facet_wrap(~pvalue) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(size=15,face="bold"))+
  scale_y_continuous() +
  labs(title = "GLP-1 (Active)") +
  geom_violin(#position = "dodge",
    trim = FALSE, scale = "area" ,adjust = 1, na.rm = TRUE
    ,draw_quantiles = TRUE) +
  geom_boxplot(outlier.shape = 19, outlier.size = 0.1, na.rm = TRUE, varwidth = TRUE, width=0.05) +
  ylab("Gene Expression Density (Log10-transformed Expression)") +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 14),axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  xlab("") +
  ylab("") +
  #theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  annotate("segment", x = xpos, xend = xpos, y = ypos, yend = ypos + 0.1) +
  annotate("segment", x = xpos[1], xend = xpos[2], y = ypos + 0.1, yend = ypos + 0.1, color = "black") +
  #geom_text(aes(x = mean(xpos), y = ypos + 0.3, label = paste("", pvalue)),
            #size = 3.5) +
  theme(legend.position = "none")
GLP1plot


SICAM <- read.csv("violinDat_SICAM1.csv", header = TRUE, sep = ",")
SICAM <-as.data.frame(SICAM, stringsAsFactors = FALSE)
str(SICAM)
#colnames(violinDat)<-c("GeneName","XprValue","ClusterID")
groupLabels <-c("High Urolithin \nHigh BMI", "High Urolithin \nLow BMI", "Low Urolithin \nHigh BMI","Low Urolithin \nLow BMI")

ypos <- max(SICAM$XprValue) + 0.4
SICAM$pvalue <- "p < 0.0033"

SICAMplot <- ggplot(SICAM, aes(x = factor(as.integer(ClusterID)),
                             y = as.numeric(XprValue),
                             fill = factor(as.integer(ClusterID))
)) +
  scale_x_discrete(labels= groupLabels)+
  scale_fill_manual(values = c("#FF0000BF", "#A6CEE3" ,  "#FF8000BF" ,"#1F78B4") #use.palette#(rainbow(maxNumClust))
                    ,labels = groupLabels )+
  theme_bw()  +
  facet_wrap(~pvalue) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(size=15,face="bold"))+
  scale_y_continuous() +
  labs(title = "SICAM") +
  geom_violin(#position = "dodge",
    trim = FALSE, scale = "area" ,adjust = 1, na.rm = TRUE
    ,draw_quantiles = TRUE) +
  geom_boxplot(outlier.shape = 19, outlier.size = 0.1, na.rm = TRUE, varwidth = TRUE, width=0.05) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 14)) +
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  annotate("segment", x = xpos, xend = xpos, y = ypos, yend = ypos + 0.1) +
  annotate("segment", x = xpos[1], xend = xpos[2], y = ypos + 0.1, yend = ypos + 0.1, color = "black") +
  #geom_text(aes(x = mean(xpos), y = ypos + 0.3, label = paste("", pvalue)),
            #size = 3.5) +
  theme(legend.position = "none")
SICAMplot

LEPTIN <- read.csv("violinDat_LEPTIN.csv", header = TRUE, sep = ",")
LEPTIN <-as.data.frame(LEPTIN, stringsAsFactors = FALSE)
str(LEPTIN)
#colnames(violinDat)<-c("GeneName","XprValue","ClusterID")
groupLabels <-c("High Urolithin \nHigh BMI", "High Urolithin \nLow BMI", "Low Urolithin \nHigh BMI","Low Urolithin \nLow BMI")


ypos <- max(LEPTIN$XprValue) + 0.5
LEPTIN$pvalue <- "p = 0.06"

LEPTINplot <- ggplot(LEPTIN, aes(x = factor(as.integer(ClusterID)),
                               y = as.numeric(XprValue),
                               fill = factor(as.integer(ClusterID))
)) +
  scale_x_discrete(labels= groupLabels)+
  scale_fill_manual(values = c("#FF0000BF", "#A6CEE3" ,  "#FF8000BF" ,"#1F78B4") #use.palette#(rainbow(maxNumClust))
                    ,labels = groupLabels )+
  theme_bw()  +
  facet_wrap(~pvalue) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(size=15,face="bold"))+
  scale_y_continuous() +
  labs(title = "LEPTIN") +
  geom_violin(#position = "dodge",
    trim = FALSE, scale = "area" ,adjust = 1, na.rm = TRUE
    ,draw_quantiles = TRUE) +
  geom_boxplot(outlier.shape = 19, outlier.size = 0.1, na.rm = TRUE, varwidth = TRUE, width=0.05) +
  ylab("Gene Expression Density (Log10-transformed Expression)") +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 14)) +
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  annotate("segment", x = xpos, xend = xpos, y = ypos, yend = ypos + 0.1) +
  annotate("segment", x = xpos[1], xend = xpos[2], y = ypos + 0.1, yend = ypos + 0.1, color = "black") +
  #geom_text(aes(x = mean(xpos), y = ypos + 0.3, label = paste("", pvalue)),
            #size = 3.5) +
  theme(legend.position = "none")
LEPTINplot


MIP <- read.csv("violinDat_MIP1D.csv", header = TRUE, sep = ",")
MIP <-as.data.frame(MIP, stringsAsFactors = FALSE)
str(MIP)
#colnames(violinDat)<-c("GeneName","XprValue","ClusterID")
groupLabels <-c("High Urolithin \nHigh BMI", "High Urolithin \nLow BMI", "Low Urolithin \nHigh BMI","Low Urolithin \nLow BMI")


ypos <- max(MIP$XprValue) + 0.9
MIP$pvalue <- "p < 0.024"

MIPplot <- ggplot(MIP, aes(x = factor(as.integer(ClusterID)),
                                 y = as.numeric(XprValue),
                                 fill = factor(as.integer(ClusterID))
)) +
  scale_x_discrete(labels= groupLabels)+
  scale_fill_manual(values = c("#FF0000BF", "#A6CEE3" ,  "#FF8000BF" ,"#1F78B4") #use.palette#(rainbow(maxNumClust))
                    ,labels = groupLabels )+
  theme_bw()  +
  facet_wrap(~pvalue) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(size=15,face="bold"))+
  scale_y_continuous() +
  labs(title = "MIP-1D") +
  geom_violin(#position = "dodge",
    trim = FALSE, scale = "area" ,adjust = 1, na.rm = TRUE
    ,draw_quantiles = TRUE) +
  geom_boxplot(outlier.shape = 19, outlier.size = 0.1, na.rm = TRUE, varwidth = TRUE, width=0.05) +
  ylab("Gene Expression Density (Log10-transformed Expression)") +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 14)) +
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  annotate("segment", x = xpos, xend = xpos, y = ypos, yend = ypos + 0.1) +
  annotate("segment", x = xpos[1], xend = xpos[2], y = ypos + 0.1, yend = ypos + 0.1, color = "black") +
  #geom_text(aes(x = mean(xpos), y = ypos + 0.3, label = paste("", pvalue)),
            #size = 3.5) +
  guides(fill = guide_legend(title = "Cluster"))
MIPplot


arranged_violins <- ggarrange(CRPplot, ENA78plot, GLP1plot, LEPTINplot, MIPplot, SICAMplot,
          common.legend = TRUE, legend = "bottom", align = "hv", heights = c(3,3))
arranged_violins

get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}



cowplot::plot_grid(CRPplot, ENA78plot, GLP1plot, LEPTINplot, MIPplot, SICAMplot, ncol = 3)


