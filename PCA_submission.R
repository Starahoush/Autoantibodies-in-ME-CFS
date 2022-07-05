#Loading libraries and preparing directories

library(factoextra)
library(ggplot2)
library(scales)
library(openxlsx)

mainDir <- ("blob")
outDir <- ("blob")
#Import the table as a data frame and prepare it

PCAf <- read.xlsx(file.path(mainDir, "PCA.xlsx"), rowNames = T)
res.pca <- prcomp(PCA_log, scale = TRUE)

#Scree plot
pca.var <- res.pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
scp <- barplot(pca.var.per, main="Scree Plot", 
               xlab="Principal Component", ylab="Percentage of Variation",
               col = "#79a8e0", border = F)

ggplot()+
  geom_bar(aes(c(1:length(pca.var.per)), pca.var.per), stat='identity', colour = "white", fill = "#79a8e0")+
  ylab("Percentage of Explained Variation")+
  xlab("Principal Component")+
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 20))
ggsave(file.path(outDir, "log_scp.png"), dpi = 600, units = "cm", width = 20, height = 20)

#####plot eigenvalues---------
fviz_eig(res.pca)
eigen<-get_eig(res.pca)
ggplot()+
  geom_line(aes(c(1:length(eigen$eigenvalue)), eigen$eigenvalue))+
  geom_point(aes(c(1:length(eigen$eigenvalue)), eigen$eigenvalue), 
             colour = c(ifelse(eigen$eigenvalue>1, "red", "black")),
             size = 2)+
  geom_hline(yintercept = 1)+
  scale_y_continuous(breaks = c(1,3,5,7,9))+
  ylab("Eigenvalue")+
  xlab("Principal Component")+
  theme(panel.background = element_blank(),
        text = element_text(size = 20))
ggsave(file.path(outDir, "log_eigen.png"), dpi = 600, units = "cm", width = 20, height = 20)

####plot var contrib-----
var_cont <- get_pca_var(res.pca)
var <- as.data.frame(var_cont$contrib)
var$aab <- rownames(var)
variab <- as.data.frame(reshape::melt(var))

variabbb <- variab[1:80,]
unique(variabbb$variable)
ggplot(variabbb, aes(x=value, y=aab))+
  geom_col(fill = ifelse(variabbb$value>5, "#79a8e0", "black"))+
  geom_vline(xintercept = 5, linetype = "dashed", colour = "black")+
  xlab("Contributions (%)")+
  ylab("Autoantibody")+
  facet_wrap(~variable, nrow = 1, ncol = 4)+
  theme(text = element_text(size = 15),
        strip.background = element_rect(fill="white", colour = "white", size = 0.01),
        panel.background = element_rect(fill = "white", colour = "gray"),
        panel.grid.major = element_blank())
ggsave(file.path(outDir, "log_contrib.png"), dpi = 600, units = "cm", width = 30, height = 20)

fviz_contrib(res.pca, choice = "var", axes = 4, top = 10)
### Preparing plots of the first 4 PCs ----
p12 <- fviz_pca_ind(res.pca,
                    axes = c(1,2),
                    col.ind = groups, # color by groups
                    geom = c("point"),
                    pointsize = 2.5,
                    palette = c("#C5C4BE", "#EF813E", "#95B250", "#42655D"),
                    addEllipses = TRUE, # Concentration ellipses
                    ellipse.type = "confidence",
                    legend.title = "Groups",
                    repel = TRUE) + 
  theme(legend.position = "bottom") + ggtitle("PCA") + 
  theme(text = element_text(size = 18), axis.text = element_text(size = 18))
p12
ggsave(file.path(outDir, "log_12.png"), dpi = 600, units = "cm", width = 20, height = 20)

p13 <- fviz_pca_ind(res.pca,
                    axes = c(1,3),
                    col.ind = groups, # color by groups
                    geom = c("point"),
                    pointsize = 2.5,
                    palette = c("#C5C4BE", "#EF813E", "#95B250", "#42655D"),
                    addEllipses = TRUE, # Concentration ellipses
                    ellipse.type = "confidence",
                    legend.title = "Groups",
                    repel = TRUE) + 
  theme(legend.position = "bottom") + ggtitle("PCA") + 
  theme(text = element_text(size = 18), axis.text = element_text(size = 18)) 
p13
ggsave(file.path(outDir, "log_13.png"), dpi = 600, units = "cm", width = 20, height = 20)

p23 <- fviz_pca_ind(res.pca,
                    axes = c(2,3),
                    col.ind = groups, # color by groups
                    geom = c("point"),
                    pointsize = 2.5,
                    palette = c("#C5C4BE", "#EF813E", "#95B250", "#42655D"),
                    addEllipses = TRUE, # Concentration ellipses
                    ellipse.type = "confidence",
                    legend.title = "Groups",
                    repel = TRUE
) + theme(legend.position = "bottom") + ggtitle("PCA") + 
  theme(text = element_text(size = 18), axis.text = element_text(size = 18)) 
p23
ggsave(file.path(outDir, "log_23.png"), dpi = 600, units = "cm", width = 20, height = 20)

p14 <- fviz_pca_ind(res.pca,
                    axes = c(1,4),
                    col.ind = groups, # color by groups
                    geom = c("point"),
                    pointsize = 2.5,
                    palette = c("#C5C4BE", "#EF813E", "#95B250", "#42655D"),
                    addEllipses = TRUE, # Concentration ellipses
                    ellipse.type = "confidence",
                    legend.title = "Groups",
                    repel = TRUE
) + theme(legend.position = "bottom") + ggtitle("PCA") + 
  theme(text = element_text(size = 18), axis.text = element_text(size = 18)) 
p14
ggsave(file.path(outDir, "log_14.png"), dpi = 600, units = "cm", width = 20, height = 20)

p24 <- fviz_pca_ind(res.pca,
                    axes = c(2,4),
                    col.ind = groups, # color by groups
                    geom = c("point"),
                    pointsize = 2.5,
                    palette = c("#C5C4BE", "#EF813E", "#95B250", "#42655D"),
                    addEllipses = TRUE, # Concentration ellipses
                    ellipse.type = "confidence",
                    legend.title = "Groups",
                    repel = TRUE
) + theme(legend.position = "bottom") + ggtitle("PCA") + 
  theme(text = element_text(size = 18), axis.text = element_text(size = 18)) 
p24
ggsave(file.path(outDir, "log_24.png"), dpi = 600, units = "cm", width = 20, height = 20)

p34 <- fviz_pca_ind(res.pca,
                    axes = c(3,4),
                    col.ind = groups, # color by groups
                    geom = c("point"),
                    pointsize = 2.5,
                    palette = c("#C5C4BE", "#EF813E", "#95B250", "#42655D"),
                    addEllipses = TRUE, # Concentration ellipses
                    ellipse.type = "confidence",
                    legend.title = "Groups",
                    repel = TRUE
) + theme(legend.position = "bottom") + ggtitle("PCA") + 
  theme(text = element_text(size = 18), axis.text = element_text(size = 18)) 
p34
ggsave(file.path(outDir, "log_34.png"), dpi = 600, units = "cm", width = 20, height = 20)

fviz_pca_biplot(res.pca,
                col.ind = groups, # color by groups,
                geom = c("point"),
                pointsize = 3,
                col.var = "black",
                alpha.var = 0.3,#transparencia
                palette = c("#C5C4BE", "#EF813E", "#95B250", "#42655D"),
                addEllipses = TRUE, # Concentration ellipses
                ellipse.type = "confidence",
                legend.title = "Groups: ",
                repel = TRUE,
                labelsize = 5) +
  theme(legend.position = "bottom") + ggtitle("")+ theme(text = element_text(size = 18), axis.text = element_text(size = 18)) 
ggsave(file.path(outDir, "log_biplot.png"), dpi = 600, units = "cm", width = 20, height = 20)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#95B250", "#2a64b0"),
             alpha.var = 0.4,
             repel = TRUE,
             labelsize = 5)+
  theme(text = element_text(size = 18), 
        axis.text = element_text(size = 20),
        panel.grid = element_blank())
ggsave(file.path(outDir, "log_var.png"), dpi = 600, units = "cm", width = 20, height = 20)