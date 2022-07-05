#Loading the libraries and preparing the directories
library(ggpubr)
library(openxlsx)
library(rstatix)
library(lemon)

mainDir <- ("E:/Artigos/autoanticorpos - CFSME/data") #where data is
outDir <- ("E:/Artigos/autoanticorpos - CFSME/boxplot/fdr") #where outputs are going to
dir.create(outDir, recursive = T)

#Import the table as a data frame
tab_t <- read.xlsx(file.path(mainDir, "original_table.xlsx")) #read file from mainDir
groups <- rep(c("PCS/ME/CFS","PCS/non-ME/CFS", "PCHC", "HC"), c(40, 40, 40, 38)) #Number of individuals and respective group names

###Rearanging the dataframe----
#This section is rearranging a table that is in the format: gene|...|gene in the columns and sample|...|sample| in the rows to a dataframe that is groups|counts|gene
#if you use these lines, check the df produced to see if it matches your own
table <- as.data.frame(cbind(groups, tab_t[,3], rep(colnames(tab_t[3]),158))) # tab_t['first gene column/first desired variable'], 'number of observations'

for(i in c(4:22)){ #i in c('second desired variable':'last desired variable')
  table_temp <- as.data.frame(cbind(groups, tab_t[,i], rep(colnames(tab_t[i]),158))) 
  table <- rbind(table, table_temp)
}
colnames(table) <- c("group", "counts", "gene")

### Plotting ----
table$counts <- as.numeric(table$counts)
table$counts <- log2(table$counts)
table$group <- factor(table$group, levels=c("HC","PCHC", "PCS/non-ME/CFS", "PCS/ME/CFS"))

bxp <- ggboxplot(table, x = "group", y = "counts",
               color = "group", palette = c("#C5C4BE", "#EF813E", "#95B250", "#42655D"),
               add = c("jitter", "median_iqr"),
               add.params = list(size = 0.5))+
  scale_y_continuous(breaks = c(1,3,5,7), #Can use pretty here, as it automates the values: (breaks = pretty(c(1,5), n=4))
                     limits = c(0,9), name=expression(log[2] (Units/mL)))+ # y scale + axis' name
  facet_rep_wrap(~gene, strip.position="bottom", ncol=5)+ #if you have more than 2 lines, use ( facet_rep_wrap(~gene, strip.position="bottom") ) instead of facet_Wrap
  stat_compare_means(method= "kruskal.test", label.y = 0.1, size = 3, label.x = 1.2, #select which statistical test you want
                     aes(label = ifelse(p < 1.e-2, #If the number is lower than 0.01:
                                        sprintf("KW p = %2.1e", as.numeric(..p.format..)), #it prints as scientific notation 
                                        sprintf("KW p = %5.3f", as.numeric(..p.format..)))))+ #or as float .xyz (.3f)
  theme(strip.text.x = element_text(size = 10))+
  theme(axis.line.x=element_line(), axis.ticks.x=element_blank(), axis.title.x=element_blank(), axis.text.x=element_blank(),
        strip.background = element_rect(color="white", fill="white"), strip.placement = "outside",
        panel.border = element_blank(), panel.background = element_blank(), panel.grid = element_blank(), 
        panel.spacing.x = unit(0,"line"), panel.spacing.y = unit(0,"line"))

stat.test1 <- table %>%
  df_group_by(gene) %>%
  dunn_test(counts ~ group) %>%
  adjust_pvalue(method = "fdr") %>%
  add_xy_position()

write.xlsx(stat.test1[,c(1,3,4,7,8,9,10)], file.path(outDir, "Sup_pval_table.xlsx"), overwrite = T)
stat.test1$y.position <- rep(c(7,7.6,8.2,7,8.8,7),20) #defining what are the brackets for statistical difference being plotted
stat.test1$xmin <- stat.test1$xmin+0.05 #this + 62 allow the brackets to be placed side by side without touching eachother
stat.test1$xmax <- stat.test1$xmax-0.05
stat.test1 <- stat.test1 %>%
  filter(p.adj.signif != "ns") #removes every row that has non significant adj p values (keeps the plot cleaner)

bxp <- bxp + stat_pvalue_manual(stat.test1,  label = "p.adj.signif", tip.length = 0.005)

png(file = file.path(outDir, "boxplot_kruskal_fdr.png"), bg = "transparent", width = 25, height = 20, units = "cm", res = 300) #this saves everything plotted until you run dev.off()
bxp
dev.off()