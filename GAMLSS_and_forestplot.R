library(dplyr)
library(ggplot2)
library(gamlss)
library(tidyr)

quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

forest_plot <- function(x) {
  ggplot(x, aes(x = coef, y = aab, color = sig)) +
    geom_point(size = 2) +
    geom_errorbarh(aes(xmin = min, xmax = max), height = 0.25) +
    geom_vline(xintercept = 0, color = "#000000", linetype = "dashed", cex = 0.2, alpha = 1)+
    facet_wrap(~ID, nrow = 1, ncol = 6, scales = "free_x")+
    xlab("Regression Coefficient")+
    theme(axis.line = element_line(colour = "black"),
          axis.title.y=element_blank(),
          panel.background = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.text.y = element_text(size = 13),
          axis.text.x = element_text(size = 10),
          strip.background = element_rect(fill="white", colour = "white", size = 0.01),
          legend.position="none",
          panel.spacing.x = unit(1,"line"),
          text = element_text(size = 15))+
    scale_colour_manual(values=c("#270082", "#C5C4BE", "#FF0000"))
}

#import data and data manipulation
data =openxlsx::read.xlsx("regression.xlsx", rowNames = FALSE)
data$gender = as.factor(ifelse(data$gender == 'f', 0, 1))

#### Loop to statistical analysis with table and graph saving 
l = lapply(colnames(data)[!(colnames(data) %in% covariables)], function(i){
  cat("modelo = ", i, "-----------------------------------------------------------------------------------", "\n")
  df = na.omit(data[c(covariables,i)])
  k = which(colnames(df) == i)
  y = df[, k]
  mod_1 <- gamlss(y ~ group + age + gender + time_m, data = df, family = LOGNO)
  
  if (!is.character(mod_1)){
    png(paste0(outDir, i, ".png"), bg = "transparent", width = 30, height = 20, units = "cm", res = 150)
    quiet(plot(mod_1))
    dev.off()
    
    r = quiet(capture.output(summary(mod_1)))
    r_1 = quiet(summary(mod_1))
    r_2 = r_1[c(2, 3, 4, 6), c(1, 4)]
    write.table(r, paste0(outDir, i, ".txt"), sep = ",", row.names = FALSE, quote = FALSE)
    
    conf = confint(mod_1)
    coef = coef(mod_1)
    
    df$pred = predict(object = mod_1, type = "response", what = "mu", data = df)
    colnames(df)[k] = "y"
  } else{
    mod_1 = 'erro'
    df = 'erro'
    pred = 'erro'
  }
  
  return(list(mod = mod_1, df = data.frame(df, aab = i), pvalor = data.frame(r_1, aab = i), conf = data.frame(conf,aab = i),  coef = data.frame(coef, aab = i)))
})

l[[2]]

df1 = do.call(rbind, lapply(l, "[[", 2))
df2 = do.call(rbind, lapply(l, "[[", 4))
df3 = do.call(rbind, lapply(l, "[[", 5))
df4 = do.call(rbind, lapply(l, "[[", 3))

cint = df2[-grep("Intercept", rownames(df2)),]
coef = df3[-grep("Intercept", rownames(df3)),]
pval = df4[-grep("Intercept", rownames(df4)),]

forest <- cbind(cint, coef$coef)
colnames(forest) <- c("min","max","aab", "coef")
forest$sig <- ifelse(forest$min > 0, 'up', ifelse(forest$max < 0, 'down', 'ns'))
forest$pval <- pval$Pr...t..
forest$sig2 <- ifelse(forest$pval < 0.0001, '****', ifelse(forest$pval < 0.001, '***', ifelse(forest$pval < 0.01, '**', ifelse(forest$pval < 0.05, '*', 'ns'))))
forest$ID <- rep(c("group_HC", "group_PCHC", "group_PCS/non-ME/CFS", "Age", "Gender", "Time"),20)

write.csv(forest, file.path(outDir,"forest.csv"))

#####Forest plot-----
forest <- read.csv("forest.csv", sep = ",")

library(ggplot2)

forest_plot(forest)
ggsave("forest_plot_reg.png", dpi = 600, width = 31, height = 15, units = "cm")

forest_confounders <- subset(forest, ID == "Age" | ID == "Gender" | ID == "Time")

forest_plot(forest_confounders)
ggsave("forest_plot_reg_confounders.png", dpi = 600, width = 31, height = 15, units = "cm")