#Import data as my_data
# script's parameters ----------------------------------------------------------
# you can change the parameters below according to your needs

glog_data <- FALSE # set to TRUE if you want to transform the data using glog

std_data <- TRUE # set to TRUE if you wnat to standardize the data

redux_method <- "none" # choose between "none", "PCA", "FA"

redux_n_dim <- 4 # number of dimensions to retain in PCA or FA

# random forest related parameters
n_varibles_in_impt_plots <- 10 # choose number of variables to show plots

n_trees <- 5000 # initial number of trees

# graphics related parameters
width_factor <- 2 # increase width_factor to save plots with larger width

height_factor <- 2 # increase height_factor to save plots with larger height

color_ramp <- RColorBrewer::brewer.pal(n = 7, name = "PuBuGn") # heatmap color
#RdYlBu
color_border <- grey(0.4) # heatmap border color

fontsize <- 8 # heatmap font size

# load sources -----------------------------------------------------------------
source("packages.R")) # load packages
source("functions.R")) # load functions

# seed -------------------------------------------------------------------------
set.seed(2022) # seed for replicate the replicating the analysis

# load dataset -----------------------------------------------------------------
n <- ncol(my_data) # number of columns

if (glog_data) my_data[, -n] <- LogSt(my_data[, -n]) # apply glog
if (std_data) my_data[, -n] <- scale(my_data[, -n]) # standardize data

# descriptive measures ---------------------------------------------------------
# basic data struture
desc_data <- c(
  n_obs = nrow(my_data), n_vars = ncol(my_data[, -n]),
  n_in_group = table(my_data$group)
)

desc_stat <- mvn(my_data[, -n])$Descriptives # standard descriptive statistics

desc_cor <- cor(my_data[, -n], method = "spearman") # spearman correlation

# apply none/PCA/FA method according to redux_method ---------------------------
# perform data reduction if redux_method != "none"
redux_data <- switch(redux_method,
                     "none" = list(data = my_data, fit = "none"),
                     "PCA" = {
                       my_pca <- pca(my_data[, -n], redux_n_dim, rotate = "varimax")
                       my_data <- data.frame(my_pca$scores, Group = my_data[, n])
                       list(data = my_data, fit = fa.sort(my_pca))
                     },
                     "FA" = {
                       my_fa <- fa(my_data[, -n], redux_n_dim, rotate = "varimax")
                       my_data <- data.frame(my_fa$scores, Group = my_data[, n])
                       list(data = my_data, fit = fa.sort(my_fa))
                     }
)

my_data <- redux_data$data # data to be used in random forest

# plot loading matrix if redux_method != "none"
if (redux_method != "none") {
  cat("(Print) results for ", redux_method, "\n\n")
  print(redux_data$fit)
  cat("\n\n")
  
  # reset some parameters due to PCA or FA
  n <- ncol(my_data)
  k <- n_varibles_in_impt_plots
  n_varibles_in_impt_plots <- min(k, redux_n_dim)
  if (k != n_varibles_in_impt_plots) {
    cat("(Print) n_varibles_in_impt_plots was changed to ", redux_n_dim, "\n\n")
  }
  
  redux_fit <- loadings(redux_data$fit) # loading matrix
  redux_sorted_load <- matrix(unlist(redux_fit), ncol = redux_n_dim)
  rownames(redux_sorted_load) <- rownames(redux_fit)
  colnames(redux_sorted_load) <- colnames(redux_fit)
  
  redux_vars <- rownames(redux_fit) # variables
  redux_n <- seq_len(redux_n_dim) # factors
  
  # dataset for ggplot
  redux_df <- data.frame(
    Loading = c(redux_fit),
    Var = rep(redux_vars, redux_n_dim),
    Factor = paste0("Factor ", rep(redux_n, each = nrow(redux_fit)))
  )
  redux_df$Var <- factor(redux_df$Var,
                         levels = rownames(redux_fit),
                         ordered = TRUE
  )
  
  p_redux <- ggplot(redux_df, aes(x = Var, y = Loading, fill = Loading)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    facet_wrap(~Factor, nrow = 1) +
    scale_fill_gradient2(
      name = "Loading: ", high = "blue", mid = "white",
      low = "red", midpoint = 0, guide = "none"
    ) +
    theme_light(base_size = 12) +
    theme(
      legend.position = "top",
      legend.box.background = element_rect(colour = "black", fill = NA),
      panel.grid.minor.x = element_blank(),
      strip.text = element_text(face = "bold", colour = "black"),
      strip.background = element_rect(fill = grey(0.9), colour = "black")
    ) +
    labs(x = "", y = "Loading strength")
}

# split dataset into training and testing samples ------------------------------
data_split <- initial_split(my_data, prop = 0.75, strata = "group")
sample_train <- training(data_split)
sample_test <- testing(data_split)

# create a 'recipe' object -----------------------------------------------------
sample_recipe <- recipe(group ~ ., data = sample_train)
sample_prep <- sample_recipe %>% prep(training = sample_train, retain = TRUE)

# tune random forest hyperparameters (mtry) ------------------------------------
invisible(capture.output(
  mtry <- tuneRF(sample_train[, -n], as.factor(sample_train$group),
                 ntreeTry = n_trees, stepFactor = 1.5, improve = 0.01,
                 trace = FALSE, plot = FALSE
  )
))
m <- mtry[mtry[, 2] == min(mtry[, 2]), 1][1] # best value of mtry

# fit random forest model ------------------------------------------------------
rf <- rand_forest(trees = n_trees, mtry = m, mode = "classification") %>%
  set_engine("randomForest", importance = TRUE, localImp = TRUE) %>%
  fit(group ~ ., data = juice(sample_prep))

print(rf$fit) #show basic results for the random forest model

#Saving .txt--------------------------------------------
seed_message <- sprintf("Seed used for this analysis: %s", 2022)
write.table(print(seed_message), file = file.path(outDir, "seed.txt"), row.names = F, col.names = F, quote = F)
write.table(rf$fit$confusion, file = file.path(outDir, "confusion_training.txt"), row.names = T, col.names = NA, quote = F)


# defining colors for each group
groups <- levels(sample_test$group)
#g_legend <- c("HC","PCHC", "PCS/non-ME/CFS", "PCS/ME/CFS")
#group_color <- c("#C5C4BE", "#EF813E", "#95B250", "#42655D")
g_legend <- c("Healthy", "PCS patients")
group_color <- c("#C5C4BE", "#EF813E")

png(file = file.path(outDir, "ROC.png"), bg = "transparent", width = 1500, height = 1500, units = "px", res = 300)
plot(rf$fit, main = "Error rates", col = c("black", group_color)) # error rate
legend("topright",
       legend = c("OOB", g_legend), col = c("black", group_color), lty = 1) # legends
dev.off()

# evaluate the model -----------------------------------------------------------
# roc curves
pred_for_roc_curve <- predict(rf, sample_test[, -n], type = "prob")

auc <- rep(NA, length(groups)) # vector for holding values of area under ROC
names(auc) <- g_legend
auc

png(file = file.path(outDir, "ROC_Curve_rc.png"), bg = "transparent", width = 1500, height = 1500, units = "px", res = 300)
for (i in seq_len(length(groups))) {
  # Define which observations belong to class[i]
  true_values <- sample_test$group == groups[i]
  # Assess the performance of classifier for class[i]
  pred <- prediction(pred_for_roc_curve[, i], true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i == 1) {
    plot(perf, main = "ROC Curve", col = group_color[i])
  }
  else {
    plot(perf, col = group_color[i], add = TRUE)
  }
  if (i == length(groups)){
    #abline(a=1, b=1, lty=2)
    lines(c(0,1),c(0,1), lty=2, cex=1)
  }
  
  # Calculate the area under the curve (AUC) and print it to screen
  auc[i] <- unlist(performance(pred, measure = "auc")@ y.values)
}
legend("bottomright", legend = c(g_legend, "Random Classifier"), col = c(group_color, "black"), lty = c(1,1,2)) # legends
dev.off()

# confusion matrix
pred_for_table <- predict(rf, sample_test[, -n])

confusion_mat <- table(
  observed = sample_test[, n],
  predicted = unlist(pred_for_table)
)

cat("(Print) confusion matrix based on testing samples\n\n")
confusion_mat
#Export txt
write.table(print(confusion_mat), file = file.path(outDir, "confusion_test.txt"), row.names = T, col.names = NA, quote = F)
write.table(print(auc), file = file.path(outDir, "auc.txt"), row.names = T, col.names = F, quote = F)

# plot main results ------------------------------------------------------------
# tree with least number of nodes
tree_num <- which(rf$fit$forest$ndbigtree == min(rf$fit$forest$ndbigtree))[1]
p_rf_tree <- tree_func(final_model = rf$fit, tree_num)

# measures of variable importance for the fitted random forest model -----------
cat("\n(This process can take a while) please wait ...\n\n")

impt_measures <- measure_importance(rf$fit)

# chosing best set of importance measures to use
p_choose_imp_1 <- plot_importance_ggpairs(impt_measures)
p_choose_imp_2 <- plot_importance_rankings(impt_measures)

png(file = file.path(outDir, "impt1.png"), bg = "transparent", width = 4200, height = 3700, units = "px", res = 300)
print(p_choose_imp_1)
dev.off()

png(file = file.path(outDir, "impt2.png"), bg = "transparent", width = 4200, height = 3700, units = "px", res = 300)
print(p_choose_imp_2)
dev.off()

# !!! STOP HERE AND EVALUATE THE RESULTS BEFORE CHOOSING THE THREE MEASURES !!!

# define your chosen measures replacing NULL by the measure' name
first_measure <- "gini_decrease" # ex: first_measure <- "no_of_trees"
second_measure <- "no_of_nodes" # ex: second_measure <- "no_of_nodes"
third_measure <- "mean_min_depth" #mean_min_depth # ex: third_measure <- "mean_min_depth"

cat("(Again ... this process can take a while) please wait ...\n\n")

# test if user has chosen the three importance measures
if (is.null(first_measure) | is.null(first_measure) | is.null(first_measure)) {
  stop("You did not choose the three inportance meansures...
        please start the hole script again")
}

# plot the chosen measures
p_imp <- plot_multi_way_importance(impt_measures,
                                   x_measure = first_measure,
                                   y_measure = second_measure,
                                   size_measure = third_measure,
                                   no_of_labels = n_varibles_in_impt_plots
)+ theme(text = element_text(size = 18), axis.text = element_text(size = 18))    

# plot variable depth distribution
#dev.new()
min_depth <- min_depth_distribution(rf$fit)
p_min_depth <- plot_min_depth_distribution(min_depth, mean_sample = "top_trees")+
  theme(text = element_text(size = 18), axis.text = element_text(size = 18)) + 
  scale_fill_manual(values = colorRampPalette(c("#a8ddb5", "gray70"))(length(unique(min_depth$minimal_depth)))) #era ncol(my_data)-1)); era ncol(my_data)

png(file = file.path(outDir, "min_depth_xwide.png"), bg = "transparent", width = 3500, height = 2500, units = "px", res = 300)
p_min_depth
dev.off()

# plot interaction between pairs of variables in the random forest
impt_vars <- important_variables(impt_measures,
                                 k = n_varibles_in_impt_plots,
                                 measures = c(first_measure, second_measure, third_measure))    

# level of interaction between variables (min depth interacitons)
interaction_vars <- min_depth_interactions(rf$fit, impt_vars)
p_interaction <- plot_min_depth_interactions(interaction_vars)+
  theme(text = element_text(size = 12), axis.text = element_text(size = 9))

#I generate p_interaction_sub in order to use the x axis title (it is cropped in the above plot)
p_interaction_sub <- plot_min_depth_interactions(interaction_vars)+
  theme(text = element_text(size = 12), axis.text = element_text(size = 9))+
  expand_limits(x = c(-3, 25), size=1)

png(file = file.path(outDir, "interaction_sub.png"), bg = "transparent", width = 2400, height = 1800, units = "px", res = 300)
p_interaction_sub 
dev.off()
png(file = file.path(outDir, "interaction.png"), bg = "transparent", width = 2300, height = 1800, units = "px", res = 300)
p_interaction 
dev.off()

ggsave(
  filename = file.path(outDir, "S2.png"),
  plot = p_rf_tree,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm")
ggsave(
  filename = file.path(outDir, "multiway_plot.png"),
  plot = p_imp,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm")



# generate outputs ----------------------------------------------------------
suppressWarnings(suppressMessages(source("outputs.R"))))

###Heatmap para confusion matrix ====
library(pheatmap)
x = matrix(rf$fit$confusion[,-ncol(rf$fit$confusion)], 2,2, byrow = F)
p <- pheatmap(x, cluster_cols = FALSE, cluster_rows = FALSE, angle_col = 90, 
              labels_row = c("Healthy","PCS patients"), 
              labels_col = c("Healthy","PCS patients"), display_numbers = x, 
              color = colorRampPalette(c("#eeeeee", "#1fb876", "#427dc9"))(100), fontsize = 18, fontsize_number = 20, na_col = "white", border_color = "white")

png(file = file.path(outDir, "heatmap_confusion_training.png"), bg = "transparent", width = 1500, height = 1500, units = "px", res = 300)
p
dev.off()