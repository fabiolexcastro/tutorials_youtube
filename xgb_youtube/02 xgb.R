
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, xgboost, hrbrthemes, caret, glue, fs, sf, rnaturalearthdata, rnaturalearth, rmapshaper, fs, tidytext)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Functions ---------------------------------------------------------------

# Function to find a "good" xgb model for each cluster
xgb.model.function <- function(level, run, cv_folds, cv_repeats, min_rsamp, tlength){
  
  fseed <- run ^ 3 #seed defined using variable run
  
  # start time 
  time_start <- Sys.time()
  
  # print current level and run
  cat(crayon::red(paste("starting\n[group: ", level, "]\n[run: run", run, "]\n", sep="")))
  
  # subset data according to selected level
  if(level %in% contn) {
    dbfao_level = dbfao %>% 
      filter(continent == level)
    print(paste("continent == ", level, sep=""))
  } else if(level == "glb") {
    dbfao_level = dbfao
    print(paste("no filter, taking all contries", sep=""))
  } else stop("problem with group selection in xgb.model.function")
  
  #print heads of data to be used
  print(summary(dbfao_level))
  print(dbfao_level %>% arrange(iso3))
  
  # select relevant columns
  data_model <- dbfao_level %>%
    select(df_ha, gdp_perc, food_exports, exports_gdp, food_infl,
           pop_growth, gdp_growth, foreing_invest, tmp_change)
  
  # create training and testing data set
  set.seed(fseed) # set seed for reproducibility
  indata <- createDataPartition(y = data_model$df_ha, p = 0.8)[[1]] # index for testing and training data
  training <- data_model[indata,] 
  testing <- data_model[-indata,]
  
  # trainControl object for repeated cross-validation with random search 
  adaptControl <- trainControl(method = "adaptive_cv",
                               number = cv_folds, 
                               repeats = cv_repeats,
                               adaptive = list(min = min_rsamp,
                                               alpha = 0.05, 
                                               method = "gls",
                                               complete = FALSE),
                               search = "random",
                               verbose=TRUE,
                               allowParallel = TRUE)
  
  # train model
  set.seed(fseed) # set seed for reproducibility
  xgb_model <- train(df_ha ~ ., data = training,
                     method = "xgbTree", 
                     trControl = adaptControl,
                     metric = "RMSE",
                     tuneLength = tlength,
                     na.action = na.pass)
  
  # model summary
  model_summary <- xgb_model$results %>%
    arrange(RMSE) %>%
    head(1) %>%
    transmute(rsquared = Rsquared, rmse = RMSE, eta, max_depth, gamma, colsample_bytree, 
              min_child_weight, subsample, nrounds)
  
  # prepare testing data
  x_test <- select(testing, -df_ha)
  y_test <- testing$df_ha
  
  # apply test data to model
  predicted <- predict(xgb_model, x_test, na.action=na.pass)
  
  # calcuate residuals
  test_residuals <- y_test - predicted
  
  # calculation of rsquare
  tss <- sum((y_test - mean(y_test))^2) #total sum of squares
  rss <- sum(test_residuals^2) #residual sum of squares
  test_rsquared <- 1 - (rss/tss) #rsquare
  
  # calculate root mean square error
  test_rmse <- sqrt(mean(test_residuals^2))
  # calculate mean absolut error
  test_mae <- mean(abs(test_residuals))
  
  # model evaluation
  model_eval <- list(x_test=x_test, y_test=y_test, predicted=predicted, test_residuals=test_residuals, 
                     test_rsquared=test_rsquared, test_rmse=test_rmse, test_mae=test_mae)
  
  # variable importance
  model_var_importance <- 
    xgb.importance(feature_names = dimnames(data_model[,c(-1)])[[2]], model = xgb_model$finalModel) %>%
    transmute(Feature, Gain) %>%
    pivot_wider(names_from = Feature, values_from = Gain, names_prefix = "imp_") %>% 
    transmute(imp_gdb_perc = ifelse(exists("imp_gdp_perc"), imp_gdp_perc, 0),
              imp_food_exports = ifelse(exists("imp_food_exports"), imp_food_exports, 0),
              imp_exports_gdp = ifelse(exists("imp_exports_gdp"), imp_exports_gdp, 0),
              imp_food_infl = ifelse(exists("imp_food_infl"), imp_food_infl, 0),
              imp_gdp_growth = ifelse(exists("imp_gdp_growth"), imp_gdp_growth, 0),
              imp_pop_growth = ifelse(exists("imp_pop_growth"), imp_pop_growth, 0),
              imp_foreing_invest = ifelse(exists("imp_foreing_invest"), imp_foreing_invest, 0),
              imp_tmp_change = ifelse(exists("imp_tmp_change"), imp_tmp_change, 0)
    )
  
  # save model settings
  model_settings <- data.frame(level=level, seed=fseed, cv_folds=cv_folds, cv_repeats= cv_repeats, 
                               min_rsamp=min_rsamp, tlength=tlength)
  
  # prepare output
  output <- list(xgb_model, model_settings, model_summary, model_eval, model_var_importance, training, testing)
  names(output) <- c(paste("xgb_model", level, sep = "_"),
                     paste("model_settings", level, sep = "_"),
                     paste("model_summary", level, sep = "_"),
                     paste("model_evaluation", level, sep = "_"),
                     paste("model_var_importance", level, sep = "_"),
                     paste("data_training", level, sep = "_"),
                     paste("data_testing", level, sep = "_"))
  
  # stop time
  time_end <- Sys.time()
  time_run <- time_end-time_start
  
  # print run time
  cat(crayon::green(paste("[model run time: ", round(time_run, 2), "]\n", sep="")))
  
  # write time file
  run_time <- data.frame(time_start, time_end, time_run, level, run)
  
  write.table(run_time, file.path(path_output, "run_time.csv"), 
              sep = ",", col.names = FALSE, append=TRUE, row.names = FALSE)
  
  #return xgb output
  assign(paste("output", level, fseed, sep = "_"), output)
  
}

# function to iterate over levels (clusters, geographical regions, continents)
xgb.run.function <- function(level, run, cv_folds, cv_repeats, min_rsamp, tlength){
  group <- ifelse(identical(level, contn), "contn",
                  ifelse(identical(level, glb), "glb", 
                         stop("problem with group selection in xgb.run.function")))
  
  run_x <- lapply(level, xgb.model.function, run, cv_folds, cv_repeats, min_rsamp, tlength)
  names(run_x) <- level
  saveRDS(run_x, file=file.path(path_output, paste(group, "_run", run, ".RDS", sep="")))
}

# Parameters --------------------------------------------------------------
cv_folds = 10; cv_repeats = 10; min_rsamp = 5; tlength = 100 # Full
cv_folds = 2; cv_repeats = 3; min_rsamp = 5; tlength = 6 # Testing
nrun <- 5

# Load data ---------------------------------------------------------------
dbfao <- read_csv('./inp/database.csv')
path_output <- './out/run_1'
varbl <- c('df_ha', 'gdp_perc', 'food_exports', 'exports_gdp', 'food_infl',
           'pop_growth', 'gdp_growth', 'foreing_invest', 'tmp_change')

# Data preparation --------------------------------------------------------
contn <- dbfao %>%
  select(iso3, continent) %>%
  unique() %>%
  group_by(continent) %>%
  dplyr::summarise(n = n())

# Define variable for whole world (no filtering)
glb <- "glb"

# Run model ---------------------------------------------------------------
write.table(Sys.time(), file.path(path_output, "run_time.csv"), sep = ",", row.names = FALSE)

# run with level glb
nrun <- 5
level <- glb
lapply(c(1:nrun), xgb.run.function, level = level, cv_folds = cv_folds, cv_repeats = cv_repeats, min_rsamp = min_rsamp, tlength = tlength) # Normalizado

# run with lemvel contn
level <- contn
lapply(c(1:nrun), xgb.run.function, level = level, cv_folds = cv_folds, cv_repeats = cv_repeats, min_rsamp = min_rsamp, tlength = tlength)

# save run file and save ending time
write.table(Sys.time(), file.path(path_output, "run_time.csv"), sep = ",", col.names = FALSE, append=TRUE, row.names = FALSE)


