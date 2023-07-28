
## Fabio A. Castro-Llanos 
## Geographer / Msc. GIS 
## Apply SDM tunning
## July 26 / 2023

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, zeallot, sf, glue, virtualspecies, tidyverse, rgeos, gtools, stringr, ggspatial, RColorBrewer, SDMtune, rJava, microbenchmark, plotROC, dismo, rgbif)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

# Presences data
prsc <- read_csv('tbl/03_presences.csv')
back <- read_csv('tbl/02_background.csv')

# Climate data
bioc <- terra::rast('tif/bioc_zone.tif')
names(bioc) <- glue('bioc_{1:19}')

# Prepare SWD object ------------------------------------------------------
data <- prepareSWD(species = 'Coffee', p = prsc[,1:2], a = back[,1:2], env = bioc)

# Split presence locations in training (80%) and testing (20%) datasets 
datasets <- trainValTest(data, val = 0.2, test = 0.2, only_presence = TRUE, seed = 25)

c(train, val, test) %<-% trainValTest(data, 
                                      val = 0.2, 
                                      test = 0.2,
                                      only_presence = TRUE, 
                                      seed = 61516)

cat("# Training  : ", nrow(train@data))
cat("\n# Validation: ", nrow(val@data))
cat("\n# Testing   : ", nrow(test@data))

# Train Maxnet / Maxent model with default settings
model <- train('Maxent', data = train)

# Know the tunable arguments
getTunableArgs(model)

# Hyperparameter ----------------------------------------------------------

# Define the values for the regularizazction multiplier
h <- list(reg = seq(0.2, 1, 0.1))

# Call the gridsearch function
exp_1 <- gridSearch(model, hypers = h, metric = 'auc', test = val)


# Static plot / Interactive plot
plot(exp_1, title = "Experiment 1")
plot(exp_1, title = 'Experiment 1', interactive = T)

# SDMTune results
exp_1@results
exp_1@results[order(-exp_1@results$test_AUC), ]

# In the next example we check how the TSS changes varying the regularization multiplier from 1 to 4:

# Define the values for reg
h <- list(reg = 1:4)

# Call the gridSearch function
exp_2 <- gridSearch(model, hypers = h, metric = "tss", test = val)

# Define the values for fc
h <- list(fc = c("l", "lq", "lh", "lqp", "lqph", "lqpht"))

# Call the gridSearch function
exp_3 <- gridSearch(model, 
                    hypers = h, 
                    metric = "auc", 
                    test = val)
exp_3@results

# Maxent model, how to change the AUC varying the number of iterations
maxent_model <- train("Maxent",  data = data)

# Define the values for fc
h <- list("iter" = seq(100, 1100, 200))

# Call the gridSearch function
exp_4 <- gridSearch(maxent_model, 
                    hypers = h, 
                    metric = "auc", 
                    test = val)

exp_4@results

# To create the model again 
model <- train('Maxent', data = train, reg = 0.8, fc =  'lqp', iter = 900)
rstr <- terra::predict(model, bioc)

model <- train('Maxent', data = train, reg = 0.4, fc =  'lqpht', iter = 100)
rstr <- terra::predict(model, bioc)
plot(rstr)

# Tune hyperparameters
h <- list(reg = seq(0.2, 2, 0.2), fc = c("l", "lq", "lh", "lqp", "lqph", "lqpht"))
exp_5 <- gridSearch(model, hypers = h, metric = "auc", test = val)
exp_5@results

# Random search
h <- list(reg = seq(0.2, 5, 0.2), 
          fc = c("l", "lq", "lh", "lp", "lqp", "lqph"))

exp_6 <- randomSearch(model, hypers = h, metric = "auc", test = val, pop = 10, seed = 65466)
exp_6@results

# To optimize the model
exp_7 <- optimizeModel(model, hypers = h, metric = "auc", test = val, pop = 15, gen = 2, seed = 798)
exp_7@results

# To evaluate the final model 

# Index of the best model in the experiment
index <- which.max(exp_6@results$test_AUC)

# New train dataset containing only the selected variables
new_train <- exp_6@models[[index]]@data 

# Merge only presence data
merged_data <- mergeSWD(new_train, val, only_presence = TRUE)

final_model <- train("Maxnet", data = merged_data, fc = exp_6@results[index, 1], reg = exp_6@results[index, 2])
auc(final_model, test = test)

# Hyperparameters tuing with cross validation
# Create the folds from the training dataset
folds <- randomFolds(train, k = 4, only_presence = TRUE, seed = 25)

# Train the model
cv_model <- train("Maxent", data = train, folds = folds)
cv_model <- train("Maxnet", data = train, folds = folds)
cv_model

h <- list(reg = seq(0.2, 5, 0.2),
          fc = c("l", "lq", "lh", "lp", "lqp", "lqph"))

exp_8 <- randomSearch(cv_model,
                      hypers = h,
                      metric = "auc",
                      pop = 10,
                      seed = 65466)

exp_8@results

final_model <- combineCV(exp_8@models[[1]])

auc(final_model, test = test)

rstr <- terra::predict(final_model, bioc)



# Train and test
train <- datasets[[1]]
testn <- datasets[[2]]

# Train a Maxnet model
model <- SDMtune::train(method = "Maxent", data = train)
testn <- test(method = 'Maxent', data = test)

# Importance variable
vi <- varImp(model, permut = 1)
plotVarImp(vi)

# Thresholds
thrs <- thresholds(model = model, type = 'logistic')
auc(model)

