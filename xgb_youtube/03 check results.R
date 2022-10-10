
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, xgboost, hrbrthemes, caret, glue, fs, sf, showtext, rnaturalearthdata, rnaturalearth, rmapshaper, fs, tidytext)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)


# List the results --------------------------------------------------------
dbse <- suppressMessages(read_csv('./inp/database.csv')[,-1])
path_output <- './out/run_1'

# Global results
glb <- dir_ls(path_output, regexp = 'RDS') %>% grep('glb', ., value = TRUE) %>% as.character() %>% purrr::map(., readRDS)

# Check the varnames 
vrs <- glb[[1]]$glb$data_training_glb %>% colnames()

# Variabels 
lbels <- tibble(abb = c('df_ha', 'rsquared', 'gdb_perc', 'food_exports', 'exports_gdp', 'food_infl',
                        'pop_growth', 'gdp_growth', 'foreing_invest', 'tmp_change'),
                nme = c('Deforestation', 'R squared', 'GDP percapita', 'Food exports', 'Exports GDP', 'Food inflation',
                        'Population growth', 'GDP growth', 'Foreing invest', 'Temperatrure change'), 
                nombre = c('Deforestación', 'R^2', 'PIB Percápita', 'Exportación alimentos', 'Exportaciones PIB', 'Inflación de los alimentos', 
                           'Crecimiento de la población', 'Crecimiento del PIB', 'Inversión extranjera', 'Cambio en la temperatura'), 
                units = c('Ha', '%', 'US$', '%', '% of GDP', '%', '%', '% anual', 'Billion (US$)', 'C'))

lbels

# Importance
prc <- list()
for(i in 1:5){
  prc[[i]] <- glb[[i]]$glb$model_var_importance_glb
}
prc <- bind_rows(prc) %>% mutate(run = LETTERS[1:5]) %>% gather(variable, value, -run) %>% mutate(run = factor(run, levels = LETTERS[1:5]))
prc %>% spread(run, value)

# RSquared
rsq <- tibble(run = LETTERS[1:5], rsquared = unlist(map(.x = 1:5, .f = function(i)glb[[i]]$glb$model_summary_glb[1,1])))
rsq <- mutate(rsq, rsquared = round(rsquared, 2))
rsq <- transmute(rsq, value = rsquared, run, variable = 'rsquared')
rsq <- dplyr::select(rsq, run, variable, value)

# Join importance table and rsquared table into only one table
all <- rbind(prc, rsq)
unique(all$variable)
unique(lbels$abb)
all <- mutate(all, 
              variable = factor(variable, 
                                levels = unique(all$variable)))
all <- all %>% group_by(variable) %>% dplyr::summarise(min = min(value), avg = mean(value), max = max(value))
all <- mutate(all, variable = gsub('imp_', '', variable))
all <- inner_join(all, lbels, by = c('variable' = 'abb'))

write.csv(all, './out/results_run1.csv', row.names = FALSE)


