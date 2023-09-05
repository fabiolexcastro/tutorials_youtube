
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, readxl, xgboost, hrbrthemes, caret, glue, fs, sf, showtext, rnaturalearthdata, rnaturalearth, rmapshaper, fs, tidytext)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Fonts -------------------------------------------------------------------
font_add(family = "Bahnschrift", regular = "Bahnschrift.ttf")
showtext_auto()

# List the results --------------------------------------------------------
dbse <- suppressMessages(read_excel('./tbl/dabase_global_total.xlsx')[,-1])
path_output <- './out/run_2'

# Global results
glb <- dir_ls(path_output, regexp = 'RDS') %>% grep('glb', ., value = TRUE) %>% as.character() %>% purrr::map(., readRDS)

# Check the varnames 
vrs <- glb[[1]]$glb$data_training_glb %>% colnames()

# Variabels 
lbels <- tibble(abb = c('df_ha', 'rsquared', 'pop_growth', 'food_imports', 'food_exports', 'foreing_invest',
                        'agricultural_land', 'food_inflation'),
                nme = c('Deforestation', 'R squared', 'Population growth', 'Food imports', 'Food exports', 'Foreing invest',
                        'Agricultural land', 'Food inflation'), 
                nombre = c('Deforestación', 'R^2', 'Crecimiento poblacional', 'Importación alimentos', 'Exportación alimentos', 'Inversión extranjera',
                           'Superficie agrícola', 'Inflación de los alimentos'),
                units = c('Ha', '%', '%', '%', '% ', '(US$)', '%', '%')
                )

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
lbels
fix(lbels)
unique(all$variable)
lbels
all <- inner_join(all, lbels, by = c('variable' = 'abb'))

write.csv(all, 'tbl/results_run2.csv', row.names = FALSE)

#  To make the graph ------------------------------------------------------
all
# avg <- all %>% 
#   group_by(variable, nme, nombre, units) %>% 
#   dplyr::summarise(min = min(value), 
#                    avg = mean(value), 
#                    max = max(value)) %>% 
#   ungroup()
filter(avg, variable != 'rsquared') %>% pull(value) %>% sum()
avg <- all

# To order ------------------------------------------------------------------
lvls <- pull(arrange(avg, desc(avg)), variable)
avg <- mutate(avg, variable = factor(variable, levels = lvls))
avg <- avg %>% arrange(variable)

gbar <- ggplot(data = avg, aes(x = variable, y = avg, fill = variable, col = variable)) + 
  geom_col(position = 'dodge') +
  geom_errorbar(aes(x = variable, ymin = min, ymax = max, group = variable), position = position_dodge(width = 0.9), width = 0.5, color = '#252525') +
  scale_x_discrete(labels = avg$nombre) +
  scale_fill_manual(values = c('#969696', "#d53e4f", "#f46d43", "#fdae61", "#e6f598", "#abdda4", "#66c2a5", "#3288bd", '#EDC949'),
                    labels =  avg$nombre
                    ) +
  scale_color_manual(values = c('#969696', "#d53e4f", "#f46d43", "#fdae61", "#e6f598", "#abdda4", "#66c2a5", "#3288bd", '#EDC949')) +
  # geom_text(aes(y = avg, label = round(avg, 2)), vjust = 0, size = 10, fontface = 'bold', position =  position_dodge(width = 0.9), col = 'grey40') + 
  ggtitle(label = 'Importancia de variables socio-económicas en función de la deforestación a nivel de país') +
  theme_ipsum_ps() + 
  labs(x = '', y = 'Importance', fill = '', caption = 'Modelo XGBoost') +
  theme(axis.text.x = element_text(angle = 45, family = 'Bahnschrift', size = 28, vjust = 0.5),
        axis.text = element_text(family = 'Bahnschrift', size = 28), 
        axis.text.y = element_text(family = 'Bahnschrift', size = 28, face = 'bold'),
        axis.title.x = element_text(family = 'Bahnschrift', face = 'bold', size = 28),
        axis.title.y = element_text(family = 'Bahnschrift', face = 'bold', size = 28),
        plot.title = element_text(family = 'Bahnschrift', face = 'bold', size = 40, hjust = 0.5),
        panel.grid.major = element_blank(),
        plot.caption = element_text(family = 'Bahnschrift', size = 28),
        legend.text = element_text(family = 'Bahnschrift', size = 28), 
        legend.title = element_text(family = 'Bahnschrift', face = 'bold', size = 28),
        strip.text = element_text(family = 'Bahnschrift', face = 'bold', hjust = 0.5, size = 28),
        legend.background = element_rect(color = NA),
        legend.position = 'bottom') +
  guides(fill = guide_legend(nrow = 2), 
         color = 'none')

gbar

ggsave(plot = gbar, filename = 'png/graph.png', units = 'in', width = 9, height = 7, dpi = 300)
