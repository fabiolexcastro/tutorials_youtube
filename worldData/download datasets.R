
# Load libraries ----------------------------------------------------------
require(pacman)
p_load(ggplot2, tidyverse, readxl, plotly, WDI, xlsx)

# World indicators --------------------------------------------------------
clss <- WDIsearch() %>% as_tibble()
View(clss)

# Vars to download --------------------------------------------------------
lbls <- tibble(vars = c('agricultural land', 'food inflation', 'population growth', 'food imports', 'food exports', 'foreing invest'), 
               abbr = c('AC.LND.AGRI.K2', NA, 'SP.POP.GROW', 'TM.VAL.FOOD.ZS.UN', 'TX.VAL.FOOD.ZS.UN', 'BX.KLT.DINV.CD.WD'), 
               source = c('Worldbank', 'FAO', 'Worldbank', 'Worldbank', 'Worldbank', 'Worldbank'))

# Agricultural land = % of agricultural land 
# Food inflation = %
# Pop growth = (%)
# Food imports = Food imports (% of merchandise imports)
# Food exports = Food exports (% of merchandise exports)
# Foreign invest = Foreign direct investment, net inflows (BoP, current US$)/

# To download -------------------------------------------------------------
abbs <- lbls$abbr
abbs <- abbs[!is.na(abbs)]

tble <- WDI(country = "all", indicator = abbs, start = 2001, end = 2020, extra = FALSE, cache = NULL, latest = NULL, language = "en")
tble <- as_tibble(tble)
tble <- dplyr::select(tble, country, iso = iso3c , year, SP.POP.GROW:BX.KLT.DINV.CD.WD)
colnames(tble)[4:7] <- c('pop_growth', 'food_imports', 'food_exports', 'foreing_invest')

# Foreing invest
frgn <- WDI(country = "all", indicator = 'BX.KLT.DINV.CD.WD', start = 2001, end = 2020, extra = FALSE, cache = NULL, latest = NULL, language = "en")
frgn <- as_tibble(frgn)
frgn <- dplyr::select(frgn, iso = iso3c, year, foreing = BX.KLT.DINV.CD.WD)

# Agricultural land (% of land area)
agrc <- WDI(country = "all", indicator = 'AC.LND.AGRI.K2', start = 2001, end = 2020, extra = FALSE, cache = NULL, latest = NULL, language = "en")
# Source: https://data.worldbank.org/indicator/AG.LND.AGRI.ZS
agrc <- read_csv('./downloaded/API_AG.LND.AGRI.ZS_DS2_en_csv_v2_4669757/API_AG.LND.AGRI.ZS_DS2_en_csv_v2_4669757.csv', skip = 4)
agrc <- gather(agrc, var, value, -1, -2, -3, -4)
unique(agrc$`Indicator Name`)
unique(agrc$`Indicator Code`)
colnames(agrc) <- c('country_name', 'country_code', 'indicator_name', 'indicator_code', 'year', 'value')
agrc <- filter(agrc, year %in% as.character(2001:2020))
agrc <- dplyr::select(agrc, country_name, country_code, indicator_name, year, value)
agrc <- dplyr::select(agrc, ISO = country_code, year, value)
 
agrc <- mutate(agrc, year = as.numeric(year))
tble
tble <- inner_join(tble, agrc, by = c('iso' = 'ISO', 'year' = 'year'))

# Food inflation # Source: https://www.fao.org/faostat/en/#data/CP
food <- read_csv('./downloaded/FAOSTAT_data_en_11-25-2022.csv')
colnames(food)[3] <- 'ISO'
food <- dplyr::select(food, ISO, Year, Item, Months, Value)
food <- food %>% group_by(ISO, Year, Item) %>% dplyr::summarise(Value = mean(Value, na.rm = T)) %>% ungroup()
food <- dplyr::select(food, -Item)
food <- rename(food, food_inflation = Value)

tble <- full_join(tble, food, by = c('iso' = 'ISO', 'year' = 'Year'))
write.xlsx(tble, file = './tble/database_global.xlsx', row.names = FALSE)

# Deforestation
source_deforestation <- 'https://www.globalforestwatch.org/dashboards/global/?category=summary&location=WyJnbG9iYWwiXQ%3D%3D&map=eyJjZW50ZXIiOnsibGF0IjoxLjkwODMzMjgwODg3ODExZS0xMiwibG5nIjoxMS45OTk5OTk5OTk5OTcyNDN9LCJ6b29tIjoyLjIyMjE5MTAyNTA4OTE0NywiZGF0YXNldHMiOlt7ImRhdGFzZXQiOiJwb2xpdGljYWwtYm91bmRhcmllcyIsImxheWVycyI6WyJkaXNwdXRlZC1wb2xpdGljYWwtYm91bmRhcmllcyIsInBvbGl0aWNhbC1ib3VuZGFyaWVzIl0sImJvdW5kYXJ5Ijp0cnVlLCJvcGFjaXR5IjoxLCJ2aXNpYmlsaXR5Ijp0cnVlfSx7ImRhdGFzZXQiOiJOZXQtQ2hhbmdlLVNUQUdJTkciLCJsYXllcnMiOlsiZm9yZXN0LW5ldC1jaGFuZ2UiXSwib3BhY2l0eSI6MSwidmlzaWJpbGl0eSI6dHJ1ZSwicGFyYW1zIjp7InZpc2liaWxpdHkiOnRydWUsImFkbV9sZXZlbCI6ImFkbTAifX1dfQ%3D%3D&showMap=true' 
dfrs <- read_csv('./downloaded/Pérdida mundial de bosques primarios/treecover_loss__ha.csv')
dfrs <- dfrs[,1:3]
colnames(dfrs) <- c('iso', 'year', 'loss_ha') # loss_ha = cambio de cobertura boscosa

# To join both tables -----------------------------------------------------
dfrs
tble <- as_tibble(full_join(tble, dfrs, by = c('iso' = 'iso', 'year' = 'year')))

# To write the final table ------------------------------------------------
write.xlsx(tble, './tble/dabase_global_total.xlsx')



