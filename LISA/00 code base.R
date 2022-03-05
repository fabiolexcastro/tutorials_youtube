


# Source: https://stackoverflow.com/questions/45177590/map-of-bivariate-spatial-correlation-in-r-bivariate-lisa
library(boot)
library(dplyr)
library(ggplot2)
library(sf)
library(spdep)
library(rgdal)
library(stringr)
library(UScensus2000tract)

# local_bimoran: Bivariate Local Moran Statistics
local_bimoran()
library(sf)
guerry_path <- system.file("extdata", "Guerry.shp", package = "rgeoda")
guerry <- st_read(guerry_path)
queen_w <- queen_weights(guerry)
lisa <- local_bimoran(queen_w, guerry[c('Crm_prs','Litercy')])
lms <- lisa_values(lisa)
lms
library(boot)
library(dplyr)
library(ggplot2)
library(sf)
library(spdep)
library(rgdal)
library(stringr)
library(UScensus2000tract)

#======================================================
# load data
data("oregon.tract")

# Variables to use in the correlation: white and black population in each census track
x <- oregon.tract$white
y <- oregon.tract$black

# ----------------------------------------------------- #
# Program a function
## Permutations for Lee's L statistic
## Modification of the lee.mc() function within the {spdep} package
## Saves 'localL' output instead of 'L' output
simula_lee <- function(x, y, listw, nsim = nsim, zero.policy = NULL, na.action = na.fail) {
  
  if (deparse(substitute(na.action)) == "na.pass") 
    stop ("na.pass not permitted")
  na.act <- attr(na.action(cbind(x, y)), "na.action")
  x[na.act] <- NA
  y[na.act] <- NA
  x <- na.action(x)
  y <- na.action(y)
  if (!is.null(na.act)) {
    subset <- !(1:length(listw$neighbours) %in% na.act)
    listw <- subset(listw, subset, zero.policy = zero.policy)
  }
  n <- length(listw$neighbours)
  if ((n != length(x)) | (n != length(y))) 
    stop ("objects of different length")
  gamres <- suppressWarnings(nsim > gamma(n + 1))
  if (gamres) 
    stop ("nsim too large for this number of observations")
  if (nsim < 1) 
    stop ("nsim too small")
  xy <- data.frame(x, y)
  S2 <- sum((unlist(lapply(listw$weights, sum)))^2)
  
  lee_boot <- function(var, i, ...) {
    return(lee(x = var[i, 1], y = var[i, 2], ...)$localL)
  }
  
  res <- boot(xy, statistic = lee_boot, R = nsim, sim = "permutation", 
              listw = listw, n = n, S2 = S2, zero.policy = zero.policy)
}

# ----------------------------------------------------- #
# Adjacency Matrix
nb <- poly2nb(oregon.tract)
lw <- nb2listw(nb, style = "B", zero.policy = T)
W  <- as(lw, "symmetricMatrix")
W  <- as.matrix(W / rowSums(W))
W[which(is.na(W))] <- 0

# ----------------------------------------------------- #
# Calculate the index and its simulated distribution
# for global and local values

# Global Lee's L
lee.test(x = x, y = y, listw = lw, zero.policy = TRUE,
         alternative = "two.sided", na.action = na.omit)

# Local Lee's L values
m <- lee(x = x, y = y, listw = lw, n = length(x), 
         zero.policy = TRUE, NAOK = TRUE)

# Local Lee's L simulations
local_sims <- simula_lee(x = x, y = y, listw = lw, nsim = 10000,
                         zero.policy = TRUE, na.action = na.omit)

m_i <- m[[2]]  # local values

# Identify the significant values 
alpha <- 0.05  # for a 95% confidence interval
probs <- c(alpha/2, 1-alpha/2)
intervals <- t(apply(t(local_sims[[2]]), 1, function(x) quantile(x, probs = probs)))
sig <- (m_i < intervals[ , 1] ) | ( m_i > intervals[ , 2])

#======================================================
# Preparing for plotting
oregon.tract <- st_as_sf(oregon.tract)
oregon.tract$sig <- sig

# Identifying the Lee's L patterns
xp <- scale(x)
yp <- scale(y)

patterns <- as.character(interaction(xp > 0, W%*%yp > 0)) 
patterns <- patterns %>% 
  str_replace_all("TRUE","High") %>% 
  str_replace_all("FALSE","Low")
patterns[oregon.tract$sig == 0] <- "Not significant"
oregon.tract$patterns <- patterns

# Plotting
ggplot() +
  geom_sf(data = oregon.tract, aes(fill = patterns), color = "NA") +
  scale_fill_manual(values = c("red", "pink", "light blue", "dark blue", "grey95")) + 
  guides(fill = guide_legend(title = "Lee's L clusters")) +
  theme_minimal()
