params <-
structure(list(EVAL = TRUE), .Names = "EVAL")

## ---- SETTINGS-knitr, include=FALSE--------------------------------------
stopifnot(require(knitr))
opts_chunk$set(
  comment=NA, 
  message = FALSE, 
  warning = FALSE, 
  eval = params$EVAL,
  dev = "png",
  dpi = 150,
  fig.asp = 0.618,
  fig.width = 5,
  out.width = "60%",
  fig.align = "center"
)

## ---- SETTINGS-gg, include=FALSE-----------------------------------------
library(ggplot2)
theme_set(bayesplot::theme_default())

## ---- SETTINGS-rstan, include=FALSE--------------------------------------
ITER <- 500L
CHAINS <- 2L
CORES <- 2L
SEED <- 12345

## ---- SETTINGS-loo, include=FALSE----------------------------------------
loo.cores <- if (exists("CORES")) CORES else 1L
options(loo.cores = loo.cores)

## ---- results = "hide"---------------------------------------------------
library(rstanarm)
data(roaches)
roaches$roach1 <- roaches$roach1 / 100
post <- stan_gamm4(y ~ s(roach1) + treatment + log(roaches$exposure2), 
                   random = ~(1 | senior),
                   data = roaches, family = neg_binomial_2, QR = TRUE,
                   chains = CHAINS, cores = CORES, seed = SEED)

## ------------------------------------------------------------------------
plot_nonlinear(post)

