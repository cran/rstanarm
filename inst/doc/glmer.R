params <-
structure(list(EVAL = TRUE), .Names = "EVAL")

## ---- SETTINGS-knitr, include=FALSE--------------------------------------
stopifnot(require(knitr))
opts_chunk$set(
  comment=NA, message = FALSE, warning = FALSE, eval = params$EVAL,
  fig.align='center', fig.width = 7, fig.height = 3
)

## ---- SETTINGS-gg, include=FALSE-----------------------------------------
library(ggplot2)
thm_els <- theme(axis.text.y = element_blank(), 
                 legend.position = "none",
                 legend.background = element_rect(fill = "gray"),
                 legend.text = element_text(size = 7))
theme_set(theme_classic() %+replace% thm_els)

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

