params <-
list(EVAL = TRUE)

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

## ----pbcLong-------------------------------------------------------------
head(pbcLong)

## ----pbcSurv-------------------------------------------------------------
head(pbcSurv)

## ----datasets_help, eval = FALSE-----------------------------------------
#  help("datasets", package = "rstanarm")

## ----univariate_fit, results = "hold", message = FALSE, warning = FALSE----
library(rstanarm)
mod1 <- stan_jm(formulaLong = logBili ~ year + (year | id), 
                dataLong = pbcLong,
                formulaEvent = survival::Surv(futimeYears, death) ~ sex + trt, 
                dataEvent = pbcSurv,
                time_var = "year",
                chains = 1, refresh = 2000, seed = 12345)

## ----print, echo = FALSE-------------------------------------------------
print(mod1)

## ----summary-------------------------------------------------------------
summary(mod1, probs = c(.025,.975))

## ----VarCorr-------------------------------------------------------------
as.data.frame(VarCorr(mod1))

## ----assoc_etaslope, eval = FALSE----------------------------------------
#  mod2 <- stan_jm(formulaLong = logBili ~ year + (year | id),
#                  dataLong = pbcLong,
#                  formulaEvent = survival::Surv(futimeYears, death) ~ sex + trt,
#                  dataEvent = pbcSurv,
#                  assoc = c("etavalue", "etaslope"),
#                  time_var = "year",
#                  chains = 1, refresh = 2000, seed = 12345)

## ----fitmodel_mv_ev_ev, warning = FALSE, message = FALSE-----------------
mod3 <- stan_jm(
    formulaLong = list(
        logBili ~ year + (year | id), 
        albumin ~ year + (year | id)),
    formulaEvent = survival::Surv(futimeYears, death) ~ sex + trt, 
    dataLong = pbcLong, dataEvent = pbcSurv,
    time_var = "year",
    chains = 1, refresh = 2000, seed = 12345)

## ----results_print-------------------------------------------------------
print(mod3)

## ----results_summary-----------------------------------------------------
summary(mod3, pars = "assoc")

## ----plots, fig.width=6.5, fig.height=6----------------------------------
p1 <- posterior_traj(mod3, m = 1, ids = 6:8)
p2 <- posterior_traj(mod3, m = 2, ids = 6:8)
p3 <- posterior_survfit(mod3, ids = 6:8, draws = 200)
pp1 <- plot(p1, plot_observed = TRUE, vline = TRUE)
pp2 <- plot(p2, plot_observed = TRUE, vline = TRUE)
plot_stack_jm(yplot = list(pp1, pp2), survplot = plot(p3))

