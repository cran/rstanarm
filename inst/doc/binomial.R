## ---- SETTINGS-knitr, include=FALSE--------------------------------------
stopifnot(require(svglite))
stopifnot(require(knitr))
opts_chunk$set(
  comment=NA, message = FALSE, warning = FALSE,
  dev = 'svglite', fig.ext='svg', fig.path = "SVGs/",
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
CORES <- 1L
SEED <- 12345

## ---- SETTINGS-loo, include=FALSE----------------------------------------
loo.cores <- if (exists("CORES")) CORES else 1L
options(loo.cores = loo.cores)

## ----ARSENIC-load-data---------------------------------------------------
# Load data and create dist100 variable 
wells <- read.csv(file.path("data", "wells.csv.xz"))
wells$dist100 <- wells$dist / 100

## ---- ARSENIC-plot-dist100, fig.height=3---------------------------------
ggplot(wells, aes(x = dist100, y = ..density..)) + 
  geom_histogram(data = subset(wells, switch == 0)) +
  geom_histogram(data = subset(wells, switch == 1), fill = "skyblue", alpha = 0.3) 

## ---- ARSENIC-stan_glm, results="hide"-----------------------------------
library(rstanarm)
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
fit1 <- stan_glm(switch ~ dist100, data = wells, 
                 family = binomial(link = "logit"), 
                 prior = t_prior, prior_intercept = t_prior,  
                 chains = CHAINS, cores = CORES, seed = SEED, iter = ITER)

## ---- echo=FALSE---------------------------------------------------------
(coef_fit1 <- round(coef(fit1), 3))

## ---- ARSENIC-CI---------------------------------------------------------
round(posterior_interval(fit1, prob = 0.5), 2)

## ---- ARSENIC-plot-model-------------------------------------------------
# Predicted probability as a function of x
pr_switch <- function(x, ests) plogis(ests[1] + ests[2] * x)
# A function to slightly jitter the binary data
jitt <- function(...) {
  geom_point(aes_string(...), position = position_jitter(height = 0.05, width = 0.1), 
             size = 2, shape = 21, stroke = 0.2)
}
ggplot(wells, aes(x = dist100, y = switch, color = switch)) + 
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  jitt(x="dist100") + 
  stat_function(fun = pr_switch, args = list(ests = coef(fit1)), 
                size = 2, color = "gray35")

## ----ARSENIC-stan_glm2, results="hide"-----------------------------------
fit2 <- update(fit1, formula = switch ~ dist100 + arsenic) 

## ------------------------------------------------------------------------
(coef_fit2 <- round(coef(fit2), 3))

## ----echo=FALSE----------------------------------------------------------
theme_update(legend.position = "right")

## ---- ARSENIC-plot-model2------------------------------------------------
pr_switch2 <- function(x, y, ests) plogis(ests[1] + ests[2] * x + ests[3] * y)
grid <- expand.grid(dist100 = seq(0, 4, length.out = 100), 
                    arsenic = seq(0, 10, length.out = 100))
grid$prob <- with(grid, pr_switch2(dist100, arsenic, coef(fit2)))
ggplot(grid, aes(x = dist100, y = arsenic)) + 
  geom_tile(aes(fill = prob)) + 
  geom_point(data = wells, aes(color = factor(switch)), size = 2, alpha = 0.85) + 
  scale_fill_gradient() +
  scale_color_manual("switch", values = c("white", "black"), labels = c("No", "Yes"))

## ----echo=FALSE----------------------------------------------------------
theme_update(legend.position = "none")

## ---- ARSENIC-plot-model2-alt--------------------------------------------
library(gridExtra)
# Quantiles
q_ars <- quantile(wells$dist100, seq(0, 1, 0.25))
q_dist <- quantile(wells$arsenic, seq(0, 1, 0.25))  
base <- ggplot(wells) + xlim(c(0, NA)) +
  scale_y_continuous(breaks = c(0, 0.5, 1))
vary_arsenic <- base + jitt(x="arsenic", y="switch", color="switch")
vary_dist <- base + jitt(x="dist100", y="switch", color="switch")
for (i in 1:5) {
  vary_dist <- 
    vary_dist + stat_function(fun = pr_switch2, color = "gray35", 
                              args = list(ests = coef(fit2), y = q_dist[i]))
  vary_arsenic <-
    vary_arsenic + stat_function(fun = pr_switch2, color = "gray35", 
                                 args = list(ests = coef(fit2), x = q_ars[i]))
}
grid.arrange(vary_dist, vary_arsenic, ncol = 2)

## ---- LOO----------------------------------------------------------------
(loo1 <- loo(fit1))
(loo2 <- loo(fit2))
compare(loo1, loo2)

