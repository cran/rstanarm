## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(collapse = TRUE)
knitr::opts_chunk$set(eval = identical(Sys.getenv("NOT_CRAN"), "true"))
library(rstanarm)

## -----------------------------------------------------------------------------
set.seed(123)
group <- c(rep(1,10), rep(2,12))
group <- factor(c(rep("A",10), rep("B",12)))
N <- length(group)
hc <- sample(c(-1,1), N, replace = TRUE)
effect <- c(3,5)

lp <- effect[group] + 0.7*hc
y <- rnorm(N, lp, 0.5)

experiment <- data.frame(y = y,
                         group = factor(group),
                         hc = hc)
experiment

## ----results='hide'-----------------------------------------------------------
fit <- stan_glm(y ~ 0 + group + hc,
                data = experiment,
                family = gaussian(link="identity"),
                prior = normal(c(3,3,0), 1),
                seed = 123)

## -----------------------------------------------------------------------------
c(coef(fit), sigma = sigma(fit))

## -----------------------------------------------------------------------------
#' Quantify Overlapping Proportion
#' Compute how much of the smaller distribution overlaps with the larger (i.e. wider) distribution.
#' @param large Posterior predictive samples that have larger range than \code{small}.
#' @param small Posterior predictive samples that have smaller range than \code{large}.
#' @param p Probability to compute prediction interval.
#' @return A proportion between 0 and 1 indicating how much of \code{small} is contained in \code{large} given the credible interval specification.
overlap_prop <- function(large, small, p = 1) {
  p_lwr <- (1-p)/2
  p_upr <- 1 - p_lwr
  large_ci <- quantile(large, probs = c(p_lwr, p_upr))
  left <- min(large_ci)
  right <- max(large_ci)
  indxs <- which(small >= left & small <= right)
  return(length(indxs)/length(small))
}

#' Quantify Overlapping Posterior Predictive Distributions
#' Quantify the overlap between posterior samples from two distributions.
#' @param a Group A posterior predictive samples.
#' @param b Group B posterior predictive samples.
#' @param p Probability to compute credible interval.
#' @return A proportion between 0 and 1 indicating how much of the credible intervals for \code{a} and \code{b} overlap with one another.
overlap <- function(a, b, p = 1) {
  length_a <- dist(range(a))
  length_b <- dist(range(b))
  if (length_a >= length_b) {
    out <- overlap_prop(a, b, p)
  }
  else if (length_a < length_b) {
    out <- overlap_prop(b, a, p)
  }
  return(out)
}

## ----fig.align='center', fig.height=8, fig.width=6----------------------------
pp_a <- posterior_predict(fit, newdata = data.frame(group = factor("A"), hc = experiment$hc))
pp_b <- posterior_predict(fit, newdata = data.frame(group = factor("B"), hc = experiment$hc))
pp_a_quant <- quantile(pp_a, probs = c(0.05,0.95))
pp_b_quant <- quantile(pp_b, probs = c(0.05,0.95))

overlap(pp_a, pp_b, p = 0.9)

par(mfrow=c(2,1))
# group A
hist(pp_a, breaks = 50, col = '#808080', border = '#FFFFFF',
     main = "Group A",
     xlab = "Avg Streaming (hrs)",
     xlim = c(0,10))
abline(v = pp_a_quant[1], lwd = 2, col = "red")
abline(v = pp_a_quant[2], lwd = 2, col = "red")
# group B
hist(pp_b, breaks = 50, col = '#808080', border = '#FFFFFF',
     main = "Group B",
     xlab = "Avg Streaming (hrs)",
     xlim = c(0,10))
abline(v = pp_b_quant[1], lwd = 2, col = "red")
abline(v = pp_b_quant[2], lwd = 2, col = "red")

## ----fig.align='center', fig.height=6, fig.width=10---------------------------
pp_a0 <- posterior_predict(fit, newdata = data.frame(group = factor("A"), hc = -1))
pp_b0 <- posterior_predict(fit, newdata = data.frame(group = factor("B"), hc = -1))
pp_a1 <- posterior_predict(fit, newdata = data.frame(group = factor("A"), hc = 1))
pp_b1 <- posterior_predict(fit, newdata = data.frame(group = factor("B"), hc = 1))
pp_a0_quant <- quantile(pp_a0, probs = c(0.05,0.95))
pp_b0_quant <- quantile(pp_b0, probs = c(0.05,0.95))
pp_a1_quant <- quantile(pp_a1, probs = c(0.05,0.95))
pp_b1_quant <- quantile(pp_b1, probs = c(0.05,0.95))

par(mfrow=c(2,2))
# group A, x = 0
hist(pp_a0, breaks = 50, col = '#808080', border = '#FFFFFF',
     main = "Group A (hc=-1)",
     xlab = "Avg Streaming (hrs)",
     xlim = c(0,10))
abline(v = pp_a0_quant[1], lwd = 2, col = "red")
abline(v = pp_a0_quant[2], lwd = 2, col = "red")
# group B, x = 0
hist(pp_b0, breaks = 50, col = '#808080', border = '#FFFFFF',
     main = "Group B (hc=-1)",
     xlab = "Avg Streaming (hrs)",
     xlim = c(0,10))
abline(v = pp_b0_quant[1], lwd = 2, col = "red")
abline(v = pp_b0_quant[2], lwd = 2, col = "red")
# group A, x = 1
hist(pp_a1, breaks = 50, col = '#808080', border = '#FFFFFF',
     main = "Group A (hc=1)",
     xlab = "Avg Streaming (hrs)",
     xlim = c(0,10))
abline(v = pp_a1_quant[1], lwd = 2, col = "red")
abline(v = pp_a1_quant[2], lwd = 2, col = "red")
# group B, x = 1
hist(pp_b1, breaks = 50, col = '#808080', border = '#FFFFFF',
     main = "Group B (hc=1)",
     xlab = "Avg Streaming (hrs)",
     xlim = c(0,10))
abline(v = pp_b1_quant[1], lwd = 2, col = "red")
abline(v = pp_b1_quant[2], lwd = 2, col = "red")

## ----fig.align='center', fig.height=5, fig.width=5----------------------------
# prediction interval probabilities
ci_p <- seq(0.1,1, by = 0.05)
# compute proportions
overlap_ab <- sapply(ci_p, function(s){overlap(pp_a, pp_b, s)})
# plot
plot(ci_p, overlap_ab, type = "o", pch = 20,
     xaxt = "n", yaxt = "n",
     main = "Group A vs Group B",
     xlab = "Prediction Interval Probability (1-Risk)",
     ylab = "Overlap Proportion (Group Similarity)")
axis(1, seq(0,1,by=0.1), cex.axis = 0.8)
axis(2, seq(0,1,by=0.1), cex.axis = 0.8)
abline(v = 0.5, lty = 2)

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
experiment_bin <- data.frame(group = factor(c("C","D")),
                             y = c(10,14),
                             trials = c(19,22))
fit_group_bin <- stan_glm(cbind(y, trials - y) ~ 0 + group, data = experiment_bin,
                          family = binomial(link="logit"), seed = 123)

## ----fig.align='center', fig.height=5, fig.width=10---------------------------
# pp_c <- posterior_linpred(fit_group_bin, newdata = data.frame(group = factor("C")), transform = TRUE)
# pp_d <- posterior_linpred(fit_group_bin, newdata = data.frame(group = factor("D")), transform = TRUE)
# below doesn't work as expected (predictions are bigger than the number of trials)
# pp_c <- posterior_predict(fit_group_bin, newdata = data.frame(group = factor("C"), trials = 19))
# pp_d <- posterior_predict(fit_group_bin, newdata = data.frame(group = factor("D"), trials = 22))
pp <- posterior_predict(fit_group_bin)
pp_c <- pp[,1]
pp_d <- pp[,2]
pp_c_quant <- quantile(pp_c, probs = c(0.05,0.95))
pp_d_quant <- quantile(pp_d, probs = c(0.05,0.95))

# compute overlap
overlap(pp_c, pp_d, p = 0.9)

# plot
# group C
par(mfrow=c(1,2))
hist(pp_c, breaks = 50, col = '#808080', border = '#FFFFFF',
     main = "Group C",
     xlab = "Product Consumption",
     xlim = c(0,25))
abline(v = pp_c_quant[1], lwd = 2, col = "red")
abline(v = pp_c_quant[2], lwd = 2, col = "red")
# group D
hist(pp_d, breaks = 50, col = '#808080', border = '#FFFFFF',
     main = "Group D",
     xlab = "Product Consumption",
     xlim = c(0,25))
abline(v = pp_d_quant[1], lwd = 2, col = "red")
abline(v = pp_d_quant[2], lwd = 2, col = "red")

## ----fig.align='center', fig.height=5, fig.width=5----------------------------
# prediction interval probabilities
ci_p <- rev(seq(0.1,1, by = 0.05))
# compute proportions
overlap_cd <- sapply(ci_p, function(s){overlap(pp_c, pp_d, s)})
# plot
plot(ci_p, overlap_cd, type = "o", pch = 20,
     xaxt = "n", yaxt = "n",
     main = "Group C vs Group D", xlab = "Prediction Interval Probability (1-Risk)", ylab = "Overlap Proportion (Group Similarity)")
axis(1, seq(0,1,by=0.1), cex.axis = 0.8)
axis(2, seq(0,1,by=0.1), cex.axis = 0.8)
abline(v = 0.5, lty = 2)

## -----------------------------------------------------------------------------
group_a <- experiment$y[experiment$group == "A"]
group_b <- experiment$y[experiment$group == "B"]
# Relevant dplyr code
# group_a <- experiment %>% filter(group == "A") %>% select(y) %>% unlist %>% unname
# group_b <- experiment %>% filter(group == "B") %>% select(y) %>% unlist %>% unname

t_test <- t.test(x=group_a, y=group_b)
t_stat <- abs(t_test$statistic)
p_value <- t_test$p.value
print(p_value)
# You can manually compute the p-value with the following code
# p_value <- pt(-t_stat, t_test$parameter)*2

# you can manually compute the confidence intervals with the following code
# group_a_mean <- mean(group_a)
# group_b_mean <- mean(group_b)
# v <- sqrt((var(group_a)/length(group_a)) + (var(group_b)/length(group_b)))
# ci_lwr <- (group_a_mean - group_b_new_mean) - abs(qt(0.025, t_test$parameter[['df']])*v)
# ci_upr <- (group_a_mean - group_b_new_mean) + abs(qt(0.025, t_test$parameter[['df']])*v)

## ----fig.align='center', fig.height=5, fig.width=5----------------------------
dof <- t_test$parameter[["df"]]
x <- seq(-10,10,length.out = 1e3)
plot(x, dt(x, dof), type = "l",
     main = "Distribution of Test Statistics Under Null Hypothesis",
     xlab = "t-statistic value",
     ylab = "t-distribution density")
abline(v=-t_stat, col="red", lwd=2)
abline(v=t_stat, col="red", lwd=2)

## ----results='hide'-----------------------------------------------------------
fit_hier <- stan_glmer(y ~ 0 + (1 | group) + hc,
                       prior = normal(0, 1),
                       data = experiment,
                       family = gaussian(link="identity"),
                       seed = 123)

## -----------------------------------------------------------------------------
coef(fit_hier)
fixef(fit_hier)
ranef(fit_hier)

