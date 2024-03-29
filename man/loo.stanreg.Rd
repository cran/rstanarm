% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loo.R
\name{loo.stanreg}
\alias{loo.stanreg}
\alias{loo}
\alias{waic.stanreg}
\alias{waic}
\alias{loo_compare.stanreg}
\alias{loo_compare}
\alias{loo_compare.stanreg_list}
\alias{loo_model_weights.stanreg_list}
\alias{loo_model_weights}
\alias{compare_models}
\title{Information criteria and cross-validation}
\usage{
\method{loo}{stanreg}(
  x,
  ...,
  cores = getOption("mc.cores", 1),
  save_psis = FALSE,
  k_threshold = NULL
)

\method{waic}{stanreg}(x, ...)

\method{loo_compare}{stanreg}(x, ..., criterion = c("loo", "kfold", "waic"), detail = FALSE)

\method{loo_compare}{stanreg_list}(x, ..., criterion = c("loo", "kfold", "waic"), detail = FALSE)

\method{loo_model_weights}{stanreg_list}(x, ..., cores = getOption("mc.cores", 1), k_threshold = NULL)

compare_models(..., loos = list(), detail = FALSE)
}
\arguments{
\item{x}{For \code{loo} and \code{waic}, a fitted model object returned by
  one of the rstanarm modeling functions. See \link{stanreg-objects}.

  For the \code{loo_model_weights} method, \code{x} should be a
  "stanreg_list" object, which is a list of fitted model objects created by
  \code{\link{stanreg_list}}. \code{loo_compare} also allows \code{x} to be a
  single stanreg object, with the remaining objects passed via \code{...}, or
  a single \code{stanreg_list} object.}

\item{...}{For \code{loo_compare.stanreg}, \code{...} can contain objects
  returned by the \code{loo}, \code{\link[=kfold.stanreg]{kfold}}, or
  \code{waic} method (see the \strong{Examples} section, below).

  For \code{loo_model_weights}, \code{...} should contain arguments (e.g.
  \code{method}) to pass to the default \code{\link[loo]{loo_model_weights}}
  method from the \pkg{loo} package.}

\item{cores, save_psis}{Passed to \code{\link[loo]{loo}}.}

\item{k_threshold}{Threshold for flagging estimates of the Pareto shape
parameters \eqn{k} estimated by \code{loo}. See the \emph{How to proceed
when \code{loo} gives warnings} section, below, for details.}

\item{criterion}{For \code{loo_compare.stanreg} and
\code{loo_compare.stanreg_list}, should the comparison be based on LOO-CV
(\code{criterion="loo"}), K-fold-CV (\code{criterion="kfold"}), or WAIC
(\code{criterion="waic"}). The default is LOO-CV. See the \strong{Comparing
models} and \strong{Examples} sections below.}

\item{detail}{For \code{loo_compare.stanreg} and
\code{loo_compare.stanreg_list}, if \code{TRUE} then extra information
about each model (currently just the model formulas) will be printed with
the output.}

\item{loos}{a list of objects produced by the \code{\link{loo}} function}
}
\value{
The structure of the objects returned by \code{loo} and \code{waic}
  methods are documented in detail in the \strong{Value} section in
  \code{\link[loo]{loo}} and \code{\link[loo]{waic}} (from the \pkg{loo}
  package).

\code{loo_compare} returns a matrix with class 'compare.loo'. See the
  \strong{Comparing models} section below for more details.
}
\description{
For models fit using MCMC, compute approximate leave-one-out
  cross-validation (LOO, LOOIC) or, less preferably, the Widely Applicable
  Information Criterion (WAIC) using the \pkg{\link[=loo-package]{loo}}
  package. (For \eqn{K}-fold cross-validation see \code{\link{kfold.stanreg}}.)
  Functions for  model comparison, and model weighting/averaging are also
  provided. 
  
  \strong{Note}: these functions are not guaranteed to work
  properly unless the \code{data} argument was specified when the model was
  fit. Also, as of \pkg{loo} version \code{2.0.0} the default number of cores
  is now only 1, but we recommend using as many (or close to as many) cores
  as possible by setting the \code{cores} argument or using
  \code{options(mc.cores = VALUE)} to set it for an entire session.
}
\section{Approximate LOO CV}{
 The \code{loo} method for stanreg objects
  provides an interface to the \pkg{\link[=loo-package]{loo}} package for
  approximate leave-one-out cross-validation (LOO). The LOO Information
  Criterion (LOOIC) has the same purpose as the Akaike Information Criterion
  (AIC) that is used by frequentists. Both are intended to estimate the
  expected log predictive density (ELPD) for a new dataset. However, the AIC
  ignores priors and assumes that the posterior distribution is multivariate
  normal, whereas the functions from the \pkg{loo} package do not make this
  distributional assumption and integrate over uncertainty in the parameters.
  This only assumes that any one observation can be omitted without having a
  major effect on the posterior distribution, which can be judged using the
  diagnostic plot provided by the \code{\link[loo:pareto-k-diagnostic]{plot.loo}} method and the
  warnings provided by the \code{\link[loo]{print.loo}} method (see the
  \emph{How to Use the rstanarm Package} vignette for an example of this
  process).

  \subsection{How to proceed when \code{loo} gives warnings (k_threshold)}{
  The \code{k_threshold} argument to the \code{loo} method for \pkg{rstanarm}
  models is provided as a possible remedy when the diagnostics reveal
  problems stemming from the posterior's sensitivity to particular
  observations. Warnings about Pareto \eqn{k} estimates indicate observations
  for which the approximation to LOO is problematic (this is described in
  detail in Vehtari, Gelman, and Gabry (2017) and the
  \pkg{\link[=loo-package]{loo}} package documentation). The
  \code{k_threshold} argument can be used to set the \eqn{k} value above
  which an observation is flagged. If \code{k_threshold} is not \code{NULL}
  and there are \eqn{J} observations with \eqn{k} estimates above
  \code{k_threshold} then when \code{loo} is called it will refit the
  original model \eqn{J} times, each time leaving out one of the \eqn{J}
  problematic observations. The pointwise contributions of these observations
  to the total ELPD are then computed directly and substituted for the
  previous estimates from these \eqn{J} observations that are stored in the
  object created by \code{loo}. Another option to consider is K-fold
  cross-validation, which is documented on a separate page (see
  \code{\link[=kfold.stanreg]{kfold}}).

  \strong{Note}: in the warning messages issued by \code{loo} about large
  Pareto \eqn{k} estimates we recommend setting \code{k_threshold} to at
  least \eqn{0.7}. There is a theoretical reason, explained in Vehtari,
  Gelman, and Gabry (2017), for setting the threshold to the stricter value
  of \eqn{0.5}, but in practice they find that errors in the LOO
  approximation start to increase non-negligibly when \eqn{k > 0.7}.
  }
}

\section{Comparing models}{
 "loo" (or "waic" or "kfold") objects can be passed
  to the \code{\link[loo]{loo_compare}} function in the \pkg{loo} package to
  perform model comparison. \pkg{rstanarm} also provides a
  \code{loo_compare.stanreg} method that can be used if the "loo" (or "waic"
  or "kfold") object has been added to the fitted model object (see the
  \strong{Examples} section below for how to do this). This second method
  allows \pkg{rstanarm} to perform some extra checks that can't be done by
  the \pkg{loo} package itself (e.g., verifying that all models to be
  compared were fit using the same outcome variable).

  \code{loo_compare} will return a matrix with one row per model and columns
  containing the ELPD difference and the standard error of the difference. In
  the first row of the matrix will be the model with the largest ELPD
  (smallest LOOIC) and will contain zeros (there is no difference between
  this model and itself). For each of the remaining models the ELPD
  difference and SE are reported relative to the model with the best ELPD
  (the first row). See the \strong{Details} section at the
  \code{\link[loo]{loo_compare}} page in the \pkg{loo} package for more
  information.
}

\section{Model weights}{
 The \code{loo_model_weights} method can be used to
  compute model weights for a \code{"stanreg_list"} object, which is a list
  of fitted model objects made with \code{\link{stanreg_list}}. The end of
  the \strong{Examples} section has a demonstration. For details see the
  \code{\link[loo]{loo_model_weights}} documentation in the \pkg{loo}
  package.
}

\examples{
if (.Platform$OS.type != "windows" || .Platform$r_arch != "i386") {
\donttest{
fit1 <- stan_glm(mpg ~ wt, data = mtcars, refresh = 0)
fit2 <- stan_glm(mpg ~ wt + cyl, data = mtcars, refresh = 0)

# (for bigger models use as many cores as possible)
loo1 <- loo(fit1, cores = 1)
print(loo1)
loo2 <- loo(fit2, cores = 1)
print(loo2)

# when comparing models the loo objects can be passed to loo_compare
# as individual arguments or as a list of loo objects
loo_compare(loo1, loo2)
loo_compare(list(loo1, loo2))

# if the fitted model objects contain a loo object in the component "loo"
# then the model objects can be passed directly or as a stanreg_list
fit1$loo <- loo1
fit2$loo <- loo2
loo_compare(fit1, fit2)

# if the fitted model objects contain a loo object _and_ a waic or kfold
# object, then the criterion argument determines which of them the comparison
# is based on 
fit1$waic <- waic(fit1)
fit2$waic <- waic(fit2)
loo_compare(fit1, fit2, criterion = "waic")

# the models can also be combined into a stanreg_list object, and more 
# informative model names can be provided to use when printing
model_list <- stanreg_list(fit1, fit2, model_names = c("Fewer predictors", "More predictors"))
loo_compare(model_list)

fit3 <- stan_glm(mpg ~ disp * as.factor(cyl), data = mtcars, refresh = 0)
loo3 <- loo(fit3, cores = 2, k_threshold = 0.7)
loo_compare(loo1, loo2, loo3)

# setting detail=TRUE will also print model formulas if used with
# loo_compare.stanreg or loo_compare.stanreg_list
fit3$loo <- loo3
model_list <- stanreg_list(fit1, fit2, fit3)
loo_compare(model_list, detail=TRUE)

# Computing model weights
#
# if the objects in model_list already have 'loo' components then those
# will be used. otherwise loo will be computed for each model internally
# (in which case the 'cores' argument may also be used and is passed to loo())
loo_model_weights(model_list)  # defaults to method="stacking"
loo_model_weights(model_list,  method = "pseudobma")
loo_model_weights(model_list,  method = "pseudobma", BB = FALSE)

# you can also pass precomputed loo objects directly to loo_model_weights
loo_list <- list(A = loo1, B = loo2, C = loo3) # names optional (affects printing)
loo_model_weights(loo_list)
}
}
}
\references{
Vehtari, A., Gelman, A., and Gabry, J. (2017). Practical
  Bayesian model evaluation using leave-one-out cross-validation and WAIC.
  \emph{Statistics and Computing}. 27(5), 1413--1432.
  doi:10.1007/s11222-016-9696-4. arXiv preprint:
  \url{https://arxiv.org/abs/1507.04544}

  Yao, Y., Vehtari, A., Simpson, D., and Gelman, A. (2018) Using
  stacking to average Bayesian predictive distributions. \emph{Bayesian
  Analysis}, advance publication,  \doi{10.1214/17-BA1091}.

Gabry, J. , Simpson, D. , Vehtari, A. , Betancourt, M. and
  Gelman, A. (2019), Visualization in Bayesian workflow. \emph{J. R. Stat.
  Soc. A}, 182: 389-402. doi:10.1111/rssa.12378,
  \href{https://arxiv.org/abs/1709.01449}{arXiv preprint},
  \href{https://github.com/jgabry/bayes-vis-paper}{code on GitHub})
}
\seealso{
\itemize{
  \item The new \href{https://mc-stan.org/loo/articles/}{\pkg{loo} package vignettes}
  and various \href{https://mc-stan.org/rstanarm/articles/}{\pkg{rstanarm} vignettes}
  for more examples using \code{loo} and related functions with \pkg{rstanarm} models.
  \item \code{\link[loo]{pareto-k-diagnostic}} in the \pkg{loo} package for
  more on Pareto \eqn{k} diagnostics.
  \item \code{\link{log_lik.stanreg}} to directly access the pointwise
  log-likelihood matrix.
}
}
