% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{pairs.stanreg}
\alias{pairs.stanreg}
\alias{pairs_style_np}
\alias{pairs_condition}
\title{Pairs method for stanreg objects}
\usage{
\method{pairs}{stanreg}(
  x,
  pars = NULL,
  regex_pars = NULL,
  condition = pairs_condition(nuts = "accept_stat__"),
  ...
)
}
\arguments{
\item{x}{A fitted model object returned by one of the 
\pkg{rstanarm} modeling functions. See \code{\link{stanreg-objects}}.}

\item{pars}{An optional character vector of parameter names. All parameters 
are included by default, but for models with more than just a few 
parameters it may be far too many to visualize on a small computer screen 
and also may require substantial computing time.}

\item{regex_pars}{An optional character vector of \link[=grep]{regular 
expressions} to use for parameter selection. \code{regex_pars} can be used
in place of \code{pars} or in addition to \code{pars}. Currently, all
functions that accept a \code{regex_pars} argument ignore it for models fit
using optimization.}

\item{condition}{Same as the \code{condition} argument to 
\code{\link[bayesplot:MCMC-scatterplots]{mcmc_pairs}} except the \emph{default is different}
for \pkg{rstanarm} models. By default, the \code{mcmc_pairs} function in
the \pkg{bayesplot} package plots some of the Markov chains (half, in the
case of an even number of chains) in the panels above the diagonal and the
other half in the panels below the diagonal. However since we know that 
\pkg{rstanarm} models were fit using Stan (which \pkg{bayesplot} doesn't 
assume) we can make the default more useful by splitting the draws 
according to the \code{accept_stat__} diagnostic. The plots below the 
diagonal will contain realizations that are below the median 
\code{accept_stat__} and the plots above the diagonal will contain 
realizations that are above the median \code{accept_stat__}. To change this
behavior see the documentation of the \code{condition} argument at 
\code{\link[bayesplot:MCMC-scatterplots]{mcmc_pairs}}.}

\item{...}{Optional arguments passed to 
\code{\link[bayesplot:MCMC-scatterplots]{mcmc_pairs}}. 
The \code{np}, \code{lp}, and \code{max_treedepth} arguments to 
\code{mcmc_pairs} are handled automatically by \pkg{rstanarm} and do not 
need to be specified by the user in \code{...}. The arguments that can be 
specified in \code{...} include \code{transformations}, \code{diag_fun},
\code{off_diag_fun}, \code{diag_args}, \code{off_diag_args},
and \code{np_style}. These arguments are
documented thoroughly on the help page for
\code{\link[bayesplot:MCMC-scatterplots]{mcmc_pairs}}.}
}
\description{
Interface to \pkg{bayesplot}'s
\code{\link[bayesplot:MCMC-scatterplots]{mcmc_pairs}} function for use with
\pkg{rstanarm} models. Be careful not to specify too many parameters to
include or the plot will be both hard to read and slow to render.
}
\examples{
if (.Platform$OS.type != "windows" || .Platform$r_arch != "i386") {
\donttest{
if (!exists("example_model")) example(example_model)

bayesplot::color_scheme_set("purple")

# see 'condition' argument above for details on the plots below and 
# above the diagonal. default is to split by accept_stat__.
pairs(example_model, pars = c("(Intercept)", "log-posterior"))

# for demonstration purposes, intentionally fit a model that
# will (almost certainly) have some divergences
fit <- stan_glm(
  mpg ~ ., data = mtcars,
  iter = 1000,
  # this combo of prior and adapt_delta should lead to some divergences
  prior = hs(),
  adapt_delta = 0.9,
  refresh = 0
)

pairs(fit, pars = c("wt", "sigma", "log-posterior"))

# requires hexbin package
# pairs(
# fit, 
#   pars = c("wt", "sigma", "log-posterior"), 
#   transformations = list(sigma = "log"), # show log(sigma) instead of sigma
#  off_diag_fun = "hex" # use hexagonal heatmaps instead of scatterplots
# )

bayesplot::color_scheme_set("brightblue")
pairs(
  fit, 
  pars = c("(Intercept)", "wt", "sigma", "log-posterior"), 
  transformations = list(sigma = "log"), 
  off_diag_args = list(size = 3/4, alpha = 1/3), # size and transparency of scatterplot points
  np_style = pairs_style_np(div_color = "black", div_shape = 2) # color and shape of the divergences
)

# Using the condition argument to show divergences above the diagonal 
pairs(
  fit, 
  pars = c("(Intercept)", "wt", "log-posterior"), 
  condition = pairs_condition(nuts = "divergent__")
)

}
}
}
