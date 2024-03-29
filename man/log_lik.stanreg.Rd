% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/log_lik.R
\name{log_lik.stanreg}
\alias{log_lik.stanreg}
\alias{log_lik}
\alias{log_lik.stanmvreg}
\alias{log_lik.stanjm}
\title{Pointwise log-likelihood matrix}
\usage{
\method{log_lik}{stanreg}(object, newdata = NULL, offset = NULL, ...)

\method{log_lik}{stanmvreg}(object, m = 1, newdata = NULL, ...)

\method{log_lik}{stanjm}(object, newdataLong = NULL, newdataEvent = NULL, ...)
}
\arguments{
\item{object}{A fitted model object returned by one of the 
\pkg{rstanarm} modeling functions. See \code{\link{stanreg-objects}}.}

\item{newdata}{An optional data frame of new data (e.g. holdout data) to use
when evaluating the log-likelihood. See the description of \code{newdata}
for \code{\link{posterior_predict}}.}

\item{offset}{A vector of offsets. Only required if \code{newdata} is
specified and an \code{offset} was specified when fitting the model.}

\item{...}{Currently ignored.}

\item{m}{Integer specifying the number or name of the submodel}

\item{newdataLong, newdataEvent}{Optional data frames containing new data 
(e.g. holdout data) to use when evaluating the log-likelihood for a 
model estimated using \code{\link{stan_jm}}. If the fitted model 
was a multivariate joint model (i.e. more than one longitudinal outcome),
then \code{newdataLong} is allowed to be a list of data frames. If supplying 
new data, then \code{newdataEvent} should also include variables corresponding
to the event time and event indicator as these are required for evaluating the
log likelihood for the event submodel. For more details, see the description 
of \code{newdataLong} and \code{newdataEvent} for \code{\link{posterior_survfit}}.}
}
\value{
For the \code{stanreg} and \code{stanmvreg} methods an \eqn{S} by 
  \eqn{N} matrix, where \eqn{S} is the size of the posterior sample and 
  \eqn{N} is the number of data points. For the \code{stanjm} method 
  an \eqn{S} by \eqn{Npat} matrix where \eqn{Npat} is the number of individuals.
}
\description{
For models fit using MCMC only, the \code{log_lik} method returns the
\eqn{S} by \eqn{N} pointwise log-likelihood matrix, where \eqn{S} is the size
of the posterior sample and \eqn{N} is the number of data points, or in the
case of the \code{stanmvreg} method (when called on \code{\link{stan_jm}}
model objects) an \eqn{S} by \eqn{Npat} matrix where \eqn{Npat} is the number 
of individuals.
}
\examples{
if (.Platform$OS.type != "windows" || .Platform$r_arch != "i386") {
\donttest{
 roaches$roach100 <- roaches$roach1 / 100
 fit <- stan_glm(
    y ~ roach100 + treatment + senior,
    offset = log(exposure2),
    data = roaches,
    family = poisson(link = "log"),
    prior = normal(0, 2.5),
    prior_intercept = normal(0, 10),
    iter = 500, # just to speed up example,
    refresh = 0
 )
 ll <- log_lik(fit)
 dim(ll)
 all.equal(ncol(ll), nobs(fit))

 # using newdata argument
 nd <- roaches[1:2, ]
 nd$treatment[1:2] <- c(0, 1)
 ll2 <- log_lik(fit, newdata = nd, offset = c(0, 0))
 head(ll2)
 dim(ll2)
 all.equal(ncol(ll2), nrow(nd))
}
}
}
