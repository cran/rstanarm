% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pp_validate.R
\name{pp_validate}
\alias{pp_validate}
\title{Model validation via simulation}
\usage{
pp_validate(object, nreps = 20, seed = 12345, ...)
}
\arguments{
\item{object}{A fitted model object returned by one of the 
\pkg{rstanarm} modeling functions. See \code{\link{stanreg-objects}}.}

\item{nreps}{The number of replications to be performed. \code{nreps} must be
sufficiently large so that the statistics described below in Details are
meaningful. Depending on the model and the size of the data, running
\code{pp_validate} may be slow. See also the Note section below for advice
on avoiding numerical issues.}

\item{seed}{A seed passed to Stan to use when refitting the model.}

\item{...}{Currently ignored.}
}
\value{
A ggplot object that can be further customized using the
  \pkg{ggplot2} package.
}
\description{
The \code{pp_validate} function is based on the methods described in
Cook, Gelman, and Rubin (2006) for validating software developed to fit
particular Bayesian models. Here we take the perspective that models
themselves are software and thus it is useful to apply this validation
approach to individual models.
}
\details{
We repeat \code{nreps} times the process of simulating parameters and data
from the model and refitting the model to this simulated data. For each of
the \code{nreps} replications we do the following:
\enumerate{
\item Refit the model but \emph{without} conditioning on the data (setting
\code{prior_PD=TRUE}), obtaining draws \eqn{\theta^{true}}{\theta_true}
from the \emph{prior} distribution of the model parameters.
\item Given \eqn{\theta^{true}}{\theta_true}, simulate data \eqn{y^\ast}{y*}
from the \emph{prior} predictive distribution (calling
\code{\link{posterior_predict}} on the fitted model object obtained in step
1).
\item Fit the model to the simulated outcome \eqn{y^\ast}{y*}, obtaining
parameters \eqn{\theta^{post}}{\theta_post}.
}
For any individual parameter, the quantile of the "true" parameter value with
respect to its posterior distribution \emph{should} be uniformly distributed.
The validation procedure entails looking for deviations from uniformity by
computing statistics for a test that the quantiles are uniformly distributed.
The absolute values of the computed  test statistics are plotted for batches
of parameters (e.g., non-varying coefficients are grouped into a batch called
"beta", parameters that vary by group level are in batches named for the
grouping variable, etc.). See Cook, Gelman, and Rubin (2006) for more details
on the validation procedure.
}
\note{
In order to make it through \code{nreps} replications without running
  into numerical difficulties you may have to restrict the range for randomly
  generating initial values for parameters when you fit the \emph{original}
  model. With any of \pkg{rstanarm}'s modeling functions this can be done by
  specifying the optional argument \code{init_r} as some number less than the
  default of \eqn{2}.
}
\examples{
if (.Platform$OS.type != "windows" || .Platform$r_arch != "i386") {
\dontrun{
if (!exists("example_model")) example(example_model)
try(pp_validate(example_model)) # fails with default seed / priors
}
}
}
\references{
Cook, S., Gelman, A., and Rubin, D.
(2006). Validation of software for Bayesian models using posterior quantiles.
\emph{Journal of Computational and Graphical Statistics}. 15(3), 675--692.
}
\seealso{
\code{\link{pp_check}} for graphical posterior predictive checks and
\code{\link{posterior_predict}} to draw from the posterior predictive
distribution.

\code{\link[bayesplot:bayesplot-colors]{color_scheme_set}} to change the color scheme of the
plot.
}
