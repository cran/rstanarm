% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stan_jm.R
\name{stan_jm}
\alias{stan_jm}
\title{Bayesian joint longitudinal and time-to-event models via Stan}
\usage{
stan_jm(
  formulaLong,
  dataLong,
  formulaEvent,
  dataEvent,
  time_var,
  id_var,
  family = gaussian,
  assoc = "etavalue",
  lag_assoc = 0,
  grp_assoc,
  scale_assoc = NULL,
  epsilon = 1e-05,
  basehaz = c("bs", "weibull", "piecewise"),
  basehaz_ops,
  qnodes = 15,
  init = "prefit",
  weights,
  priorLong = normal(autoscale = TRUE),
  priorLong_intercept = normal(autoscale = TRUE),
  priorLong_aux = cauchy(0, 5, autoscale = TRUE),
  priorEvent = normal(autoscale = TRUE),
  priorEvent_intercept = normal(autoscale = TRUE),
  priorEvent_aux = cauchy(autoscale = TRUE),
  priorEvent_assoc = normal(autoscale = TRUE),
  prior_covariance = lkj(autoscale = TRUE),
  prior_PD = FALSE,
  algorithm = c("sampling", "meanfield", "fullrank"),
  adapt_delta = NULL,
  max_treedepth = 10L,
  QR = FALSE,
  sparse = FALSE,
  ...
)
}
\arguments{
\item{formulaLong}{A two-sided linear formula object describing both the 
fixed-effects and random-effects parts of the longitudinal submodel,
similar in vein to formula specification in the \strong{lme4} package
(see \code{\link[lme4]{glmer}} or the \strong{lme4} vignette for details). 
Note however that the double bar (\code{||}) notation is not allowed 
when specifying the random-effects parts of the formula, and neither
are nested grouping factors (e.g. \code{(1 | g1/g2))} or 
\code{(1 | g1:g2)}, where \code{g1}, \code{g2} are grouping factors. 
Offset terms can also be included in the model formula.
For a multivariate joint model (i.e. more than one longitudinal marker) 
this should be a list of such formula objects, with each element
of the list providing the formula for one of the longitudinal submodels.}

\item{dataLong}{A data frame containing the variables specified in
\code{formulaLong}. If fitting a multivariate joint model, then this can
be either a single data frame which contains the data for all 
longitudinal submodels, or it can be a list of data frames where each
element of the list provides the data for one of the longitudinal 
submodels.}

\item{formulaEvent}{A two-sided formula object describing the event
submodel. The left hand side of the formula should be a \code{Surv()} 
object. See \code{\link[survival]{Surv}}.}

\item{dataEvent}{A data frame containing the variables specified in
\code{formulaEvent}.}

\item{time_var}{A character string specifying the name of the variable 
in \code{dataLong} which represents time.}

\item{id_var}{A character string specifying the name of the variable in
\code{dataLong} which distinguishes between individuals. This can be
left unspecified if there is only one grouping factor (which is assumed
to be the individual). If there is more than one grouping factor (i.e.
clustering beyond the level of the individual) then the \code{id_var}
argument must be specified.}

\item{family}{The family (and possibly also the link function) for the 
longitudinal submodel(s). See \code{\link[lme4]{glmer}} for details. 
If fitting a multivariate joint model, then this can optionally be a
list of families, in which case each element of the list specifies the
family for one of the longitudinal submodels.}

\item{assoc}{A character string or character vector specifying the joint
model association structure. Possible association structures that can
be used include: "etavalue" (the default); "etaslope"; "etaauc"; 
"muvalue"; "muslope"; "muauc"; "shared_b"; "shared_coef"; or "null". 
These are described in the \strong{Details} section below. For a multivariate 
joint model, different association structures can optionally be used for 
each longitudinal submodel by specifying a list of character
vectors, with each element of the list specifying the desired association 
structure for one of the longitudinal submodels. Specifying \code{assoc = NULL}
will fit a joint model with no association structure (equivalent  
to fitting separate longitudinal and time-to-event models). It is also 
possible to include interaction terms between the association term 
("etavalue", "etaslope", "muvalue", "muslope") and observed data/covariates. 
It is also possible, when fitting a multivariate joint model, to include 
interaction terms between the association terms ("etavalue" or "muvalue") 
corresponding to the different longitudinal outcomes. See the 
\strong{Details} section as well as the \strong{Examples} below.}

\item{lag_assoc}{A non-negative scalar specifying the time lag that should be
used for the association structure. That is, the hazard of the event at 
time \emph{t} will be assumed to be associated with the value/slope/auc of 
the longitudinal marker at time \emph{t-u}, where \emph{u} is the time lag.
If fitting a multivariate joint model, then a different time lag can be used
for each longitudinal marker by providing a numeric vector of lags, otherwise
if a scalar is provided then the specified time lag will be used for all 
longitudinal markers. Note however that only one time lag  can be specified 
for linking each longitudinal marker to the 
event, and that that time lag will be used for all association structure
types (e.g. \code{"etavalue"}, \code{"etaslope"}, \code{"etaauc"}, 
\code{"muvalue"}, etc) that are specified for that longitudinal marker in
the \code{assoc} argument.}

\item{grp_assoc}{Character string specifying the method for combining information
across lower level units clustered within an individual when forming the
association structure. This is only relevant when a grouping factor is  
specified in \code{formulaLong} that corresponds to clustering within 
individuals. This can be specified as either \code{"sum"}, \code{mean},
\code{"min"} or \code{"max"}. For example, specifying \code{grp_assoc = "sum"}
indicates that the association structure should be based on a summation across 
the lower level units clustered within an individual, or specifying
\code{grp_assoc = "mean"}  indicates that the association structure 
should be based on the mean (i.e. average) taken across the lower level 
units clustered within an individual.
So, for example, specifying \code{assoc = "muvalue"} 
and \code{grp_assoc = "sum"} would mean that the log hazard at time 
\emph{t} for individual \emph{i} would be linearly related to the sum of
the expected values at time \emph{t} for each of the lower level 
units (which may be for example tumor lesions) clustered within that 
individual.}

\item{scale_assoc}{A non-zero numeric value specifying an optional scaling 
parameter for the association structure. This multiplicatively scales the 
value/slope/auc of the longitudinal marker by \code{scale_assoc} within the 
event submodel. When fitting a multivariate joint model, a scaling parameter 
must be specified for each longitudinal submodel using a vector of numeric 
values. Note that only one scaling parameter can be specified for each 
longitudinal submodel, and it will be used for all association structure 
types (e.g. \code{"etavalue"}, \code{"etaslope"}, \code{"etaauc"}, 
\code{"muvalue"}, etc) that are specified for that longitudinal marker in
the \code{assoc} argument.}

\item{epsilon}{The half-width of the central difference used to numerically
calculate the derivate when the \code{"etaslope"} association structure 
is used.}

\item{basehaz}{A character string indicating which baseline hazard to use
for the event submodel. Options are a B-splines approximation estimated 
for the log baseline hazard (\code{"bs"}, the default), a Weibull 
baseline hazard (\code{"weibull"}), or a piecewise
constant baseline hazard (\code{"piecewise"}). (Note however that there  
is currently limited post-estimation functionality available for
models estimated using a piecewise constant baseline hazard).}

\item{basehaz_ops}{A named list specifying options related to the baseline
hazard. Currently this can include: \cr
\describe{
  \item{\code{df}}{A positive integer specifying the degrees of freedom 
  for the B-splines if \code{basehaz = "bs"}, or the number of
  intervals used for the piecewise constant baseline hazard if 
  \code{basehaz = "piecewise"}. The default is 6.}
  \item{\code{knots}}{An optional numeric vector specifying the internal knot 
  locations for the B-splines if \code{basehaz = "bs"}, or the 
  internal cut-points for defining intervals of the piecewise constant 
  baseline hazard if \code{basehaz = "piecewise"}. Knots cannot be
  specified if \code{df} is specified. If not specified, then the 
  default is to use \code{df - 4} knots if \code{basehaz = "bs"},
  or \code{df - 1} knots if \code{basehaz = "piecewise"}, which are
  placed at equally spaced percentiles of the distribution of
  observed event times.}
}}

\item{qnodes}{The number of nodes to use for the Gauss-Kronrod quadrature
that is used to evaluate the cumulative hazard in the likelihood function. 
Options are 15 (the default), 11 or 7.}

\item{init}{The method for generating the initial values for the MCMC.
The default is \code{"prefit"}, which uses those obtained from 
fitting separate longitudinal and time-to-event models prior to 
fitting the joint model. The separate longitudinal model is a 
(possibly multivariate) generalised linear mixed 
model estimated using variational bayes. This is achieved via the 
\code{\link{stan_mvmer}} function with \code{algorithm = "meanfield"}.
The separate Cox model is estimated using \code{\link[survival]{coxph}}. 
This is achieved
using the and time-to-event models prior  
to fitting the joint model. The separate models are estimated using the
\code{\link[lme4]{glmer}} and \code{\link[survival]{coxph}} functions.
This should provide reasonable initial values which should aid the 
MCMC sampler. Parameters that cannot be obtained from 
fitting separate longitudinal and time-to-event models are initialised 
using the "random" method for \code{\link[rstan]{stan}}.
However it is recommended that any final analysis should ideally
be performed with several MCMC chains each initiated from a different
set of initial values; this can be obtained by setting
\code{init = "random"}. In addition, other possibilities for specifying 
\code{init} are the same as those described for \code{\link[rstan]{stan}}.}

\item{weights}{Experimental and should be used with caution. The 
user can optionally supply a 2-column data frame containing a set of
'prior weights' to be used in the estimation process. The data frame should
contain two columns: the first containing the IDs for each individual, and 
the second containing the corresponding weights. The data frame should only
have one row for each individual; that is, weights should be constant 
within individuals.}

\item{priorLong, priorEvent, priorEvent_assoc}{The prior distributions for the 
regression coefficients in the longitudinal submodel(s), event submodel,
and the association parameter(s). Can be a call to one of the various functions 
provided by \pkg{rstanarm} for specifying priors. The subset of these functions 
that can be used for the prior on the coefficients can be grouped into several 
"families":

\tabular{ll}{
  \strong{Family} \tab \strong{Functions} \cr 
  \emph{Student t family} \tab \code{normal}, \code{student_t}, \code{cauchy} \cr 
  \emph{Hierarchical shrinkage family} \tab \code{hs}, \code{hs_plus} \cr 
  \emph{Laplace family} \tab \code{laplace}, \code{lasso} \cr
}

See the \link[=priors]{priors help page} for details on the families and 
how to specify the arguments for all of the functions in the table above.
To omit a prior ---i.e., to use a flat (improper) uniform prior---
\code{prior} can be set to \code{NULL}, although this is rarely a good
idea.

\strong{Note:} Unless \code{QR=TRUE}, if \code{prior} is from the Student t
family or Laplace family, and if the \code{autoscale} argument to the 
function used to specify the prior (e.g. \code{\link{normal}}) is left at 
its default and recommended value of \code{TRUE}, then the default or 
user-specified prior scale(s) may be adjusted internally based on the scales
of the predictors. See the \link[=priors]{priors help page} for details on
the rescaling and the \code{\link{prior_summary}} function for a summary of
the priors used for a particular model.}

\item{priorLong_intercept, priorEvent_intercept}{The prior distributions
for the intercepts in the longitudinal submodel(s) and event submodel. 
Can be a call to \code{normal}, \code{student_t} or 
\code{cauchy}. See the \link[=priors]{priors help page} for details on 
these functions. To omit a prior on the intercept ---i.e., to use a flat
(improper) uniform prior--- \code{prior_intercept} can be set to
\code{NULL}.

\strong{Note:} The prior distribution for the intercept is set so it
applies to the value when all predictors are centered. Moreover, 
note that a prior is only placed on the intercept for the event submodel
when a Weibull baseline hazard has been specified. For the B-splines and
piecewise constant baseline hazards there is not intercept parameter that
is given a prior distribution; an intercept parameter will be shown in 
the output for the fitted model, but this just corresponds to the 
necessary post-estimation adjustment in the linear predictor due to the
centering of the predictiors in the event submodel.}

\item{priorLong_aux}{The prior distribution for the "auxiliary" parameters
in the longitudinal submodels (if applicable). 
The "auxiliary" parameter refers to a different parameter 
depending on the \code{family}. For Gaussian models \code{priorLong_aux} 
controls \code{"sigma"}, the error 
standard deviation. For negative binomial models \code{priorLong_aux} controls 
\code{"reciprocal_dispersion"}, which is similar to the 
\code{"size"} parameter of \code{\link[stats:NegBinomial]{rnbinom}}:
smaller values of \code{"reciprocal_dispersion"} correspond to 
greater dispersion. For gamma models \code{priorLong_aux} sets the prior on 
to the \code{"shape"} parameter (see e.g., 
\code{\link[stats:GammaDist]{rgamma}}), and for inverse-Gaussian models it is the 
so-called \code{"lambda"} parameter (which is essentially the reciprocal of
a scale parameter). Binomial and Poisson models do not have auxiliary 
parameters. 

\code{priorLong_aux} can be a call to \code{exponential} to 
use an exponential distribution, or \code{normal}, \code{student_t} or 
\code{cauchy}, which results in a half-normal, half-t, or half-Cauchy 
prior. See \code{\link{priors}} for details on these functions. To omit a 
prior ---i.e., to use a flat (improper) uniform prior--- set 
\code{priorLong_aux} to \code{NULL}.

If fitting a multivariate joint model, you have the option to
specify a list of prior distributions, however the elements of the list
that correspond to any longitudinal submodel which does not have an 
auxiliary parameter will be ignored.}

\item{priorEvent_aux}{The prior distribution for the "auxiliary" parameters
in the event submodel. The "auxiliary" parameters refers to different  
parameters depending on the baseline hazard. For \code{basehaz = "weibull"}
the auxiliary parameter is the Weibull shape parameter. For 
\code{basehaz = "bs"} the auxiliary parameters are the coefficients for the
B-spline approximation to the log baseline hazard.
For \code{basehaz = "piecewise"} the auxiliary parameters are the piecewise
estimates of the log baseline hazard.}

\item{prior_covariance}{Cannot be \code{NULL}; see \code{\link{priors}} for
more information about the prior distributions on covariance matrices.
Note however that the default prior for covariance matrices in 
\code{stan_jm} is slightly different to that in \code{\link{stan_glmer}} 
(the details of which are described on the \code{\link{priors}} page).}

\item{prior_PD}{A logical scalar (defaulting to \code{FALSE}) indicating
whether to draw from the prior predictive distribution instead of
conditioning on the outcome.}

\item{algorithm}{A string (possibly abbreviated) indicating the 
estimation approach to use. Can be \code{"sampling"} for MCMC (the
default), \code{"optimizing"} for optimization, \code{"meanfield"} for
variational inference with independent normal distributions, or
\code{"fullrank"} for variational inference with a multivariate normal
distribution. See \code{\link{rstanarm-package}} for more details on the
estimation algorithms. NOTE: not all fitting functions support all four
algorithms.}

\item{adapt_delta}{Only relevant if \code{algorithm="sampling"}. See 
the \link{adapt_delta} help page for details.}

\item{max_treedepth}{A positive integer specifying the maximum treedepth 
for the non-U-turn sampler. See the \code{control} argument in 
\code{\link[rstan]{stan}}.}

\item{QR}{A logical scalar defaulting to \code{FALSE}, but if \code{TRUE}
applies a scaled \code{\link{qr}} decomposition to the design matrix. The
transformation does not change the likelihood of the data but is
recommended for computational reasons when there are multiple predictors.
See the \link{QR-argument} documentation page for details on how
\pkg{rstanarm} does the transformation and important information about how
to interpret the prior distributions of the model parameters when using
\code{QR=TRUE}.}

\item{sparse}{A logical scalar (defaulting to \code{FALSE}) indicating
whether to use a sparse representation of the design (X) matrix. 
If \code{TRUE}, the the design matrix is not centered (since that would 
destroy the sparsity) and likewise it is not possible to specify both 
\code{QR = TRUE} and \code{sparse = TRUE}. Depending on how many zeros
there are in the design matrix, setting \code{sparse = TRUE} may make
the code run faster and can consume much less RAM.}

\item{...}{Further arguments passed to the function in the \pkg{rstan} 
package (\code{\link[rstan:stanmodel-method-sampling]{sampling}}, 
\code{\link[rstan:stanmodel-method-vb]{vb}}, or 
\code{\link[rstan:stanmodel-method-optimizing]{optimizing}}), 
corresponding to the estimation method named by \code{algorithm}. For example, 
if \code{algorithm} is \code{"sampling"} it is possible to specify \code{iter}, 
\code{chains}, \code{cores}, and other MCMC controls.  

Another useful argument that can be passed to \pkg{rstan} via \code{...} is
\code{refresh}, which specifies how often to print updates when sampling
(i.e., show the progress every \code{refresh} iterations). \code{refresh=0}
turns off the iteration updates.}
}
\value{
A \link[=stanreg-objects]{stanjm} object is returned.
}
\description{
\if{html}{\figure{stanlogo.png}{options: width="25" alt="https://mc-stan.org/about/logo/"}}
Fits a shared parameter joint model for longitudinal and time-to-event 
(e.g. survival) data under a Bayesian framework using Stan.
}
\details{
The \code{stan_jm} function can be used to fit a joint model (also 
  known as a shared parameter model) for longitudinal and time-to-event data 
  under a Bayesian framework. The underlying
  estimation is carried out using the Bayesian C++ package Stan 
  (\url{https://mc-stan.org/}). \cr
  \cr 
  The joint model may be univariate (with only one longitudinal submodel) or
  multivariate (with more than one longitudinal submodel). 
  For the longitudinal submodel a (possibly multivariate) generalised linear 
  mixed model is assumed with any of the \code{\link[stats]{family}} choices 
  allowed by \code{\link[lme4]{glmer}}. If a multivariate joint model is specified 
  (by providing a list of formulas in the \code{formulaLong} argument), then
  the multivariate longitudinal submodel consists of a multivariate generalized  
  linear model (GLM) with group-specific terms that are assumed to be correlated
  across the different GLM submodels. That is, within
  a grouping factor (for example, patient ID) the group-specific terms are
  assumed to be correlated across the different GLM submodels. It is 
  possible to specify a different outcome type (for example a different
  family and/or link function) for each of the GLM submodels, by providing
  a list of \code{\link[stats]{family}} objects in the \code{family} 
  argument. Multi-level 
  clustered data are allowed, and that additional clustering can occur at a 
  level higher than the individual-level (e.g. patients clustered within 
  clinics), or at a level lower than the individual-level (e.g. tumor lesions
  clustered within patients). If the clustering occurs at a level lower than
  the individual, then the user needs to indicate how the lower level 
  clusters should be handled when forming the association structure between
  the longitudinal and event submodels (see the \code{grp_assoc} argument
  described above). \cr
  \cr
  For the event submodel a parametric
  proportional hazards model is assumed. The baseline hazard can be estimated 
  using either a cubic B-splines approximation (\code{basehaz = "bs"}, the
  default), a Weibull distribution (\code{basehaz = "weibull"}), or a
  piecewise constant baseline hazard (\code{basehaz = "piecewise"}).
  If the B-spline or piecewise constant baseline hazards are used, 
  then the degrees of freedom or the internal knot locations can be 
  (optionally) specified. If
  the degrees of freedom are specified (through the \code{df} argument) then
  the knot locations are automatically generated based on the 
  distribution of the observed event times (not including censoring times). 
  Otherwise internal knot locations can be specified 
  directly through the \code{knots} argument. If neither \code{df} or
  \code{knots} is specified, then the default is to set \code{df} equal to 6.
  It is not possible to specify both \code{df} and \code{knots}. \cr
  \cr
  Time-varying covariates are allowed in both the 
  longitudinal and event submodels. These should be specified in the data 
  in the same way as they normally would when fitting a separate 
  longitudinal model using \code{\link[lme4]{lmer}} or a separate 
  time-to-event model using \code{\link[survival]{coxph}}. These time-varying
  covariates should be exogenous in nature, otherwise they would perhaps 
  be better specified as an additional outcome (i.e. by including them as an 
  additional longitudinal outcome in the joint model). \cr
  \cr
  Bayesian estimation of the joint model is performed via MCMC. The Bayesian  
  model includes independent priors on the 
  regression coefficients for both the longitudinal and event submodels, 
  including the association parameter(s) (in much the same way as the
  regression parameters in \code{\link{stan_glm}}) and
  priors on the terms of a decomposition of the covariance matrices of the
  group-specific parameters. 
  See \code{\link{priors}} for more information about the priors distributions
  that are available. \cr
  \cr
  Gauss-Kronrod quadrature is used to numerically evaluate the integral  
  over the cumulative hazard in the likelihood function for the event submodel.
  The accuracy of the numerical approximation can be controlled using the
  number of quadrature nodes, specified through the \code{qnodes} 
  argument. Using a higher number of quadrature nodes will result in a more 
  accurate approximation.
  
  \subsection{Association structures}{
  The association structure for the joint model can be based on any of the 
  following parameterisations: 
    \itemize{
      \item current value of the linear predictor in the 
        longitudinal submodel (\code{"etavalue"}) 
      \item first derivative (slope) of the linear predictor in the 
        longitudinal submodel (\code{"etaslope"}) 
      \item the area under the curve of the linear predictor in the 
        longitudinal submodel (\code{"etaauc"}) 
      \item current expected value of the longitudinal submodel 
        (\code{"muvalue"})
      \item the area under the curve of the expected value from the 
        longitudinal submodel (\code{"muauc"})
      \item shared individual-level random effects (\code{"shared_b"}) 
      \item shared individual-level random effects which also incorporate 
        the corresponding fixed effect as well as any corresponding 
        random effects for clustering levels higher than the individual)
        (\code{"shared_coef"})
      \item interactions between association terms and observed data/covariates
        (\code{"etavalue_data"}, \code{"etaslope_data"}, \code{"muvalue_data"}, 
        \code{"muslope_data"}). These are described further below.
      \item interactions between association terms corresponding to different 
        longitudinal outcomes in a multivariate joint model 
        (\code{"etavalue_etavalue(#)"}, \code{"etavalue_muvalue(#)"},
        \code{"muvalue_etavalue(#)"}, \code{"muvalue_muvalue(#)"}). These
        are described further below.      
      \item no association structure (equivalent to fitting separate 
        longitudinal and event models) (\code{"null"} or \code{NULL}) 
    }
  More than one association structure can be specified, however,
  not all possible combinations are allowed.   
  Note that for the lagged association structures baseline values (time = 0) 
  are used for the instances 
  where the time lag results in a time prior to baseline. When using the 
  \code{"etaauc"} or \code{"muauc"} association structures, the area under
  the curve is evaluated using Gauss-Kronrod quadrature with 15 quadrature 
  nodes. By default, \code{"shared_b"} and \code{"shared_coef"} contribute 
  all random effects to the association structure; however, a subset of the 
  random effects can be chosen by specifying their indices between parentheses 
  as a suffix, for example, \code{"shared_b(1)"} or \code{"shared_b(1:3)"} or 
  \code{"shared_b(1,2,4)"}, and so on. \cr
  \cr 
  In addition, several association terms (\code{"etavalue"}, \code{"etaslope"},
  \code{"muvalue"}, \code{"muslope"}) can be interacted with observed 
  data/covariates. To do this, use the association term's main handle plus a
  suffix of \code{"_data"} then followed by the model matrix formula in 
  parentheses. For example if we had a variable in our dataset for gender 
  named \code{sex} then we might want to obtain different estimates for the 
  association between the current slope of the marker and the risk of the 
  event for each gender. To do this we would specify 
  \code{assoc = c("etaslope", "etaslope_data(~ sex)")}. \cr
  \cr
  It is also possible, when fitting  a multivariate joint model, to include 
  interaction terms between the association terms themselves (this only
  applies for interacting \code{"etavalue"} or \code{"muvalue"}). For example, 
  if we had a joint model with two longitudinal markers, we could specify 
  \code{assoc = list(c("etavalue", "etavalue_etavalue(2)"), "etavalue")}.
  The first element of list says we want to use the value of the linear
  predictor for the first marker, as well as it's interaction with the
  value of the linear predictor for the second marker. The second element of 
  the list says we want to also include the expected value of the second marker 
  (i.e. as a "main effect"). Therefore, the linear predictor for the event 
  submodel would include the "main effects" for each marker as well as their
  interaction. \cr
  \cr
  There are additional examples in the \strong{Examples} section below.
  }
}
\examples{
if (.Platform$OS.type != "windows" || .Platform$r_arch !="i386") {
\donttest{

#####
# Univariate joint model, with association structure based on the 
# current value of the linear predictor
f1 <- stan_jm(formulaLong = logBili ~ year + (1 | id), 
              dataLong = pbcLong,
              formulaEvent = Surv(futimeYears, death) ~ sex + trt, 
              dataEvent = pbcSurv,
              time_var = "year",
              # this next line is only to keep the example small in size!
              chains = 1, cores = 1, seed = 12345, iter = 1000)
print(f1) 
summary(f1) 
        
#####
# Univariate joint model, with association structure based on the 
# current value and slope of the linear predictor
f2 <- stan_jm(formulaLong = logBili ~ year + (year | id), 
              dataLong = pbcLong,
              formulaEvent = Surv(futimeYears, death) ~ sex + trt, 
              dataEvent = pbcSurv,
              assoc = c("etavalue", "etaslope"),
              time_var = "year",
              chains = 1, cores = 1, seed = 12345, iter = 1000)
print(f2)  

#####
# Univariate joint model, with association structure based on the 
# lagged value of the linear predictor, where the lag is 2 time 
# units (i.e. 2 years in this example)
f3 <- stan_jm(formulaLong = logBili ~ year + (1 | id), 
              dataLong = pbcLong,
              formulaEvent = Surv(futimeYears, death) ~ sex + trt, 
              dataEvent = pbcSurv,
              time_var = "year",
              assoc = "etavalue", lag_assoc = 2,
              chains = 1, cores = 1, seed = 12345, iter = 1000)
print(f3) 

#####
# Univariate joint model, where the association structure includes 
# interactions with observed data. Here we specify that we want to use 
# an association structure based on the current value of the linear 
# predictor from the longitudinal submodel (i.e. "etavalue"), but we 
# also want to interact this with the treatment covariate (trt) from
# pbcLong data frame, so that we can estimate a different association 
# parameter (i.e. estimated effect of log serum bilirubin on the log 
# hazard of death) for each treatment group
f4 <- stan_jm(formulaLong = logBili ~ year + (1 | id), 
              dataLong = pbcLong,
              formulaEvent = Surv(futimeYears, death) ~ sex + trt, 
              dataEvent = pbcSurv,
              time_var = "year",
              assoc = c("etavalue", "etavalue_data(~ trt)"),
              chains = 1, cores = 1, seed = 12345, iter = 1000)
print(f4)

######
# Multivariate joint model, with association structure based 
# on the current value and slope of the linear predictor in the 
# first longitudinal submodel and the area under the marker 
# trajectory for the second longitudinal submodel
mv1 <- stan_jm(
        formulaLong = list(
          logBili ~ year + (1 | id), 
          albumin ~ sex + year + (year | id)),
        dataLong = pbcLong,
        formulaEvent = Surv(futimeYears, death) ~ sex + trt, 
        dataEvent = pbcSurv,
        assoc = list(c("etavalue", "etaslope"), "etaauc"), 
        time_var = "year",
        chains = 1, cores = 1, seed = 12345, iter = 100)
print(mv1)

#####
# Multivariate joint model, where the association structure is formed by 
# including the expected value of each longitudinal marker (logBili and 
# albumin) in the linear predictor of the event submodel, as well as their 
# interaction effect (i.e. the interaction between the two "etavalue" terms). 
# Note that whether such an association structure based on a marker by 
# marker interaction term makes sense will depend on the context of your 
# application -- here we just show it for demostration purposes).
mv2 <- stan_jm(
        formulaLong = list(
          logBili ~ year + (1 | id), 
          albumin ~ sex + year + (year | id)),
        dataLong = pbcLong,
        formulaEvent = Surv(futimeYears, death) ~ sex + trt, 
        dataEvent = pbcSurv,
        assoc = list(c("etavalue", "etavalue_etavalue(2)"), "etavalue"),
        time_var = "year", 
        chains = 1, cores = 1, seed = 12345, iter = 100)
        
#####
# Multivariate joint model, with one bernoulli marker and one
# Gaussian marker. We will artificially create the bernoulli
# marker by dichotomising log serum bilirubin
pbcLong$ybern <- as.integer(pbcLong$logBili >= mean(pbcLong$logBili))
mv3 <- stan_jm(
        formulaLong = list(
          ybern ~ year + (1 | id), 
          albumin ~ sex + year + (year | id)),
        dataLong = pbcLong,
        formulaEvent = Surv(futimeYears, death) ~ sex + trt, 
        dataEvent = pbcSurv,
        family = list(binomial, gaussian),
        time_var = "year", 
        chains = 1, cores = 1, seed = 12345, iter = 1000)
}
}

}
\seealso{
\code{\link{stanreg-objects}}, \code{\link{stanmvreg-methods}}, 
  \code{\link{print.stanmvreg}}, \code{\link{summary.stanmvreg}},
  \code{\link{posterior_traj}}, \code{\link{posterior_survfit}}, 
  \code{\link{posterior_predict}}, \code{\link{posterior_interval}},
  \code{\link{pp_check}}, \code{\link{ps_check}}, \code{\link{stan_mvmer}}.
}
