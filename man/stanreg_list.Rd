% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stanreg_list.R
\name{stanreg_list}
\alias{stanreg_list}
\alias{stanmvreg_list}
\alias{stanjm_list}
\alias{print.stanreg_list}
\title{Create lists of fitted model objects, combine them, or append new models to
existing lists of models.}
\usage{
stanreg_list(..., model_names = NULL)

stanmvreg_list(..., model_names = NULL)

stanjm_list(..., model_names = NULL)

\method{print}{stanreg_list}(x, ...)
}
\arguments{
\item{...}{Objects to combine into a \code{"stanreg_list"},
\code{"stanmvreg_list"}, or \code{"stanjm_list"}. Can be fitted model
objects, existing \code{"stan*_list"} objects to combine, or one existing
\code{"stan*_list"} object followed by fitted model objects to append to
the list.}

\item{model_names}{Optionally, a character vector of model names. If not
specified then the names are inferred from the name of the objects passed
in via \code{...}. These model names are used, for example, when printing
the results of the \code{loo_compare.stanreg_list} and
\code{loo_model_weights.stanreg_list} methods.}

\item{x}{The object to print.}
}
\value{
A list of class \code{"stanreg_list"}, \code{"stanmvreg_list"}, or
  \code{"stanjm_list"}, containing the fitted model objects and some metadata
  stored as attributes.
}
\description{
Create lists of fitted model objects, combine them, or append new models to
existing lists of models.
}
\seealso{
\code{\link{loo_model_weights}} for usage of \code{stanreg_list}.
}
