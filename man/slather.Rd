% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layers.R
\name{slather}
\alias{slather}
\title{Spread a layer of frosting on a fitted workflow}
\usage{
slather(object, components, workflow, new_data, ...)
}
\arguments{
\item{object}{a workflow with \code{frosting} postprocessing steps}

\item{components}{a list of components containing model information. These
will be updated and returned by the layer. These should be
\itemize{
\item \code{mold} - the output of calling \code{hardhat::mold()} on the workflow. This
contains information about the preprocessing, including the recipe.
\item \code{forged} - the output of calling \code{hardhat::forge()} on the workflow.
This should have predictors and outcomes for the \code{new_data}. It will
have three components \code{predictors}, \code{outcomes} (if these were in the
\code{new_data}), and \code{extras} (usually has the rest of the data, including
\code{keys}).
\item \code{keys} - we put the keys (\code{time_value}, \code{geo_value}, and any others)
here for ease.
}}

\item{workflow}{an object of class workflow}

\item{new_data}{a data frame containing the new predictors to preprocess
and predict on}

\item{...}{additional arguments used by methods. Currently unused.}
}
\value{
The \code{components} list. In the same format after applying any updates.
}
\description{
Slathering frosting means to implement a postprocessing layer. When
creating a new postprocessing layer, you must implement an S3 method
for this function
}
