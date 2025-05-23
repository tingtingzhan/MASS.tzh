% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stepAIC_complete.R
\name{stepAIC_complete}
\alias{stepAIC_complete}
\title{Perform \link[MASS]{stepAIC} on Complete Data}
\usage{
stepAIC_complete(object, lower = ~1, upper, ...)
}
\arguments{
\item{object}{a regression model}

\item{lower}{\link[stats]{formula} of the lower \code{scope} (see function \link[MASS]{stepAIC}),
default \code{~1} indicating an intercept-only minimum model in a \strong{downward} selection.}

\item{upper}{\link[stats]{formula} of \strong{additional} predictors of the upper \code{scope} (see function \link[MASS]{stepAIC})
of a \strong{forward} selection.
If parameter \code{upper} is provided, then parameter \code{lower} is ignored.}

\item{...}{additional parameters not in use}
}
\description{
Perform \link[MASS]{stepAIC} on complete data, and \link[stats]{update} the return with full data.
}
\details{
Read section \strong{Note} of function \link[MASS]{stepAIC} documentation.
}
\note{
Function \link[MASS]{stepAIC} (as of 2025-02-19) ??? does not have a parameter for the end user to specify the
threshold of AIC improvement, that should be regarded as an improvement.
Instead, this quantity is hard coded (in \code{if (bAIC >= AIC + 1e-07) break}).

Function \link[MASS]{stepAIC} (as of 2025-02-19) is hard coded for S3 regression object (e.g., \code{object$formula <- Terms}).
}
\examples{
library(MASS)
sapply(airquality, FUN = \(i) mean(is.na(i))) # missingness in `Ozone` and `Solar.R`
summary(m <- lm(Temp ~ Ozone + Solar.R + Wind, data = airquality))
tryCatch(m |> stepAIC(trace = FALSE), error = identity)
m |> stepAIC_complete()
lm(Temp ~ Solar.R + Wind, data = airquality) |>
 stepAIC_complete(upper = ~ Ozone) # not necessarily the same!
 
library(lme4); library(HSAUR3)
?lme4::glmer
gm2 = glmer(outcome ~ treatment*visit + (1|patientID), data=toenail, family=binomial,nAGQ=20)
gm2 |> stepAIC_complete()
}
