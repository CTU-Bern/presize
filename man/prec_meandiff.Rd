% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/differences.R
\name{prec_meandiff}
\alias{prec_meandiff}
\title{Sample size or precision for a mean difference}
\usage{
prec_meandiff(
  delta,
  sd1,
  sd2 = sd1,
  n1 = NULL,
  r = 1,
  conf.width = NULL,
  conf.level = 0.95,
  variance = c("equal", "unequal"),
  ...
)
}
\arguments{
\item{delta}{difference in means between the two groups.}

\item{sd1}{standard deviation in group 1.}

\item{sd2}{standard deviation in group 2.}

\item{n1}{number of patients in group 1.}

\item{r}{allocation ratio (relative size of group 2 and group 1 (n2 / n1)).}

\item{conf.width}{precision (the full width of the confidence interval).}

\item{conf.level}{confidence level.}

\item{variance}{\code{equal} (\emph{default}) or \code{unequal} variance.}

\item{...}{other options to uniroot (e.g. \code{tol})}
}
\value{
Object of class "presize", a list of arguments (including the
  computed one) augmented with method and note elements.
}
\description{
\code{prec_meandiff} returns the sample size or the precision for the
provided mean difference and standard deviations. For paired differences, use
\code{prec_mean}, as it is equivalent to a simple mean.
}
\details{
Exactly one of the parameters \code{n} or \code{conf.width} must be passed as NULL,
and that parameter is determined from the other.
}
\examples{
# mean difference of 5, SD of 2.5, CI width with 20 participants assuming equal variances
prec_meandiff(delta = 5, sd1 = 2.5, n1 = 20, var = "equal")
# mean difference of 5, SD of 2.5, number of participants for a CI width of 3,
#  assuming equal variances
prec_meandiff(delta = 5, sd1 = 2.5, conf.width = 3, var = "equal")
}
