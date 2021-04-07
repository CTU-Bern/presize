# functions for precision based sample size
#
# Practical considerations
#  - budget




# Budget ---------------
#' Sample size or precision for a given budget
#'
#' \code{prec_budget} returns the sample size or the precision for the provided
#' budget and cost per sample.
#'
#' Exactly one of the parameters \code{n} or \code{conf.width} must be passed as NULL,
#' and that parameter is determined from the other.
#'
#' The precision is defined as the full width of the confidence interval. The
#' confidence interval calculated as \eqn{t(n - 1) * sd / sqrt(n)}, with t(n-1)
#' from the t-distribution with n-1 degrees of freedom.
#'
#' \code{\link[stats]{uniroot}} is used to solve \code{n}.
#'
#' @param budget total research budget available.
#' @param price cost per sample.
#' @param sd standard deviation.
#' @param n number of observations.
#' @param fail proportion of samples expected to fail.
#' @return Object of class "presize", a list with total \code{budget}, \code{price} per sample, \code{n} sample size,
#'  augmented with method and note elements.
#' @examples
#' prec_budget(budget = 5000, price = 250, n = 20)
#' prec_budget(budget = 1000, price = 42, conf.width = 2.34)  # approximately the inverse of above
#' @importFrom stats qt
#' @importFrom stats qnorm
#' @importFrom stats uniroot
#' @export
prec_budget <- function(budget, price, sd = NULL, n = NULL, fail = 0, conf.width = NULL, conf.level = 0.95,
                      ...) {
  if (!is.null(budget) && !is.numeric(budget))
    stop("'budget' must be numeric")
  if (!is.null(price) && !is.numeric(price))
    stop("'price' must be numeric")
  if (sum(sapply(list(n, conf.width), is.null)) != 1)
    stop("exactly one of 'n', and 'conf.width' must be NULL")
  numrange_check(conf.level)
  if (!is.null(fail) && !is.numeric(fail)){
    stop("'fail' must be numeric")
  } else {
    if (fail >= 1 & fail <= 100){
      fail <- fail / 100
    } else {
      if (fail < 0 | fail > 100){
        stop("'fail' must be a proportion (0 <= x <= 1) or a percentage (1 <= x <= 100)")
      }
    }
  }

  alpha <- (1 - conf.level) / 2

  ci <- quote({
    tval <- qt(1 - alpha, n - 1)
    tval * sd / sqrt(n)
  })

  if (is.null(conf.width)) {
    if (!is.null(sd) && !is.numeric(sd))
      stop("'sd' must be numeric")
    prec <- eval(ci)
    est <- "precision"
  }
  if (is.null(n)) {
    if (!is.null(sd) && !is.numeric(sd))
      sd <- 1
    prec <- conf.width / 2
    f <- function(budget, price, fail){
      floor((budget / price) * (1 - fail))
    }
    n <- mapply(f, budget = budget, price = price, fail = fail)
    est <- "sample size"
  }

  conf.level <- NA
  conf.width <- NA
  prec <- NA
  mu <- NA
  sd <- NA


  structure(list(budget = budget,
                 price = price,
                 mu = mu,
                 sd = sd,
                 n = n,
                 conf.width = 2 * prec,
                 conf.level = conf.level,

                 #note = "n is number in *each* group",
                 method = paste(est, "for budget")),
            class = "presize")
}
