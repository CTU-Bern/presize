# functions for precision based sample size
#
# Descriptive statistics
#  - mean
#  - rate
#  - proportion



# Mean ---------------
#' Sample size or precision for a mean
#'
#' \code{prec_mean} returns the sample size or the precision for the provided
#' mean and standard deviation
#'
#' Exactly one of the parameters \code{n, prec} must be passed as NULL, and that
#' parameter is determined from the other.
#'
#' The confidence interval calculated as \eqn{z * sd / sqrt(n)}, with z from
#' the standard normal distribution.
#'
#' @param mu mean. A vector of length one.
#' @param sd standard deviation. A vector of length one.
#' @param n number of observations
#' @param prec precision (half the width of the conficende interval)
#' @param conf.level confidence level
#' @return Object of class "power.htest", a list with
#'   \describe{
#'   \item{mu}{mean}
#'   \item{sd}{standard deviation}
#'   \item{n}{sample size}
#'   \item{prec}{precision (half the width of the conficence interval)}
#'   \item{lwr}{lower end of confidence interval}
#'   \item{upr}{higher end of confidence interval}
#'   } augmented with method and note elements.
#' @examples
#' prec_mean(mu = 5, sd = 2.5, n = 20)
#' prec_mean(mu = 5, sd = 2.5, prec = 1.1)
prec_mean <- function(mu, sd, n = NULL, prec = NULL, conf.level = 0.95) {
  if (!is.null(mu) && !is.numeric(mu))
    stop("'mu' must be numeric")
  if (!is.null(sd) && !is.numeric(sd))
    stop("'sd' must be numeric")
  null_arg <- sapply(list(n, prec), is.null)
  if (sum(null_arg) != 1)
    stop("exactly one of 'n', and 'prec' must be NULL")
  numrange_check(conf.level)

  if (is.null(n) & sum(sapply(list(mu, sd, prec, conf.level), length) > 1) > 1) {
    dat <- expand.grid(mu, sd, prec, conf.level)
    mu <- dat[[1]]
    sd <- dat[[2]]
    prec <- dat[[3]]
    conf.level <- dat[[4]]
  }
  if (is.null(prec) & sum(sapply(list(mu, sd, n, conf.level), length) > 1) > 1) {
    dat <- expand.grid(mu, sd, n, conf.level)
    mu <- dat[[1]]
    sd <- dat[[2]]
    n <- dat[[3]]
    conf.level <- dat[[4]]
  }

  z <- qnorm((1 + conf.level) / 2)
  if (is.null(prec)) {
    prec <- z * sd / sqrt(n)
  }
  if (is.null(n)) {
    n <- (z * sd / prec) ^ 2
  }

  structure(list(mu = mu,
                 sd = sd,
                 n = n,
                 prec = prec,
                 lwr = mu - prec,
                 upr = mu + prec,
                 #note = "n is number in *each* group",
                 method = "Sample size or precision for mean"),
            class = "power_prec")
}




# Rate ---------------
#' Sample size or precision for a rate
#'
#' \code{prec_rate} returns the sample size or the precision for the provided
#' rate
#'
#' Exactly one of the parameters \code{r, prec} must be passed as NULL, and that
#' parameter is determined from the other.
#'
#' The \code{score}, variance stabilizing (\code{vs}), \code{exact}, and
#' \code{wald} method are implemented to calculate the rate and the precision.
#' For few events \code{x} (<5), the exact method is recommended.
#'
#' If more than one method is specified or the method is miss-specified, the
#' 'score' method will be used.
#'
#' #' \code{\link[stats]{uniroot}} is used to solve n for the score and
#' exact method. Agresti-coull can be abbreviated by ac.
#'
#' @param r rate or rate ratio.
#' @param x number of events
#' @param method The method to use to calculate precision. Exactly one method
#'   may be provided. Methods can be abbreviated.
#' @param tol numerical tolerance used in root finding, the default providing
#'   (at least) four significant digits
#' @inheritParams prec_mean
#' @return Object of class "power.htest", a list of arguments (including the
#'   computed one) augmented with method and note elements.
#' @seealso \code{\link[stats]{poisson.test}}
#' @references Barker, L. (2002) \emph{A Comparison of Nine Confidence Intervals
#' for a Poisson Parameter When the Expected Number of Events is \eqn{\le} 5},
#' The American Statistician, 56:2, 85-89,
#' \href{https://doi.org/10.1198/000313002317572736}{DOI:
#' 10.1198/000313002317572736}
prec_rate <- function(r, x = NULL, prec = NULL, conf.level = 0.95,
                      method = c("score", "vs", "exact", "wald"),
                      tol = .Machine$double.eps^0.25) {
  if (length(method) > 1) {
    warning("more than one method was chosen, 'score' will be used")
    method <- "score"
  }
  methods <- c("score", "vs", "exact", "wald")
  i <- pmatch(method, methods)
  meth <- methods[i]
  if (is.na(i)) {
    warning("Method '", method, "' is not available. 'score' will be used.")
    meth <- "score"
  }
  #x <- r * t
  alpha <- (1 - conf.level) / 2
  z <- qnorm((1 + conf.level) / 2)
  z2 <- z * z
  # Wald
  if (meth == "wald") {
    if (is.null(prec)) {
      prec <- z * r * sqrt(1 / x)
    }
    if (is.null(x)) {
      x <- (z * r / prec) ^ 2
    }
    radj <- r
  }

  # score
  if (meth == "score") {
    sc <- quote({
      z * sqrt(r * (4 + z2 / x)) / sqrt(4 * x / r)
    })
    if (is.null(prec)) {
      prec <- eval(sc)
    }
    if (is.null(x)) {
      x <- uniroot(function(x) eval(sc) - prec,
                   c(1, 1e+07), tol = tol)$root
    }
    radj <- r + z2 * r / (2 * x)
  }

  # variance stabilizing
  if (meth == "vs") {
    if (is.null(prec))
      prec <- z * r * sqrt(1 / x)
    if (is.null(x))
      x <- (z * r / prec) ^ 2
    if (r == 0)
      warning("The conficence interval is degenerate at z^2/(4t), if r is 0.")
    radj <- r * (1 + z2 / (4 * x))
  }


  # exact
  if (meth == "exact") {
    ex <- function(x, r) {
      t <- x / r
      lwr <- qgamma(alpha, x) / t
      upr <- qgamma(1 - alpha, x + 1) / t
      prec <- (upr - lwr) / 2
      list(lwr = lwr,
           upr = upr,
           prec = prec)
    }
    if (is.null(x)) {
      x <- uniroot(function(x) ex(x, r)$prec - prec,
                   c(1, 1e+07), tol = tol)$root
    }
    res <- ex(x, r)
    lwr <- res$lwr
    upr <- res$upr
    radj <- r
    if (is.null(prec))
      prec <- res$prec
  } else {
    lwr <- radj - prec
    upr <- radj + prec
  }

  if(lwr < 0)
    warning("The lower end of the confidence interval is numerically below 0 and non-sensible. Please choose another method.")

  structure(list(r = r,
                 x = x,
                 prec = prec,
                 radj = radj,
                 lwr = lwr,
                 upr = upr,
                 note = paste(round(x / r, 1), "units of time are needed to accumulate 'x' events."),
                 method = paste("Sample size or precision for a rate with", meth, "confidence interval")),
            class = "power.htest")
}




# Proportion ---------------
#' Sample size or precision for a proportion
#'
#' \code{prec_prop} returns the sample size or the precision for the provided
#' proportion
#'
#' Exactly one of the parameters \code{n, prec} must be passed as NULL, and that
#' parameter is determined from the other.
#'
#' The wilson, agresti-coull, exact, and wald method are implemented. The
#' wilson mehtod is suggested for small n (< 40), and the agresti-coull method
#' is suggested for larger n (see reference). The wald method is not suggested,
#' but provided due to its widely distributed use.
#'
#' \code{\link[stats]{uniroot}} is used to solve n for the agresti-coull,
#' wilson, and exact method. Agresti-coull can be abbreviated by ac.
#'
#' @param p proportion
#' @param method The method to use to calculate sample size or precision.
#'   Exactly one method may be provided. Methods can be abbreviated.
#' @inheritParams prec_mean
#' @inheritParams prec_rate
#' @return Object of class "power.htest", a list of arguments (including the
#'   computed one) augmented with method and note elements.
#' @seealso \code{\link[stats]{binom.test}}, \code{\link[binom]{binom.confint}},
#'   \code{\link[Hmisc]{binconf}}
#' @references Brown LD, Cai TT, DasGupta A (2001) \emph{Interval Estimation for
#'   a Binomial Proportion}, Statistical Science, 16:2, 101-117,
#'   \href{https://doi.org/10.1214/ss/1009213286}{doi:10.1214/ss/1009213286}
#' @examples
prec_prop <- function(p, n = NULL, prec = NULL, conf.level = 0.95,
                      method = c("wilson", "agresti-coull", "exact", "wald"),
                      tol = .Machine$double.eps^0.25) {
  if (sum(sapply(list(n, prec), is.null)) != 1)
    stop("exactly one of 'n', and 'prec' must be NULL")
  numrange_check(conf.level)
  numrange_check(p)

  if (length(method) > 1) {
    warning("more than one method was chosen, 'wilson' will be used")
    method <- "wilson"
  }

  methods <- c("wald", "ac", "agresti-coull", "exact", "wilson")
  id <- pmatch(method, methods)
  meth <- methods[id]
  if (meth == "ac")
    meth <- "agresti-coull"
  if (is.na(id)) {
    warning("Method '", method, "' is not available, 'wilson' will be used.")
    meth <- "wilson"
  }

  alpha <- (1 - conf.level) / 2
  z <- qnorm(1 - alpha)
  z2 <- z * z

  if (meth == "wald") {
    if (is.null(prec)) {
      prec <- z * sqrt(p * (1 - p) / n)
    }
    if (is.null(n))
      n <- p * (1 - p) / (prec / z) ^ 2
    padj <- p
  }

  if (meth == "agresti-coull") {
    ac <- quote({
      n_ <- n + z2
      x_ <- p * n + 0.5 * z2
      padj <- x_ / n_
      z * sqrt(padj * (1 - padj) / n_)
    })
    if (is.null(prec)) {
      prec <- eval(ac)
    }
    if (is.null(n)) {
      n <- uniroot(function(n) eval(ac) - prec,
                   c(1, 1e+07), tol = tol)$root
    }
    padj <- (p * n + 0.5 * z2) / (n + z2)   # check for correctness
  }

  if (meth == "wilson") {
    wil <- quote({(z * sqrt(n) / (n + z2)) * sqrt(p * (1 - p) + z2 / (4 * n))})
    if (is.null(prec))
      prec <- eval(wil)
    if (is.null(n)) {
      n <- uniroot(function(n) eval(wil) - prec,
                   c(1, 1e+07), tol = tol)$root
    }
    padj <- (n * p + z2 / 2) / (n + z2)
  }

  if (meth == "exact") {
    ex <- function(n, p) {
      x <- p * n
      lwr <- qbeta(alpha, x, n - x + 1)
      if (x == 0)
        lwr <- 0
      upr <- qbeta(1 - alpha, x + 1, n - x)
      if (x == 1)
        upr <- 1
      prec <- (upr - lwr) / 2
      list(lwr = lwr,
           upr = upr,
           prec = prec)
    }
    if (is.null(n)) {
      n <- uniroot(function(n) ex(n, p)$prec - prec,
                   c(1, 1e+07), tol = tol)$root
    }
    res <- ex(n, p)
    lwr <- res$lwr
    upr <- res$upr
    padj <- p
    if (is.null(prec))
      prec <- res$prec
  } else {
    lwr <- padj - prec
    upr <- padj + prec
  }


  if(lwr < 0)
    warning("The lower end of the confidence interval is numerically below 0 and non-sensible. Please choose 'wilson' method.")
  if(upr > 1)
    warning("The upper end of the confidence interval is numerically above 1 and non-sensible. Please choose 'wilson' method.")

  structure(list(p = p,
                 n = n,
                 prec = prec,
                 padj = padj,
                 lwr = lwr,
                 upr = upr,
                 note = "padj is the adjusted proportion, from which the ci is calculated.",
                 method = paste0("Sample size or precision for a proportion with ", meth,
                                " confidence interval.\n",
                                conf.level * 100, "% confidence level was chosen.")),
            class = "power.htest")
}
