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
#' mean and standard deviation.
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
#' @param mu mean.
#' @param sd standard deviation.
#' @param n number of observations.
#' @param conf.width precision (the full width of the confidence interval).
#' @param conf.level confidence level.
#' @param ... other arguments to uniroot (e.g. \code{tol}).
#' @return Object of class "presize", a list with \code{mu} mean, \code{sd} standard deviation, \code{n} sample size,
#' \code{conf.width} precision (the width of the confidence interval),
#' \code{lwr} lower bound of confidence interval, \code{upr} upper bound of confidence interval,
#'  augmented with method and note elements.
#' @examples
#' prec_mean(mu = 5, sd = 2.5, n = 20)
#' prec_mean(mu = 5, sd = 2.5, conf.width = 2.34)  # approximately the inverse of above
#' @importFrom stats qt
#' @importFrom stats qnorm
#' @importFrom stats uniroot
#' @export
prec_mean <- function(mu, sd, n = NULL, conf.width = NULL, conf.level = 0.95,
                      ...) {

  numrange_check_gt(sd)
  numrange_check(conf.level)
  if (sum(sapply(list(n, conf.width), is.null)) != 1)
    stop("exactly one of 'n', and 'conf.width' must be NULL")
  if(!is.null(n)) numrange_check_gt(n)
  if(!is.null(conf.width)) numrange_check_gt(conf.width)

  alpha <- (1 - conf.level) / 2

  ci <- quote({
    tval <- qt(1 - alpha, n - 1)
    tval * sd / sqrt(n)
  })

  if (is.null(conf.width)) {
    prec <- eval(ci)
    est <- "precision"
  }
  if (is.null(n)) {
    prec <- conf.width / 2
    f <- function(sd, prec, alpha) uniroot(function(n) eval(ci) - prec,
                                    c(2, 1e+07), ...)$root
    if(conf.width<min(sd)){
      n <- try(mapply(f, sd = sd, prec = prec, alpha = alpha), silent = TRUE)
      if(inherits(n,"try-error")) stop("'conf.width' too small")
      est <- "sample size"
    } else {
      n <- try(mapply(f, sd = sd, prec = prec, alpha = alpha), silent = TRUE)
      if(inherits(n,"try-error")) stop("'conf.width' too wide")
      est <- "sample size"
    }
  }

  structure(list(mu = mu,
                 sd = sd,
                 n = n,
                 conf.width = 2 * prec,
                 conf.level = conf.level,
                 lwr = mu - prec,
                 upr = mu + prec,
                 #note = "n is number in *each* group",
                 method = paste(est, "for mean")),
            class = "presize")
}




# Rate ---------------
#' Sample size or precision for a rate
#'
#' \code{prec_rate} returns the sample size or the precision for the provided
#' rate.
#'
#' Exactly one of the parameters \code{r} or \code{conf.width} must be passed as NULL,
#' and that parameter is determined from the other.
#'
#' The \code{score}, variance stabilizing (\code{vs}), \code{exact}, and
#' \code{wald} method are implemented to calculate the rate and the precision.
#' For few events \code{x} (<5), the exact method is recommended.
#'
#' If more than one method is specified or the method is miss-specified, the
#' 'score' method will be used.
#'
#' \code{\link[stats]{uniroot}} is used to solve n for the score and
#' exact method.
#'
#' @param r rate or rate ratio.
#' @param x number of events.
#' @param conf.width precision (the full width of the confidence interval).
#'  Should not exceed 5 times \code{r}.
#' @param method The method to use to calculate precision. Exactly one method
#'   may be provided. Methods can be abbreviated.
#' @inheritParams prec_mean
#' @return Object of class "presize", a list of arguments (including the
#'   computed one) augmented with method and note elements.
#' @seealso \code{\link[stats]{poisson.test}}
#' @references Barker, L. (2002) \emph{A Comparison of Nine Confidence Intervals
#' for a Poisson Parameter When the Expected Number of Events is \eqn{\le} 5},
#' The American Statistician, 56:2, 85-89,
#' \href{https://doi.org/10.1198/000313002317572736}{DOI:
#' 10.1198/000313002317572736}
#' @examples
#' prec_rate(2.5, x = 20, met = "score")
#' prec_rate(2.5, conf.width = 2.243, met = "score")
#' prec_rate(2.5, x = 20, met = "exact")
#' # vs and wald have the same conf.width, but different lwr and upr
#' prec_rate(2.5, x = 20, met = "wald")
#' prec_rate(2.5, x = 20, met = "vs")
#' @export
prec_rate <- function(r, x = NULL, conf.width = NULL, conf.level = 0.95,
                               method = c("score", "vs", "exact", "wald"),
                               ...) {

  if (sum(sapply(list(x, conf.width), is.null)) != 1)
    stop("exactly one of 'x', and 'conf.width' must be NULL")

  numrange_check_gt(r)
  if(!is.null(x)) numrange_check_gt(x)
  if(!is.null(conf.width)) numrange_check_gt(conf.width)

  # checks for the method
  if (length(method) > 1) {
    warning("more than one method was chosen, 'score' will be used")
  }
  methods <- c("score", "vs", "exact", "wald")
  matched_meth <- match.arg(method,
                            methods)
  meth <- switch(matched_meth,
                 score = "score",
                 vs = "vs",
                 exact = "exact",
                 wald = "wald",
                 "score")

  if (is.null(x)) {
    prec <- conf.width * 0.5
    est <- "sample size"
  } else {
    est <- "precision"
  }

  alpha <- (1 - conf.level) / 2
  z <- qnorm(1 - alpha)
  z2 <- z * z

  quo <- switch(meth,
                score = quote(z * sqrt(r * (4 + z2 / x)) / sqrt(4 * x / r)),
                vs = quote(z * r * sqrt(1 / x)),
                exact = quote({
                  t <- x / r
                  lwr <- qgamma(alpha, x) / t
                  upr <- qgamma(1 - alpha, x + 1) / t
                  ps <- (upr - lwr) / 2
                  list(lwr = lwr,
                       upr = upr,
                       ps = ps)
                }),
                wald = quote(z * r * sqrt(1 / x)))
  get_radj <- switch(meth,
                     wald = quote(r),
                     score = quote(r + z2 * r / (2 * x)),
                     vs = quote(r * (1 + z2 / (4 * x))),
                     exact = quote(r))
  get_x <- switch(meth,
                  wald = quote((z * r / prec) ^ 2),
                  score = quote(
                    mapply(function(r, prec, z, z2){
                      uniroot(function(x) eval(quo) - prec,
                              c(1, 1e+07), ...)$root
                    },
                    r = r, prec = prec, z = z, z2 = z2)
                  ),
                  vs = quote((z * r / prec) ^ 2),
                  exact = quote(
                    mapply(function(r, alpha, prec){
                      uniroot(function(x) eval(quo)$ps - prec,
                              c(1, 1e+07), ...)$root
                    }, r = r, alpha = alpha, prec = prec)
                  ))

  if (meth %in% c("wald", "score", "vs")) {
    if (is.null(conf.width)) {
      prec <- eval(quo)
    }
    if (is.null(x)) {
      if(conf.width<min(r)){
        n <- try( x <- eval(get_x), silent = TRUE)
        if(inherits(n,"try-error")) stop("'conf.width' too small")
      } else {
        n <- try( x <- eval(get_x), silent = TRUE)
        if(inherits(n,"try-error")) stop("'conf.width' too wide")
      }
    }
    if (any(r == 0) & meth == "vs")
      warning("The confidence interval is degenerate at z^2/(4t), if r is 0.")

    radj <- eval(get_radj)
    lwr <- radj - prec
    upr <- radj + prec
    conf.width <- 2 * prec
  }

  # exact
  if (meth == "exact") {
    if (is.null(x)) {
      if(conf.width<min(r)){
        n <- try( x <- eval(get_x), silent = TRUE)
        if(inherits(n,"try-error")) stop("'conf.width' too small")
      } else {
        n <- try( x <- eval(get_x), silent = TRUE)
        if(inherits(n,"try-error")) stop("'conf.width' too wide")
      }
    }
    res <- eval(quo)
    radj <- eval(get_radj)
    lwr <- res$lwr
    upr <- res$upr
    if (is.null(conf.width))
      conf.width <- upr - lwr
  }

  if (any(lwr < 0))
    warning("The lower end of the confidence interval is numerically below 0",
            "and non-sensible. Please choose another method.")

  structure(
    list(r = r,
         radj = radj,
         x = x,
         time = x / r,
         conf.width = conf.width,
         conf.level = conf.level,
         lwr = lwr,
         upr = upr,
         note = "'x / r' units of time are needed to accumulate 'x' events.",
         method = paste(est, "for a rate with", meth, "confidence interval")),
    class = "presize")
}




# Proportion ---------------
#' Sample size or precision for a proportion
#'
#' \code{prec_prop} returns the sample size or the precision for the provided
#' proportion.
#'
#' Exactly one of the parameters \code{n} or \code{conf.width} must be passed as NULL,
#' and that parameter is determined from the other.
#'
#' The wilson, agresti-coull, exact, and wald method are implemented. The
#' wilson method is suggested for small \code{n} (< 40), and the agresti-coull method
#' is suggested for larger \code{n} (see reference). The wald method is not suggested,
#' but provided due to its widely distributed use.
#'
#' \code{\link[stats]{uniroot}} is used to solve \code{n} for the agresti-coull,
#' wilson, and exact methods. Agresti-coull can be abbreviated by ac.
#'
#'
#' @param p proportion.
#' @param ... other arguments to uniroot (e.g. \code{tol}).
#' @inheritParams prec_mean
#' @inheritParams prec_rate
#' @return Object of class "presize", a list of arguments (including the
#'   computed one) augmented with method and note elements. In the wilson and
#'   agresti-coull formula, the p from which the confidence interval is
#'   calculated is adjusted by a term (i.e. \eqn{p + term \pm ci}). This
#'   adjusted p is returned in \code{padj}.
#'
#' @seealso \code{\link[stats]{binom.test}}, \code{\link[binom]{binom.confint}}
#'   in package \pkg{binom}, and \code{\link[Hmisc]{binconf}} in package
#'   \pkg{Hmisc}
#'
#' @references Brown LD, Cai TT, DasGupta A (2001) \emph{Interval Estimation for
#'   a Binomial Proportion}, Statistical Science, 16:2, 101-117,
#'   \href{https://doi.org/10.1214/ss/1009213286}{doi:10.1214/ss/1009213286}
#'
#' @examples
#' prec_prop(p = 1:9 / 10, n = 100, method = "wilson")
#' prec_prop(p = 1:9 / 10, conf.width = .192, method = "wilson")
#' @export
prec_prop <- function(p, n = NULL, conf.width = NULL, conf.level = 0.95,
                       method = c("wilson", "agresti-coull", "exact", "wald"),
                       ...) {
  if (sum(sapply(list(n, conf.width), is.null)) != 1)
    stop("exactly one of 'n', and 'conf.width' must be NULL")
  numrange_check(conf.level)
  numrange_check(p)
  if(!is.null(n)) numrange_check_gt(n)
  if(!is.null(conf.width)) numrange_check_gt(conf.width)
  if(!is.null(conf.width) && conf.width > .7) warning("risk of failure in calculations increases with higher 'conf.width', particularly when 'p' is close to 0 or 1")

  if (length(method) > 1) {
    warning("more than one method was chosen, 'wilson' will be used")
    method <- "wilson"
  }

  methods <- c("wald", "ac", "agresti-coull", "exact", "wilson")
  matched_meth <- match.arg(method,
                            methods)
  meth <- switch(matched_meth,
                 wilson = "Wilson",
                 ac = "Agresti-Coull",
                 'agresti-coull' = "Agresti-Coull",
                 exact = "exact",
                 wald = "Wald",
                 "wilson")

  if (is.null(n)) {
    prec <- conf.width / 2
    est <- "sample size"
  } else est <- "precision"

  alpha <- (1 - conf.level) / 2
  z <- qnorm(1 - alpha)
  z2 <- z * z

  padj <- NA

  if (meth == "Wald") {
    if (is.null(conf.width)) {
      prec <- z * sqrt(p * (1 - p) / n)
    }
    if (is.null(n))
      n <- p * (1 - p) / (prec / z) ^ 2
    padj <- p
  } else {

    quo <- switch(meth,
                  'Agresti-Coull' = quote({
                    n_ <- n + z2
                    x_ <- p * n + 0.5 * z2
                    padj <- x_ / n_
                    z * sqrt(padj * (1 - padj) / n_)
                  }),
                  Wilson = quote({(z * sqrt(n) / (n + z2)) *
                      sqrt(p * (1 - p) + z2 / (4 * n))}),
                  exact = quote({
                    x <- p * n
                    lwr <- qbeta(alpha, x, n - x + 1)
                    lwr[x == 0] <- 0
                    upr <- qbeta(1 - alpha, x + 1, n - x)
                    upr[x == 1] <- 1
                    ps <- (upr - lwr) / 2
                    list(lwr = lwr,
                         upr = upr,
                         ps = ps)
                  }))

    uniroot_fun <- switch(meth,
                          'Agresti-Coull' = {
                            function(p, prec, z, z2) {
                              uniroot(function(n) eval(quo) - prec,
                                      c(1, 1e+07), ...)$root
                            }
                          },
                          Wilson = {
                            function(p, prec, z, z2) {
                              uniroot(function(n) eval(quo) - prec,
                                      c(1, 1e+07), ...)$root
                            }
                          },
                          exact = {
                            function(p, prec, alpha){
                              uniroot(function(n) eval(quo)$ps - prec,
                                      c(1, 1e+07), ...)$root
                            }
                          }
    )

    if (meth %in% c("Agresti-Coull", "Wilson")){
      if (is.null(conf.width)) {
        prec <- eval(quo)
      }
      if (is.null(n)) {
        f <- uniroot_fun
        
        if(conf.width<min(p)){
          n <- try(mapply(f, p = p, prec = prec, z = z, z2 = z2), silent = TRUE)
          if(inherits(n,"try-error")) stop("'conf.width' too small")
          est <- "sample size"
        } else {
          n <- try(mapply(f, p = p, prec = prec, z = z, z2 = z2), silent = TRUE)
          if(inherits(n,"try-error")) stop("'conf.width' too wide")
          est <- "sample size"
        }
        
      }
      padj <- switch(meth,
                     'Agresti-Coull' = (p * n + 0.5 * z2) / (n + z2),
                     Wilson = (n * p + z2 / 2) / (n + z2))

    }

    if (meth == "exact") {
      if (is.null(n)) {
        f <- uniroot_fun
        
        if(conf.width<min(p)){
          n <- try(mapply(f, p = p, prec = prec, alpha = alpha), silent = TRUE)
          if(inherits(n,"try-error")) stop("'conf.width' too small")
          est <- "sample size"
        } else {
          n <- try(mapply(f, p = p, prec = prec, alpha = alpha), silent = TRUE)
          if(inherits(n,"try-error")) stop("'conf.width' too wide")
          est <- "sample size"
        }
        
      }
      res <- eval(quo)
      lwr <- res$lwr
      upr <- res$upr
      padj <- NA
      if (is.null(conf.width))
        conf.width <- upr - lwr
    }

  }
  if (meth != "exact") { # lwr and upr ci for all other methods
    lwr <- padj - prec
    upr <- padj + prec
    conf.width <- prec * 2
  }


  if(any(lwr < 0))
    warning("The lower end of at least one confidence interval is below 0 and non-sensible. Please choose 'wilson' or 'exact' method.")
  if(any(upr > 1))
    warning("The upper end of at least one confidence interval is above 1 and non-sensible. Please choose 'wilson' or 'exact' method.")


  structure(list(p = p,
                 padj = padj,
                 n = n,
                 conf.width = conf.width,
                 conf.level = conf.level,
                 lwr = lwr,
                 upr = upr,
                 note = "padj is the adjusted proportion, from which the ci is calculated.",
                 method = paste(est, "for a proportion with",
                                meth, "confidence interval.")),
            class = "presize")
}
