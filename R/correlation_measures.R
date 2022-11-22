# functions for precision based sample size
#
# Correlation measures
# - intraclass correlation
# - pearsons r


# intraclass correlation ---------------
#' Sample size or precision for an intraclass correlation
#'
#' \code{prec_icc} returns the sample size or the precision for the given
#' intraclass correlation.
#'
#' Exactly one of the parameters \code{n} or \code{conf.width} must be passed as NULL,
#' and that parameter is determined from the others.
#'
#' Sample size or precision is calculated according to formula 3 in Bonett
#' (2002), which is an approximation. Whether ICC is calculated for a one-way or
#' a two-way ANOVA does not matter in the approximation. As suggested by the
#' author, \eqn{5*rho} is added to n, if \eqn{k = 2} and \eqn{rho \ge 7}. This
#' makes the assumption that there is no interaction between rater and subject.
#'
#' n is rounded up to the next whole number using \code{ceiling}.
#'
#' @references Bonett DG (2002). \emph{Sample size requirements for estimating
#'   intraclass correlations with desired precision}. Statistics in Medicine,
#'   21:1331-1335. \doi{10.1002/sim.1108}
#' @param rho desired intraclass correlation.
#' @param k number of observations per n (subject).
#' @param n number of subjects.
#' @examples
#' # Bonett (2002) gives an example using 4 raters, with an ICC of 0.85 and want
#' # a confidence width of 0.2. Bonett calculated that a sample size of 19.2 was
#' # required. This can be done via
#' prec_icc(0.85, 4, conf.width = 0.2)
#' # note that \code{presamp} rounds up to the nearist integer.
#'
#' # Bonett then goes on to estimate the width given the sample size, finding a
#' # value 'close to 0.2':
#' prec_icc(0.85, 4, 20)
#' @inheritParams prec_riskdiff
#' @export
#' @return Object of class "presize", a list of arguments (including the
#'   computed one) augmented with method and note elements.

prec_icc <- function(rho, k, n = NULL, conf.width = NULL, conf.level = 0.95) {

  if (any(!is.null(k)) && (!is.numeric(k) || any(!is.wholenumber(k))))
    stop("'k' must be numeric and a whole number")
  if (sum(sapply(list(n, conf.width), is.null)) != 1)
    stop("exactly one of 'n', and 'conf.width' must be NULL")
  numrange_check(rho)
  numrange_check(conf.level)
  if(!is.null(n)) numrange_check_gt(n)
  if(!is.null(conf.width)) numrange_check_gt(conf.width)
  numrange_check_gt(k, 1)

  alpha <- 1 - conf.level
  z <- qnorm(1 - alpha / 2)
  z2 <- z * z

  if (is.null(n)) {
    est <- "sample size"
  } else
    est <- "precision"

  if (is.null(n))
    n <- 8 * z2 * (1 - rho) ^ 2 * (1 + (k - 1) * rho) ^ 2 /
    (k * (k - 1) * conf.width ^ 2) + 1

  # add cases after calculation of n, if n is unknown, or before calculation of
  # conf.width, if n is known
  n <- n + (k == 2 & rho >= 0.7) * 5 * rho

  if (is.null(conf.width))
    conf.width <- sqrt(8) * z * (1 - rho) * (1 + (k - 1) * rho) /
    sqrt(k * (k - 1) * (n - 1))

  n <- ceiling(n)

  structure(list(rho = rho,
                 k = k,
                 n = n,
                 conf.width = conf.width,
                 conf.level = conf.level,
                 #lwr = rho - prec,
                 #upr = rho + prec,
                 note = ifelse(any(k == 2 & rho >= 0.7), "5*rho is added to n if k == 2 and rho >= 0.7", NA),
                 method = paste(est, "for intraclass correlation")),
            class = "presize")
}



# Correlation coefficients ---------------
#' Sample size or precision for correlation coefficient
#'
#' \code{prec_cor} returns the sample size or the precision for the given
#' pearson, spearman, or kendall correlation coefficient.
#'
#' Exactly one of the parameters \code{n} or \code{conf.width} must be passed as NULL,
#' and that parameter is determined from the other.
#'
#' Sample size or precision is calculated according to formula 2 in Bonett and
#' Wright (2000). The use of pearson is only recommended, if \eqn{n \ge 25}. The
#' pearson correlation coefficient assumes bivariate normality. If the
#' assumption of bivariate normality cannot be met, spearman or kendall should
#' be considered.
#'
#' n is rounded up to the next whole number using \code{ceiling}.
#'
#' \code{\link[stats]{uniroot}} is used to solve n.
#'
#' @references Bonett DG, and Wright TA (2000) \emph{Sample size requirements
#'   for estimating Pearson, Kendall and Spearman correlations} Psychometrika
#'   65:23-28. \doi{10.1007/BF02294183}
#'
#' @param r desired correlation coefficient.
#' @param n sample size.
#' @param method Exactly one of \code{pearson} (\emph{default}), \code{kendall},
#'   or \code{spearman}. Methods can be abbreviated.
#' @inheritParams prec_riskdiff
#' @export
#' @return Object of class "presize", a list of arguments (including the
#'   computed one) augmented with method and note elements.
#' @examples
#' # calculate confidence interval width...
#' # Pearson correlation coefficient
#' prec_cor(r = 0.5, n = 100)
#' # Kendall rank correlation coefficient (tau)
#' prec_cor(r = 0.5, n = 100, method = "kendall")
#' # Spearman's rank correlation coefficient
#' prec_cor(r = 0.5, n = 100, method = "spearman")
#' # calculate N required for a given confidence interval width...
#' # Pearson correlation coefficient
#' prec_cor(r = 0.5, conf.width = .15)
#' # Kendall rank correlation coefficient (tau)
#' prec_cor(r = 0.5, conf.width = .15, method = "kendall")
#' # Spearman's rank correlation coefficient
#' prec_cor(r = 0.5, conf.width = .15, method = "spearman")
prec_cor <-  function(r, n = NULL, conf.width = NULL, conf.level = 0.95,
                      method = c("pearson", "kendall", "spearman"),
                      ...) {

  if (sum(sapply(list(n, conf.width), is.null)) != 1)
    stop("exactly one of 'n', and 'conf.width' must be NULL")
  numrange_check(r, -1, 1)
  numrange_check(conf.level)
  if(!is.null(n)) numrange_check_gt(n)
  if(!is.null(conf.width)) numrange_check_gt(conf.width)


  default_meth <- "pearson"
  if (length(method) > 1) {
    warning("more than one method was chosen, '", default_meth, "' will be used")
    method <- default_meth
  }

  methods <- c("pearson", "kendall", "spearman")
  id <- pmatch(method, methods)
  meth <- methods[id]
  if (is.na(id)) {
    warning("Method '", method, "' is not available, '", default_meth, "' will be used.")
    meth <- default_meth
  }


  alpha <- 1 - conf.level
  z <- qnorm(1 - alpha / 2)

  if (is.null(n)) {
    est <- "sample size"
  } else
    est <- "precision"


  if (meth == "pearson") {
    b <- 3
    c <- 1
  }
  if (meth == "kendall") {
    b <- 4
    c <- sqrt(0.437)
  }
  if (meth == "spearman") {
    b <- 3
    c <- sqrt(1 + r ^ 2 / 2)
  }
  lwr <- upr <- NA

  calc_ci <- quote({
    Zz <- 0.5 * log( (1 + r) / (1 - r))
    A <- c * z / sqrt(n - b)
    ll <- Zz - A
    lu <- Zz + A
    ell <- exp(2 * ll)
    elu <- exp(2 * lu)
    lwr <- (ell - 1) / (ell + 1)
    upr <- (elu - 1) / (elu + 1)
    list(lwr = lwr,
         upr = upr,
         cw = upr - lwr)
  })

  if (is.null(conf.width)) {
    ci <- eval(calc_ci)
    conf.width <- ci$cw
  }
  if (is.null(n)) {
    f <- function(r, z, b, c, conf.width) uniroot(function(n) eval(calc_ci)$cw - conf.width,
                                            c(5, 1e+07), ...,
                                            extendInt = "yes")$root
    if(conf.width<min(r)){
      n <- try(mapply(f, r = r, z = z, b = b, c = c, conf.width = conf.width), silent = TRUE)
      if(inherits(n,"try-error")) stop("'conf.width' too small")
      n <- ceiling(n)
      eval(calc_ci)
    } else {
      n <- try(mapply(f, r = r, z = z, b = b, c = c, conf.width = conf.width), silent = TRUE)
      if(inherits(n,"try-error")) stop("'conf.width' too wide")
      n <- ceiling(n)
      eval(calc_ci)
    }
  }

  structure(list(r = r,
                 n = n,
                 conf.width = conf.width,
                 conf.level = conf.level,
                 lwr = lwr,
                 upr = upr,
                 method = paste(est, "for", meth, "'s correlation coefficient")),
            class = "presize")
}





# Limit of agreement ---------------
#' Sample size or precision for limit of agreement on Bland-Altman plots
#'
#' \code{prec_lim_agree} returns the sample size or the precision for the limit
#' of agreement, i.e. the confidence interval around the limit of agreement,
#' expressed in SD-units. It is an approximation based on the Normal distribution,
#' instead of a Student t distribution.
#'
#' Exactly one of the parameters \code{n} or \code{conf.width} must be passed as NULL,
#' and that parameter is determined from the other.
#'
#' The sample size and precision are calculated according to formulae in Bland &
#' Altman (1986). The CI width is a simple function of the sample size only.
#'
#' @param n sample size.
#' @param conf.width precision (the full width of the confidence interval).
#' @param conf.level confidence level.
#' @return Object of class "presize", a list of arguments (including the
#'   computed one) augmented with method and note elements.
#' @references Bland & Altman (1986) \emph{Statistical methods for assessing agreement
#' between two methods of clinical measurement} Lancet i(8476):307-310
#' \doi{10.1016/S0140-6736(86)90837-8}
#' @export
#' @examples
#' # calculate confidence interval width, given N
#' prec_lim_agree(200)
#' # calculate N given, confidence interval width
#' prec_lim_agree(conf.width = .1)

prec_lim_agree <- function(n = NULL, conf.width = NULL, conf.level = 0.95){

  if (sum(sapply(list(n, conf.width), is.null)) != 1)
    stop("exactly one of 'n', and 'conf.width' must be NULL")
  if(!is.null(n)) numrange_check_gt(n)
  if(!is.null(conf.width)) numrange_check_gt(conf.width)

  alpha <- 1 - conf.level
  z <- qnorm(1 - alpha / 2) * 2

  if (is.null(n)) {
    est <- "sample size"
  } else
    est <- "precision"

  if (is.null(conf.width)) {
    cwidth <- z * sqrt(3 / n)
  }

  if (!is.null(conf.width)) {
    cwidth <- conf.width
    n <- 3 / (conf.width / z) ^ 2
  }

  structure(list(n = n,
                 conf.width = cwidth,
                 conf.level = conf.level,
                 method = paste(est, "for limit of agreement")),
            class = "presize")

}








# Cohens kappa ----
#' Sample size or precision for Cohen's kappa
#'
#' \code{prec_kappa} returns the sample size or the precision for the provided Cohen's kappa coefficient.
#'
#' @param kappa expected value of Cohen's kappa.
#' @param raters number of raters (maximum of 6).
#' @param n_category number of categories of outcomes (maximum of 5).
#' @param props expected proportions of each outcome (should have length
#' \code{n_category}).
#' @param conf.width precision (the full width of the confidence interval).
#' @param n sample size.
#' @param conf.level confidence level.
#' @details This function wraps the \code{FixedN} and \code{CI} functions in the
#' \code{kappaSize} package.
#' The \code{FixedN} functions in \code{kappaSize} return a one sided confidence
#' interval. The values that are passed to \code{kappaSize} ensure that two-sided
#' confidence intervals are returned, although we assume that confidence intervals
#' are symmetrical.
#' @seealso
#' \code{\link[kappaSize]{FixedNBinary}},
#' \code{\link[kappaSize]{FixedN3Cats}},
#' \code{\link[kappaSize]{CIBinary}},
#' \code{\link[kappaSize]{CI3Cats}}
#'
#' @return Object of class "presize", a list of arguments (including the
#'   computed one) augmented with method and note elements.
#' @export
#' @import kappaSize
#'
#' @examples
#' # precision based on sample size
#' #   two categories with proportions of 30 and 70\%, four raters
#' prec_kappa(kappa = .5, n = 200, raters = 4, n_category = 2, props = c(.3,.7))
#' # sample size to get a given precision
#' prec_kappa(kappa = .5, conf.width = .15, raters = 4, n_category = 2,
#'            props = c(.3,.7))
#'
#' # as above, but with two scenarios for kappa
#' prec_kappa(kappa = c(.5, .75), conf.width = .15, raters = 4, n_category = 2,
#'            props = c(.3,.7))
#' prec_kappa(kappa = c(.5, .75), conf.width = c(.15, 0.3), raters = 4,
#'            n_category = 2, props = c(.3,.7))
#'
prec_kappa <- function(kappa,
                       n = NULL,
                       raters = 2,
                       n_category = 2,
                       props,
                       conf.width = NULL,
                       conf.level = 0.95){

  if (n_category < 2) stop("there must be at least 2 outcome categories")
  if (n_category > 5) stop("more than 5 outcome categories is not supported")
  if (any(!is.wholenumber(n_category)))
    stop("'n_category' must be numeric and a whole number")
  if (sum(sapply(list(n, conf.width), is.null)) != 1)
    stop("exactly one of 'n', and 'conf.width' must be NULL")
  numrange_check(conf.level)
  if(!is.null(n)) numrange_check_gt(n)
  if(!is.null(conf.width)) numrange_check_gt(conf.width, .01)
  if (any(!is.wholenumber(raters)))
    stop("'raters' must be numeric and a whole number")
  if(!is.null(raters)) numrange_check(raters,2,6)
  if(!is.null(props)) numrange_check(props)

  alpha <- 1-conf.level

  if (!all(is.null(n))){
    est <- "precision"
    vals <- data.frame(kappa = kappa,
                       n = n)
    fun <- switch(n_category - 1,
                  kappaSize::FixedNBinary,
                  kappaSize::FixedN3Cats,
                  kappaSize::FixedN4Cats,
                  kappaSize::FixedN5Cats)

    res <- mapply(function(kap, N){
             f <- fun(kappa0 = kap,
               n = N,
               props = props,
               raters = raters,
               alpha = alpha)
               lwr <- f$kappaL
               upr <- kap + (kap - f$kappaL)
               data.frame(
                 kappa = kap,
                 n = N,
                 raters = raters,
                 conf.level = conf.level,
                 lwr = lwr,
                 upr = upr,
                 conf.width = upr - lwr)
      }, kap = vals$kappa, N = vals$n, SIMPLIFY = FALSE)

  }
  if (!is.null(conf.width)){

    est <- "sample size"

    vals <- data.frame(kappa = kappa,
                       conf.width = conf.width)

    vals <- within(vals, {
      kappa_L <- kappa - conf.width/2
      kappa_U <- kappa + conf.width/2
    })

    fun <- switch(n_category - 1,
                  kappaSize::CIBinary,
                  kappaSize::CI3Cats,
                  kappaSize::CI4Cats,
                  kappaSize::CI5Cats)

    res <- try(mapply(function(kappa, kappa_L, kappa_U, conf.width){
                  f <- fun(kappa0 = kappa,
                           kappaL = kappa_L,
                           kappaU = kappa_U,
                           props = props,
                           raters = raters,
                           alpha = alpha*2)
                  data.frame(
                    kappa = kappa,
                    lwr = kappa_L,
                    upr = kappa_U,
                    conf.width = conf.width,
                    conf.level = conf.level,
                    n = f$n
                  )
    }, vals$kappa, vals$kappa_L, vals$kappa_U, vals$conf.width, SIMPLIFY = FALSE), silent = TRUE)
    if(inherits(res,"try-error")) stop("'conf.width' too wide")
  }

  res <- do.call("rbind", res)


  structure(list(kappa = kappa,
                 n = res$n,
                 lwr = res$lwr,
                 upr = res$upr,
                 conf.width = res$conf.width,
                 conf.level = res$conf.level,
                 method = paste(est, "for Cohen's kappa")),
            class = "presize")

}




# Cronbach’s alpha ---------------
#' Sample size or precision for Cronbach’s alpha
#'
#' \code{prec_cronb} returns the sample size or the precision for the given
#' Cronbach’s alpha.
#'
#' Exactly one of the parameters \code{n} or \code{conf.width} must be passed as NULL,
#' and that parameter is determined from the other.
#'
#' Sample size or precision is calculated according to the formula & code and
#' provided in Bonett and Wright (2014).
#'
#' n is rounded up to the next whole number using \code{ceiling}.
#'
#' @references Bonett, D. G. and Wright, T. A. (2015) \emph{Cronbach's alpha reliability: Interval estimation, hypothesis testing, and sample size planning} J. Organiz. Behav., 36, pages 3– 15. \doi{10.1002/job.1960}.
#'  # k= number of items
# n = Sample size used
# conf.level
#' @param calpha desired Cronbach’s alpha.
#' @param n sample size.
#' @param conf.width precision (the full width of the confidence interval).
#' @param conf.level confidence level.
#' @param k number of measurements/items.
#'
#' @inheritParams prec_riskdiff
#' @export
#' @return Object of class "presize", a list of arguments (including the
#'   computed one) augmented with method and note elements.
#' @examples
#' # calculate confidence interval width...
#' prec_cronb (k=5,calpha=0.7,n= 349,conf.level= 0.95, conf.width= NULL)
#' # calculate N required for a given confidence interval width...
#' prec_cronb (k=5,calpha=0.7,n= NULL,conf.level= 0.95, conf.width= 0.1)

prec_cronb <- function(k,calpha,n= NULL,conf.level= 0.95, conf.width= NULL)  {

  #. Few checks
  #Check if either n or conf.width are not null
  if (sum(sapply(list(n, conf.width), is.null)) != 1)
    stop("exactly one of 'n', and 'conf.width' must be NULL")

  #Check the range of the parameters
  numrange_check_lt(calpha,1)
  numrange_check(conf.level)
  if(!is.null(n)) numrange_check_gt(n)
  if(!is.null(conf.width)) numrange_check_gt(conf.width)
  numrange_check_gt(k)

  #.Estimation
  #..Alpha & z
  alpha <- 1 - conf.level
  z <- qnorm(1 - alpha / 2)

  #..Calculate the expected CI
  if (is.null(conf.width)) {
    est <- "precision"
    #lwr = 1 - exp(log(1 - calpha) + z * sqrt(2 * k / ((k - 1) * (n - 2))))
    #upr = 1 - exp(log(1 - calpha) - z * sqrt(2 * k / ((k - 1) * (n - 2))))

    #with ln[n/(n-1)]  a bias adjustment proposed by Bonett (2010)
    b<- log(n/(n - 1))
    lwr <- 1 - exp(log(1 - calpha) - b + z*sqrt(2*k/((k - 1)*(n - 2))))
    upr <- 1 - exp(log(1 - calpha) - b - z*sqrt(2*k/((k - 1)*(n - 2))))
    conf.width <-  upr - lwr}

  #..Calculate n
  if (is.null(n)) {
    est <- "sample size"
    n0 <- ceiling((8*k/(k - 1))*(1 - calpha)^2*(z/conf.width)^2 + 2)
    b <- log(n0/(n0 - 1))
    lwr <- 1 - exp(log(1 - calpha) - b + z*sqrt(2*k/((k - 1)*(n0 - 2))))
    upr <- 1 - exp(log(1 - calpha) - b - z*sqrt(2*k/((k - 1)*(n0 - 2))))
    w0 <- upr - lwr
    n <- ceiling((n0 - 2)*(w0/conf.width)^2 + 2)}

  #.Report the results
  structure(list(calpha = calpha,
                 n = n,
                 conf.width = conf.width,
                 conf.level = conf.level,
                 lwr = lwr,
                 upr = upr,
                 method = paste(est, "for Cronbach’s alpha")),
            class = "presize")


}
