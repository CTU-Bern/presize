# diagnostic measures


#' Sample size or precision for sensitivity and specificity with known
#' prevalence
#'
#' Exactly one of the parameters \code{n, conf.width} must be passed as NULL,
#' and that parameter is determined from the other.
#'
#' When calculating sample sizes, the returned sample size is for both
#' \code{sens} and \code{spec} to have confidence intervals at most
#' \code{conf.width} wide, given the other parameters.
#'
#' When calulating sample precisions, the returned value is the widest
#' confidence interval that both \code{sens} and \code{spec} could take, given
#' the other parameters.
#'
#' This function is suitable for cases where the disease state is unknown at the
#' time of sampling (e.g. a prospective study with consecutive patients).
#'
#' @param sens sensitivity
#' @param spec specificity
#' @param prev prevalence in population
#' @param n number of observations
#' @param conf.width precision (the full width of the conficende interval)
#' @param conf.level confidence level
#' @param tol numerical tolerance used in root finding, the default providing
#'   (at least) four significant digits
#'
#' @return Object of class "presize", a list of arguments (including the
#'   computed one) augmented with method and note elements.
#' @export
#'
#' @references Buderer, N.M.F. (1996) \emph{Statistical Methodology: I.
#' Incorporating the Prevalence of Disease into the Sample Size Calculation for
#' Sensitivity and Specificity}, Academic Emergency Medicine, 3:9, 895-900, \href{https://doi.org/10.1111/j.1553-2712.1996.tb03538.x}{DOI: 10.1111/j.1553-2712.1996.tb03538.x}
#'
#' @examples
#' # calculate number of observations with sensitivity of 90%, specificity of
#' # 85%, prevalence of 20%, confidence interval around \code{sens} or
#' # \code{spec} of 10%
#' prec_sens_spec(.9, .85, prev = .2, conf.width = .1)
#' # calculate confidence interval width at sensitivity of 90%, specificity of
#' # 85%, prevalence of 20%, 173 observations
#' prec_sens_spec(.9, .85, prev = .2, n = 173)
#' # multiple values
#' sens <- c(.9, .85)
#' spec <- c(.85, .75)
#' prec_sens_spec(sens, spec, prev = .2, conf.width = .1)
#' prec_sens_spec(sens, spec, prev = .2, n = 245)

prec_sens_spec <- function(sens,
                           spec,
                           prev,
                           n = NULL,
                           conf.width = NULL,
                           conf.level = 0.95,
                           tol = .Machine$double.eps^0.25){

  if (sum(sapply(list(n, conf.width), is.null)) != 1)
    stop("exactly one of 'n', and 'conf.width' must be NULL")
  for (i in c("sens", "spec", "prev", "conf.width")){
    x <- get(i)
    if (!is.null(x)) {
      if (any(sapply(x, function(x) x < 0 | x > 1))) stop("'", i, "' must be numeric in [0, 1]")
    }
  }

  # Buderer 1996 Statistical Methodology 1 Incorporating Prevalence of disease into the sample size calculation for sensitivity and specificity. Acad. Emerg. Med 3:895-900

  # step 1 - specs
  p <- prev
  sn <- sens
  sp <- spec

  if (is.null(n)) {
    prec <- conf.width / 2
    est <- "sample size"
    w <- conf.width
  } else est <- "precision"

  alpha <- (1 - conf.level) / 2
  z <- qnorm(1 - alpha)
  z2 <- z * z

  fn <- quote({
    w2 <- w * w
    # step 2 - calc n diseased
    tp_fn <- z2 * (sn * (1 - sn)) / w2
    # step 3 - calc n required for sens
    n1 <- tp_fn / p
    # step 4 - calc n without disease
    fp_tn <- z2 * (sp * (1 - sp)) / w2
    # step 5 - calc n required for spec
    n2 <- fp_tn / (1 - p)
    # step 6 - select final N
    n_ <- max(n1, n2)
    w <- sqrt(w2)
    list(n_ = n_,
         w = w)
  })

  if (is.null(n)){
    n <- eval(fn)$n
  } else {
    f <- function(w, sn, sp, z2, p) uniroot(function(w) eval(fn)$n_ - n,
                                        c(0, 1),
                                        tol = tol)$root
    conf.width <- mapply(f, sn = sn, sp = sp, p = p, z2 = z2)
  }

  structure(list(n = n,
                 sens = sens,
                 spec = spec,
                 conf.width = conf.width,
                 conf.level = conf.level,
                 method = paste(est, "for", "sens and spec")),
            class = "presize")
}


# sens/spec
#' Sample size and precision of sensitivity and specificity
#'
#' Because sensitivity and specificity are simple proportions, these functions
#' act as wrappers for \code{prec_prop}.
#'
#' If \code{ntot} and \code{prev} are given, they are used to calculate
#'   \code{n}.
#'
#' @rdname sensspec
#' @param sens,spec proportions
#' @param ntot total sample size
#' @param prev prevalence of cases/disease (i.e. proportion of \code{ntot} with the disease)
#' @param ... options passed to prec_prop (e.g. method,
#'   conf.width, conf.level)
#' @inheritParams prec_prop
#' @aliases prec_sens prec_spec
#' @return Object of class "presize", a list of arguments (including the
#'   computed one) augmented with method and note elements.
#' @seealso \code{prec_prop}, \code{prec_sens_spec}
#' @export
prec_sens <- function(sens, n = NULL, ntot = NULL, prev = NULL, conf.width = NULL, round = "ceiling", ...){
  if (is.null(ntot) & !is.null(prev)) stop("specify ntot and prev together to calculate n")
  if (!round %in% c("ceiling", "floor")) stop("choices for 'round' are 'ceiling' or 'floor'")
  numrange_check(prev)
  rounder <- switch(round,
                    ceiling = ceiling,
                    floor = floor)
  if (!is.null(ntot) & !is.null(prev)){
    n <- rounder(ntot * prev)
  } else {
    ntot <- NA
    prev <- NA
  }

  pp <- prec_prop(sens, n, conf.width, ...)

  structure(list(sens = pp$p,
                 sensadj = pp$padj,
                 n = pp$n,
                 prev = prev,
                 ntot = ntot,
                 conf.width = pp$conf.width,
                 conf.level = pp$conf.level,
                 lwr = pp$lwr,
                 upr = pp$upr,
                 note = "padj is the adjusted proportion, from which the ci is calculated.",
                 method = gsub("proportion", "sensitivity", pp$method)),
            class = "presize")

}
#' @export
#' @rdname sensspec
prec_spec <- function(spec, n = NULL, ntot = NULL, prev = NULL, conf.width = NULL, round = "ceiling",...){
  if (is.null(ntot) & !is.null(prev)) stop("specify ntot and prev together to calculate n")
  if (!round %in% c("ceiling", "floor")) stop("choices for 'round' are 'ceiling' or 'floor'")
  rounder <- switch(roundup,
                    ceiling = ceiling,
                    floor = floor)
  if (!is.null(ntot) & !is.null(prev)) n <- rounder(ntot * (1-prev))

  pp <- prec_prop(spec, n, conf.width, ...)

  structure(list(spec = pp$p,
                 specadj = pp$padj,
                 n = pp$n,
                 conf.width = pp$conf.width,
                 conf.level = pp$conf.level,
                 lwr = pp$lwr,
                 upr = pp$upr,
                 note = "padj is the adjusted proportion, from which the ci is calculated.",
                 method = gsub("proportion", "specificity", pp$method)),
            class = "presize")

}


# AUC ----
#' Sample size or precision for AUC
#'
#' Calculate the sample size from AUC, prevalence and confidence interval width
#' or the expected confidence interval width from AUC, prevalence and sample
#' size, following Hanley and McNeil (1982).
#'
#'
#' Sample size is derived by optimizing the difference between the difference
#' between the lower and upper limits of the confidence interval and
#' \code{conf.width}.
#'
#' @param auc AUC value
#' @param prev prevalence
#' @param n number of observations
#' @param conf.width precision (the full width of the confidence interval)
#' @param conf.level confidence level
#' @param ... other arguments to \code{optimize}
#' @return Object of class "presize", a list of arguments (including the
#'   computed one) augmented with method and note elements.
#' @references Hanley, JA and McNeil, BJ (1982) \emph{The Meaning and Use of the Area under a Receiver Operating Characteristic (ROC) Curve.} Radiology 148, 29-36
#' @examples
#' # confidence interval width
#' N <- 500
#' prev <- .1
#' auc <- .65
#' (prec <- prec_auc(auc, prev, n = N))
#' cwidth <- prec$conf.width
#' # sample size
#' prec_auc(auc, prev, conf.width = cwidth)
#' @export
prec_auc <- function(auc, prev, n = NULL, conf.width = NULL, conf.level = .95,
                     ...){

  if (sum(sapply(list(n, conf.width), is.null)) != 1)
    stop("exactly one of 'n', and 'conf.width' must be NULL")
  if (prev < 0 | prev > 1) stop("'prev' must be numeric in [0, 1]")

  fn <- function(n, prev, auc){
    n1 <- n*prev
    n2 <- n*(1-prev)
    q0 <- auc*(1-auc)
    q1 <- auc/(2-auc)-auc^2
    q2 <- 2*auc^2/(1+auc)-auc^2
    se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2))

    alpha <- (1 - conf.level) / 2
    z <- qnorm(1 - alpha)

    upr <- auc + z*se
    lwr <- auc - z*se

    conf.width <- upr - lwr
    list(upr = upr,
         lwr = lwr,
         width = conf.width)
  }
  opti_fn <- function(n, prev, auc, conf.width){
    n1 <- n*prev
    n2 <- n*(1-prev)
    q0 <- auc*(1-auc)
    q1 <- auc/(2-auc)-auc^2
    q2 <- 2*auc^2/(1+auc)-auc^2
    se <- sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2))

    alpha <- (1 - conf.level) / 2
    z <- qnorm(1 - alpha)

    upr <- auc + z*se
    lwr <- auc - z*se

    abs((upr - lwr) - conf.width)
  }

  if (is.null(n)) {
    est <- "sample size"
    op <- stats::optimize(opti_fn,
                   interval = c(0, 1e6),
                   auc = auc,
                   prev = prev,
                   conf.width = conf.width,
                   ...)
    n <- op$minimum
    n1 <- n*prev
    n2 <- n*(1-prev)
    res <- fn(n, prev, auc)
    lwr <- res$lwr
    upr <- res$upr

  } else {
    res <- fn(n, prev, auc)
    conf.width <- res$width
    lwr <- res$lwr
    upr <- res$upr
    n1 <- n*prev
    n2 <- n*(1-prev)

    est <- "precision"

  }

  structure(list(auc = auc,
                 n = n,
                 prev = prev,
                 n1 = n1,
                 n2 = n2,
                 lwr = lwr,
                 upr = upr,
                 conf.width = conf.width,
                 conf.level = conf.level,
                 method = paste(est, "for", "AUC")),
            class = "presize")

}

