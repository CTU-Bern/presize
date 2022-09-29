# diagnostic measures


# sens/spec
#' Sample size and precision of sensitivity and specificity
#'
#' Because sensitivity (true positives/total number of positives) and specificity (true
#' negatives/total number of negatives) are simple proportions, these
#' functions act as wrappers for \code{prec_prop}.
#'
#' If \code{ntot} and \code{prev} are given, they are used to calculate
#'   \code{n}.
#'
#' @rdname sensspec
#' @param sens,spec proportions.
#' @param ntot total sample size.
#' @param prev prevalence of cases/disease (i.e. proportion of \code{ntot} with
#'   the disease).
#' @param round string, round calculated \code{n} up (\code{ceiling}) or down
#'   (\code{floor}).
#' @param ... options passed to prec_prop (e.g. method,
#'   conf.width, conf.level).
#' @note Calculated \code{n} can take on non-integer numbers, but
#'   \code{prec_prop} requires integers, so the calculated \code{n} is rounded
#'   according to the approach indicated in \code{round}.
#' @inheritParams prec_prop
#' @aliases prec_sens prec_spec
#' @return Object of class "presize", a list of arguments (including the
#'   computed one) augmented with method and note elements.
#' @seealso \code{prec_prop}
#' @examples
#'   # confidence interval width with n
#'   prec_sens(.6, 50)
#'   # confidence interval width with ntot and prevalence (assuming 50% prev)
#'   prec_sens(.6, ntot = 100, prev = .5)
#'   # sample size with confidence interval width
#'   prec_sens(.6, conf.width = 0.262)
#' @export
prec_sens <- function(sens, n = NULL, ntot = NULL, prev = NULL,
                      conf.width = NULL, round = "ceiling", ...){
  if (!is.null(n) & !is.null(ntot)) stop("supply 'n' or 'ntot' and 'prev'")
  if (!is.null(ntot) & is.null(prev)) stop("'prev' required when 'ntot' is specified")
  if (is.null(conf.width)) if(is.null(ntot) & !is.null(prev)) stop("'ntot' required when 'prev' is specified")
  # if (!round %in% c("ceiling", "floor")) stop("choices for 'round' are 'ceiling' or 'floor'")

  if (!is.null(sens)) numrange_check(sens)
  if (!is.null(n)) numrange_check_gt(n)
  if (!is.null(ntot)) numrange_check_gt(ntot)
  if (!is.null(conf.width)) numrange_check_gt(conf.width)

  round <- match.arg(round, c("ceiling", "floor"))
  if (!is.null(prev)) numrange_check(prev)
  rounder <- switch(round,
                    ceiling = ceiling,
                    floor = floor)
  if (!is.null(ntot) & !is.null(prev)){
    message("estimating n from 'ntot' and 'prev'")
    n <- rounder(ntot * prev)
  } else if (is.null(ntot) & is.null(n) & !is.null(prev)) {
    ntot <- NA
    n <- NULL
  } else {
    ntot <- NA
    prev <- NA
  }

  pp <- prec_prop(sens, n, conf.width, ...)

  if (any(!is.null(prev) & is.na(ntot))) {
    ntot <- pp$n * (1 / prev)
  }

  structure(list(sens = pp$p,
                 sensadj = pp$padj,
                 n = pp$n,
                 prev = prev,
                 ntot = ntot,
                 conf.width = pp$conf.width,
                 conf.level = pp$conf.level,
                 lwr = pp$lwr,
                 upr = pp$upr,
                 note = c("sensadj is the adjusted sensitivity, from which the ci is calculated.",
                          "      n is the number of positives, ntot the full sample"),
                 method = gsub("proportion", "sensitivity", pp$method)),
            class = "presize")

}


#' @export
#' @rdname sensspec
prec_spec <- function(spec, n = NULL, ntot = NULL, prev = NULL, conf.width = NULL, round = "ceiling",...){
  if (!is.null(n) & !is.null(ntot)) stop("supply 'n' or 'ntot' and 'prev'")
  # if (!round %in% c("ceiling", "floor")) stop("choices for 'round' are 'ceiling' or 'floor'")
  if (!is.null(ntot) & is.null(prev)) stop("'prev' required when 'ntot' is specified")
  if (is.null(conf.width)) if(is.null(ntot) & !is.null(prev)) stop("'ntot' required when 'prev' is specified")

  if (!is.null(spec)) numrange_check(spec)
  if (!is.null(n)) numrange_check_gt(n)
  if (!is.null(ntot)) numrange_check_gt(ntot)
  if (!is.null(conf.width)) numrange_check_gt(conf.width)

  if (!is.null(prev)) numrange_check(prev)
  round <- match.arg(round, c("ceiling", "floor"))
  rounder <- switch(round,
                    ceiling = ceiling,
                    floor = floor)
  if (!is.null(ntot) & !is.null(prev)){
    message("estimating n from 'ntot' and 'prev'")
    n <- rounder(ntot * (1-prev))
  } else if (is.null(ntot) & is.null(n) & !is.null(prev)) {
    ntot <- NA
    n <- NULL
  } else {
    ntot <- NA
    prev <- NA
  }

  pp <- prec_prop(spec, n, conf.width, ...)

  if (any(!is.null(prev) & is.na(ntot))) {
    ntot <- pp$n * (1 / (1 - prev))
  }

  structure(list(spec = pp$p,
                 specadj = pp$padj,
                 n = pp$n,
                 prev = prev,
                 ntot = ntot,
                 conf.width = pp$conf.width,
                 conf.level = pp$conf.level,
                 lwr = pp$lwr,
                 upr = pp$upr,
                 note = c("specadj is the adjusted specificity, from which the ci is calculated.",
                          "      n is the number of negatives, ntot the full sample."),
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
#' @param auc AUC value.
#' @param prev prevalence.
#' @param n number of observations.
#' @param conf.width precision (the full width of the confidence interval).
#' @param conf.level confidence level.
#' @param ... other arguments to \code{optimize}.
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
  if (any(prev < 0 | prev > 1)) stop("'prev' must be numeric in [0, 1]")
  numrange_check(conf.level)

  if (!is.null(conf.width)) numrange_check_gt(conf.width)
  if (!is.null(n)) numrange_check_gt(n)
  if (!is.null(auc)) numrange_check(auc)

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


# likelihood ratios ----

#' Sample size or precision for likelihood ratios
#'
#' These functions calculate the precision or sample size for likelihood ratios (LRs).
#' \code{prec_lr} is a generalized method for that can be used for positive and
#' negative LRs as well as conditional LRs.
#' @param prev disease/case prevalence in the study group.
#' @param p1 proportion of positives in group 1 (e.g. sensitivity).
#' @param p2 proportion of positives in group 2 (e.g. 1 - specificity).
#' @param n total group size.
#' @param conf.width precision (the full width of the confidence interval).
#' @param conf.level confidence level (defaults to 0.95).
#' @param ... other arguments to uniroot (e.g. \code{tol}).
#'
#' @details
#' These functions implement formula 10 from Simel et al 1991.
#' \code{prec_lr} is a generalized function allowing for many scenarios, while
#' \code{prec_pos_lr} and \code{prec_neg_lr} are specific to positive and
#' negative likelihood ratios in the 2*2 setting (e.g. disease status and test
#' positive/negative).
#'
#' For the positive likelihood ratio (LR+), in a 2x2 style experiment, \code{p1}
#' should be sensitivity, \code{p2} should be 1-specificity. Alternatively, use
#' \code{prec_pos_lr}.
#'
#' For the negative likelihood ratio (LR-), in a 2x2 style experiment, \code{p1}
#' should be 1-sensitivity, \code{p2} should be specificity. Alternatively, use
#' \code{prec_neg_lr}.
#'
#' For conditional likelihood ratios with 3x2 tables, such as positive or
#' negative tests against inconclusive ones (yields), \code{p1} would be the
#' proportion of positive or negative tests in the diseased group and \code{p2}
#' would be the proportion of positive or negative tests in the non-diseased group.
#'
#' @references Simel, DL, Samsa, GP and Matchar, DB (1991) \emph{Likelihood ratios with confidence: Sample size estimation for diagnostic test studies.} J Clin Epidemiol 44(8), 763-770
#' @return
#' Object of class "presize", a list of arguments (including the
#'   computed one) augmented with method and note elements.
#' @export
#'
#' @examples
#' # equal numbers of diseased/non-diseased, 80% sens, 73% spec, 74 participants total
#' prec_lr(.5, .8, .27, 74)
#'
#' # Simel et al 1991, problem 1 - LR+ CI width from N
#' # Sensitivity of a new test is at least 80%, specificity is 73% and the LR+
#' # is 2.96 (= 0.8/(1-0.73)). We have as many diseased as not diseased
#' # (n1 = n2, n = 2*n1 = 146.8, prevalence = .5)
#' prec_lr(prev = .5, p1 = .8, p2 = 1-.73, n = 146.8)
#' prec_pos_lr(prev = .5, sens = .8, spec = .73, n = 146.8)
#'
#' # problem 1 of Simel et al actually derives n1 rather than the width of the
#' # confidence interval (ie N from CI width). If we know that the lower limit
#' # of the CI should be 2.0, the confidence interval width is approximately
#' # exp(2*(log(2.96) - log(2))) = 2.19 (approximate because the CI Of the LR
#' # is only symetrical on the log(LR) scale), which we can put in conf.width
#' prec_lr(prev = .5, p1 = .8, p2 = 1-.73, conf.width = 2.2)
#' # same, but using the wrapper to specify sens and spec
#' prec_pos_lr(prev = .5, sens = .8, spec = .73, conf.width = 2.2)
#'
#' # Simel et al 1991, problem 2 - LR- CI width from N
#' # p1 = 1 - sens = .1, p2 = spec = .5
#' # n1 = n2, n = 160, prev = .5
#' prec_lr(prev = .5, p1 = .1, p2 = .5, n = 160)
#' # same, but using the wrapper to specify sens and spec
#' prec_neg_lr(prev = .5, sens = .9, spec = .5, n = 160)
#'
prec_lr <- function(prev, p1, p2, n = NULL, conf.width = NULL, conf.level = 0.95, ...){
  upr <- lwr <- n1 <- n2 <- lr <- NULL
  if (sum(sapply(list(n, conf.width), is.null)) != 1)
    stop("exactly one of 'n', and 'conf.width' must be NULL")
  numrange_check(prev)
  numrange_check(p1)
  numrange_check(p2)
  numrange_check(conf.level)

  if (!is.null(n)) numrange_check_gt(n)
  if (!is.null(conf.width)) numrange_check_gt(conf.width)

  quo <- quote({
    n1 <- n * prev
    n2 <- n - n1
    lr <- p1/p2
    se <- sqrt(((1 - p1)/(p1 * n1)) + ((1 - p2)/(p2 * n2)))
    lwr <- exp(log(lr) + qnorm((1 - conf.level)/2) * se)
    upr <- exp(log(lr) + qnorm(1 - (1 - conf.level)/2) * se)
    upr - lwr
  })

  if(is.null(n)){
      if(conf.width<min(p1)){
    n_ <- try(mapply(function(p1, p2, conf.width, prev)
      uniroot(function(n) eval(quo) - conf.width,
              c(1, 1e7), ...)$root,
      p1, p2, conf.width, prev), silent = TRUE)
    if(inherits(n_,"try-error")) stop("'conf.width' too small")
    n <- n_
    eval(quo)
    est <- "sample size"
    } else {
      n_ <- try(mapply(function(p1, p2, conf.width, prev)
        uniroot(function(n) eval(quo) - conf.width,
                c(1, 1e7), ...)$root,
        p1, p2, conf.width, prev), silent = TRUE)
      if(inherits(n_,"try-error")) stop("'conf.width' too wide")
      n <- n_
      eval(quo)
      est <- "sample size"
    }
  } else {
    eval(quo)
    conf.width <- upr - lwr
    est <- "precision"
  }

  structure(list(prev = prev,
                 p1 = p1,
                 p2 = p2,
                 n = n,
                 n1 = n1,
                 n2 = n2,
                 lr = lr,
                 lwr = lwr,
                 upr = upr,
                 conf.width = conf.width,
                 conf.level = conf.level,
                 method = paste(est, "for likelihood ratios")),
            class = "presize")

}

#' @describeIn prec_lr "Positive likelihood ratio"
#' @description \code{prec_pos_lr} is a wrapper to \code{prec_lr} to ease
#' calculations for positive likelihood ratios by allowing sensitivity and
#' specificity to be given explicitly.
#' @param sens sensitivity.
#' @param spec specificity.
#' @export
prec_pos_lr <- function(prev, sens, spec,
                        n = NULL, conf.width = NULL, conf.level = 0.95, ...) {
  lr <- prec_lr(prev = prev, p1 = sens, p2 = 1-spec,
                n = n, conf.width = conf.width, ...)
  lr$method <- sub("likelihood ratios", "positive likelihood ratio", lr$method)
  lr
}

#' @describeIn prec_lr "Negative likelihood ratio"
#' @description \code{prec_neg_lr} is a wrapper to \code{prec_lr} to ease
#' calculations for negative likelihood ratios by allowing sensitivity and
#' specificity to be given explicitly.
#' @export
prec_neg_lr <- function(prev, sens, spec,
                        n = NULL, conf.width = NULL, conf.level = 0.95, ...) {
  lr <- prec_lr(prev = prev, p1 = 1-sens, p2 = spec,
                n = n, conf.width = conf.width, ...)
  lr$method <- sub("likelihood ratios", "negative likelihood ratio", lr$method)
  lr
}

