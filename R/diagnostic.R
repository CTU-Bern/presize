# diagnostic measures


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
#' @param prev prevalence of cases/disease (i.e. proportion of \code{ntot} with
#'   the disease)
#' @param round string, round calculated \code{n} up (\code{ceiling}) or down
#'   (\code{floor})
#' @param ... options passed to prec_prop (e.g. method,
#'   conf.width, conf.level)
#' @note Calculated \code{n} can take on non-integer numbers, but
#'   \code{prec_prop} requires integers, so the calculated \code{n} is rounded
#'   according to the approach indicated in \code{round}.
#' @inheritParams prec_prop
#' @aliases prec_sens prec_spec
#' @return Object of class "presize", a list of arguments (including the
#'   computed one) augmented with method and note elements.
#' @seealso \code{prec_prop}, \code{prec_sens_spec}
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
  if (!round %in% c("ceiling", "floor")) stop("choices for 'round' are 'ceiling' or 'floor'")
  numrange_check(prev)
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

  if (!is.null(prev) & is.na(ntot)) {
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
                 note = "sensadj is the adjusted sensitivity, from which the ci is calculated.",
                 method = gsub("proportion", "sensitivity", pp$method)),
            class = "presize")

}


#' @export
#' @rdname sensspec
prec_spec <- function(spec, n = NULL, ntot = NULL, prev = NULL, conf.width = NULL, round = "ceiling",...){
  if (!is.null(n) & !is.null(ntot)) stop("supply 'n' or 'ntot' and 'prev'")
  if (!round %in% c("ceiling", "floor")) stop("choices for 'round' are 'ceiling' or 'floor'")
  numrange_check(prev)
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

  if (!is.null(prev) & is.na(ntot)) {
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
                 note = "specadj is the adjusted specificity, from which the ci is calculated.",
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

