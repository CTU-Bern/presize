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
#' @return
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
                           conf.method =
                           tol = .Machine$double.eps^0.25){

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
    conf.width <- mapply(f, sn = sn, sp = sp, p = p, w = w, z2 = z2)
  }

  structure(list(n = n,
                 sens = sens,
                 spec = spec,
                 conf.width = conf.width,
                 conf.level = conf.level,
                 method = paste(est, "for", "sens and spec")),
            class = "presize")
}




