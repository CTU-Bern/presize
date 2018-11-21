
# Check whether numeric is within specified range lo, hi
numrange_check <- function(x, lo = 0, hi = 1) {
  param <- substitute(x)
  if (!is.null(x) && !is.numeric(x) || any(lo > x | x > hi))
    stop(paste0("'", param, "' must be numeric in [", lo, ", ", hi, "]"))
}

# # calculate the z-value of standard normal distribution
# calc_zval <- function(conf.level) qnorm((1 + conf.level) / 2)

