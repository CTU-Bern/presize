
# Check whether numeric is within specified range lo, hi
numrange_check <- function(x, lo = 0, hi = 1) {
  param <- substitute(x)
  if (!is.null(x) && !is.numeric(x) || any(lo > x | x > hi))
    stop(paste0("'", param, "' must be numeric in [", lo, ", ", hi, "]"))
}

# # calculate the z-value of standard normal distribution
# calc_zval <- function(conf.level) qnorm((1 + conf.level) / 2)


# print definition for class
print.presize <- function(x, ...) {
  cat("\n    ", x$method, "\n\n")
  note <- x$note
  x[c("method", "note")] <- NULL
  print(data.frame(x), max = 50)
  if (!is.null(note))
    cat("\n", "NOTE: ", note, "\n\n", sep = "")
  else cat("\n")
  invisible(x)
}

as.data.frame.presize <- function(x, ...) {
  meth <- x$method
  x[c("method", "note")] <- NULL
  class(x) <- "list"
  res <- data.frame(x)
  attr(res, "method") <- meth
  return(res)
}

as.matrix.presize <- function(x, ...) {
  x <- as.data.frame(x)
  meth <- attr(x, "method")
  res <- as.matrix(x)
  attr(res, "method") <- meth
  return(res)
}
