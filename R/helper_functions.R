
# Check whether numeric is within specified range lo, hi
numrange_check <- function(x, lo = 0, hi = 1) {
  param <- substitute(x)
  if (any(!is.null(x) && !is.numeric(x) || any(lo > x | x > hi)))
    stop(paste0("'", param, "' must be numeric in [", lo, ", ", hi, "]"))
}

# # calculate the z-value of standard normal distribution
# calc_zval <- function(conf.level) qnorm((1 + conf.level) / 2)




# print definition for class
#' @importFrom utils head
#' @export
print.presize <- function(x, n = 10L, ...) {
  cat("\n    ", x$method, "\n\n")
  note <- x$note
  if (length(note) > 1) note <- paste(note, collapse = "\n")
  #x[c("method", "note")] <- NULL
  dd <- data.frame(x)
  print(head(dd, n = n))
  l <- nrow(dd)
  if (n < l)
    cat("\n", "[Output truncated at", n, "of", l, "rows]")
  if (!is.null(note)) if(!is.na(note))
    cat("\n", "NOTE: ", note, "\n\n", sep = "")
  else cat("\n")
  invisible(x)
}


#' @export
as.data.frame.presize <- function(x, ...) {
  meth <- x$method
  x[c("method", "note")] <- NULL
  class(x) <- "list"
  res <- data.frame(x)
  attr(res, "method") <- meth
  return(res)
}


#' @export
as.matrix.presize <- function(x, ...) {
  x <- as.data.frame(x)
  meth <- attr(x, "method")
  res <- as.matrix(x)
  attr(res, "method") <- meth
  return(res)
}
