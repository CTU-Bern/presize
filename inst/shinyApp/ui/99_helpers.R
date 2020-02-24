# helper functions


# to help with debugging ---
db <- function(input, type){
  input2 <- reactiveValuesToList(input)
  input2 <- input2[grepl(type, names(input))]
  sapply(input2, cat, "\n", file = stderr())
}
