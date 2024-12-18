extract_last_name <- function(name) {
  sapply(strsplit(name, " "), tail, 1)
}