extract_last_name_initials <- function(name) {
  parts <- strsplit(name, " ")[[1]]                       
  initials <- paste(substr(parts[1:(length(parts) - 1)], 1, 1), collapse = ".")  
  last_name <- tail(parts, 1)                            
  return(paste(last_name, paste0(initials, "."), sep = " "))       
}