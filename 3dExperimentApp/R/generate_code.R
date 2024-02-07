## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Tyler Wiederich
##
## Date Created: 2024-02-01
##
## ---------------------------
##
## Notes: creates and stores completion code
##   
##
## ---------------------------

generate_code <- function(seed = NULL) {
  set.seed(NULL)
  code <- paste(sample(10000:99999, 3), collapse = "-")
  write_lines(code, 'Responses/codes.txt', append = TRUE)
  return(code)
}