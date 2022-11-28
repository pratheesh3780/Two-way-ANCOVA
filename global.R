library(shiny)
library(shinymanager)
#####################password credentials

credentials <- data.frame(
  user = c("ancovagrapes"), # mandatory
  password = c("123456"), # mandatory
  #start = c("2019-04-15"), # optinal (all others)
  #expire = c(NA, "2019-12-31"),
  #admin = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

#####################################