
init <- function() {
  
  library("devtools")
  library("plyr")
  library("plyr")
  library("mime")
  library("curl")
  library("R6")
  library("openssl")
  library("httr")
  library("jsonlite")
  library("XML")
  library("vkR")
  library("data.table")
  vkOAuth(6031255, 'friends,groups', 'l', 'p')
}
