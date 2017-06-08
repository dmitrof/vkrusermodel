statusfetch <- function(user_ids) {
  return(getUsersExecute(user_ids, fields='status')$status)
}