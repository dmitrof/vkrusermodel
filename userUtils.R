filterDeactivated <- function(users)
{
  return(users[rowSums(is.na(users)) > 0,])
}

getDeactivated <- function(users)
{
  return (na.omit(users))
}