#Функции для обработки профилей пользователей группы


filterDeactivated <- function(users)
{
  return(users[rowSums(is.na(users)) > 0,])
}

getDeactivated <- function(users)
{
  return (na.omit(users))
}

#Возвращает отфильтрованный список юзеров группы
fetchGroupMembers <- function(group_id, sample_size=100)
{
  members <- getGroupsMembersExecute(group_id=group_id, fields='deactivated,friends', count=sample_size)
  filteredMembers <- filterDeactivated(members)
  return(filteredMembers);
}

fetchNames <- function(user_id, friends_lists)
{
  return(friends_lists[[user_id]])
}
#sapply(user_ids, fetchNames, friends_lists=my_ff,simplify=FALSE, USE.NAMES = TRUE)
  