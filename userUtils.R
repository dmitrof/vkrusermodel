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

fetchFromNamedList <- function(friend_item)
{
  print(friend_item)
  print("and legth")
  print(length(friend_item))
  #return(friend_item)
}

fromff <- function(ff)
{
  for (list in ff)
  {
    print(list)
  }
}