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
  

getUserFriendList <- function(user_id)
{
  friends <- getFriends(user_id, fields='deactivated')
  friends <- filterDeactivated(friends$items)
  return(friends$id)
}


getFriendsOfFriends <- function(friend_list, bounded, friends_sample_size=friends_sample_global)
{
  if (bounded)
  {
    l <- length(friend_list)
    if (l < friends_sample_size)
    {
      friends_of_friends <- getFriendsFor(sample(x=friend_list, size=l, replace=FALSE))
    }
    else 
    {
      friends_of_friends <- getFriendsFor(sample(x=friend_list, size=friends_sample_size, replace=FALSE))  
    }
    
  }
  else
  {
    friends_of_friends <- getFriendsFor(friend_list)
  }
  
  return(friends_of_friends)
}