#-------------------------------RelationsFakeSearch-----------------------------------------------------------------#

#пример сомнительной группы
sample_group_global = 457918

####################Настраиваемые параметры#################
sample_group_global = 457918


#минимальное число триад, при котром пользователь - не бот 
smintr_global = 1000 #тестовое значение, на самом деле эта величина намного меньше

#размер максимальной выборки друзей для проверки каждого пользователя
friends_sample_global = 100

# количество сильных связей у пользователя, который не является фейком
min_rel_global = 10

# количество триад, при которых связь можно считать устойчивой
rel_factor_global = 20
#--------------------------------------------------------------#

getFakeRatio <- function(group_id=sample_group_global, sample_size=10, min_triads=smintr_global,
                         bounded=TRUE)
{
  members <- fetchGroupMembers(group_id=group_id, sample_size)
  fakes <- findFakesByRelations(members$id)
  return (length(fakes) / length(members$id))
}


#Возвращает список с id "фейковых" аккаунтов
findFakesByRelations <- function(members) {
  friendlists <- getFriendsFor(members)
  
  member_ids <- names(friendlists)
  
  candidates <- sapply(member_ids, function(member_id) {
    checkUserByRelations(member_id, friendlists[[member_id]])
  })
  
  n <- names(candidates)
  
  fake_list <- names(which(candidates))
  return(fake_list)
}


checkUserByRelations <- function(member_id, friend_list, bounded=TRUE, min_rel=min_rel_global, rel_factor=rel_factor_global)
{
  #здесь должен прогресс бар тикать
  cat("checking member ", member_id)
  
  #Если число триад в друзьяъ пользователя меньше, чем min_triads 
  if (length(getStrongRelations(friend_list, rel_factor, bounded)) < min_rel)
  {
    return(TRUE)
  }
  else 
  {
    return(FALSE)
  }
}

# Возвращает список друзей пользователя, у которых с ним более rel_factor общих друзей
# пример использования: getStrongRelations(getUserFriendList(user_id), rel_factor=5, bounded=TRUE)
getStrongRelations <- function(friend_list, rel_factor, bounded)
{
  friends_of_friends <- getFriendsOfFriends(friend_list, bounded)
  
  friend_ids <- names(friends_of_friends)
  
  candidates <- sapply(friend_ids, function(friend_id) {
    #println(friend_list)
    #println(friends_of_friends[friend_id])
    if (length(intersect(friend_list, friends_of_friends[[friend_id]])) >= rel_factor) 
    {
      return(TRUE)
    }
    else
    {
      return(FALSE)
    }
  })
  strong_relations <- names(which(candidates))
  return(strong_relations)
}

#---------------------------------------------PrimitiveTriadFakeSearch---------------------------------------------#
####################################################################################################################
getGroupFakePercentage <- function(group_id=sample_group_global, sample_size=10, min_triads=smintr_global,
                                   bounded=TRUE)
{
  members <- fetchGroupMembers(group_id=group_id, sample_size)
  fakes <- findFakesByTriads(members$id, sample_size, min_triads, bounded)
  return (length(fakes) / length(members$id))
}


#Возвращает список с id "фейковых" аккаунтов
findFakesByTriads <- function(members, sample_size=10, min_triads=smintr_global,
                              bounded=TRUE) {
  
  friendlists <- getFriendsFor(members)
  #получили список "фейков"
  member_ids <- names(friendlists)
  
  candidates <- sapply(member_ids, function(member_id) checkUserByTriads(member_id, friendlists[[member_id]], min_triads, bounded))
  
  n <- names(candidates)
  #print(candidates)
  return(names(which(candidates)))
}


checkUserByTriads <- function(member_id, friend_list, min_triads,
                              bounded=TRUE)
{
  #здесь должен прогресс бар тикать
  cat("checking member ", member_id)
  #Если число триад в друзьяъ пользователя меньше, чем min_triads 
  if (getUserTriads(friend_list, bounded) < min_triads)
  {
    return(TRUE)
  }
  else 
  {
    return(FALSE)
  }
}

#Находит число триад для каждого пользователя
getUserTriads <- function(friend_list, bounded, friends_sample_size = friends_sample_global)
{
  # извлекли списки друзей для каждого из друзей
  
  friends_of_friends <- getFriendsOfFriends(friend_list, bounded, friends_sample_size)
  
  #нашли пересечение каждого списка с friendlist
  triadsCount <- Reduce("+", lapply(lapply(friends_of_friends, intersect, y=friend_list), length))
  return(triadsCount)
}

#----------------------------------------------userUtils------------------------------------------------------------#
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

getUserPostDates <- function(user_id)
{
  
}

getStatusForUsers <- function(user_ids) {
  return(getUsersExecute(user_ids, fields='status')$status)
}
