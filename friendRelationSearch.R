source("myframework.R")
source("triadsearch.R")

myid <- 181306445

anotherid <- 47122493

init();
#пример сомнительной группы
sample_group_global = 457918

####################Настраиваемые параметры#################

# количество сильных связей у пользователя, который не является фейком
min_rel_global = 10

# количество триад, при которых связь можно считать устойчивой
rel_factor_global = 10
#--------------------------------------------------------------#

#Возвращает список с id "фейковых" аккаунтов
findFakesByRelations <- function(group_id=sample_group_global, sample_size=10) {
  members <- fetchGroupMembers(group_id=group_id, sample_size)
  friendlists <- getFriendsFor(members$id)

  member_ids <- names(friendlists)
  
  candidates <- sapply(member_ids, function(member_id) {
    checkUserByRelations(member_id, friendlists[[member_id]])
    })
  
  n <- names(candidates)
  
  #fake_list <- list.filter()
  
  fake_list <- names(which(candidates))
  #filtered_fake_list
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


