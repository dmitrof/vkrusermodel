#Алгоритм определения фейков группы по триадам
#На вход - url группы, показатель минимального количества триад и максимальный размер выборки
source("myframework.R")

myid <- 181306445

anotherid <- 47122493

init();

sample_group_global = 457918


####################Настраиваемые параметры#################
#минимальное число триад, при котром пользователь - не бот 
smintr_global = 1000 #тестовое значение, на самом деле эта величина намного меньше

#размер максимальной выборки друзей для проверки каждого пользователя
friends_sample_global = 100

#--------------------------------------------------------------#




#Возвращает список с id "фейковых" аккаунтов
findFakesByTriads <- function(group_id=sample_group_global, sample_size=10, min_triads=smintr_global,
                              bounded=TRUE) {
  members <- fetchGroupMembers(group_id=group_id, sample_size)
  friendlists <- getFriendsFor(members$id)
  #получили список "фейков"
  member_ids <- names(friendlists)
  
  candidates <- sapply(member_ids, function(member_id) checkUserByTriads(member_id, friendlists[[member_id]], min_triads, bounded))
  
  n <- names(candidates)
  print(candidates)
  #fake_list <- list.filter()
  
  #fake_list <- names(which(candidates))
  #filtered_fake_list
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








  