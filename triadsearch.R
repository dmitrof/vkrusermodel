#Алгоритм определения фейков группы по триадам
#На вход - url группы, показатель минимального количества триад и максимальный размер выборки
source("myframework.R")

myid <- 181306445

anotherid <- 47122493

init();

sample_group = 457918

smintr = 100


#Возвращает список с id "фейковых" аккаунтов
findFakesByTriads <- function(group_id=sample_group, sample_size=100, min_triads=smintr) {
  members <- fetchGroupMembers(group_id=group_id, sample_size)
  friendlists <- getFriendsFor(members$id)
  #получили список "фейков"
  member_ids <- names(friendlists)

  fake_list <- sapply(member_ids, function(member_id) checkUserByTriads(member_id, friendlists[[member_id]]))
  
  return 
}
  

checkUserByTriads <- function(member_id, friend_list, min_triads=smintr)
{
  #здесь должен прогресс бар тикать
  cat("checking member ", member_id)
  #Если число триад в друзьяъ пользователя меньше, чем min_triads 
  if (getUserTriadsBounded(friend_list) < min_triads)
  {
    return(TRUE)
  }
  else 
  {
    return(FALSE)
  }
}

#Находит число триад для каждого пользователя
getUserTriads <- function(friendlist)
{
  # извлекли списки друзей для каждого из друзей
  friends_of_friends <- getFriendsFor(friendlist)
  #нашли пересечение каждого списка с friendlist
  triadsCount <- Reduce("+", lapply(lapply(friends_of_friends, intersect, y=friendlist), length))
  return(triadsCount)
}
#Нахождение аппроксимированного числа триад по выборке (для производительности)
getUserTriadsBounded <- function(friend_list, sample_size=50)
{
  l <- length(friend_list)
  if (l < sample_size)
  {
    return(getUserTriads(friend_list[1:l]))
    
  }
  else
  {
    triads <- getUserTriads(friend_list[1:sample_size])
    return (l / sample_size * triads)
  }
}








  