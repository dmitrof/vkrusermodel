#Алгоритм определения фейков группы по триадам
#На вход - url группы, показатель минимального количества триад и максимальный размер выборки
source("myframework.R")

myid <- 181306445

init();

sample_group = 457918

smintr = 0



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

#Возвращает список с id "фейковых" аккаунтов
findFakesByTriads <- function(group_id=sample_group, sample_size=100, min_triads=smintr) {
  #извлекли всех членов группы
  members <- fetchGroupMembers(group_id=group_id, sample_size)
  #извлекли списки друзей для каждого из членов группы  
  friendlists <- getFriendsFor(members$id)
  #для каждого из списков посчитали кол-во триад
  lapply(friendlists, getTriads)
}
  

getTriads <- function(friendlist)
{
  #получили список друзей с именем 
  print(friendlist)
  # извлекли списки друзей для каждого из друзей
  friends_of_friends <- getFriendsFor(friendlist)
  #нашли пересечение каждого списка с friendlist
  lapply(friends_of_friends, intersect, )
}



#Извлекаем всех друзей пользователя

  