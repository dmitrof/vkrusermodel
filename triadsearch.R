#Алгоритм определения фейков группы по триадам
#На вход - url группы, показатель минимального количества триад и максимальный размер выборки
source("myframework.R")
init();

sample_group = "club457918"

smintr = 0

getTriads <- function(friendlist)
{
  print(friendlist)
  friends_of_friends <- getFriendsFor(friendlist)
  return(1)
}

#Возвращает список с id "фейковых" аккаунтов
findFakesByTriads <- function(group_id=sample_group, sample_size=100, min_triads=smintr) {
  members <- fetchGroupMembers(group_id=group_id, sample_size)
  friendlists <- getFriendsFor(members$id)
  lapply(friendlists, getTriads)
}
  


#Возвращает отфильтрованный список юзеров группы
fetchGroupMembers <- function(group_id, sample_size=100)
{
  members <- getGroupsMembersExecute(group_id=group_id, fields='deactivated,friends', count=sample_size)
  filteredMembers <- filterDeactivated(members)
  return(filteredMembers);
}



#Извлекаем всех друзей пользователя

  