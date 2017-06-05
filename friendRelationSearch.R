source("myframework.R")
source("triadsearch.R")

myid <- 181306445

anotherid <- 47122493

init();

sample_group_global = 457918

# количество сильных связей у пользователя, который не является фейком
min_rel_global = 5

# количество триад, при которых связь можно считать устойчивой
rel_factor_global = 1


#Возвращает список с id "фейковых" аккаунтов
findFakesByRelations <- function(group_id=sample_group_global, sample_size=10, min_triads=smintr_global) {
  members <- fetchGroupMembers(group_id=group_id, sample_size)
  friendlists <- getFriendsFor(members$id)
  #получили список "фейков"
  member_ids <- names(friendlists)
  
  candidates <- sapply(member_ids, function(member_id) checkUserByTriads(member_id, friendlists[[member_id]], min_triads))
  
  n <- names(candidates)
  
  #fake_list <- list.filter()
  
  fake_list <- triad_list
  #filtered_fake_list
  return(fake_list)
}


checkUserByRelations <- function(member_id, friend_list, min_triads)
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