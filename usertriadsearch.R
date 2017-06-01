usr_id = 63161912

colnames(friends$items)
for (friend in friends$items)
  print("friend")
items = friends$items
usertriads <- function(usr_id=181306445) {
  triads = 0
  friends = getFriends(user_id=usr_id, )
  for (friend_id in friends$items) {
    friends2 <- getFriends(friend_id)
    print(friends$items)
    #print(length(intersect(friends$items, friends2$items)))
    #prints(friends)
  }
}

triads <- function(usr_id)  {
  triads = 0
  friends = getFriends(user_id=usr_id)
  print(friends)
  #print(length(id_list))
}

library("data.table")
friends2 <- as.data.table(friends3)

friends3 <- getFriends(fields = "bdate")

friend4 <-  friends2[which(is.na(items.deactivated))]

friend5 <- sapply(friend4$items.id, triads)

class(friends3)
str(friends3)
