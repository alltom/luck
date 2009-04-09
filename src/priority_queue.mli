
type priority = float

type 'a queue

val empty : 'a queue

val insert : 'a queue -> priority -> 'a -> 'a queue

val extract : 'a queue -> priority * 'a * 'a queue

exception Queue_is_empty
