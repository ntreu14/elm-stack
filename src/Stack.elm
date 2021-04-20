module Stack exposing 
  ( Stack
  , empty, isEmpty, size
  , toList, fromList
  , pop, push, peek, member
  , append, concat
  )

type Stack a
  = Stack Int (List a)

empty : Stack a
empty = Stack 0 []

isEmpty : Stack a -> Bool
isEmpty (Stack _ xs) =
  List.isEmpty xs

size : Stack a -> Int
size (Stack count _) = count

toList : Stack a -> List a
toList (Stack _ xs) = xs

fromList : List a -> Stack a
fromList xs = 
  Stack (List.length xs) xs

pop : Stack a -> (Maybe a, Stack a)
pop (Stack count xs) =
  case xs of
    [] -> 
      (Nothing, empty)

    x :: tail ->
      (Just x, Stack (count - 1) tail)

push : a -> Stack a -> Stack a
push v (Stack count xs) =
  Stack (count + 1) (v :: xs)

peek : Stack a -> Maybe a
peek (Stack _ xs) =
  List.head xs

member : a -> Stack a -> Bool
member sought (Stack _ xs) =
  List.member sought xs

append : Stack a -> Stack a -> Stack a
append (Stack xsCount xs) (Stack ysCount ys) =
  List.append xs ys |> Stack (xsCount + ysCount)

concat : List (Stack a) -> Stack a
concat = List.foldr append empty 
