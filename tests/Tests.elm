module Tests exposing (tests)

import Expect
import Fuzz exposing (..)
import Test exposing (..)
import Stack exposing (..)

nonEmptyListFuzzer : Fuzzer a -> Fuzzer (List a)
nonEmptyListFuzzer fuzzer =
  list fuzzer |> Fuzz.map2 (::) fuzzer

flip : (a -> b -> c) -> b -> a -> c
flip f a b = f b a

uncurry : (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

both : (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

tests : Test
tests = 
  describe "Stack tests" 
    [ describe "empty" 
        [ test "has a size of 0" <| 
            \() ->
                Stack.size Stack.empty |> Expect.equal 0
        
        , test "encapsulates an empty list" <|
            \() ->
                Expect.true
                  "the inner list is empty"
                  (Stack.toList Stack.empty |> List.isEmpty)
        ]

    , describe "isEmpty" 
        [ test "returns true for an empty stack" <|
            \() ->
                Expect.true
                  "empty stack is empty"
                  (Stack.isEmpty Stack.empty)
        
        , fuzz (nonEmptyListFuzzer int) "returns false for a non empty stack" <|
            \xs ->
                Expect.false
                  "non empty stack is not empty"
                  (Stack.fromList xs |> Stack.isEmpty)

        , fuzz (nonEmptyListFuzzer int) "has a size greater than zero" <|
            \xs ->
                Expect.greaterThan
                  0
                  (Stack.fromList xs |> Stack.size)
        ]
    
    , describe "size" 
        [ test "getting a size from a stack constructed using push" <|
            \() ->
                Stack.empty
                  |> Stack.push 1
                  |> Stack.push 2
                  |> Stack.push 3
                  |> Stack.size
                  |> Expect.equal 3
        ]

    , describe "toList"
        [ fuzz (list int) "the size should be the size of the original list" <|
            \xs ->
                Expect.equal
                  (List.length xs)
                  (Stack.fromList xs |> Stack.size)

        , fuzz (list int) "the original list should equal the list from the stack" <|
            \xs ->
                Expect.equalLists
                  xs
                  (Stack.fromList xs |> Stack.toList)
        ]

    , describe "fromList"
        [ fuzz (list int) "fromList is equal to popping the same elements on the list" <|
            \xs ->
                Expect.equal
                  (List.foldr Stack.push Stack.empty xs)
                  (Stack.fromList xs)
        ]

    , describe "pop" 
        [ fuzz (nonEmptyListFuzzer int) "popping off the stack is Just the head of the list as the first element of the tuple" <|
            \xs ->
                Expect.equal
                  (List.head xs)
                  (Stack.fromList xs |> Stack.pop |> Tuple.first)
      
      , fuzz (nonEmptyListFuzzer int) "popping off the stack is Just the tail of the list as the second element of the tuple" <|
            \xs ->
                Expect.equal
                  (List.tail xs |> Maybe.withDefault [] |> Stack.fromList)
                  (Stack.fromList xs |> Stack.pop |> Tuple.second)
        
        , test "popping on an empty stack yields nothing as the first element of the tuple" <|
            \() ->
                Expect.equal
                  Nothing
                  (Stack.pop Stack.empty |> Tuple.first)

        , test "popping on an empty stack returns an empty stack as the second element of the tuple" <|
            \() ->
                Expect.equal
                  Stack.empty
                  (Stack.pop Stack.empty |> Tuple.second)
        ]
      
      , describe "push" 
          [ fuzz (list int) "pushing elements onto a stack has the same length as the original list" <|
              \xs ->
                  Expect.equal
                    (List.length xs)
                    (List.foldr Stack.push Stack.empty xs |> Stack.size)
          ]
      
      , describe "peek" 
          [ fuzz (nonEmptyListFuzzer int) "peeking and the head of a list are Just the first element of both" <|
              \xs ->
                  Expect.equal
                    (List.head xs)
                    (Stack.fromList xs |> Stack.peek)
          
          , test "peeking an empty list yields nothing" <|
              \() ->
                  Expect.equal
                    Nothing
                    (Stack.peek Stack.empty)
          ]
      
    , describe "member" 
        [ fuzz (nonEmptyListFuzzer int) "every member of the list is a member of the stack" <|
            \xs ->
                let
                  stack = Stack.fromList xs
                in
                  List.all (flip Stack.member stack) xs
                    |> Expect.true "every member of the stack is a member of the list"
        
        , test "an element that is part of the stack yields false" <|
            \() ->
                List.range 0 10 
                  |> Stack.fromList
                  |> Stack.member 20
                  |> Expect.false "is not a member of the list"

        , fuzz int "an empty stack should always yield nothing" <|
            \x ->
                Stack.empty
                  |> Stack.member x
                  |> Expect.false "an empty stack has no members"
        ]
    
    , describe "append" 
        [ fuzz (list int) "appending a list with the second stack as empty is equal to the original stack" <|
            \xs ->
                Expect.all
                  [ Expect.equal (Stack.fromList xs) 
                  , Expect.equal (Stack.fromList xs |> Stack.size) << Stack.size
                  , Expect.equal (List.head xs) << Stack.peek
                  ]
                  (Stack.append (Stack.fromList xs) Stack.empty)
          
        , fuzz (list int) "appending a list with the first stack as empty is equal to the original stack" <|
            \xs ->
                Expect.all
                  [ Expect.equal (Stack.fromList xs) 
                  , Expect.equal (Stack.fromList xs |> Stack.size) << Stack.size
                  , Expect.equal (List.head xs) << Stack.peek
                  ]
                  (Stack.append Stack.empty (Stack.fromList xs))
        
        , fuzz (tuple (list int, list int)) "appending two stacks" <|
            \stacks ->
                Expect.all
                  [ Expect.equal (uncurry List.append stacks |> Stack.fromList) 
                  , Expect.equal (uncurry List.append stacks |> Stack.fromList |> Stack.size) << Stack.size
                  ]
                  (both Stack.fromList stacks |> uncurry Stack.append)
      
        , test "appending two stacks empty stacks" <|
            \() ->
                Expect.all
                  [ Expect.equal Stack.empty 
                  , Expect.equal (Stack.size Stack.empty) << Stack.size
                  , Expect.equal Nothing << Stack.peek
                  ]
                  (Stack.append Stack.empty Stack.empty)
        ]
      
      , describe "concat"
          [ fuzz (list (list int)) "concating stacks is the same as appending lists together" <|
              \xs ->
                let
                  appendedList = List.foldr List.append [] xs 
                in
                  Expect.all
                    [ Expect.equal (Stack.fromList appendedList)
                    , Expect.equal (List.length appendedList) << Stack.size
                    , Expect.equal (List.head appendedList) << Stack.peek
                    ]
                    (List.map Stack.fromList xs |> Stack.concat)
            
          , test "concating an empty list gives an empty stack" <|
              \() -> 
                  Expect.equal
                    Stack.empty
                    (Stack.concat [])
          ]

      , describe "map"
          [ fuzz (list int) "mapping the same function over a list and stack should result in the same stack" <|
              \xs ->
                  Expect.equal
                    (List.map ((+) 1) xs |> Stack.fromList)
                    (Stack.fromList xs |> Stack.map ((+) 1)) 
          ]
    ]