module test where

data List (A : Set) : Set where
  [] : List A
  _::_ : A → List A → List A

emptyList : {A : Set} → List A
emptyList = []
