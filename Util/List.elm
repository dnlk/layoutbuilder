module Util.List exposing (listRange)

listRangeRev : Int -> List Int
listRangeRev x =
  if x == 0
  then []
  else x :: listRange (x - 1)

listRange : Int -> List Int
listRange = List.reverse << listRangeRev