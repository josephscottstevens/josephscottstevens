module Keyboard exposing (Direction(..), keyDecoder)

import Json.Decode as Decode

type Direction
  = Left
  | Right
  | Down
  | Up
  | Unknown

keyDecoder : Decode.Decoder Direction
keyDecoder =
  Decode.map toDirection (Decode.field "key" Decode.string)

toDirection : String -> Direction
toDirection string =
  case string of
    "ArrowLeft" ->
      Left

    "ArrowRight" ->
      Right

    "ArrowDown" ->
      Down

    "ArrowUp" ->
      Up

    _ ->
      Unknown