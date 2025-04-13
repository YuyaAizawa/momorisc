module MomoRisc.Device exposing
  ( Chario
  , charioInit
  , getInput
  , setInput
  , getOutput
  , clearOutput
  , readNumber
  , writeNumber
  , readChar
  , writeChar
  )

import Dict exposing (Dict)

import MomoRisc.Dudit as Dudit exposing (Dudit)



type Chario = Chario CharioImpl
type alias CharioImpl =
  { input : String
  , output : String
  }


charListWoBr : List Char
charListWoBr =
  [       ' ', ',',  '.', '!', '?', '\'', '"', ':',  ';'
  ,  '0', '1', '2',  '3', '4', '5',  '6', '7', '8',  '9'
  ,  'A', 'B', 'C',  'D', 'E', 'F',  'G', 'H', 'I',  'J'
  ,  'K', 'L', 'M',  'N', 'O', 'P',  'Q', 'R', 'S',  'T'
  ,  'U', 'V', 'W',  'X', 'Y', 'Z',  '(', ')', '{',  '}'
  ,  'a', 'b', 'c',  'd', 'e', 'f',  'g', 'h', 'i',  'j'
  ,  'k', 'l', 'm',  'n', 'o', 'p',  'q', 'r', 's',  't'
  ,  'u', 'v', 'w',  'x', 'y', 'z',  '<', '>', '[',  ']'
  ,  '+', '-', '/', '\\', '|', '_',  '$', '%', '&',  '@'
  ,  '^', '~', '=',  '*', '`'
  ]


char2int : Dict Char Int
char2int =
  charListWoBr
    |> List.indexedMap (\i c -> ( c, i + 1 ))
    |> (::) ( '\n', 99 )
    |> Dict.fromList


int2char : Dict Int Char
int2char =
  charListWoBr
    |> List.indexedMap (\i c -> ( i + 1, c ))
    |> (::) ( 99, '\n' )
    |> Dict.fromList


charioInit : Chario
charioInit = CharioImpl "" "" |> Chario


getInput : Chario -> String
getInput (Chario { input }) =
  input


setInput : String -> Chario -> Chario
setInput str (Chario { output }) =
  CharioImpl str output |> Chario


getOutput : Chario -> String
getOutput (Chario { output }) =
  output


clearOutput : Chario -> Chario
clearOutput (Chario { input }) =
  CharioImpl input "" |> Chario


readNumber : Chario -> ( Dudit, Chario )
readNumber (Chario impl) =
  let
    ( int, input ) =
      impl.input |> readUntilNumber (\l tl ->
        tl |> readUntilNumber (\r rest ->
          case ( l, r ) of
            ( Just lnum, Just rnum ) ->
              ( lnum * 10 + rnum, rest )

            _ ->
              ( 0, rest )))
  in
    ( Dudit.fromInt int, Chario { impl | input = input } )


readUntilNumber : (Maybe Int -> String -> ( a, String )) -> String -> ( a, String )
readUntilNumber fun str =
  case String.uncons str of
    Nothing ->
      fun Nothing str

    Just ( hd, tl ) ->
      hd
        |> String.fromChar
        |> String.toInt
        |> Maybe.map (\i -> fun (Just i) tl)
        |> Maybe.withDefault (readUntilNumber fun tl)


writeNumber : Dudit -> Chario -> Chario
writeNumber dudit (Chario impl) =
  let
    output =
      impl.output ++ (Dudit.toString dudit)
  in
    Chario { impl | output = output }


readChar : Chario -> ( Dudit, Chario )
readChar (Chario impl) =
  let
    ( dudit, input ) =
      impl.input
        |> readUntilChar
        |> Maybe.withDefault ( Dudit.zero, "" )
  in
    ( dudit, Chario { impl | input = input } )


readUntilChar : String -> Maybe ( Dudit, String )
readUntilChar str =
  case String.uncons str of
    Nothing ->
      Nothing

    Just ( hd, tl ) ->
      case Dict.get hd char2int of
        Nothing ->
          readUntilChar tl

        Just i ->
          Just ( Dudit.fromInt i, tl )


writeChar : Dudit -> Chario -> Chario
writeChar dudit (Chario impl) =
  case Dict.get (Dudit.toInt dudit) int2char of
    Nothing ->
      (Chario impl)

    Just ch ->
      let
        output =
          impl.output ++ String.fromChar ch
      in
        Chario { impl | output = output }
