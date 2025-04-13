module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onChange)
import Task
import Time

import MomoRisc.Cpu as Cpu exposing (Cpu, MemoryAccessPhaseAction(..))
import MomoRisc.Device as Device exposing (Chario)
import MomoRisc.Inst as Inst exposing (Inst, ParseErr)
import MomoRisc.Memory as Memory exposing (Memory)
import MomoRisc.Dudit as Dudit exposing (Dudit)
import MomoRisc.Program as Program exposing (Program, DebugInfo, Errors, LineNum)



main =
  Browser.element
    { init = (\() -> ( init, Cmd.none ))
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-----------
-- MODEL --
-----------

type alias Model =
  { cpu : Cpu
  , program : Program
  , memory : Memory
  , chario : Chario
  , source : String
  , defaultInput : String
  , errors : Errors
  , debug : DebugInfo
  , lastCycle : Cpu
  , wroteAddr : Maybe Dudit
  , tab : Tab
  , speed : Speed
  }


type Tab
  = EditorTab
  | RunnerTab


init : Model
init =
  let
    ( program, debug, errors ) =
      Program.compile sampleCode
  in
    { cpu = Cpu.init
    , program = program
    , memory = Memory.zeros
    , chario = Device.charioInit |> Device.setInput sampleInput
    , source = sampleCode
    , defaultInput = sampleInput
    , errors = errors
    , debug = debug
    , lastCycle = Cpu.init
    , wroteAddr = Nothing
    , tab = RunnerTab
    , speed = Stop
    }


sampleCode : String
sampleCode =
  """; Read the input and output it
; in reverse order.
LDI B 91
LDI C 01
LD A B
BEQ A D 07
ST A C
ADI C C 01
JPI A 02
ADI C C 99
LD A C
BEQ A D 12
ST A B
JPI A 07
HLT
"""


sampleInput : String
sampleInput = "CSIRomoM"



------------
-- UPDATE --
------------

type Msg
  = TabChange Tab
  | SourceEdited String
  | DefaultInputEdited String
  | Compile
  | ResetMachine
  | Step
  | SpeedChange Speed
  | CharioInputEdited String
  | CharioOutputClear


type Speed
  = Stop
  | NormalSpeed
  | HighSpeed


type alias MemoryAccessPhaseResult =
  { cpu : Cpu
  , memory : Memory
  , chario : Chario
  , wroteAddr : Maybe Dudit
  }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of

    TabChange tab ->
      if tab == model.tab then
        model |> noCmd
      else
        { model
        | speed = Stop
        , tab = tab
        } |> noCmd


    SourceEdited str ->
      { model | source = str }
        |> noCmd


    DefaultInputEdited str ->
      { model | defaultInput = str }
        |> noCmd


    Compile ->
      let
        ( program, debug, errors ) =
          Program.compile model.source

        model_ =
          { model
          | program = program
          , debug = debug
          , errors = errors
          }
      in
        ( model_, continueMsgs [ ResetMachine, TabChange RunnerTab ] )


    ResetMachine ->
      { model
      | cpu = Cpu.init
      , memory = Memory.zeros
      , lastCycle = Cpu.init
      , wroteAddr = Nothing
      , chario = Device.charioInit |> Device.setInput model.defaultInput
      , speed = Stop
      } |> noCmd


    Step ->
      let
        result =
          case Cpu.step model.program model.cpu of
            NoAccessNeeded cpu ->
              MemoryAccessPhaseResult cpu model.memory model.chario Nothing

            ReadNeeded addr fun ->
              let
                ( data, chario ) =
                  case Dudit.toInt addr of
                    90 -> Device.readNumber model.chario
                    91 -> Device.readChar model.chario
                  --92 -> plottouchGetX
                  --93 -> plottouchGetY
                    x ->
                      if x < 90 then
                        ( Memory.read addr model.memory, model.chario )
                      else
                        ( Dudit.zero, model.chario )

                cpu = fun data
              in
                MemoryAccessPhaseResult cpu model.memory chario Nothing

            WriteNeeded addr data cpu ->
              let
                ( memory, chario ) =
                  case Dudit.toInt addr of
                    90 -> ( model.memory, Device.writeNumber data model.chario )
                    91 -> ( model.memory, Device.writeChar data model.chario )
                  --92 -> plottouchSetX
                  --93 -> plottouchSetY
                  --94 -> plottouchExecute
                    x ->
                      if x < 90 then
                        ( Memory.write addr data model.memory, model.chario )
                      else
                        ( model.memory, model.chario )
              in
                MemoryAccessPhaseResult cpu memory chario (Just addr)
      in
        { model
        | cpu = result.cpu
        , memory = result.memory
        , chario = result.chario
        , lastCycle = model.cpu
        , wroteAddr = result.wroteAddr
        }  |> noCmd

    SpeedChange speed ->
      { model | speed = speed }
        |> noCmd

    CharioInputEdited str ->
      { model | chario = Device.setInput str model.chario }
        |> noCmd

    CharioOutputClear ->
      { model | chario = Device.clearOutput model.chario }
        |> noCmd


noCmd : Model -> ( Model, Cmd msg )
noCmd model =
  ( model, Cmd.none )


continueMsgs : List Msg -> Cmd Msg
continueMsgs =
  List.map (\msg -> Task.perform (\_ -> msg) (Task.succeed ()))
    >> Cmd.batch


----------
-- VIEW --
----------

type alias TabBuildInfo =
  { title: String
  , labelId: String
  , panelId: String
  , contents: Model -> List (Html Msg)
  }


tabBuildInfo : Tab -> TabBuildInfo
tabBuildInfo tab =
  case tab of
    EditorTab ->
      { title = "Editor"
      , labelId = "editor-tab"
      , panelId = "editor-panel"
      , contents = editorContents
      }

    RunnerTab ->
      { title = "Runner"
      , labelId = "runner-tab"
      , panelId = "runner-panel"
      , contents = runnerContents
      }


view : Model -> Html Msg
view model =
  let
    tabs = [ EditorTab, RunnerTab ]

    ( labels, panels ) =
      tabs
        |> List.map (labelAndPanel model)
        |> List.unzip
  in
    Html.div [ Attr.id "elm-area" ]
      [ Html.div [ Attr.class "tab-container" ]
        [ Html.div
          [ Attr.class "tab-labels"
          , Attr.attribute "role" "tablist"
          ] labels
        , Html.div [ Attr.class "tab-content" ]
          panels
        ]
      ]


labelAndPanel : Model -> Tab -> ( Html Msg, Html Msg )
labelAndPanel model tab =
  let
    { title, labelId, panelId, contents } =
      tabBuildInfo tab

    ( selected, hidden ) =
      if model.tab == tab then
        ( "true", "false" )
      else
        ( "false", "true" )

    label =
      Html.button
        [ Attr.id labelId
        , Attr.class "tab"
        , Attr.attribute "role" "tab"
        , Attr.attribute "aria-selected" selected
        , Attr.attribute "aria-controles" panelId
        , onClick <| TabChange tab
        ]
        [ Html.text title ]

    panel =
      Html.div
        [ Attr.id panelId
        , Attr.attribute "role" "tabpanel"
        , Attr.attribute "aria-hidden" hidden
        , Attr.attribute "aria-labelledby" labelId
        ]
        (contents model)
  in
    ( label, panel )



-- EDITOR CONTENTS --

editorContents : Model -> List (Html Msg)
editorContents model =
  [ Html.h3 [] [ Html.text "Program" ]
  , Html.div [ Attr.class "button-overlay-wrapper" ]
    [ Html.textarea
      [ Attr.class "program source-code box-frame"
      , Attr.placeholder "Program"
      , onChange SourceEdited
      , Attr.value model.source
      , Attr.spellcheck False
      ] []
    , Html.div [Attr.class "buttons"]
      [ Html.button [ onClick Compile ] [ Html.text "Compile" ] ]
    ]
  , Html.h3 [] [ Html.text "Default Input"]
  , Html.input
    [ Attr.type_ "text"
    , Attr.class "chario box-frame"
    , Attr.placeholder "Program default input here..."
    , Attr.value model.defaultInput
    , onChange DefaultInputEdited
    ] []
  ]



-- RUNNER CONTENSTS --

runnerContents : Model -> List (Html Msg)
runnerContents model =
  [ Html.div [ Attr.class "two-column-wrapper" ]
      [ Html.div [ Attr.class "left-column" ]
        [ Html.h3 [] [ Html.text "Program" ]
        , checkAndRunArea model.source model.debug model.errors model.cpu
        ]
      , Html.div [ Attr.class "right-column" ]
        [ Html.h3 [] [ Html.text "Register" ]
        , registerArea model.cpu model.lastCycle
        , Html.h3 [] [ Html.text "Memory" ]
        , memoryArea model.wroteAddr model.memory
        ]
      ]
  , Html.h3 [] [ Html.text "Input" ]
  , Html.input
    [ Attr.type_ "text"
    , Attr.class "chario box-frame"
    , Attr.placeholder "Program input here..."
    , Attr.value (Device.getInput model.chario)
    , onChange CharioInputEdited
    ] []
  , Html.div [ Attr.class "h-block" ]
    [ Html.h3 [] [ Html.text "Output" ]
    , Html.button [ onClick CharioOutputClear ]
      [ Html.text "clear" ]
    ]
  , Html.div [ Attr.class "chario box-frame" ]
    [ Html.pre []
      [ Html.text <| Device.getOutput model.chario ]
    ]
  ]



-- CHECK AND RUN AREA --

checkAndRunArea : String -> Dict LineNum Dudit -> Dict LineNum ParseErr -> Cpu -> Html Msg
checkAndRunArea source debug errors cpu =
  let
    addrAndCodeView : LineNum -> String -> ( Html msg, Html msg )
    addrAndCodeView lineNum str =
      let
        addr =
          Dict.get lineNum debug

        now =
          addr
            |> Maybe.map (Dudit.eq cpu.pc)
            |> Maybe.withDefault False

        error =
          Dict.get lineNum errors
      in
        ( addrView addr, lineView now str error )

    ( lineNums, code ) =
      (source ++ "\n")
        |> String.lines
        |> List.indexedMap addrAndCodeView
        |> List.unzip
  in
    Html.div [ Attr.class "button-overlay-wrapper" ]
      [
        Html.div [ Attr.class "program compile-result box-frame" ]
          [ Html.div [ Attr.class "code-with-addr" ]
            [ Html.div [ Attr.class "addr" ] lineNums
            , Html.div [ Attr.class "code" ] code
            ]
          ]
      , Html.div [ Attr.class "buttons" ]
        [ Html.button [ onClick ResetMachine ] [ resetMark ]
        , Html.button [ onClick Step ] [ stepMark ]
        , Html.button [ onClick <| SpeedChange Stop ] [ stopMark ]
        , Html.button [ onClick <| SpeedChange NormalSpeed ] [ playMark ]
        , Html.button [ onClick <| SpeedChange HighSpeed ] [ fastForwardMark ]
        ]
      ]


addrView : Maybe Dudit -> Html msg
addrView maybeAddr =
  let
    content =
      maybeAddr
        |> Maybe.map Dudit.toString
        |> Maybe.withDefault ""
        |> brIfEmpty
  in
    Html.div [] [ content ]


lineView : Bool -> String -> Maybe ParseErr -> Html msg
lineView executing str maybeErr =
  let
    line =
      case maybeErr of
        Nothing ->
          [ brIfEmpty str ]

        Just err ->
          case err of
            Inst.OpName _ ->
              wordErr 0 str

            Inst.ArgsLength _ _ ->
              [ errSpan str ]

            Inst.ArgsFormat idx ->
              wordErr idx str

    cursor =
      Html.div [ Attr.class "cursor" ] []

    line_ =
      line
        |> List.intersperse (Html.text " ")

    contents =
      if executing then
        cursor :: line_
      else
        line_
  in
    Html.div [ Attr.class "line" ] contents


brIfEmpty : String -> Html msg
brIfEmpty str =
  case str of
    "" ->
      Html.br [] []

    _ ->
      Html.text str


wordErr : Int -> String -> List (Html msg)
wordErr i str =
  str
    |> String.words
    |> List.indexedMap (\j word ->
      if i == j then
        errSpan word
      else
        Html.text word)


errSpan : String -> Html msg
errSpan str =
  Html.span [ Attr.class "error" ] [ Html.text str ]


resetMark : Html msg
resetMark =
  Html.div [ Attr.class "mark" ]
    [ Html.div [ Attr.class "circle-arrow" ]
      [ Html.div [ Attr.class "arrowhead" ] [] ]
    ]


stepMark : Html msg
stepMark =
  Html.div [ Attr.class "mark" ]
    [ Html.div [ Attr.class "triangle" ] []
    , Html.div [ Attr.class "bar crimp" ] []
    ]


stopMark : Html msg
stopMark =
  Html.div [ Attr.class "mark" ]
    [ Html.div [ Attr.class "bar" ] []
    , Html.div [ Attr.class "bar gap" ] []
    ]


playMark : Html msg
playMark =
  Html.div [ Attr.class "mark" ]
    [ Html.div [ Attr.class "triangle" ] []
    ]


fastForwardMark : Html msg
fastForwardMark =
  Html.div [ Attr.class "mark" ]
    [ Html.div [ Attr.class "triangle-thin" ] []
    , Html.div [ Attr.class "triangle-thin crimp" ] []
    ]



-- REGISTER AREA --

registerArea current last =
  Html.div [ Attr.class "registers box-frame" ]
    [ registerItem "PC" current.pc last.pc
    , registerItem "A" current.a last.a
    , registerItem "B" current.b last.b
    , registerItem "C" current.c last.c
    , registerItem "D" current.d last.d
    ]

registerItem : String -> Dudit -> Dudit -> Html msg
registerItem label value prev =
  let
    text = Html.text (Dudit.toString value)

    inner =
      if Dudit.eq value prev then
        text
      else
        Html.span [ Attr.class "highlight" ] [ text ]
  in
    Html.div [ Attr.class "register" ]
      [ Html.div [ Attr.class "register-label" ] [ Html.text (label ++ ":") ]
      , Html.div [ Attr.class "register-value" ] [ inner ]
      ]



-- MEMORY AREA --

memoryArea : Maybe Dudit -> Memory -> Html msg
memoryArea wroteAddr memory =
  let
    celler coords =
      case coords of
        ( 0, 0 ) ->
          Html.th [] []

        ( x, 0 ) ->
          Html.th [] [ Html.text <| String.fromInt <| x - 1 ]

        ( 0, y ) ->
          Html.th [] [ Html.text <| String.fromInt <| (y - 1) * 10 ]

        ( x, y ) ->
          let
            addr =
              (y - 1) * 10 + (x - 1)

            text =
              case addr of
                90 -> "NM"
                91 -> "CH"
              --92 -> "PX"
              --93 -> "PY"
              --94 -> "PE"
                _ ->
                  if addr < 90 then
                    memory
                      |> Memory.read (Dudit.fromInt addr)
                      |> Dudit.toString
                  else
                    "--"

            highlight =
              wroteAddr
                |> Maybe.map (Dudit.eq (Dudit.fromInt addr))
                |> Maybe.withDefault False

            inner =
              if highlight then
                Html.span [ Attr.class "highlight" ] [ Html.text text ]
              else
                Html.text text
          in
            Html.td [] [ inner ]
  in
    table [ Attr.class "memory" ] 11 11 celler


table : List (Html.Attribute msg) -> Int -> Int -> (( Int, Int ) -> Html msg) -> Html msg
table attributes columns rows celler =
  let
    tbody =
      List.range 0 (rows - 1)
        |> List.map (\y ->
        List.range 0 (columns - 1)
          |> List.map (\x -> celler ( x, y ))
          |> Html.tr [])
        |> Html.tbody []
  in
    Html.table attributes [ tbody ]



-------------------
-- SUBSCRIPTIONS --
-------------------

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.speed of
    Stop ->
      Sub.none

    NormalSpeed ->
      Time.every 140 (\_ -> Step)

    HighSpeed ->
      Time.every 28 (\_ -> Step)
