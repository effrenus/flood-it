import Browser
import Html exposing (Html, h1, button, div, label, span, text, select, option)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import Random
import Array
import Maybe
import Time
import Task
import Debug exposing (log)
import Set

main =
  Browser.document { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


-- MODEL

type alias Color = String 

type alias Board = Array.Array Color

type alias Settings =
  { size : Int
  , paletteSize : Int
  }

type alias Game =
  { settings : Settings
  , board : Board
  , movesCount : Int
  , maxMovesCount : Int
  }

type alias Model =
  { settings : Settings
  , game : Game
  }

palette : Array.Array Color
palette = 
  Array.fromList [
    "#f44336"
  , "#e91e63"
  , "#9c27b0"
  , "#673ab7"
  , "#3f51b5"
  , "#2196f3"
  , "#009688"
  , "#4caf50"
  , "#cddc39"
  , "#795548"
  , "#ff6c37"
  ]

boardIsFlooded : Board -> Bool
boardIsFlooded board =
  let
    currentColor = Array.get 0 board |> Maybe.withDefault "#yyyyyy"
  in
  board
  |> Array.toList
  |> List.all (\color -> color == currentColor)


fillBoard : Game -> Int -> Game
fillBoard game initSeed =
  let
    { paletteSize, size } = game.settings
    gen = Random.int 0 <| min (paletteSize - 1) <| (Array.length palette) - 1
    make board n seed =
      if n == 0 then
        board
      else
        let
          (i, newSeed) = Random.step gen seed
          color = Array.get i palette |> Maybe.withDefault "#ffffff"
        in
        make (color :: board) (n - 1) newSeed
  in
  {
    game | board = make [] (size*size) (Random.initialSeed initSeed) |> Array.fromList
  }

init : () -> ( Model, Cmd Msg )
init _ =
  let
    defaultSettings = { size = 10, paletteSize = 3 }
  in
  ( { settings = defaultSettings
    , game = { settings = defaultSettings
             , board = Array.fromList []
             , movesCount = 0
             , maxMovesCount = 30
             } 
    }
  , Task.perform FillBoard Time.now )


-- UPDATE

type Msg = 
  StartGame 
  | FillBoard Time.Posix
  | SetColor String
  | SetBoardSize String
  | SetPaletteSize String

updateBoard : Game -> Color -> Game
updateBoard game selectedColor =
  let
    { size } = game.settings
    currentColor = Array.get 0 game.board |> Maybe.withDefault "#yyyyyy"
    getColor (i, j) board = Array.get (i*size + j) board |> Maybe.withDefault "#zzzzzz"
    bfs board queue visited =
      case queue of
        [] -> board
        (i, j) :: tl ->
          if Set.member (i, j) visited then
            bfs board tl visited
          else
            let
              updatedBoard = Array.set (i*size + j) selectedColor board
              updatedQueue = 
                [(i+1, j), (i-1, j), (i, j-1), (i, j+1)]
                |> List.filter (\(r, c) -> 0 <= r && r < size && 0 <= c && c < size && getColor (r, c) updatedBoard == currentColor)
                |> List.append tl
            in
            bfs updatedBoard updatedQueue <| Set.insert (i, j) visited
  in
  if currentColor == selectedColor then
    game
  else
    { game | movesCount = game.movesCount + 1, board = bfs game.board [(0, 0)] Set.empty }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    { settings, game } = model
  in
  case msg of
    StartGame ->
      let
        newGame = {
            settings = settings
          , movesCount = 0
          , maxMovesCount = 30
          , board = Array.fromList []
          }
      in
      ( 
          { model | game = newGame }
        , Task.perform FillBoard Time.now
      )
    FillBoard time -> 
      (
          { model | game = fillBoard game <| Time.posixToMillis time }
        , Cmd.none 
      )
    SetColor newColor ->
      (
          { model | game = updateBoard game newColor }
        , Cmd.none
      )
    SetPaletteSize paletteSize ->
      (
          { model | settings = { settings | paletteSize = String.toInt paletteSize |> Maybe.withDefault 3 } }
        , Cmd.none 
      )
    SetBoardSize boardSize ->
      (
          { model | settings = { settings | size = String.toInt boardSize |> Maybe.withDefault 10 } }
        , Cmd.none 
      )


-- VIEW

viewBoard : Game -> List (Html Msg)
viewBoard game =
  let
    { size } = game.settings
    viewRow board =
      board
      |> Array.map (\cell -> div [
          onClick (SetColor cell),
          style "display" "inline-block",
          style "backgroundColor" cell, 
          style "width" "20px",
          style "height" "20px"
         ] [])
      |> Array.toList
  in
  Array.initialize size (\i -> div [ style "font-size" "0"] (game.board |> Array.slice (i*size) (i*size+size) |> viewRow))
  |> Array.toList

viewSizeSelector : Html Msg
viewSizeSelector =
  div []
  [ label [ style "font-variant" "small-caps", style "font-size" "18px", style "display" "block" ] [ text "board size" ]
  , select 
      [ onInput SetBoardSize
      , style "font-size" "14px"
      , style "margin" "0 15px"
      , style "padding" "4px 10px"
      ]
      [ option [ value "10" ] [ text "10x10" ]
      , option [ value "15" ] [ text "15x15" ]
      , option [ value "18" ] [ text "18x18" ]
      , option [ value "22" ] [ text "22x22" ]
      , option [ value "28" ] [ text "28x28" ]
      ]
  ]

viewPaletteSelector : Html Msg
viewPaletteSelector =
  div []
  [ label [ style "font-variant" "small-caps", style "font-size" "18px", style "display" "block" ] [ text "palette" ]
  , select
    [ onInput SetPaletteSize
    , style "font-size" "14px"
    , style "margin" "0 15px"
    , style "padding" "4px 10px"
    ]
    [ option [ value "3" ] [ text "3" ]
    , option [ value "5" ] [ text "5" ]
    , option [ value "7" ] [ text "7" ]
    ]
  ]

viewScore : Game -> Html Msg
viewScore game =
  div [ style "font-size" "23px", style "margin-bottom" "30px" ]
  [ text "Score: "
  , text <| String.fromInt game.movesCount
  , text "/"
  , text <| String.fromInt game.maxMovesCount
  ]

viewResultMessage : Game -> Html Msg
viewResultMessage game =
  if game.movesCount <= game.maxMovesCount && boardIsFlooded game.board then
    div [ style "font-size" "21px" ] [ text "Win! 🤩" ]
  else if game.movesCount > game.maxMovesCount then
    div [ style "font-size" "21px" ] [ text "Next try! ☹️" ]
  else
    span [] []

view : Model -> Browser.Document Msg
view model =
  {
      title = "Flood it — The Game"
    , body = [  
        div [
            style "max-width" "1000px"
          , style "margin" "auto"
          , style "text-align" "center"
        ]
          [
            h1 [] [ text "Flood it!", span [ style "color" "#ff6200", style "font-size" "27px" ] [ text "🔥" ] ]
          , viewResultMessage model.game
          , div [ 
                style "margin" "30px auto"
              , style "box-shadow" "0 0 10px #999"
              , style "width" ((String.fromInt (model.game.settings.size * 20)) ++ "px")
            ] (viewBoard model.game)
          , viewScore model.game
          , div [ style "display" "flex", style "justify-content" "center" ]
            [ viewSizeSelector
            , viewPaletteSelector
            , button [
                  onClick StartGame
                , style "padding" "5px 15px"
                , style "font-size" "15px"
                , style "border-radius" "10px"
                , style "border" "2px solid #6548ff"
                , style "background-color" "#6184ec"
                , style "color" "#fff"
                , style "cursor" "pointer" ] [ text "Start NEW game" ]
            ]
          ]
    ]
  }
