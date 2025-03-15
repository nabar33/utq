module Main exposing (..)

import Browser
import Random
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = (\_ -> Sub.none)
    , view = view
    }


-- MODEL


type alias Player =
  { name : String
  , score : Int
  } 

type alias GameState = List Player 

type Session 
  = Lobby
  | ActiveGame GameState
  | PostGame Int

type alias Model = Session

init : () -> (Model, Cmd Msg)
init _ =
  ( Lobby
  , Cmd.none
  )


-- UPDATE


type Msg = StartGame | QuitGame | Roll | NewDie Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (model, msg) of
    (Lobby, StartGame) ->
      ( ActiveGame [ { name = "Gerry", score = 0 } ]
      , Cmd.none
      )
    (ActiveGame gameState, QuitGame) ->
      ( PostGame (Maybe.withDefault 0 (List.head (List.map .score gameState)))
      , Cmd.none
      )
    (ActiveGame gameState, Roll) ->
      ( ActiveGame gameState
      , Random.generate NewDie roll
      )
    (ActiveGame gameState, NewDie value) ->
      ( ActiveGame (List.map (\player -> { player | score = player.score + value }) gameState)
      , Cmd.none
      )
    (PostGame _, StartGame) ->
      ( ActiveGame [ { name = "Newcomer", score = 0 } ]
      , Cmd.none
      )
    _ ->
      ( model
      , Cmd.none
      )

roll : Random.Generator Int
roll =
  Random.int 1 6


-- VIEW

renderPlayer player = text (player.name ++ "'s score is " ++ (String.fromInt player.score) )

view model =
  div [] (case model of
            Lobby ->
              [ button [ onClick StartGame ] [ text "Start a new game" ] ]
            ActiveGame gameState ->
              (List.map renderPlayer gameState) ++
              [ button [ onClick Roll ] [ text "Roll dice" ]
              , button [ onClick QuitGame ] [ text "Quit this game" ]
              ]
            PostGame score ->
              [ text ("Final Score: " ++ (String.fromInt score))
              , button [ onClick StartGame ] [ text "Start another game" ]
              ]
          )

