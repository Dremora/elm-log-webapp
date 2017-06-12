module App exposing (..)

import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src)


---- MODEL ----


type alias Model =
    { message : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { message = "Your Elm App is working!" }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [] []



---- PROGRAM ----


main : Program () Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
