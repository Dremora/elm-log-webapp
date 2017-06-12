module App exposing (..)

import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import Style exposing (..)
import Style.Color as StyleColor
import Style.Border as Border
import Style.Font as Font


type alias Model =
    { message : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { message = "Your Elm App is working!" }, Cmd.none )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


type Styles
    = None
    | Navigation
    | MenuHeading
    | MenuItem


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.stylesheet
        [ style None []
        , style Navigation
            [ StyleColor.background (Color.rgb 25 24 24) ]
        , style MenuHeading
            [ StyleColor.background (Color.rgb 31 141 214)
            , StyleColor.text (Color.rgb 255 255 255)
            , Font.size 18
            , Font.uppercase
            ]
        , style MenuItem
            [ StyleColor.text (Color.rgb 153 153 153)
            , StyleColor.border (Color.rgb 51 51 51)
            , Border.bottom 1
            , Border.solid
            , hover
                [ StyleColor.background (Color.rgb 51 51 51)
                , cursor "pointer"
                ]
            ]
        ]


menuHeading =
    el MenuHeading [ paddingXY 10 11 ] (text "Community")


navigation =
    column Navigation
        [ width (px 150) ]
        [ menuHeading
        , el MenuItem [ paddingXY 10 11 ] (text "Events")
        , el MenuItem [ paddingXY 10 11 ] (text "Meetups")
        ]


contents =
    el None [ width (fill 1) ] (text "Contents")


view : Model -> Html Msg
view model =
    Element.root stylesheet <|
        row None [ height (percent 100), width (percent 100) ] [ navigation, contents ]


main : Program () Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
