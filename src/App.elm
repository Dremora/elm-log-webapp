module App exposing (..)

import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import Element.Events exposing (onWithOptions)
import Json.Decode as Json
import Navigation exposing (programWithFlags, Location)
import Style exposing (..)
import Style.Color as StyleColor
import Style.Border as Border
import Style.Font as Font


type alias Model =
    { message : String
    }


init : () -> Location -> ( Model, Cmd Msg )
init () location =
    ( { message = "Your Elm App is working!" }, Cmd.none )


type Msg
    = NoOp
    | UrlChange Location
    | NavigateTo String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateTo link ->
            ( model, Navigation.newUrl link )

        UrlChange location ->
            ( model, always Cmd.none (Debug.log "location" location) )

        _ ->
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


stopPropagation =
    onWithOptions "click"
        { stopPropagation = False
        , preventDefault = True
        }
        (Json.map NavigateTo <| Json.at [ "target", "href" ] Json.string)


navigation =
    column Navigation
        [ width (px 150) ]
        [ menuHeading
        , link ("/") <| el MenuItem [ stopPropagation, paddingXY 10 11 ] (text "Events")
        , link ("/meetups/") <| el MenuItem [ stopPropagation, paddingXY 10 11 ] (text "Meetups")
        ]


contents =
    el None [ width (fill 1) ] (text "Contents")


view : Model -> Html Msg
view model =
    Element.root stylesheet <|
        row None [ height (percent 100), width (percent 100) ] [ navigation, contents ]


main : Program () Model Msg
main =
    programWithFlags UrlChange
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
