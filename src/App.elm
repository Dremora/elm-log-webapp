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
import UrlParser exposing (Parser, (</>), s, oneOf, parsePath)


type Route
    = EventsRoute
    | MeetupsRoute


route : Parser (Route -> a) a
route =
    oneOf
        [ UrlParser.map EventsRoute (s "")
        , UrlParser.map MeetupsRoute (s "meetups")
        ]


type alias Model =
    { message : String
    , currentRoute : Maybe Route
    }


init : () -> Location -> ( Model, Cmd Msg )
init () location =
    ( { message = "Your Elm App is working!"
      , currentRoute = parsePath route location
      }
    , Cmd.none
    )


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
            ( { model | currentRoute = parsePath route location }, Cmd.none )

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


contents currentRoute =
    el None
        [ width (fill 1) ]
        (case currentRoute of
            Just EventsRoute ->
                text "Events"

            Just MeetupsRoute ->
                text "Meetups"

            Nothing ->
                text "Not found"
        )


view : Model -> Html Msg
view model =
    Element.root stylesheet <|
        row None
            [ height (percent 100), width (percent 100) ]
            [ navigation
            , contents model.currentRoute
            ]


main : Program () Model Msg
main =
    programWithFlags UrlChange
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
