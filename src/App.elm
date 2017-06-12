module App exposing (..)

import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import Http
import Element.Events exposing (onWithOptions)
import Json.Decode as Decode
import Navigation exposing (programWithFlags, Location)
import RemoteData exposing (WebData)
import Style exposing (..)
import Style.Color as StyleColor
import Style.Border as Border
import Style.Font as Font
import UrlParser exposing (Parser, (</>), s, oneOf, parsePath)


type Route
    = EventsRoute
    | MeetupsRoute


parseRoute : Parser (Route -> a) a
parseRoute =
    oneOf
        [ UrlParser.map EventsRoute (s "")
        , UrlParser.map MeetupsRoute (s "meetups")
        ]


type alias Model =
    { currentRoute : Maybe Route
    , events : WebData Events
    }


type alias Events =
    List Event


type alias Event =
    { id : Int
    , title : String
    }


init : () -> Location -> ( Model, Cmd Msg )
init () location =
    update (UrlChange location)
        ({ currentRoute = Nothing
         , events = RemoteData.NotAsked
         }
        )


type Msg
    = NoOp
    | UrlChange Location
    | NavigateTo String
    | EventsResponse (WebData Events)


getEvents : Cmd Msg
getEvents =
    Http.get "https://api.elmlog.com/events" decodeEvents
        |> RemoteData.sendRequest
        |> Cmd.map EventsResponse


decodeEvents : Decode.Decoder Events
decodeEvents =
    Decode.list <|
        Decode.map2 Event
            (Decode.field "id" Decode.int)
            (Decode.field "title" Decode.string)


routeLoadData : Maybe Route -> Cmd Msg
routeLoadData route =
    case route of
        Just EventsRoute ->
            getEvents

        _ ->
            Cmd.none


routeLoadModel : Model -> Model
routeLoadModel model =
    case model.currentRoute of
        Just EventsRoute ->
            { model | events = RemoteData.Loading }

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateTo link ->
            ( model, Navigation.newUrl link )

        UrlChange location ->
            let
                route =
                    parsePath parseRoute location
            in
                ( routeLoadModel { model | currentRoute = route }, routeLoadData route )

        EventsResponse response ->
            ( { model | events = response }
            , Cmd.none
            )

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
        (Decode.map NavigateTo <| Decode.at [ "target", "href" ] Decode.string)


menuItem url label =
    link url <| el MenuItem [ stopPropagation, paddingXY 10 11 ] (text label)


navigation =
    column Navigation
        [ width (px 150) ]
        [ menuHeading
        , menuItem "/" "Events"
        , menuItem "/meetups/" "Meetups"
        ]


eventView event =
    column None
        []
        [ text (toString event.id)
        , text event.title
        ]


eventsView events =
    case events of
        RemoteData.NotAsked ->
            text "Initialising."

        RemoteData.Loading ->
            text "Loading."

        RemoteData.Failure err ->
            text ("Error: " ++ toString err)

        RemoteData.Success events ->
            column None [] <| List.map eventView events


contents model =
    el None
        [ width (fill 1) ]
        (case model.currentRoute of
            Just EventsRoute ->
                eventsView model.events

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
            , contents model
            ]


main : Program () Model Msg
main =
    programWithFlags UrlChange
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
