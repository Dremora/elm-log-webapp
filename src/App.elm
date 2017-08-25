module App exposing (..)

import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import Http
import Element.Events exposing (onWithOptions)
import Json.Encode as Encode
import Json.Decode as Decode
import Navigation exposing (programWithFlags, Location)
import RemoteData exposing (WebData)
import Style exposing (..)
import Style.Color as Color
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
    , startsAt : String
    , city : Maybe String
    , country : Maybe String
    , host : String
    , url : String
    , description : String
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
        Decode.map8 Event
            (Decode.field "id" Decode.int)
            (Decode.field "title" Decode.string)
            (Decode.field "local_starts_at" Decode.string)
            (Decode.maybe <| Decode.field "city" Decode.string)
            (Decode.maybe <| Decode.field "country" Decode.string)
            (Decode.field "host" Decode.string)
            (Decode.field "url" Decode.string)
            (Decode.field "description" Decode.string)


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
    | Heading
    | PageHeader
    | Entry
    | EntryHeader
    | EntryStartsAt
    | EntryLocation
    | EntryDescription
    | EventLink
    | EntryFooter


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.stylesheet
        [ style None []
        , style Navigation
            [ Color.background (Color.rgb 25 24 24) ]
        , style MenuHeading
            [ Color.background (Color.rgb 31 141 214)
            , Color.text (Color.rgb 255 255 255)
            , Font.size 18
            , Font.uppercase
            ]
        , style MenuItem
            [ Color.text (Color.rgb 153 153 153)
            , Color.border (Color.rgb 51 51 51)
            , Border.bottom 1
            , Border.solid
            , hover
                [ Color.background (Color.rgb 51 51 51)
                , cursor "pointer"
                ]
            ]
        , style Heading
            [ Color.text (Color.rgb 51 51 51)
            , Font.size 48
            , Font.weight 300
            , Font.center
            , paddingTopHint 54
            , paddingBottomHint 12
            , Border.bottom 1
            , Color.border (Color.rgb 238 238 238)
            ]
        , style PageHeader
            [ Color.text (Color.rgb 119 119 119)
            , Font.size 32
            , Font.weight 700
            ]
        , style Entry
            [ Border.bottom 1
            , Border.solid
            , Color.border (Color.rgb 238 238 238)
            ]
        , style EntryHeader
            [ Color.text (Color.rgb 136 136 136)
            , Font.size 24
            , Font.weight 300
            ]
        , style EntryStartsAt
            [ Color.text (Color.rgb 119 119 119)
            , Font.size 18.72
            , Font.weight 700
            ]
        , style EntryLocation
            [ Color.text (Color.rgb 119 119 119)
            , Font.size 16
            , Font.weight 700
            ]
        , style EntryDescription
            [ Color.text (Color.rgb 119 119 119)
            , Font.size 16
            , Font.lineHeight (25.6 / 16)
            ]
        , style EventLink
            [ Color.text Color.white
            , Color.background (Color.rgb 0 120 231)
            , paddingTopHint 9
            , paddingBottomHint 9
            , paddingLeftHint 16
            , paddingRightHint 16
            , Border.rounded 2
            ]
        , style EntryFooter
            [ paddingBottomHint 16 ]
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


eventLocationView event =
    case ( event.city, event.country ) of
        ( Just city, Just country ) ->
            city ++ ", " ++ country ++ " (" ++ event.host ++ ")"

        _ ->
            event.host


buttonLink title url =
    link url <| el EventLink [] (text title)


eventView event =
    column Entry
        []
        [ node "h3" <| el EntryHeader [ paddingTop 46 ] <| text event.title
        , el EntryStartsAt [ paddingTop 25 ] <| text event.startsAt
        , el EntryLocation [ paddingTop 28 ] <| text <| eventLocationView event
        , el EntryDescription [ paddingTop 26, property "innerHTML" (Encode.string event.description) ] <| text ""
        , row EntryFooter [ paddingTop 41 ] [ buttonLink "View on Meetup.com" event.url ]
        ]


eventsView events =
    listView
        [ el PageHeader [] (text "Elm Events")
        , case events of
            RemoteData.NotAsked ->
                text "Initialising."

            RemoteData.Loading ->
                text "Loading."

            RemoteData.Failure err ->
                text ("Error: " ++ toString err)

            RemoteData.Success events ->
                column None [ spacing 4, paddingBottom 31 ] <| List.map eventView events
        ]


listView =
    column None [ paddingXY 165 19 ]


contents model =
    column None
        [ width (fill 1), inlineStyle [ ( "overflow", "auto" ) ] ]
        [ node "h1" <| el Heading [ inlineStyle [ ( "margin", "0" ) ] ] <| text "Elm Log"
        , (case model.currentRoute of
            Just EventsRoute ->
                eventsView model.events

            Just MeetupsRoute ->
                text "Meetups"

            Nothing ->
                text "Not found"
          )
        ]


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
