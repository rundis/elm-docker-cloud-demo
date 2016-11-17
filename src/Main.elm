module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (class, style, href, value, type_, placeholder, autofocus)
import Html.Events exposing (onClick, onInput)
import Http
import Platform.Cmd
import CDN exposing (bootstrap)
import String
import WebSocket
import Service as Api exposing (Stack, Service, DockerMsg, ServiceLogMsg, DockerState(..), ServiceState(..))


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { stacks : List Stack
    , services : List Service
    , selectedStack : Maybe Stack
    , selectedService : Maybe Service
    , serviceLog : Maybe (List ServiceLogMsg)
    , stackFilter : String
    }


type Msg
    = NoOp
    | MessageRecieved (Result String DockerMsg)
    | LogStarted String
    | LogMessageReceived (Result String Api.ServiceLogMsg)
    | StacksRetrieved (Result Http.Error (List Stack))
    | ServicesRetrieved (Result Http.Error (List Service))
    | SelectStack Stack
    | SelectService Service
    | FilterChanged String






apiKey : String
apiKey =
    "HELLO IM NOT GIVING YOU THAT !"


init : ( Model, Cmd Msg )
init =
    ( { stacks = []
      , services = []
      , selectedStack = Nothing
      , selectedService = Nothing
      , serviceLog = Nothing
      , stackFilter = ""
      }
    , Cmd.batch
        [ Api.listStacks apiKey StacksRetrieved
        , Api.listServices apiKey ServicesRetrieved
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FilterChanged filter ->
            ( { model | stackFilter = filter }, Cmd.none )

        MessageRecieved res ->
            case res of
                Result.Ok dockerMsg ->
                    handleDockerMessage dockerMsg model

                Result.Err err ->
                    let
                        _ =
                            Debug.log "Err: " err
                    in
                        ( model, Cmd.none )

        LogMessageReceived res ->
            let
                _ =
                    Debug.log "Service log msg: " res
            in
                case res of
                    Result.Ok logMsg ->
                        let
                            serviceLog =
                                model.serviceLog
                                    |> Maybe.map (\xs -> logMsg :: xs)
                                    |> Maybe.map (\xs -> List.take 1000 xs)
                        in
                            ( { model | serviceLog = serviceLog }, Cmd.none )

                    Result.Err err ->
                        ( model, Cmd.none )

        LogStarted source ->
            let
                _ =
                    Debug.log "Service log started for: " source
            in
                ( { model | serviceLog = Just [] }, Cmd.none )

        StacksRetrieved res ->
            case res of
                Result.Err err ->
                    let
                        _ =
                            Debug.log "Error retrieving stacks from http: " err
                    in
                        ( model, Cmd.none )

                Result.Ok stacks ->
                    ( { model | stacks = stacks }, Cmd.none )

        ServicesRetrieved res ->
            case res of
                Result.Err err ->
                    let
                        _ =
                            Debug.log "Error retrieving services from http: " err
                    in
                        ( model, Cmd.none )

                --Debug.crash ("Error retrieving from http: " ++ toString err)
                Result.Ok services ->
                    ( { model | services = services }, Cmd.none )

        SelectStack stack ->
            ( { model
                | selectedStack = Just stack
                , selectedService = Nothing
                , serviceLog = Nothing
              }
            , Cmd.none
            )

        SelectService service ->
            ( { model
                | selectedService = Just service
                , serviceLog = Nothing
              }
            , Cmd.none
            )


handleDockerMessage : DockerMsg -> Model -> ( Model, Cmd Msg )
handleDockerMessage msg model =
    case msg.state of
        DockerStateStack stackState ->
            let
                updateStackState stack =
                    if stack.uri == msg.resourceUri then
                        { stack | state = stackState }
                    else
                        stack
            in
                ( { model | stacks = List.map updateStackState model.stacks }, Cmd.none )

        DockerStateService serviceState ->
            let
                updateServiceState service =
                    if service.uri == msg.resourceUri then
                        let
                            _ =
                                Debug.log ("State change for " ++ service.nickname ++ ": ") { oldState = service.state, newState = serviceState }
                        in
                            { service | state = serviceState }
                    else
                        service
            in
                ( { model | services = List.map updateServiceState model.services }, Cmd.none )

        _ ->
            let
                _ =
                    Debug.log "Not yet handling this docker msg: " msg
            in
                ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        eventSub =
            WebSocket.listen "wss://ws.cloud.docker.com/api/audit/v1/events/" (Api.decodeDockerMsg MessageRecieved)
    in
        case model.selectedService of
            Just service ->
                let
                    _ =
                        Debug.log "We should listen for log messages: " service.nickname

                    logSub =
                        WebSocket.listen ("wss://ws.cloud.docker.com/api/app/v1/service/" ++ service.uuid ++ "/logs/")
                            (Api.decodeServiceLogMsg LogStarted LogMessageReceived)
                in
                    Sub.batch [ eventSub, logSub ]

            Nothing ->
                eventSub


view : Model -> Html Msg
view model =
    div
        [ class "container-fluid" ]
        [ bootstrap.css
        , div
            [ class "row" ]
            [ div
                [ class "col-md-4" ]
                [ h1 [] [ text "Stacks" ]
                , stackFilterForm model
                , ul [ class "list-group" ]
                    (filterStacks model
                        |> List.map (\stack -> stackItem (stackItemClass model stack) stack)
                    )
                ]
            , div [ class "col-md-8" ]
                [ selectedStackView model ]
            ]
        ]


stackItemClass : Model -> Stack -> String
stackItemClass model stack =
    case model.selectedStack of
        Just sel ->
            if sel == stack then
                "active"
            else
                ""

        Nothing ->
            ""


filterStacks : Model -> List Stack
filterStacks { stackFilter, stacks } =
    List.filter
        (\stack -> String.contains (String.toLower stackFilter) (String.toLower stack.name))
        stacks


stackFilterForm : Model -> Html Msg
stackFilterForm model =
    form [ class "form-inline" ]
        [ input
            [ class "form-control"
            , type_ "text"
            , value model.stackFilter
            , placeholder "Enter filter"
            , onInput FilterChanged
            , autofocus True
            ]
            []
        ]


stackItem : String -> Stack -> Html Msg
stackItem clazz stack =
    a
        [ href "#"
        , class ("list-group-item " ++ clazz)
        , onClick (SelectStack stack)
        ]
        [ span [ class "badge" ] [ text <| toString stack.state ]
        , text stack.name
        ]


stackStateClass : String -> String
stackStateClass state =
    case state of
        "Partly running" ->
            "list-group-item-warning"

        "Stopped" ->
            "list-group-item-danger"

        "Not running" ->
            "list-group-item-danger"

        _ ->
            ""


selectedStackView : Model -> Html Msg
selectedStackView model =
    case model.selectedStack of
        Just stack ->
            let
                services =
                    List.filter (\service -> service.stackUri == stack.uri) model.services
            in
                div []
                    [ h2 [] [ text stack.name ]
                    , stackServicesView stack services
                    , serviceLogView model
                    ]

        Nothing ->
            div [] [ h2 [] [ text "No stack selected" ] ]


stackServicesView : Stack -> List Service -> Html Msg
stackServicesView stack services =
    div []
        [ h3 [] [ text "Services" ]
        , ul [ class "list-group" ]
            (List.map serviceItem services)
        ]


serviceItem : Service -> Html Msg
serviceItem service =
    a
        [ href "#"
        , class <| "list-group-item " ++ (serviceStateClass service.state)
        , onClick (SelectService service)
        ]
        [ span [ class "badge" ] [ text <| toString service.state ]
        , text service.name
        ]


serviceStateClass : ServiceState -> String
serviceStateClass serviceState =
    case serviceState of
        ServiceStopped ->
            "list-group-item-danger"

        ServiceTerminated ->
            "list-group-item-danger"

        ServiceTerminating ->
            "list-group-item-danger"

        ServiceStopping ->
            "list-group-item-danger"

        ServiceRunning ->
            ""

        _ ->
            "list-group-item-warning"


serviceLogView : Model -> Html Msg
serviceLogView model =
    case model.selectedService of
        Nothing ->
            div [] [ text "No service selected yet" ]

        Just service ->
            case model.serviceLog of
                Nothing ->
                    div []
                        [ h3 [] [ text <| service.nickname ]
                        , text "Waiting for logs..."
                        ]

                Just logItems ->
                    div
                        []
                        [ h3 [] [ text <| service.nickname ]
                        , ul [ serviceLogStyle ]
                            (List.map (\logItem -> li [] [ text logItem.log ]) logItems)
                        ]


serviceLogStyle : Attribute Msg
serviceLogStyle =
    style
        [ ( "display", "block" )
        , ( "background", "black" )
        , ( "color", "white" )
        , ( "list-style", "none" )
        , ( "height", "500px" )
        , ( "padding-left", "5px" )
        , ( "overflow-y", "scroll" )
        , ( "font-family", "Roboto Mono,consolas,monaco,monospace" )
        ]
