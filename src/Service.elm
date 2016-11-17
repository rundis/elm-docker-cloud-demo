module Service
    exposing
        ( decodeDockerMsg
        , decodeServiceLogMsg
        , listServices
        , listStacks
        , Stack
        , Service
        , DockerMsg
        , ServiceLogMsg
        , DockerState(..)
        , ServiceState(..)
        )

import Json.Decode as Json
import Http


dockerApiUrl : String
dockerApiUrl =
    "https://cloud.docker.com"


type alias Stack =
    { uuid : String
    , uri : String
    , name : String
    , state : StackState
    }





type alias Service =
    { uuid : String
    , uri : String
    , stackUri : String
    , name : String
    , nickname : String
    , state : ServiceState
    }


{-| Represents a [Docker Cloud Event](https://docs.docker.com/apidocs/docker-cloud/#docker-cloud-event)
-}
type alias DockerMsg =
    { uuid : String
    , resourceUri : String
    , event : DockerEvent
    , action : DockerAction
    , state : DockerState
    }


{-| Representas a log message from a service
-}
type alias ServiceLogMsg =
    { source : String
    , log : String
    }


type StackState
    = StackNotRunning
    | StackStarting
    | StackRunning
    | StackPartlyRunning
    | StackStopping
    | StackStopped
    | StackRedeploying
    | StackTerminating
    | StackTerminated


type ServiceState
    = ServiceNotRunning
    | ServiceStarting
    | ServiceRunning
    | ServicePartlyRunning
    | ServiceScaling
    | ServiceRedeploying
    | ServiceStopping
    | ServiceStopped
    | ServiceTerminating
    | ServiceTerminated


type DockerEvent
    = StackEvent
    | ServiceEvent
    | ContainerEvent
    | NodeClusterEvent
    | NodeEvent
    | ActionEvent
    | ErrorEvent


type DockerAction
    = CreateAction
    | UpdateAction
    | DeleteAction


type DockerState
    = DockerStateStack StackState
    | DockerStateService ServiceState
    | DockerStateNode NodeState
    | DockerStateNodeCluster NodeClusterState
    | DockerStateAction ActionState
    | DockerStateContainer ContainerState


type ContainerState
    = ContainerStarting
    | ContainerRunning
    | ContainerStopping
    | ContainerStopped
    | ContainerTerminating
    | ContainerTerminated


type NodeState
    = NodeDeploying
    | NodeDeployed
    | NodeUnreachable
    | NodeUpgrading
    | NodeTerminating
    | NodeTerminated


type NodeClusterState
    = NodeClusterInit
    | NodeClusterDeploying
    | NodeClusterDeployed
    | NodeClusterPartlyDeployed
    | NodeClusterScaling
    | NodeClusterTerminating
    | NodeClusterTerminated
    | NodeClusterEmpty


type ActionState
    = ActionPending
    | ActionInProgress
    | ActionCanceling
    | ActionCanceled
    | ActionSuccess
    | ActionFailed


listStacks : String -> (Result Http.Error (List Stack) -> msg) -> Cmd msg
listStacks apiKey returnMsg =
    dockerApiGet "/api/app/v1/stack/" apiKey stacksDecoder returnMsg


listServices : String -> (Result Http.Error (List Service) -> msg) -> Cmd msg
listServices apiKey returnMsg =
    dockerApiGet "/api/app/v1/service/" apiKey servicesDecoder returnMsg


decodeDockerMsg : (Result String DockerMsg -> msg) -> String -> msg
decodeDockerMsg returnMsg wsMsg =
    Json.decodeString dockerMsgDecoder wsMsg
        |> returnMsg


dockerMsgDecoder : Json.Decoder DockerMsg
dockerMsgDecoder =
    Json.map5 DockerMsg
        (Json.field "uuid" Json.string)
        (Json.field "resource_uri" Json.string)
        (Json.field "type" eventDecoder)
        (Json.field "action" actionDecoder)
        (Json.field "type" eventDecoder
            |> Json.andThen
                (\evt ->
                    case evt of
                        StackEvent ->
                            (Json.field "state" stackStateDecoder)
                                |> Json.andThen
                                    (\state -> Json.succeed (DockerStateStack state))

                        ServiceEvent ->
                            (Json.field "state" serviceStateDecoder)
                                |> Json.andThen
                                    (\state -> Json.succeed (DockerStateService state))

                        NodeEvent ->
                            (Json.field "state" nodeStateDecoder)
                                |> Json.andThen
                                    (\state -> Json.succeed (DockerStateNode state))

                        NodeClusterEvent ->
                            (Json.field "state" nodeClusterStateDecoder)
                                |> Json.andThen
                                    (\state -> Json.succeed (DockerStateNodeCluster state))

                        ActionEvent ->
                            (Json.field "state" actionStateDecoder)
                                |> Json.andThen
                                    (\state -> Json.succeed (DockerStateAction state))

                        ContainerEvent ->
                            (Json.field "state" containerStateDecoder)
                                |> Json.andThen
                                    (\state -> Json.succeed (DockerStateContainer state))

                        ErrorEvent ->
                            Json.fail "An Error event occured, check the logs"
                )
        )


decodeServiceLogMsg :
    (String -> msg)
    -> (Result String ServiceLogMsg -> msg)
    -> String
    -> msg
decodeServiceLogMsg logStartMsg logMsg wsMsg =
    let
        typeRes =
            Json.decodeString (Json.field "type" Json.string) wsMsg
    in
        case typeRes of
            Result.Ok kind ->
                case kind of
                    "log-started" ->
                        logStartMsg kind

                    "log" ->
                        logMsg <| Json.decodeString serviceLogMsgDecoder wsMsg

                    _ ->
                        let
                            _ = Debug.log "Unknow logtype" kind
                        in
                            Debug.crash "Unknown logtype Shit !"

            Result.Err err ->
                let
                    _ =
                        Debug.log "Error decoding type:" err
                in
                    Debug.crash "Shit"


serviceLogMsgDecoder : Json.Decoder ServiceLogMsg
serviceLogMsgDecoder =
    Json.map2 ServiceLogMsg
        (Json.field "source" Json.string)
        (Json.field "log" Json.string)


dockerApiGet :
    String
    -> String
    -> Json.Decoder a
    -> (Result Http.Error a -> msg)
    -> Cmd msg
dockerApiGet uri apiKey decoder msg =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" ("Basic " ++ apiKey)
            , Http.header "accept" "application/json"
            ]
        , url = dockerApiUrl ++ uri ++ "?limit=1000"
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send msg


stacksDecoder : Json.Decoder (List Stack)
stacksDecoder =
    Json.field "objects" (Json.list stackDecoder)


stackDecoder : Json.Decoder Stack
stackDecoder =
    Json.map4 Stack
        (Json.field "uuid" Json.string)
        (Json.field "resource_uri" Json.string)
        (Json.field "name" Json.string)
        (Json.field "state" stackStateDecoder)


servicesDecoder : Json.Decoder (List Service)
servicesDecoder =
    Json.field "objects" (Json.list serviceDecoder)


serviceDecoder : Json.Decoder Service
serviceDecoder =
    Json.map6 Service
        (Json.field "uuid" Json.string)
        (Json.field "resource_uri" Json.string)
        (Json.field "stack" Json.string)
        (Json.field "name" Json.string)
        (Json.field "nickname" Json.string)
        (Json.field "state" serviceStateDecoder)


stackStateDecoder : Json.Decoder StackState
stackStateDecoder =
    Json.string
        |> Json.andThen
            (\stateStr ->
                case stateStr of
                    "Not running" ->
                        Json.succeed StackNotRunning

                    "Starting" ->
                        Json.succeed StackStarting

                    "Running" ->
                        Json.succeed StackRunning

                    "Partly running" ->
                        Json.succeed StackPartlyRunning

                    "Stopping" ->
                        Json.succeed StackStopping

                    "Stopped" ->
                        Json.succeed StackStopped

                    "Redeploying" ->
                        Json.succeed StackRedeploying

                    "Terminating" ->
                        Json.succeed StackTerminating

                    "Terminated" ->
                        Json.succeed StackTerminated

                    _ ->
                        Json.fail <| "Unknown stack state: " ++ stateStr
            )


serviceStateDecoder : Json.Decoder ServiceState
serviceStateDecoder =
    Json.string
        |> Json.andThen
            (\stateStr ->
                case stateStr of
                    "Not running" ->
                        Json.succeed ServiceNotRunning

                    "Starting" ->
                        Json.succeed ServiceStarting

                    "Running" ->
                        Json.succeed ServiceRunning

                    "Partly running" ->
                        Json.succeed ServicePartlyRunning

                    "Scaling" ->
                        Json.succeed ServiceScaling

                    "Redeploying" ->
                        Json.succeed ServiceRedeploying

                    "Stopping" ->
                        Json.succeed ServiceStopping

                    "Stopped" ->
                        Json.succeed ServiceStopped

                    "Terminating" ->
                        Json.succeed ServiceTerminating

                    "Terminated" ->
                        Json.succeed ServiceTerminated

                    _ ->
                        Json.fail <| "Unknown service state: " ++ stateStr
            )


nodeStateDecoder : Json.Decoder NodeState
nodeStateDecoder =
    Json.string
        |> Json.andThen
            (\stateStr ->
                case stateStr of
                    "Deploying" ->
                        Json.succeed NodeDeploying

                    "Deployed" ->
                        Json.succeed NodeDeployed

                    "Unreachable" ->
                        Json.succeed NodeUnreachable

                    "Upgrading" ->
                        Json.succeed NodeUpgrading

                    "Terminating" ->
                        Json.succeed NodeTerminating

                    "Terminated" ->
                        Json.succeed NodeTerminated

                    _ ->
                        Json.fail <| "Unknown node state: " ++ stateStr
            )


nodeClusterStateDecoder : Json.Decoder NodeClusterState
nodeClusterStateDecoder =
    Json.string
        |> Json.andThen
            (\stateStr ->
                case stateStr of
                    "Init" ->
                        Json.succeed NodeClusterInit

                    "Deploying" ->
                        Json.succeed NodeClusterDeploying

                    "Deployed" ->
                        Json.succeed NodeClusterDeployed

                    "Partly deployed" ->
                        Json.succeed NodeClusterPartlyDeployed

                    "Scaling" ->
                        Json.succeed NodeClusterScaling

                    "Terminating" ->
                        Json.succeed NodeClusterTerminating

                    "Terminated" ->
                        Json.succeed NodeClusterTerminated

                    "Empty" ->
                        Json.succeed NodeClusterEmpty

                    _ ->
                        Json.fail <| "Unknown node cluster state: " ++ stateStr
            )


actionStateDecoder : Json.Decoder ActionState
actionStateDecoder =
    Json.string
        |> Json.andThen
            (\stateStr ->
                case stateStr of
                    "Pending" ->
                        Json.succeed ActionPending

                    "In progress" ->
                        Json.succeed ActionInProgress

                    "Canceling" ->
                        Json.succeed ActionCanceling

                    "Canceled" ->
                        Json.succeed ActionCanceled

                    "Success" ->
                        Json.succeed ActionSuccess

                    "Failed" ->
                        Json.succeed ActionFailed

                    _ ->
                        Json.fail <| "Unknown node action state: " ++ stateStr
            )


containerStateDecoder : Json.Decoder ContainerState
containerStateDecoder =
    Json.string
        |> Json.andThen
            (\stateStr ->
                case stateStr of
                    "Starting" ->
                        Json.succeed ContainerStarting

                    "Running" ->
                        Json.succeed ContainerRunning

                    "Stopping" ->
                        Json.succeed ContainerStopping

                    "Stopped" ->
                        Json.succeed ContainerStopped

                    "Terminating" ->
                        Json.succeed ContainerTerminating

                    "Terminated" ->
                        Json.succeed ContainerTerminated

                    _ ->
                        Json.fail <| "Unknown container state: " ++ stateStr
            )


eventDecoder : Json.Decoder DockerEvent
eventDecoder =
    Json.string
        |> Json.andThen
            (\typeStr ->
                case typeStr of
                    "stack" ->
                        Json.succeed StackEvent

                    "service" ->
                        Json.succeed ServiceEvent

                    "container" ->
                        Json.succeed ContainerEvent

                    "nodecluster" ->
                        Json.succeed NodeClusterEvent

                    "node" ->
                        Json.succeed NodeEvent

                    "action" ->
                        Json.succeed ActionEvent

                    "error" ->
                        Json.succeed ErrorEvent

                    _ ->
                        Json.fail <| "Unknown event type: " ++ typeStr
            )


actionDecoder : Json.Decoder DockerAction
actionDecoder =
    Json.string
        |> Json.andThen
            (\actionStr ->
                case actionStr of
                    "create" ->
                        Json.succeed CreateAction

                    "update" ->
                        Json.succeed UpdateAction

                    "delete" ->
                        Json.succeed DeleteAction

                    _ ->
                        Json.fail <| "Unknown action: " ++ actionStr
            )
