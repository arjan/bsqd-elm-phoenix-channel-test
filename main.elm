module Chat exposing (..) --where

import Html exposing (Html, h3, div, text, ul, li, input, form, button, br, table, tbody, tr, td, program)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput, onSubmit, onClick)
import Platform.Cmd
import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push
import Json.Encode as JE
import Json.Decode as JD
import Dict

-- MAIN

main : Program Never Model Msg
main =
  program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


-- CONSTANTS


socketServer : String
socketServer = "wss://bsqd.me/socket/websocket"


-- MODEL


type Msg
  = SendMessage
  | SetNewMessage String
  | PhoenixMsg (Phoenix.Socket.Msg Msg)
  | ReceiveMessage JE.Value
  | JoinChannel
  | LeaveChannel
  | ShowJoinedMessage String
  | ShowLeftMessage String
  | NoOp


type alias Model =
  { newMessage : String
  , messages : List String
  , phxSocket : Phoenix.Socket.Socket Msg
  , channel : String
  }

initPhxSocket : String -> Phoenix.Socket.Socket Msg
initPhxSocket channel =
  Phoenix.Socket.init socketServer
    |> Phoenix.Socket.withDebug
    |> Phoenix.Socket.on "message" channel ReceiveMessage

initModel : Model
initModel =
  Model "" [] ( initPhxSocket "bot:bd123342-2a5a-41e3-ba43-fffe799ef55b") "bot:bd123342-2a5a-41e3-ba43-fffe799ef55b"


init : ( Model, Cmd Msg )
init =
  ( initModel, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Phoenix.Socket.listen model.phxSocket PhoenixMsg

-- COMMANDS


-- PHOENIX STUFF

type alias ChatMessage =
  { user : String
  , body : String
  }

chatMessageDecoder : JD.Decoder ChatMessage
chatMessageDecoder =
  JD.map2 ChatMessage
    (JD.field "user" JD.string)
    (JD.field "body" JD.string)

-- UPDATE

userParams : JE.Value
userParams =
  JE.object [ ("user_id", JE.string "123") ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ReceiveMessage str ->
      ( { model | messages = str :: model.messages }
      , Cmd.none
      )

    PhoenixMsg msg ->
      let
        ( phxSocket, phxCmd ) = Phoenix.Socket.update msg model.phxSocket
      in
        ( { model | phxSocket = phxSocket }
        , Cmd.map PhoenixMsg phxCmd
        )

    SendMessage ->
      let
        payload = (JE.object [ ("user", JE.string "user"), ("body", JE.string model.newMessage) ])
        push_ =
          Phoenix.Push.init "new:msg" model.channel
            |> Phoenix.Push.withPayload payload
        (phxSocket, phxCmd) = Phoenix.Socket.push push_ model.phxSocket
      in
        ( { model
          | newMessage = ""
          , phxSocket = phxSocket
          }
        , Cmd.map PhoenixMsg phxCmd
        )

    SetNewMessage str ->
      ( { model | newMessage = str }
      , Cmd.none
      )

    JoinChannel ->
      let
        channel =
          Phoenix.Channel.init model.channel
            |> Phoenix.Channel.withPayload userParams
            |> Phoenix.Channel.onJoin (always (ShowJoinedMessage model.channel))
            |> Phoenix.Channel.onClose (always (ShowLeftMessage model.channel))

        (phxSocket, phxCmd) = Phoenix.Socket.join channel model.phxSocket
      in
        ({ model | phxSocket = phxSocket }
        , Cmd.map PhoenixMsg phxCmd
        )

    LeaveChannel ->
      let
        (phxSocket, phxCmd) = Phoenix.Socket.leave model.channel model.phxSocket
      in
        ({ model | phxSocket = phxSocket }
        , Cmd.map PhoenixMsg phxCmd
        )

    ShowJoinedMessage channelName ->
      ( { model | messages = ("Joined channel " ++ channelName) :: model.messages }
      , Cmd.none
      )

    ShowLeftMessage channelName ->
      ( { model | messages = ("Left channel " ++ channelName) :: model.messages }
      , Cmd.none
      )

    NoOp ->
      ( model, Cmd.none )


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h3 [] [ text "Channels:" ]
    , div
        []
        [ button [ onClick JoinChannel ] [ text "Join channel" ]
        , button [ onClick LeaveChannel ] [ text "Leave channel" ]
        ]
    , channelsTable (Dict.values model.phxSocket.channels)
    , br [] []
    , h3 [] [ text "Messages:" ]
    , newMessageForm model
    , ul [] ((List.reverse << List.map renderMessage) model.messages)
    ]

channelsTable : List (Phoenix.Channel.Channel Msg) -> Html Msg
channelsTable channels =
  table []
    [ tbody [] (List.map channelRow channels)
    ]

channelRow : (Phoenix.Channel.Channel Msg) -> Html Msg
channelRow channel =
  tr []
    [ td [] [ text channel.name ]
    , td [] [ (text << toString) channel.payload ]
    , td [] [ (text << toString) channel.state ]
    ]

newMessageForm : Model -> Html Msg
newMessageForm model =
  form [ onSubmit SendMessage ]
    [ input [ type_ "text", value model.newMessage, onInput SetNewMessage ] []
    ]

renderMessage : String -> Html Msg
renderMessage str =
  li [] [ text str ]
