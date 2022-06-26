module Frontend exposing (..)

import Array as Array
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html
import Html.Attributes as Attr
import Html.Attributes.Extra
import Html.Events
import Lamdera
import String exposing (fromInt)
import Types exposing (..)
import Url exposing (Url)
import Url.Parser exposing ((</>))


type alias Model =
    FrontendModel



--noinspection ElmUnusedSymbol


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        route =
            Url.Parser.parse urlDecoder url
    in
    case route of
        Just (GameRoot gameId) ->
            ( WaitingForAnotherPlayer { key = key, gameId = gameId }
            , Lamdera.sendToBackend (ConnectToGame gameId)
            )

        _ ->
            ( Initial { key = key }, Cmd.none )


urlDecoder : Url.Parser.Parser (Route -> c) c
urlDecoder =
    Url.Parser.oneOf
        [ Url.Parser.top |> Url.Parser.map Root
        , Url.Parser.int |> Url.Parser.map GameRoot
        ]


urlEncoder : GameId -> String
urlEncoder gameId =
    "/" ++ fromInt gameId


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal _ ->
                    ( model, Cmd.none )

                --( model
                --, Nav.pushUrl model.key (Url.toString url)
                --)
                External _ ->
                    ( model, Cmd.none )

        --( model
        --, Nav.load url
        --)
        UrlChanged _ ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        CreateNewGameClicked ->
            ( model, Lamdera.sendToBackend CreateNewGame )

        UserClickedCell coord ->
            -- todo lock the UI to not allow user to click on the different cell
            let
                cmd =
                    case frontendGameId model of
                        Just gameId ->
                            Lamdera.sendToBackend (CellClicked gameId coord)

                        Nothing ->
                            Cmd.none
            in
            ( model, cmd )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case ( msg, model ) of
        ( GameIsUnknown _, _ ) ->
            -- todo show an error message
            ( model, Cmd.none )

        ( ToFrontendError e, _ ) ->
            ( model, Debug.log ("ToFrontendError " ++ e) Cmd.none )

        ( GameCreated gameId, Initial d ) ->
            ( WaitingForAnotherPlayer { key = d.key, gameId = gameId }
            , Nav.pushUrl d.key (urlEncoder gameId)
            )

        ( GameCreated _, _ ) ->
            -- todo log an error
            ( model, Cmd.none )

        ( UpdateGameState _, Initial _ ) ->
            ( model, Debug.log "Not expected to receive 'UpdateGameState' in 'Initial' state" Cmd.none )

        ( UpdateGameState gameUpdate, WaitingForAnotherPlayer state ) ->
            let
                newModel =
                    Playing
                        { key = state.key
                        , gameId = state.gameId
                        , currentTurn = gameUpdate.turn
                        , me = gameUpdate.me
                        , ownField = gameUpdate.ownField
                        , enemyField = gameUpdate.enemyField
                        }
            in
            ( newModel, Cmd.none )

        ( UpdateGameState gameUpdate, Playing state ) ->
            let
                newModel =
                    Playing
                        { state
                            | currentTurn = gameUpdate.turn
                            , ownField = gameUpdate.ownField
                            , enemyField = gameUpdate.enemyField
                        }
            in
            ( newModel, Cmd.none )

        ( ClickedCellRejected _, Playing _ ) ->
            -- todo unlock the UI, show a notification that the cell cannot be used
            ( model, Cmd.none )

        ( ClickedCellRejected _, _ ) ->
            ( model, Cmd.none )


renderField : Field -> Bool -> Html.Html FrontendMsg
renderField field emitClicks =
    let
        renderCell : Int -> Int -> Cell -> Html.Html FrontendMsg
        renderCell rowIndex columnIndex cell =
            Html.td
                [ Attr.style "width" "20px"
                , Attr.style "height" "20px"
                , Attr.style "padding" "0"
                , Attr.style "border" "solid black 1px"
                , Attr.style "text-align" "center"
                , Attr.style "vertical-align" "middle"
                , Html.Attributes.Extra.attributeIf emitClicks <| Attr.style "cursor" "pointer"
                , Html.Attributes.Extra.attributeMaybe
                    (\color -> Attr.style "background" color)
                    (cellToBackground cell)
                , Html.Attributes.Extra.attributeIf emitClicks <|
                    Html.Events.onClick (UserClickedCell { x = columnIndex, y = rowIndex })
                ]
                [ Html.text <| cellToString cell ]

        renderRow : Int -> Array.Array Cell -> Html.Html FrontendMsg
        renderRow rowIndex row =
            Html.tr [] (List.indexedMap (renderCell rowIndex) <| Array.toList row)
    in
    Html.table
        [ Attr.style "border-spacing" "0" ]
        [ Html.tbody [] (List.indexedMap renderRow <| Array.toList field)
        ]


viewInitialScreen : () -> Browser.Document FrontendMsg
viewInitialScreen () =
    { title = ""
    , body =
        [ Html.div []
            [ Html.button
                [ Html.Events.onClick CreateNewGameClicked
                ]
                [ Html.text "Create new game"
                ]
            ]
        ]
    }


viewBothPlayersConnected : FrontendReady -> Browser.Document FrontendMsg
viewBothPlayersConnected model =
    let
        ownTurn =
            model.me == model.currentTurn
    in
    { title = ""
    , body =
        [ Html.div []
            [ Html.text
                (if ownTurn then
                    "Your turn"

                 else
                    "Enemy turn"
                )
            ]
        , Html.div []
            [ Html.p [] [ Html.text "Own" ]
            , renderField model.ownField False
            ]
        , Html.div []
            [ Html.p [] [ Html.text "Enemy" ]
            , renderField model.enemyField ownTurn
            ]
        ]
    }


view : Model -> Browser.Document FrontendMsg
view model =
    case model of
        Initial _ ->
            viewInitialScreen ()

        WaitingForAnotherPlayer _ ->
            { title = ""
            , body =
                [ Html.div []
                    [ Html.text "Waiting for another player to connect" ]
                ]
            }

        Playing data ->
            viewBothPlayersConnected data
