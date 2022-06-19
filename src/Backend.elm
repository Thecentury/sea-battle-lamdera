module Backend exposing (..)

import Dict
import Lamdera exposing (ClientId, SessionId)
import Types exposing (..)


type alias Model =
    BackendModel



--noinspection ElmUnusedSymbol


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { latestGameId = 0, games = Dict.empty }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )


updateGame : GameId -> BackendGameState -> BackendModel -> BackendModel
updateGame gameId state oldModel =
    { oldModel | games = Dict.insert gameId state oldModel.games }


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        ConnectToGame gameId ->
            case Dict.get gameId model.games of
                Just game ->
                    case game of
                        -- todo if sessionId = player1, do not update the game, just respond with the player data
                        Player1Connected player1Field ->
                            let
                                game2 =
                                    BothPlayersConnected { player1 = player1Field, player2 = player1Field, turn = Player1 }

                                model2 =
                                    updateGame gameId game2 model
                            in
                            ( model2, Cmd.none )

                        BothPlayersConnected _ ->
                            -- todo
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Lamdera.sendToFrontend clientId GameIsUnknown )

        CellClicked _ _ ->
            -- todo implement me
            ( model, Cmd.none )

        CreateNewGame ->
            -- todo implement me
            ( model, Cmd.none )
