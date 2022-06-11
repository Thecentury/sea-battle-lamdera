module Frontend exposing (..)

import Array as Array
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html
import Html.Attributes as Attr
import Html.Attributes.Extra
import Html.Events
import Lamdera
import Types exposing (..)
import Url


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
init _ _ =
    ( { gameId = ""
      , ownField = Array.initialize fieldSize (always (Array.initialize fieldSize (always Empty)))
      , enemyField = Array.initialize fieldSize (always (Array.initialize fieldSize (always Empty)))
      }
    , Cmd.none
    )


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

        CellClicked _ ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
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
                , if emitClicks then
                    Html.Events.onClick (CellClicked { x = columnIndex, y = rowIndex })

                  else
                    Html.Attributes.Extra.empty
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


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Html.div []
            [ Html.p [] [ Html.text "Own" ]
            , renderField model.ownField False
            ]
        , Html.div []
            [ Html.p [] [ Html.text "Enemy" ]
            , renderField model.enemyField True
            ]
        ]
    }
