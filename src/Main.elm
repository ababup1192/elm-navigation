module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Url.Parser as P exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Q



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


type alias Query =
    String


route : Parser (Route -> a) a
route =
    P.oneOf
        [ P.map Home P.top
        , P.map Foo (P.s "foo")
        , P.map User (P.s "user" </> P.int)
        , P.map Search (P.s "search" <?> Q.string "query")
        ]


type Route
    = Home
    | Foo
    | User Int
    | Search (Maybe Query)
    | NotFound


toRoute : Url.Url -> Route
toRoute url =
    Maybe.withDefault NotFound <| P.parse route url


view : Model -> Browser.Document Msg
view { url } =
    case toRoute url of
        Home ->
            { title = "ホーム"
            , body =
                [ text "ホーム画面"
                , ul []
                    [ viewLink "/foo"
                    , viewLink "/user/1234"
                    , viewLink "/search?query=word"
                    ]
                ]
            }

        Foo ->
            { title = "ふー"
            , body =
                [ text "ふーです。"
                , ul []
                    [ viewLink "/foo"
                    , viewLink "/user/1234"
                    , viewLink "/search?query=word"
                    ]
                ]
            }

        User id ->
            { title = "ユーザ画面"
            , body =
                [ text <| "ユーザIDは、" ++ String.fromInt id ++ "です。"
                , ul []
                    [ viewLink "/foo"
                    , viewLink "/user/1234"
                    , viewLink "/search?query=word"
                    ]
                ]
            }

        Search maybeQuery ->
            { title = "検索画面"
            , body =
                [ text
                    (Maybe.map (\query -> "検索ワードは、「" ++ query ++ "」です。") maybeQuery
                        |> Maybe.withDefault "検索ワードはありません。"
                    )
                , ul []
                    [ viewLink "/foo"
                    , viewLink "/user/1234"
                    , viewLink "/search?query=word"
                    ]
                ]
            }

        NotFound ->
            { title = "NotFound"
            , body =
                [ text "おしゃれな NotFound"
                , a [ href "/home" ] [ text "ホーム画面へ" ]
                ]
            }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]
