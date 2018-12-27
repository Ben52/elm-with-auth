port module Main exposing (Model, init, main, update, view)

import Browser
import Html exposing (Html, blockquote, button, div, h1, h2, h3, img, input, label, p, text)
import Html.Attributes exposing (class, classList, for, id, src, type_)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..), Expect, Response(..), expectJson, expectString, expectStringResponse)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



---- MODEL ----


type alias Model =
    { username : String, password : String, token : String, quote : String, protectedQuote : String, errorMsg : String }


init : Maybe Model -> ( Model, Cmd Msg )
init m =
    case m of
        Just model ->
            ( model, fetchRandomQuote )

        Nothing ->
            ( Model "" "" "" "" "" "", fetchRandomQuote )


port setStorage : Model -> Cmd msg


port removeStorage : Model -> Cmd msg


setStorageHelper : Model -> ( Model, Cmd Msg )
setStorageHelper model =
    ( model, setStorage model )


type Msg
    = GetQuote
    | FetchRandomQuoteCompleted (Result Http.Error String)
    | GetProtectedQuote
    | FetchProtectedQuoteCompleted (Result Http.Error String)
    | SetUsername String
    | SetPassword String
    | ClickRegisterUser
    | GotTokenCompleted (Result String String)
    | ClickLogin
    | LogOut



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetQuote ->
            ( { model | quote = model.quote ++ "A quote! " }, fetchRandomQuote )

        GetProtectedQuote ->
            ( model, fetchProtectedQuote model )

        FetchRandomQuoteCompleted result ->
            fetchRandomQuoteCompleted model result

        FetchProtectedQuoteCompleted result ->
            fetchProtectedQuoteCompleted model result

        SetUsername username ->
            ( { model | username = username }, Cmd.none )

        SetPassword password ->
            ( { model | password = password }, Cmd.none )

        ClickRegisterUser ->
            ( model, authUser model registerUrl )

        GotTokenCompleted result ->
            getTokenCompleted model result

        ClickLogin ->
            ( model, authUser model loginUrl )

        LogOut ->
            ( { model | username = "", token = "" }, removeStorage model )



---- VIEW ----


loggedIn : Model -> Bool
loggedIn model =
    String.length model.token > 0


authBoxView : Model -> Html Msg
authBoxView model =
    case loggedIn model of
        True ->
            div [ id "greeting" ]
                [ h3 [ class "text-center" ] [ text ("Hello, " ++ model.username ++ "!") ]
                , p [ class "text-center" ] [ text "You have super-secret access to protected quotes." ]
                , p [ class "text-center" ]
                    [ button [ class "btn btn-danger", onClick LogOut ] [ text "Log Out" ]
                    ]
                ]

        False ->
            div [ id "form" ]
                [ h2 [ class "text-center" ] [ text "Log In or Register" ]
                , p [ class "help-block text-center" ] [ text "If you already have an account, please Log In. Otherwise, enter your desired username and password and Register." ]
                , div [ classList [ ( "hidden d-none", String.isEmpty model.errorMsg ) ] ]
                    [ div [ class "alert alert-danger" ] [ text model.errorMsg ]
                    ]
                , div [ class "form-group row d-flex justify-content-center" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                        [ label [ for "username" ] [ text "Username:" ]
                        , input [ id "username", type_ "text", class "form-control", Html.Attributes.value model.username, onInput SetUsername ] []
                        ]
                    ]
                , div [ class "form-group row d-flex justify-content-center" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                        [ label [ for "password" ] [ text "Password:" ]
                        , input [ id "password", type_ "password", class "form-control", Html.Attributes.value model.password, onInput SetPassword ] []
                        ]
                    ]
                , div [ class "text-center" ]
                    [ button [ class "btn btn-primary", onClick ClickLogin ] [ text "Log In" ]
                    , button [ class "btn btn-link", onClick ClickRegisterUser ] [ text "Register" ]
                    ]
                ]


protectedQuoteView : Model -> Html Msg
protectedQuoteView model =
    if loggedIn model then
        div []
            [ p [ class "text-center" ]
                [ button [ class "btn btn-info", onClick GetProtectedQuote ] [ text "Grab a protected quote!" ]
                ]

            -- Blockquote with protected quote: only show if a protectedQuote is present in model
            , blockquote [ classList [ ( "hidden d-none", String.isEmpty model.protectedQuote ) ] ]
                [ p [] [ text model.protectedQuote ]
                ]
            ]

    else
        p [ class "text-center" ] [ text "Please log in or register to see protected quotes." ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h2 [ class "text-center" ] [ text "Chuck Norris Quotes" ]
        , p [ class "text-center" ]
            [ button [ class "btn btn-success", onClick GetQuote ] [ text "Grab a quote!" ]
            ]

        -- Blockquote with quote
        , blockquote []
            [ p [] [ text model.quote ]
            ]
        , div [ class "jumbotron text-left" ]
            [ authBoxView model ]
        , div []
            [ h2 [ class "text-center" ] [ text "Protected Chuck Norris Quotes" ]
            , protectedQuoteView model
            ]
        ]


api : String
api =
    "https://nodejs-jwt-authentication-sample.now.sh/"


registerUrl : String
registerUrl =
    api ++ "users"


loginUrl : String
loginUrl =
    api ++ "sessions/create"


randomQuoteUrl : String
randomQuoteUrl =
    api ++ "api/random-quote"


protectedQuoteUrl : String
protectedQuoteUrl =
    api ++ "api/protected/random-quote"


fetchRandomQuote : Cmd Msg
fetchRandomQuote =
    Http.get { url = randomQuoteUrl, expect = expectString FetchRandomQuoteCompleted }


fetchProtectedQuote : Model -> Cmd Msg
fetchProtectedQuote model =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ model.token) ]
        , url = protectedQuoteUrl
        , body = Http.emptyBody
        , expect = Http.expectString FetchProtectedQuoteCompleted
        , timeout = Nothing
        , tracker = Nothing
        }


fetchRandomQuoteCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
fetchRandomQuoteCompleted model result =
    case result of
        Ok newQuote ->
            { model | quote = newQuote } |> setStorageHelper

        Err _ ->
            ( model, Cmd.none )


fetchProtectedQuoteCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
fetchProtectedQuoteCompleted model result =
    case result of
        Ok newQuote ->
            { model | protectedQuote = newQuote } |> setStorageHelper

        Err _ ->
            ( model, Cmd.none )


userEncoder : Model -> Encode.Value
userEncoder model =
    Encode.object [ ( "username", Encode.string model.username ), ( "password", Encode.string model.password ) ]


tokenDecoder : Decoder String
tokenDecoder =
    Decode.field "access_token" Decode.string


authUser : Model -> String -> Cmd Msg
authUser model apiUrl =
    Http.post
        { url = apiUrl
        , body = Http.jsonBody (userEncoder model)
        , expect = expectJson GotTokenCompleted tokenDecoder
        }


expectJson : (Result String String -> Msg) -> Decode.Decoder String -> Expect Msg
expectJson toMsg decoder =
    expectStringResponse toMsg (parseStringResponse decoder)


parseStringResponse : Decoder String -> Http.Response String -> Result String String
parseStringResponse decoder response =
    case response of
        BadUrl_ msg ->
            Err msg

        Timeout_ ->
            Err "Timeout"

        NetworkError_ ->
            Err "Network err"

        BadStatus_ metadata body ->
            Err body

        GoodStatus_ metadata body ->
            Decode.decodeString decoder body |> Result.mapError Decode.errorToString


getTokenCompleted : Model -> Result String String -> ( Model, Cmd Msg )
getTokenCompleted model result =
    case result of
        Ok newToken ->
            { model | token = newToken, password = "", errorMsg = "" } |> setStorageHelper

        Err error ->
            ( { model | errorMsg = error }, Cmd.none )



---- PROGRAM ----


main : Program (Maybe Model) Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
