module PigLatin exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, style)
import Html.Events exposing (onInput, onClick)


type Msg
    = ChangeInput String
    | Translate


type alias Model =
    { value : Maybe String
    , translation : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeInput newValue ->
            { model | value = Just newValue } ! []

        Translate ->
            { model | translation = toPigLatin model.value } ! []


vowels : List String
vowels =
    [ "a", "e", "i", "o", "u" ]


translateWord : String -> String
translateWord value =
    let
        firstLetter =
            String.left 1 value
    in
        case List.member firstLetter vowels of
            True ->
                value ++ "ay"

            False ->
                translateWord ((String.dropLeft 1 value) ++ firstLetter)


translateSentence : String -> String
translateSentence =
    String.words
        >> List.map translateWord
        >> String.join " "


toPigLatin : Maybe String -> String
toPigLatin maybeValue =
    case maybeValue of
        Nothing ->
            ""

        Just value ->
            value
                |> translateSentence


view : Model -> Html Msg
view model =
    div [ style [ ( "margin", "50px" ) ] ]
        [ input [ placeholder "Translate...", onInput ChangeInput ] []
        , p [] [ text model.translation ]
        , button [ onClick Translate ] [ text "translate" ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = { value = Nothing, translation = "" } ! []
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }
