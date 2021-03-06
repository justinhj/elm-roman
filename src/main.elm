module Main exposing (Model, Msg(..), displayDecimal, getInt, init, main, update, view)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RomanConvert exposing (decimalToRoman, romanNumeralsToDecimal)



-- Convert between roman numerals and decimals


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { decimal : Int
    , roman : String
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( { decimal = 0
      , roman = ""
      }
    , Cmd.none
    )



-- If the decimal is non-zero return empty string otherwise return the decimal as a string


displayDecimal : Int -> String
displayDecimal d =
    case d of
        0 ->
            ""

        n ->
            String.fromInt n



-- Same as above for Roman Numerals


displayRoman : String -> String
displayRoman r =
    case r of
        "0" ->
            ""

        n ->
            n



-- UPDATE


type Msg
    = ChangeRoman String
    | ChangeDecimal String


getInt : String -> Int
getInt s =
    String.toInt s |> Maybe.withDefault 0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg updatedModel =
    case msg of
        ChangeRoman updated ->
            let
                updatedToUpper =
                    String.toUpper updated

                newDecimal =
                    romanNumeralsToDecimal updatedToUpper

                newRoman =
                    decimalToRoman newDecimal
            in
            ( { updatedModel | roman = newRoman, decimal = newDecimal }, Cmd.none )

        ChangeDecimal new ->
            ( { updatedModel | decimal = getInt new, roman = decimalToRoman (getInt new) }, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view updatedModel =
    { title = "Roman Numeral Converter in Elm"
    , body =
        [ div [ class "container", attribute "style" "margin-top: 20px;" ]
            [ h5 [ attribute "style" "color: gray;" ] [ text "Roman numeral to decimal converter" ]
            , div [ class "form-inline" ]
                [ span [ class "border" ]
                    [ div [ class "form-group", attribute "style" "margin-left: 40px; margin-right: 40px; margin-top: 20px; margin-bottom: 20px" ]
                        [ div []
                            [ input [ placeholder "Enter Roman numeral", onInput ChangeRoman, value (displayRoman updatedModel.roman) ] []
                            , br [] []
                            , label [ for "romaninput", class "text-left" ]
                                [ text "Roman Numeral" ]
                            ]
                        , div [ attribute "style" "margin-left: 10px; margin-right: 10px;" ]
                            [ text "=" ]
                        , div []
                            [ input [ placeholder "Enter number", onInput ChangeDecimal, value (displayDecimal updatedModel.decimal) ] []
                            , br [] []
                            , label [ for "decimalinput" ]
                                [ text "Decimal" ]
                            ]
                        ]
                    ]
                ]
            , small [ class "container", attribute "style" "color: gray;" ]
                [ text "See the source code on github "
                , a [ href "http://github.com/justinhj/elm-roman" ]
                    [ text "github.com/justinhj/elm-roman" ]
                ]
            ]
        ]
    }
