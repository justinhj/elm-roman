module Main exposing (Model, Msg(..), displayDecimal, getInt, init, main, update, view)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RomanConvert exposing (decimalToRoman, romanNumeralsToDecimal)


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
    { title = "Roman title"
    , body =
        [ div [ class "container" ]
            [ h2 [] [ text "Roman Numeral Converters" ]
            , label [] [ text "Roman numeral" ]
            , input [ placeholder "Enter Roman numeral", onInput ChangeRoman, value updatedModel.roman ] []
            , label [] [ text "Decimal number" ]
            , input [ placeholder "Enter number", onInput ChangeDecimal, value (displayDecimal updatedModel.decimal) ] []
            , div [ class "container" ]
                [ text "See the source code on github "
                , a [ href "http://github.com/justinhj/cljs-roman" ]
                    [ text "github.com/justinhj/cljs-roman" ]
                ]
            ]
        ]
    }
