module Main exposing (..)

import Axis
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Scale exposing (ContinuousScale)
import TypedSvg exposing (circle, defs, g, linearGradient, stop, svg)
import TypedSvg.Attributes exposing (fill, offset, opacity, stopColor, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), CoordinateSystem(..), Length(..), Opacity(..), Paint(..), Transform(..))



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { coordinatesList : List ( Float, Float )
    , coordinatesString : String
    , horizontalScale : Float
    , verticalScale : Float
    , horizontalScaleString : String
    , verticalScaleString : String
    -- , horizontalFrame : Float
    -- , verticalFrame : Float
    }


initMaxHorizontal : Float
initMaxHorizontal =
    100.0


initMaxVertical : Float
initMaxVertical =
    100.0


init : Model
init =
    Model [ ( 0, 0 ), ( 50, 45 ), ( 31, 23 ) ]
        "0 0\n50 45\n31 23"
        initMaxHorizontal
        initMaxVertical
        ""
        ""



-- UPDATE


type Msg
    = UpdateCoordinates String
    | UpdateHorizontal String
    | UpdateVertical String


update : Msg -> Model -> Model
update msg model =
    case msg of
        -- StringをList ( Float, Float )にする
        UpdateCoordinates inputStr ->
            { model
                | coordinatesString = inputStr
                , coordinatesList =
                    List.map (\maybeFloatPair -> ( Maybe.withDefault 0.0 (Tuple.first maybeFloatPair), Maybe.withDefault 0.0 (Tuple.second maybeFloatPair) )) <|
                        List.map (\floats -> ( List.head floats, List.head (List.reverse floats) )) <|
                            List.map (\maybeFloats -> List.map (\maybeFloat -> Maybe.withDefault 0.0 maybeFloat) maybeFloats) <|
                                List.map (\strs -> List.map String.toFloat strs) <|
                                    List.map String.words <|
                                        String.lines model.coordinatesString
            }

        UpdateHorizontal maxHorizontalString ->
            { model
                | horizontalScale = Maybe.withDefault initMaxHorizontal <| String.toFloat maxHorizontalString
                , horizontalScaleString = maxHorizontalString
            }

        UpdateVertical maxVerticalString ->
            { model
                | verticalScale = Maybe.withDefault initMaxHorizontal <| String.toFloat maxVerticalString
                , verticalScaleString = maxVerticalString
            }



-- VIEW


textareaRow : Int
textareaRow =
    30


textareaCol : Int
textareaCol =
    30


view : Model -> Html Msg
view model =
    div []
        [ div [] [ viewTextarea textareaRow textareaCol "input coordinates here!" model.coordinatesString UpdateCoordinates ]
        , div [] [ viewInput "input max x-axis scale" model.horizontalScaleString UpdateHorizontal ]
        , div [] [ viewInput "input max y-axis scale" model.verticalScaleString UpdateVertical ]
        , viewValidation model
        , div [] [ plot model model.coordinatesList ]
        ]



--- Input


viewInput : String -> String -> (String -> msg) -> Html msg
viewInput ph v toMsg =
    input [ placeholder ph, value v, onInput toMsg ] []


viewTextarea : Int -> Int -> String -> String -> (String -> msg) -> Html msg
viewTextarea maxRow maxCol p v toMsg =
    textarea [ rows maxRow, cols maxCol, placeholder p, value v, onInput toMsg ] [ text "" ]



--- Validation


isNothing : Maybe a -> Bool
isNothing temp =
    case temp of
        Just _ ->
            False

        Nothing ->
            True


validMessage : String -> Html msg
validMessage message =
    div [ style "color" "green", style "font-size" "100px" ] [ text message ]


invalidMessage : String -> Html msg
invalidMessage message =
    div [ style "color" "red", style "font-size" "50px" ] [ text message ]


viewValidation : Model -> Html msg
viewValidation model =
    -- 数字以外
    if
        not (String.isEmpty model.coordinatesString)
            && (String.words model.coordinatesString
                    |> List.map String.toFloat
                    |> List.any isNothing
               )
    then
        invalidMessage "Please include only numbers!"

    else if
        not (String.isEmpty model.horizontalScaleString)
            && (String.toFloat model.horizontalScaleString
                    |> isNothing
               )
    then
        invalidMessage "Please include only numbers in horizontal!"

    else if
        not (String.isEmpty model.verticalScaleString)
            && (String.toFloat model.verticalScaleString
                    |> isNothing
               )
    then
        invalidMessage "Please include only numbers in vertical!"

        -- 1行当たりの数字は2つまで

    else if
        String.lines model.coordinatesString
            |> List.map String.words
            |> List.any (\strs -> List.length strs > 2)
    then
        invalidMessage "Too many numbers on a line!"

        -- 枠の長さが0か判別

    else if model.horizontalScale == 0 then
        invalidMessage "Do not set the x-axis length to 0!"

    else if model.verticalScale == 0 then
        invalidMessage "Do not set the y-axis length to 0!"

        -- 軸の長さより大きいか判別

    else if List.any (\floatPair -> Tuple.first floatPair > model.horizontalScale || Tuple.second floatPair > model.horizontalScale) model.coordinatesList then
        invalidMessage "Make it all point smaller than the x-axis length!"

    else if List.any (\floatPair -> Tuple.first floatPair > model.verticalScale || Tuple.second floatPair > model.verticalScale) model.coordinatesList then
        invalidMessage "Make it all point smaller than the y-axis length!"

    else
        validMessage "OK!"



--- Plot


w : Float
w =
    1000


h : Float
h =
    1000


padding : Float
padding =
    30


xScale : Model -> ContinuousScale Float
xScale model =
    Scale.linear ( 0, w - 2 * padding ) ( -model.horizontalScale, model.horizontalScale )


yScale : Model -> ContinuousScale Float
yScale model =
    Scale.linear ( h - 2 * padding, 0 ) ( -model.verticalScale, model.verticalScale )


xAxis : Model -> Svg msg
xAxis model =
    Axis.bottom [] (xScale model)


yAxis : Model -> Svg msg
yAxis model =
    Axis.left [ Axis.tickCount 5 ] (yScale model)


circles : Model -> List ( Float, Float ) -> List (Svg msg)
circles model dataPoints =
    List.map (pointCircle model) dataPoints


pointCircle : Model -> ( Float, Float ) -> Svg msg
pointCircle model ( dataX, dataY ) =
    g [ TypedSvg.Attributes.class [ "data-point" ] ]
        [ circle
            [ cx (Scale.convert (xScale model) dataX)
            , cy (Scale.convert (yScale model) dataY)
            , r 3
            , fill <| Reference "linGradientRed"
            , strokeWidth 0
            , stroke <| PaintNone
            , opacity <| Opacity 0.85
            ]
            []
        ]


myDefs : List (Svg msg)
myDefs =
    [ linearGradient
        [ TypedSvg.Attributes.id "linGradientRed"
        , TypedSvg.Attributes.x1 <| Percent 0.0
        , TypedSvg.Attributes.y1 <| Percent 0.0
        , TypedSvg.Attributes.x2 <| Percent 0.0
        , TypedSvg.Attributes.y2 <| Percent 100.0
        ]
        [ stop [ offset "0%", stopColor "#e52d27" ] []
        , stop [ offset "100%", stopColor "#b31217" ] []
        ]
    , linearGradient
        [ TypedSvg.Attributes.id "linGradientGray"
        , TypedSvg.Attributes.x1 <| Percent 0.0
        , TypedSvg.Attributes.y1 <| Percent 0.0
        , TypedSvg.Attributes.x2 <| Percent 0.0
        , TypedSvg.Attributes.y2 <| Percent 100.0
        ]
        [ stop [ offset "0%", stopColor "#5b6467" ] []
        , stop [ offset "74%", stopColor "#8b939a" ] []
        ]
    ]


plot : Model -> List ( Float, Float ) -> Svg msg
plot model dataPoints =
    svg [ viewBox 0 0 w h ]
        --
        [ defs [] myDefs
        , g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis model ]
        , g [ transform [ Translate padding padding ], TypedSvg.Attributes.class [ "series" ] ]
            (circles model dataPoints)
        ]
