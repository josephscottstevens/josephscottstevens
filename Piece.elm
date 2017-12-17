module Piece exposing (Orientation(..), Piece(..), Shape(..), blockSize, getBlocks, getWidth, render, rotate)

import Collage exposing (Form, collage, filled, group, move, moveX, moveY, square)
import Color exposing (black)


type Orientation
    = North
    | South
    | East
    | West


type Shape
    = IShape
    | JShape
    | LShape
    | OShape
    | SShape
    | TShape
    | ZShape


type Piece
    = Piece Shape Orientation


blockSize =
    20


drawShape : List (List Bool) -> Form
drawShape rows =
    let
        yOffsets =
            List.map (\x -> toFloat x * -blockSize) <| List.range 0 2

        xOffsets =
            List.map (\x -> toFloat x * blockSize) <| List.range 0 2

        drawRow row =
            List.concat <|
                List.map2
                    (\offset value ->
                        if value then
                            [ moveX offset <| filled black <| square blockSize ]
                        else
                            []
                    )
                    xOffsets
                    row
    in
    group <| List.concat <| List.map2 (\row offset -> List.map (moveY <| offset) <| drawRow row) rows yOffsets


verticalIShape =
    [ [ True, False, False ]
    , [ True, False, False ]
    , [ True, False, False ]
    ]


horizontalIShape =
    [ [ True, True, True ]
    , [ False, False, False ]
    , [ False, False, False ]
    ]


northLShape =
    [ [ True, False, False ]
    , [ True, False, False ]
    , [ True, True, False ]
    ]


eastLShape =
    [ [ False, False, True ]
    , [ True, True, True ]
    , [ False, False, False ]
    ]


southLShape =
    [ [ True, True, False ]
    , [ False, True, False ]
    , [ False, True, False ]
    ]


westLShape =
    [ [ True, True, True ]
    , [ True, False, False ]
    , [ False, False, False ]
    ]


northJShape =
    [ [ False, True, False ]
    , [ False, True, False ]
    , [ True, True, False ]
    ]


eastJShape =
    [ [ True, False, False ]
    , [ True, True, True ]
    , [ False, False, False ]
    ]


southJShape =
    [ [ True, True, False ]
    , [ False, True, False ]
    , [ False, True, False ]
    ]


westJShape =
    [ [ True, True, True ]
    , [ True, False, False ]
    , [ False, False, False ]
    ]


oShape =
    [ [ True, True, False ]
    , [ True, True, False ]
    , [ False, False, False ]
    ]


verticalSShape =
    [ [ False, True, True ]
    , [ True, True, False ]
    , [ False, False, False ]
    ]


horizontalSShape =
    [ [ True, False, False ]
    , [ True, True, False ]
    , [ False, True, False ]
    ]


northTShape =
    [ [ True, True, True ]
    , [ False, True, False ]
    , [ False, False, False ]
    ]


eastTShape =
    [ [ True, False, False ]
    , [ True, True, False ]
    , [ True, False, False ]
    ]


southTShape =
    [ [ False, True, False ]
    , [ True, True, True ]
    , [ False, False, False ]
    ]


westTShape =
    [ [ False, True, False ]
    , [ True, True, False ]
    , [ False, True, False ]
    ]


verticalZShape =
    [ [ True, True, False ]
    , [ False, True, True ]
    , [ False, False, False ]
    ]


horizontalZShape =
    [ [ False, True, False ]
    , [ True, True, False ]
    , [ True, False, False ]
    ]


isVertical : Orientation -> Bool
isVertical orient =
    case orient of
        North ->
            True

        South ->
            True

        East ->
            False

        West ->
            False


getShape : Piece -> List (List Bool)
getShape (Piece shape orientation) =
    case shape of
        IShape ->
            if isVertical orientation then
                verticalIShape
            else
                horizontalIShape

        JShape ->
            case orientation of
                North ->
                    northJShape

                South ->
                    southJShape

                East ->
                    eastJShape

                West ->
                    westJShape

        LShape ->
            case orientation of
                North ->
                    northLShape

                South ->
                    southLShape

                East ->
                    eastLShape

                West ->
                    westLShape

        OShape ->
            oShape

        SShape ->
            if isVertical orientation then
                verticalSShape
            else
                horizontalSShape

        TShape ->
            case orientation of
                North ->
                    northTShape

                South ->
                    southTShape

                East ->
                    eastTShape

                West ->
                    westTShape

        ZShape ->
            if isVertical orientation then
                verticalZShape
            else
                horizontalZShape


render : Piece -> Form
render piece =
    drawShape <| getShape piece


getWidth : Piece -> Int
getWidth (Piece shape orientation) =
    case shape of
        IShape ->
            if isVertical orientation then
                1
            else
                3

        JShape ->
            if isVertical orientation then
                2
            else
                3

        LShape ->
            if isVertical orientation then
                2
            else
                3

        OShape ->
            2

        SShape ->
            if isVertical orientation then
                3
            else
                2

        TShape ->
            if isVertical orientation then
                3
            else
                2

        ZShape ->
            if isVertical orientation then
                3
            else
                2


rotate : Piece -> Piece
rotate (Piece shape orient) =
    Debug.log "rotation" <|
        case orient of
            North ->
                Piece shape East

            East ->
                Piece shape South

            South ->
                Piece shape West

            West ->
                Piece shape North


getBlocks : Piece -> List ( Int, Int )
getBlocks piece =
    let
        shape =
            getShape piece
    in
    List.concat <|
        List.concat <|
            List.indexedMap
                (\y row ->
                    List.indexedMap
                        (\x present ->
                            if present then
                                [ ( x, y ) ]
                            else
                                []
                        )
                        row
                )
                shape
