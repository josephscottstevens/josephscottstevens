module Piece exposing (Orientation(..), Piece(..), Shape(..), eastJShape, eastLShape, eastTShape, getBlocks, getColor, getLeftOffset, getOffset, getRight, getRightOffset, getShape, getShapeById, horizontalIShape, horizontalSShape, horizontalZShape, isVertical, northJShape, northLShape, northTShape, oShape, pieceGenerator, rotate, southJShape, southLShape, southTShape, startingPiece, verticalIShape, verticalSShape, verticalZShape, westJShape, westLShape, westTShape)

import Random


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
    | YShape


type Piece
    = Piece Shape Orientation


startingPiece : Piece
startingPiece =
    Piece OShape West


rotate : Piece -> Piece
rotate (Piece shape orient) =
    case orient of
        North ->
            Piece shape East

        East ->
            Piece shape South

        South ->
            Piece shape West

        West ->
            Piece shape North


isPresent : Int -> Int -> Int -> List ( Int, Int )
isPresent x y present =
    if present == 1 then
        [ ( x, y ) ]

    else
        []


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
                            isPresent x y present
                        )
                        row
                )
                shape


getRight : Piece -> Maybe Int
getRight piece =
    piece
        |> getBlocks
        |> List.map Tuple.first
        |> List.maximum
        |> Maybe.map (\t -> t + 1)


getLeftOffset : Piece -> Int
getLeftOffset piece =
    getShape piece |> getOffset


getRightOffset : Piece -> Int
getRightOffset piece =
    getShape piece |> List.map (\t -> List.reverse t) |> getOffset


getOffset : List (List Int) -> Int
getOffset list =
    list
        |> List.map (\t -> List.indexedMap Tuple.pair t)
        |> List.map (\t -> List.filter (\( _, b ) -> b == 1) t)
        |> List.map (\t -> List.map Tuple.first t)
        |> List.map List.head
        |> List.filterMap identity
        |> List.minimum
        |> Maybe.withDefault 0


getColor : Piece -> String
getColor piece =
    case piece of
        Piece IShape _ ->
            "rgb(0, 240, 240)"

        Piece JShape _ ->
            "rgb(240, 160, 0)"

        Piece LShape _ ->
            "rgb(0, 0, 240)"

        Piece OShape _ ->
            "rgb(240, 240, 0)"

        Piece SShape _ ->
            "rgb(0, 240, 0)"

        Piece TShape _ ->
            "rgb(160, 0, 240)"

        Piece ZShape _ ->
            "rgb(240, 0, 0)"


getShape : Piece -> List (List Int)
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


verticalIShape : List (List number)
verticalIShape =
    [ [ 0, 1, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    ]


horizontalIShape : List (List number)
horizontalIShape =
    [ [ 1, 1, 1, 1 ]
    , [ 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


northLShape : List (List number)
northLShape =
    [ [ 0, 0, 0, 0 ]
    , [ 1, 0, 0, 0 ]
    , [ 1, 0, 0, 0 ]
    , [ 1, 1, 0, 0 ]
    ]


eastLShape : List (List number)
eastLShape =
    [ [ 0, 0, 0, 0 ]
    , [ 0, 0, 1, 0 ]
    , [ 1, 1, 1, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


southLShape : List (List number)
southLShape =
    [ [ 0, 0, 0, 0 ]
    , [ 1, 1, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    ]


westLShape : List (List number)
westLShape =
    [ [ 0, 0, 0, 0 ]
    , [ 1, 1, 1, 0 ]
    , [ 1, 0, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


northJShape : List (List number)
northJShape =
    [ [ 0, 0, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    , [ 1, 1, 0, 0 ]
    ]


eastJShape : List (List number)
eastJShape =
    [ [ 0, 0, 0, 0 ]
    , [ 1, 0, 0, 0 ]
    , [ 1, 1, 1, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


southJShape : List (List number)
southJShape =
    [ [ 0, 0, 0, 0 ]
    , [ 1, 1, 0, 0 ]
    , [ 1, 0, 0, 0 ]
    , [ 1, 0, 0, 0 ]
    ]


westJShape : List (List number)
westJShape =
    [ [ 0, 0, 0, 0 ]
    , [ 1, 1, 1, 0 ]
    , [ 0, 0, 1, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


oShape : List (List number)
oShape =
    [ [ 0, 0, 0, 0 ]
    , [ 0, 1, 1, 0 ]
    , [ 0, 1, 1, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


verticalSShape : List (List number)
verticalSShape =
    [ [ 0, 0, 0, 0 ]
    , [ 0, 1, 1, 0 ]
    , [ 1, 1, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


horizontalSShape : List (List number)
horizontalSShape =
    [ [ 0, 0, 0, 0 ]
    , [ 1, 0, 0, 0 ]
    , [ 1, 1, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    ]


northTShape : List (List number)
northTShape =
    [ [ 0, 0, 0, 0 ]
    , [ 1, 1, 1, 0 ]
    , [ 0, 1, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


eastTShape : List (List number)
eastTShape =
    [ [ 0, 1, 0, 0 ]
    , [ 0, 1, 1, 0 ]
    , [ 0, 1, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


southTShape : List (List number)
southTShape =
    [ [ 0, 1, 0, 0 ]
    , [ 1, 1, 1, 0 ]
    , [ 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


westTShape : List (List number)
westTShape =
    [ [ 0, 1, 0, 0 ]
    , [ 1, 1, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


verticalZShape : List (List number)
verticalZShape =
    [ [ 0, 0, 0, 0 ]
    , [ 1, 1, 0, 0 ]
    , [ 0, 1, 1, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


horizontalZShape : List (List number)
horizontalZShape =
    [ [ 0, 0, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    , [ 1, 1, 0, 0 ]
    , [ 1, 0, 0, 0 ]
    ]



-- generator stuff


pieceGenerator : Random.Generator Piece
pieceGenerator =
    Random.map getShapeById (Random.int 0 6)


getShapeById : Int -> Piece
getShapeById int =
    let
        newShape =
            case int of
                0 ->
                    IShape

                1 ->
                    JShape

                2 ->
                    LShape

                3 ->
                    OShape

                4 ->
                    SShape

                5 ->
                    TShape

                _ ->
                    ZShape
    in
    Piece newShape North
