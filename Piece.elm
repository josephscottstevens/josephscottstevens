module Piece exposing (..)


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


getLeftOffset : Piece -> Int
getLeftOffset piece =
    getShape piece
        |> List.map (\t -> List.indexedMap (,) t)
        |> List.map (\t -> List.filter (\( a, b ) -> b == 1) t)
        |> List.map (\t -> List.map Tuple.first t)
        |> List.map List.head
        |> List.filterMap identity
        |> List.minimum
        |> Maybe.withDefault 0


getRightOffset : Piece -> Int
getRightOffset piece =
    getShape piece
        |> List.map (\t -> List.reverse t)
        |> List.map (\t -> List.indexedMap (,) t)
        |> List.map (\t -> List.filter (\( a, b ) -> b == 1) t)
        |> List.map (\t -> List.map Tuple.first t)
        |> List.map List.head
        |> List.filterMap identity
        |> List.minimum
        |> Maybe.withDefault 0


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
                            if present == 1 then
                                [ ( x, y ) ]
                            else
                                []
                        )
                        row
                )
                shape
