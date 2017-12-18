module Piece exposing (Orientation(..), Piece(..), Shape(..), blockSize, getBlocks, getLeftOffset, getRightOffset, rotate)


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
    10


verticalIShape =
    [ [ 0, 1, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    ]


horizontalIShape =
    [ [ 1, 1, 1, 1 ]
    , [ 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


northLShape =
    [ [ 0, 0, 0, 0 ]
    , [ 1, 0, 0, 0 ]
    , [ 1, 0, 0, 0 ]
    , [ 1, 1, 0, 0 ]
    ]


eastLShape =
    [ [ 0, 0, 0, 0 ]
    , [ 0, 0, 1, 0 ]
    , [ 1, 1, 1, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


southLShape =
    [ [ 0, 0, 0, 0 ]
    , [ 1, 1, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    ]


westLShape =
    [ [ 0, 0, 0, 0 ]
    , [ 1, 1, 1, 0 ]
    , [ 1, 0, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


northJShape =
    [ [ 0, 0, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    , [ 1, 1, 0, 0 ]
    ]


eastJShape =
    [ [ 0, 0, 0, 0 ]
    , [ 1, 0, 0, 0 ]
    , [ 1, 1, 1, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


southJShape =
    [ [ 0, 0, 0, 0 ]
    , [ 1, 1, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    ]


westJShape =
    [ [ 0, 0, 0, 0 ]
    , [ 1, 1, 1, 0 ]
    , [ 1, 0, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


oShape =
    [ [ 0, 0, 0, 0 ]
    , [ 0, 1, 1, 0 ]
    , [ 0, 1, 1, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


verticalSShape =
    [ [ 0, 0, 0, 0 ]
    , [ 0, 1, 1, 0 ]
    , [ 1, 1, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


horizontalSShape =
    [ [ 0, 0, 0, 0 ]
    , [ 1, 0, 0, 0 ]
    , [ 1, 1, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    ]


northTShape =
    [ [ 0, 0, 0, 0 ]
    , [ 1, 1, 1, 0 ]
    , [ 0, 1, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


eastTShape =
    [ [ 0, 1, 0, 0 ]
    , [ 0, 1, 1, 0 ]
    , [ 0, 1, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


southTShape =
    [ [ 0, 1, 0, 0 ]
    , [ 1, 1, 1, 0 ]
    , [ 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


westTShape =
    [ [ 0, 1, 0, 0 ]
    , [ 1, 1, 0, 0 ]
    , [ 0, 1, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


verticalZShape =
    [ [ 0, 0, 0, 0 ]
    , [ 1, 1, 0, 0 ]
    , [ 0, 1, 1, 0 ]
    , [ 0, 0, 0, 0 ]
    ]


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
    let
        count =
            getShape piece
                |> List.map List.head
                |> List.map (Maybe.withDefault 0)
                |> List.sum
    in
    if count > 0 then
        0
    else
        1


getRightOffset : Piece -> Int
getRightOffset piece =
    let
        count =
            getShape piece
                |> List.map List.reverse
                |> List.map List.head
                |> List.map (Maybe.withDefault 0)
                |> List.sum

        count2 =
            getShape piece
                |> List.map List.reverse
                |> List.drop 1
                |> List.map List.head
                |> List.map (Maybe.withDefault 0)
                |> List.sum
    in
    if count2 > 0 then
        2
    else if count > 0 then
        1
    else
        0


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
