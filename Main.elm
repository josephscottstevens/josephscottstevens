module Main exposing (main)

import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Keyboard
import Random
import Random.Extra
import RasterShapes as Raster exposing (Position, Size)
import Set exposing (Set)
import Time


numCols : Int
numCols =
    10


numRows : Int
numRows =
    20


fromJust : Maybe a -> a
fromJust maybe =
    case maybe of
        Just value ->
            value

        Nothing ->
            Debug.crash "fromJust given Nothing"


pieceGenerator : Random.Generator Piece
pieceGenerator =
    Random.map (flip Piece North << fromJust) <| Random.Extra.sample [ IShape, JShape, LShape, OShape, SShape, TShape, ZShape ]



-- XXX current speed


type alias State =
    { currentScore : Int
    , currentPiece : Piece
    , currentPiecePosition : ( Int, Int )
    , nextPiece : Piece
    , fixatedBlocks : Set ( Int, Int )
    , dropping : Bool
    }


type Model
    = Uninitialized
    | Initialized State
    | GameOver Int
    | Error String


init : ( Model, Cmd Msg )
init =
    ( Uninitialized, Random.generate (uncurry Initialize) <| Random.pair pieceGenerator pieceGenerator )


type Msg
    = Initialize Piece Piece
    | NextPiece Piece
    | Tick
    | MoveLeft
    | MoveRight
    | Drop
    | StopDrop
    | Rotate
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        GameOver _ ->
            ( model, Cmd.none )

        Uninitialized ->
            case msg of
                Initialize currentPiece nextPiece ->
                    let
                        newModel =
                            Initialized
                                { currentScore = 0
                                , currentPiece = currentPiece
                                , currentPiecePosition = ( 0, 0 )
                                , nextPiece = nextPiece
                                , fixatedBlocks = Set.empty
                                , dropping = False
                                }
                    in
                    ( newModel, Cmd.none )

                _ ->
                    ( Error ("Somehow you managed to get a " ++ toString msg ++ " msg in an uninitialized state o_O"), Cmd.none )

        Initialized state ->
            case msg of
                Initialize _ _ ->
                    ( Error "Somehow you managed to get an initialize msg in an initialized state o_O", Cmd.none )

                NoOp ->
                    ( model, Cmd.none )

                MoveLeft ->
                    ( Initialized <| moveCurrentPieceLeft state, Cmd.none )

                MoveRight ->
                    ( Initialized <| moveCurrentPieceRight state, Cmd.none )

                Rotate ->
                    ( Initialized <| rotateCurrentPiece state, Cmd.none )

                Tick ->
                    let
                        newState =
                            movePieceDown state
                    in
                    if detectCollisions newState then
                        ( fixateAndAdvance state, Random.generate NextPiece pieceGenerator )
                    else
                        ( Initialized newState, Cmd.none )

                Drop ->
                    ( Initialized { state | dropping = True }, Cmd.none )

                StopDrop ->
                    ( Initialized { state | dropping = False }, Cmd.none )

                NextPiece piece ->
                    ( Initialized { state | nextPiece = piece }, Cmd.none )

        Error _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        GameOver score ->
            text <| "Game over! Your score was " ++ toString score

        Uninitialized ->
            text ""

        Initialized state ->
            div []
                [ renderOutline
                    |> pxSize 1
                , renderBoard state.currentPiece state.currentPiecePosition (Set.toList state.fixatedBlocks)
                    |> pxSize 20
                , renderNext state.nextPiece
                    |> pxSize 20
                , pixelWithItems 1 (Position 240 110) [ text (toString state.currentScore) ]
                ]

        Error error ->
            div [] [ text error ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


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


pxSize : Int -> List Position -> Html Msg
pxSize size items =
    items
        |> List.map (pixel size)
        |> div []


anyFixated : State -> Int -> Bool
anyFixated state offset =
    let
        blocks =
            getBlocks state.currentPiece

        ( cx, cy ) =
            state.currentPiecePosition

        newBlocks =
            List.map (\( x, y ) -> ( x + offset + cx, y + cy )) blocks

        blockIsFixated pos =
            Set.member pos state.fixatedBlocks
    in
    List.any blockIsFixated newBlocks


moveCurrentPieceLeft : State -> State
moveCurrentPieceLeft model =
    let
        ( x, y ) =
            model.currentPiecePosition

        left =
            getLeftOffset model.currentPiece
    in
    if x + left <= 0 then
        model
    else if anyFixated model -1 then
        model
    else
        { model | currentPiecePosition = ( x - 1, y ) }


moveCurrentPieceRight : State -> State
moveCurrentPieceRight model =
    let
        ( x, y ) =
            model.currentPiecePosition

        right =
            getRightOffset model.currentPiece
    in
    if x + 4 - right >= numCols then
        model
    else if anyFixated model 1 then
        model
    else
        { model | currentPiecePosition = ( x + 1, y ) }


rotateCurrentPiece : State -> State
rotateCurrentPiece model =
    let
        ( x, y ) =
            model.currentPiecePosition

        newPiece =
            rotate model.currentPiece

        right =
            getRightOffset newPiece
    in
    if x + right > numCols then
        { model | currentPiece = newPiece, currentPiecePosition = ( numCols - right, y ) }
    else
        { model | currentPiece = newPiece }


movePieceDown : State -> State
movePieceDown state =
    let
        ( x, y ) =
            state.currentPiecePosition
    in
    { state | currentPiecePosition = ( x, y + 1 ) }


translateRelativeTo : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
translateRelativeTo ( dx, dy ) ( x, y ) =
    ( dx + x, dy + y )


detectCollisions : State -> Bool
detectCollisions state =
    let
        pieceBlocks =
            List.map (translateRelativeTo state.currentPiecePosition) <| getBlocks state.currentPiece
    in
    List.any (\( _, y ) -> y >= numRows) pieceBlocks || List.any (\point -> Set.member point state.fixatedBlocks) pieceBlocks


fixate : State -> State
fixate state =
    let
        pieceBlocks =
            List.map (translateRelativeTo state.currentPiecePosition) <| getBlocks state.currentPiece
    in
    { state | fixatedBlocks = Set.union state.fixatedBlocks <| Set.fromList pieceBlocks }


countBlocksByRow : List ( Int, Int ) -> List ( Int, Int )
countBlocksByRow blocks =
    let
        incrementCount ( _, row ) countDict =
            Dict.update row (Just << (+) 1 << Maybe.withDefault 0) countDict
    in
    Dict.toList <| List.foldl incrementCount Dict.empty blocks


checkForCompleteRows : State -> State
checkForCompleteRows state =
    let
        blockCounts =
            countBlocksByRow <| Set.toList state.fixatedBlocks

        completeRows =
            Set.fromList <|
                List.filterMap
                    (\( row, count ) ->
                        if count == numCols then
                            Just row
                        else
                            Nothing
                    )
                    blockCounts

        maxCompletedRow =
            List.maximum <| Set.toList completeRows
    in
    case maxCompletedRow of
        Nothing ->
            state

        Just maxCompletedRow ->
            let
                completedRowsRemoved =
                    List.filter (\( _, row ) -> not <| Set.member row completeRows) <| Set.toList state.fixatedBlocks

                shiftDown ( x, row ) =
                    if row < maxCompletedRow then
                        ( x, row + Set.size completeRows )
                    else
                        ( x, row )

                shiftedRows =
                    List.map shiftDown completedRowsRemoved
            in
            { state | fixatedBlocks = Set.fromList <| shiftedRows, currentScore = state.currentScore + 100 * Set.size completeRows }


advance : State -> State
advance state =
    { state | currentPiece = state.nextPiece, currentPiecePosition = ( 0, 0 ) }


checkGameOver : State -> Model
checkGameOver state =
    if detectCollisions state then
        GameOver state.currentScore
    else
        Initialized state


fixateAndAdvance : State -> Model
fixateAndAdvance state =
    checkGameOver <| advance <| checkForCompleteRows <| fixate <| state


translateKeyDown : Keyboard.KeyCode -> Msg
translateKeyDown keycode =
    case keycode of
        38 ->
            Rotate

        40 ->
            Drop

        37 ->
            MoveLeft

        39 ->
            MoveRight

        _ ->
            NoOp


translateKeyUp : Keyboard.KeyCode -> Msg
translateKeyUp keycode =
    if keycode == 40 then
        StopDrop
    else
        NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Uninitialized ->
            Sub.none

        GameOver _ ->
            Sub.none

        Initialized state ->
            let
                tickInterval =
                    if state.dropping then
                        Time.second / 20
                    else
                        Time.second
            in
            Sub.batch
                [ Keyboard.downs translateKeyDown
                , Keyboard.ups translateKeyUp
                , Time.every tickInterval <| always Tick
                ]

        Error _ ->
            Sub.none


px : Int -> String
px i =
    toString i ++ "px"


pixel : Int -> Position -> Html msg
pixel size position =
    pixelWithItems size position []


pixelWithItems : Int -> Position -> List (Html msg) -> Html msg
pixelWithItems size { x, y } t =
    div
        [ style
            [ ( "background", "#000000" )
            , ( "width", toString size ++ "px" )
            , ( "height", toString size ++ "px" )
            , ( "top", px (y * size) )
            , ( "left", px (x * size) )
            , ( "position", "absolute" )
            ]
        ]
        t


renderOutline : List Position
renderOutline =
    let
        boardOutline =
            Raster.rectangle (Size 200 400) (Position 20 20)

        nextPieceOutline =
            Raster.rectangle (Size 80 80) (Position 240 20)
    in
    boardOutline ++ nextPieceOutline


renderBoard : Piece -> ( Int, Int ) -> List ( Int, Int ) -> List Position
renderBoard currentPiece ( curX, curY ) fixatedBlocks =
    let
        currentBlock =
            getBlocks currentPiece
                |> List.map (\( x, y ) -> Position (x + curX + 1) (y + curY + 1))

        blocks =
            fixatedBlocks
                |> List.map (\( x, y ) -> Position (x + 1) (y + 1))
    in
    currentBlock ++ blocks


renderNext : Piece -> List Position
renderNext nextPiece =
    getBlocks nextPiece
        |> List.map (\( x, y ) -> Position (x + 12) (y + 1))


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
        |> List.map (\t -> List.filter (\( _, b ) -> b == 1) t)
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
        |> List.map (\t -> List.filter (\( _, b ) -> b == 1) t)
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
