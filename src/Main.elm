module Main exposing (main)

import Browser
import Dict
import Draw
import Html exposing (Html, div, text)
import Html.Events as Events
import Json.Decode as Json
import Piece exposing (Piece)
import Random
import Set exposing (Set)
import State exposing (State, numCols, numRows)
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = Uninitialized
    | Initialized State
    | GameOver Int
    | Error String


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


init _ =
    ( Uninitialized, Random.generate (uncurry Initialize) <| Random.pair Piece.pieceGenerator Piece.pieceGenerator )


type Msg
    = Tick
    | Initialize Piece Piece
    | NextPiece Piece
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
                    ( Error "Somehow you managed to get a  msg in an uninitialized state o_O", Cmd.none )

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
                        ( fixateAndAdvance state, Random.generate NextPiece Piece.pieceGenerator )

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
    div
        [ Events.on "keyup" (Json.map translateKeyUp Events.keyCode)
        , Events.on "keydown" (Json.map translateKeyDown Events.keyCode)
        ]
        [ case model of
            GameOver score ->
                text <| "Game over! Your score was " ++ String.fromInt score

            Uninitialized ->
                text ""

            Initialized state ->
                Draw.game state

            Error error ->
                div [] [ text error ]
        ]


anyFixated : State -> Int -> Bool
anyFixated state offset =
    let
        blocks =
            Piece.getBlocks state.currentPiece

        ( cx, cy ) =
            state.currentPiecePosition

        newBlocks : List ( Int, Int )
        newBlocks =
            List.map (\( x, y ) -> ( x + offset + cx, y + cy )) blocks

        fixatedBlocksXY : Set ( Int, Int )
        fixatedBlocksXY =
            state.fixatedBlocks
                |> Set.map (\( x, y, _ ) -> ( x, y ))

        blockIsFixated pos =
            Set.member pos fixatedBlocksXY
    in
    List.any blockIsFixated newBlocks


moveCurrentPieceLeft : State -> State
moveCurrentPieceLeft model =
    let
        ( x, y ) =
            model.currentPiecePosition

        left =
            Piece.getLeftOffset model.currentPiece
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
            Piece.getRightOffset model.currentPiece
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
            Piece.rotate model.currentPiece
    in
    case Piece.getRight newPiece of
        Just right ->
            if x + right > numCols then
                { model | currentPiece = newPiece, currentPiecePosition = ( numCols - right, y ) }

            else
                { model | currentPiece = newPiece }

        Nothing ->
            Debug.todo "invalid right position!"


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
            List.map (translateRelativeTo state.currentPiecePosition) <| Piece.getBlocks state.currentPiece

        fixatedBlocksXY =
            state.fixatedBlocks
                |> Set.map (\( x, y, _ ) -> ( x, y ))
    in
    List.any (\( _, y ) -> y >= numRows) pieceBlocks || List.any (\point -> Set.member point fixatedBlocksXY) pieceBlocks


fixate : State -> State
fixate state =
    let
        pieceBlocks =
            state.currentPiece
                |> Piece.getBlocks
                |> List.map (translateRelativeTo state.currentPiecePosition)
                |> List.map (\( x, y ) -> ( x, y, Piece.getColor state.currentPiece ))
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
            state.fixatedBlocks
                |> Set.toList
                |> List.map (\( x, y, _ ) -> ( x, y ))
                |> countBlocksByRow

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
    in
    case List.maximum <| Set.toList completeRows of
        Nothing ->
            state

        Just maxCompletedRow ->
            let
                completedRowsRemoved : List ( Int, Int, String )
                completedRowsRemoved =
                    state.fixatedBlocks
                        |> Set.toList
                        |> List.filter (\( _, row, _ ) -> not <| Set.member row completeRows)

                shiftDown ( x, row, color ) =
                    if row < maxCompletedRow then
                        ( x, row + Set.size completeRows, color )

                    else
                        ( x, row, color )

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


translateKeyDown : Int -> Msg
translateKeyDown keycode =
    Debug.log ("key translate" ++ String.fromInt keycode) <|
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


translateKeyUp : Int -> Msg
translateKeyUp keycode =
    if keycode == 40 then
        StopDrop

    else
        NoOp


calcGameSpeed : State -> Float
calcGameSpeed state =
    1000.0 - (toFloat state.currentScore / 8.0)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Uninitialized ->
            Sub.none

        GameOver _ ->
            Sub.none

        Initialized state ->
            let
                msPerTick =
                    calcGameSpeed state
                        |> clamp 250.0 1000.0

                tickInterval =
                    if state.dropping then
                        msPerTick / 20

                    else
                        msPerTick
            in
            Sub.batch
                [ Time.every tickInterval <| always Tick
                ]

        Error _ ->
            Sub.none
