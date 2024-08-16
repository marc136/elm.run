module AnsiExtra exposing
    ( Parsed
    , parse
    , view
    )

import Ansi.Color
import Ansi.Parser
import Data.Problem
import Elm.Error
import Html exposing (Html)


type alias Parsed =
    List Elm.Error.Chunk


parse : String -> Parsed
parse string =
    Ansi.Parser.parseInto { current = Nothing, reversedList = [] }
        parseAnsiInner
        string
        |> (\{ reversedList } -> List.reverse reversedList)


parseAnsiInner : Ansi.Parser.Command -> { a | current : Maybe Elm.Error.Style, reversedList : List Elm.Error.Chunk } -> { current : Maybe { bold : Bool, underline : Bool, color : Maybe Elm.Error.Color }, reversedList : List Elm.Error.Chunk }
parseAnsiInner cmd { current, reversedList } =
    case cmd of
        Ansi.Parser.Text string ->
            case current of
                Nothing ->
                    { current = Nothing, reversedList = Elm.Error.Unstyled string :: reversedList }

                Just style ->
                    { current = Nothing, reversedList = Elm.Error.Styled style string :: reversedList }

        -- Ansi.Parser.SetForeground Nothing ->
        --     case current of
        --         Nothing ->
        --             {current = Nothing, reversedList = reversedList }
        --         Just node ->
        --             {current = Nothing, reversedList = node :: reversedList }
        Ansi.Parser.SetForeground (Just color) ->
            { current =
                current
                    |> Maybe.withDefault { bold = False, underline = False, color = Nothing }
                    |> (\styled -> { styled | color = convertColor color })
                    |> Just
            , reversedList = reversedList
            }

        _ ->
            { current = Nothing, reversedList = reversedList }


convertColor : Ansi.Color.Color -> Maybe Elm.Error.Color
convertColor ansi =
    case ansi of
        Ansi.Color.Black ->
            Just Elm.Error.Black

        Ansi.Color.Red ->
            Just Elm.Error.Red

        Ansi.Color.Green ->
            Just Elm.Error.Green

        Ansi.Color.Yellow ->
            Just Elm.Error.Yellow

        Ansi.Color.Blue ->
            Just Elm.Error.Blue

        Ansi.Color.Magenta ->
            Just Elm.Error.Magenta

        Ansi.Color.Cyan ->
            Just Elm.Error.Cyan

        Ansi.Color.White ->
            Just Elm.Error.White

        Ansi.Color.BrightBlack ->
            Just Elm.Error.BLACK

        Ansi.Color.BrightRed ->
            Just Elm.Error.RED

        Ansi.Color.BrightGreen ->
            Just Elm.Error.GREEN

        Ansi.Color.BrightYellow ->
            Just Elm.Error.YELLOW

        Ansi.Color.BrightBlue ->
            Just Elm.Error.BLUE

        Ansi.Color.BrightMagenta ->
            Just Elm.Error.MAGENTA

        Ansi.Color.BrightCyan ->
            Just Elm.Error.CYAN

        Ansi.Color.BrightWhite ->
            Just Elm.Error.WHITE

        Ansi.Color.Custom256 _ ->
            Nothing

        Ansi.Color.CustomTrueColor _ ->
            Nothing


view : Parsed -> List (Html msg)
view =
    Data.Problem.viewChunks
