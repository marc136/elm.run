module InteropDefinitions exposing
    ( CheckedInput(..)
    , CompileResult(..)
    , EvaluatedInput(..)
    , EvaluatedTextInputData
    , Flags
    , FromElm(..)
    , Input
    , NewDeclaration
    , Output
    , Timestamp
    , ToElm(..)
    , compileResult
    , default
    , interop
    )

import AnsiExtra
import Data.Problem
import Elm.Error
import Json.Decode
import Theme exposing (Theme)
import TsJson.Decode as TsDecode exposing (Decoder)
import TsJson.Encode as TsEncode exposing (Encoder)


interop :
    { toElm : Decoder ToElm
    , fromElm : Encoder FromElm
    , flags : Decoder Flags
    }
interop =
    { toElm = toElm
    , fromElm = fromElm
    , flags = flags
    }


type FromElm
    = TriggerCompile


type alias Timestamp =
    Float


type ToElm
    = OnCompileResult CompileResult
    | Executed { input : Input, output : Output }
    | ClearHint
    | CheckedTextInput CheckedInput
    | EvaluatedTextInput EvaluatedTextInputData


type alias EvaluatedTextInputData =
    { input : Input, id : Timestamp, result : EvaluatedInput }


type CheckedInput
    = FoundNothing
    | FoundDeclaration NewDeclaration
    | FoundExpression { type_ : String, value : AnsiExtra.Parsed }


type alias NewDeclaration =
    { name : String, type_ : String, value : AnsiExtra.Parsed }


type EvaluatedInput
    = NoOutput
    | EvaluatedDeclaration { name : String, type_ : String, value : AnsiExtra.Parsed }
    | EvaluatedExpression { type_ : String, value : AnsiExtra.Parsed }
    | Problems (List Data.Problem.Problem)


type alias Input =
    String


type alias Output =
    { name : Maybe String, value : AnsiExtra.Parsed, type_ : String }


type CompileResult
    = CompileSuccess { file : String, name : String }
    | CompileErrors { errors : List Json.Decode.Value }
    | CompileError { path : String, title : String, message : String }


type alias Flags =
    { file : String
    , theme : Theme
    }


default : Flags
default =
    { file = ""
    , theme = Theme.Light
    }


fromElm : Encoder FromElm
fromElm =
    TsEncode.union
        (\vTriggerCompile value ->
            case value of
                TriggerCompile ->
                    vTriggerCompile ""
        )
        |> TsEncode.variantTagged "compile" TsEncode.string
        |> TsEncode.buildUnion


toElm : Decoder ToElm
toElm =
    TsDecode.discriminatedUnion "tag"
        [ ( "compile-result", compileResultEvent )
        , ( "evaluated", evaluated )
        , ( "executed", executedEvent )
        , ( "clear-hint", TsDecode.succeed ClearHint )
        , ( "checked-input", TsDecode.map CheckedTextInput checkedInput )

        --   , TsDecode.field "detail" (TsDecode.map OnCompileResult compileResult)
        --   )
        ]


checkedInput : Decoder CheckedInput
checkedInput =
    -- TsDecode.discriminatedUnion "type"
    --     [ ( "evaluated"
    --       , TsDecode.map2 (\type_ value -> FoundExpression { type_ = type_, value = value })
    --             (TsDecode.at [ "value", "type" ] TsDecode.string)
    --             (TsDecode.at [ "value", "value" ] TsDecode.string)
    --       )
    --     , ( "new-decl"
    --       , TsDecode.map FoundDeclaration <| TsDecode.field "value" newDecl
    --       )
    --     ]
    let
        combined =
            TsDecode.andThenInit
                (\eval decl type_ ->
                    case type_ of
                        "evaluated" ->
                            TsDecode.map FoundExpression eval

                        "new-decl" ->
                            TsDecode.map FoundDeclaration decl

                        _ ->
                            TsDecode.succeed FoundNothing
                )
                |> TsDecode.andThenDecoder
                    (TsDecode.map2 (\type_ value -> { type_ = type_, value = AnsiExtra.parse value })
                        (TsDecode.at [ "value", "type" ] TsDecode.string)
                        (TsDecode.at [ "value", "value" ] TsDecode.string)
                    )
                |> TsDecode.andThenDecoder
                    (TsDecode.field "value" newDecl)
    in
    TsDecode.field "type" TsDecode.string |> TsDecode.andThen combined


newDecl : Decoder NewDeclaration
newDecl =
    TsDecode.map3 (\name type_ value -> { name = name, type_ = type_, value = AnsiExtra.parse value })
        (TsDecode.field "name" TsDecode.string)
        (TsDecode.field "type" TsDecode.string)
        (TsDecode.field "value" TsDecode.string)


executedEvent : Decoder ToElm
executedEvent =
    TsDecode.map2 (\input output -> Executed { input = input, output = output })
        (TsDecode.field "input" TsDecode.string)
        (TsDecode.field "output" outputDecoder)


outputDecoder : Decoder Output
outputDecoder =
    TsDecode.map3 (\name value type_ -> { name = name, value = AnsiExtra.parse value, type_ = type_ })
        (TsDecode.field "name" (TsDecode.maybe TsDecode.string))
        (TsDecode.field "value" TsDecode.string)
        (TsDecode.field "type" TsDecode.string)


compileResultEvent : Decoder ToElm
compileResultEvent =
    TsDecode.field "detail" (TsDecode.map OnCompileResult compileResult)


evaluated : Decoder ToElm
evaluated =
    TsDecode.map3 (\input id result -> EvaluatedTextInput { input = input, id = id, result = result })
        (TsDecode.field "input" TsDecode.string)
        (TsDecode.field "id" TsDecode.float)
        (TsDecode.field "result" evaluatedResult)


evaluatedResult : Decoder EvaluatedInput
evaluatedResult =
    -- NoOutput { id : Timestamp, input : String }
    -- | EvaluatedDeclaration { id : Timestamp, input : String, name : String, type_ : String, value : String }
    -- | EvaluatedExpression { id : Timestamp, input : String, type_ : String, value : String }
    -- | Failure { id : Timestamp, input : String, problems : List Data.Problem.Problem }
    TsDecode.discriminatedUnion "type"
        [ ( "new-decl"
          , TsDecode.field "value" <|
                TsDecode.map3 (\name type_ value -> EvaluatedDeclaration { name = name, type_ = type_, value = AnsiExtra.parse value })
                    (TsDecode.field "name" TsDecode.string)
                    (TsDecode.field "type" TsDecode.string)
                    (TsDecode.field "value" TsDecode.string)
          )
        , ( "evaluated"
          , TsDecode.field "value" <|
                TsDecode.map2 (\type_ value -> EvaluatedExpression { type_ = type_, value = AnsiExtra.parse value })
                    (TsDecode.field "type" TsDecode.string)
                    (TsDecode.field "value" TsDecode.string)
          )
        , ( "compile-errors"
            -- CompilerReport in elm-compiler-wasm/builder/src/Reporting/Exit/Help.hs
            --   , TsDecode.map (\err -> Data.Problem.toIndexedProblems err |> Problems)
            --         -- (TsDecode.field "errors" (TsDecode.list TsDecode.value))
            --         (TsDecode.decoder Elm.Error.decoder)
          , TsDecode.value
                |> TsDecode.unknownAndThen (\_ -> Elm.Error.decoder)
                |> TsDecode.map (Problems << Data.Problem.toIndexedProblems)
            -- |> TsDecode.andThenDecoder
            -- -- |> Json.Decode.decodeValue Elm.Error.decoder
            -- |> Result.andThen (Json.Decode.decodeValue Elm.Error.decoder)
          )

        --     , ( "error"
        --         -- Report in elm-compiler-wasm/builder/src/Reporting/Exit/Help.hs
        --       , TsDecode.map3 (\path title message -> CompileError { path = path, title = title, message = message })
        --             (TsDecode.field "path" TsDecode.string)
        --             (TsDecode.field "title" TsDecode.string)
        --             (TsDecode.field "message" TsDecode.string)
        --       )
        ]



-- TsDecode.fail "TODO evaluatedResult"


compileResult : Decoder CompileResult
compileResult =
    -- TODO rename to evaluatedResult
    TsDecode.discriminatedUnion "type"
        [ ( "success"
          , TsDecode.map2 (\file name -> CompileSuccess { file = file, name = name })
                (TsDecode.field "file" TsDecode.string)
                (TsDecode.field "name" TsDecode.string)
          )
        , ( "compile-errors"
            -- CompilerReport in elm-compiler-wasm/builder/src/Reporting/Exit/Help.hs
          , TsDecode.map (\errors -> CompileErrors { errors = errors })
                (TsDecode.field "errors" (TsDecode.list TsDecode.value))
          )
        , ( "error"
            -- Report in elm-compiler-wasm/builder/src/Reporting/Exit/Help.hs
          , TsDecode.map3 (\path title message -> CompileError { path = path, title = title, message = message })
                (TsDecode.field "path" TsDecode.string)
                (TsDecode.field "title" TsDecode.string)
                (TsDecode.field "message" TsDecode.string)
          )
        ]


flags : Decoder Flags
flags =
    TsDecode.map2
        (\file theme ->
            { file = file
            , theme = theme
            }
        )
        (TsDecode.field "file" TsDecode.string)
        (TsDecode.field "theme" Theme.decoder)
