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
    | ScrollToBottom
    | RemoveFromState (List String)
    | ReplaceEnteredCode String
    | EnterExampleCode


type alias Timestamp =
    Float


type ToElm
    = CheckedTextInput CheckedInput
    | EvaluatedTextInput EvaluatedTextInputData


type alias EvaluatedTextInputData =
    { input : Input, id : Timestamp, result : EvaluatedInput }


type CheckedInput
    = FoundNothing
    | FoundDeclaration NewDeclaration
    | FoundExpression { type_ : String, value : AnsiExtra.Parsed }


type alias NewDeclaration =
    { name : String, type_ : String, value : AnsiExtra.Parsed, outdated : Bool }


type EvaluatedInput
    = NoOutput
    | EvaluatedDeclaration NewDeclaration
    | EvaluatedExpression { type_ : String, value : AnsiExtra.Parsed }
    | Problems (List Data.Problem.Problem)
    | Import
    | TypeDefinition { name : String, outdated : Bool }


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
        (\vTriggerCompile vScrollToBottom vRemoveFromState vReplaceEnteredCode vEnterExampleCode value ->
            case value of
                TriggerCompile ->
                    vTriggerCompile ""

                ScrollToBottom ->
                    vScrollToBottom ""

                RemoveFromState name ->
                    vRemoveFromState name

                ReplaceEnteredCode code ->
                    vReplaceEnteredCode code

                EnterExampleCode ->
                    vEnterExampleCode ""
        )
        |> TsEncode.variantTagged "compile" TsEncode.string
        |> TsEncode.variantTagged "scroll-to-bottom" TsEncode.string
        |> TsEncode.variantTagged "remove-from-state" (TsEncode.list TsEncode.string)
        |> TsEncode.variantTagged "replace-entered-code" TsEncode.string
        |> TsEncode.variantTagged "enter-example-code" TsEncode.string
        |> TsEncode.buildUnion


toElm : Decoder ToElm
toElm =
    TsDecode.discriminatedUnion "tag"
        [ ( "evaluated", evaluated )
        , ( "checked-input", TsDecode.map CheckedTextInput checkedInput )
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
    TsDecode.map3 (\name type_ value -> { name = name, type_ = type_, value = AnsiExtra.parse value, outdated = False })
        (TsDecode.field "name" TsDecode.string)
        (TsDecode.field "type" TsDecode.string)
        (TsDecode.field "value" TsDecode.string)


evaluated : Decoder ToElm
evaluated =
    TsDecode.map3 (\input id result -> EvaluatedTextInput { input = input, id = id, result = result })
        (TsDecode.field "input" TsDecode.string)
        (TsDecode.field "id" TsDecode.float)
        (TsDecode.field "result" evaluatedResult)


evaluatedResult : Decoder EvaluatedInput
evaluatedResult =
    TsDecode.discriminatedUnion "type"
        [ ( "new-decl"
          , TsDecode.field "value" <|
                TsDecode.map3 (\name type_ value -> EvaluatedDeclaration { name = name, type_ = type_, value = AnsiExtra.parse value, outdated = False })
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
          , TsDecode.value
                |> TsDecode.unknownAndThen (\_ -> Elm.Error.decoder)
                |> TsDecode.map (Problems << Data.Problem.toIndexedProblems)
          )
        , ( "new-import"
          , TsDecode.succeed Import
          )
        , ( "new-type"
          , TsDecode.map
                (\name -> TypeDefinition { name = name, outdated = False })
                (TsDecode.field "name" TsDecode.string)
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
