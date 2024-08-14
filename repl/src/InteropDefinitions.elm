module InteropDefinitions exposing
    ( CompileResult
    , Flags
    , FromElm(..)
    , Input
    , Output
    , ToElm(..)
    , compileResult
    , default
    , interop
    )

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


type ToElm
    = OnCompileResult CompileResult
    | Executed { input : Input, output : Output }


type alias Input =
    String


type alias Output =
    { name : Maybe String, value : String, type_ : String }


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
        , ( "executed", executedEvent )

        --   , TsDecode.field "detail" (TsDecode.map OnCompileResult compileResult)
        --   )
        ]


executedEvent : Decoder ToElm
executedEvent =
    TsDecode.map2 (\input output -> Executed { input = input, output = output })
        (TsDecode.field "input" TsDecode.string)
        (TsDecode.field "output" outputDecoder)


outputDecoder : Decoder Output
outputDecoder =
    TsDecode.map3 (\name value type_ -> { name = name, value = value, type_ = type_ })
        (TsDecode.field "name" (TsDecode.maybe TsDecode.string))
        (TsDecode.field "value" TsDecode.string)
        (TsDecode.field "type" TsDecode.string)


compileResultEvent : Decoder ToElm
compileResultEvent =
    TsDecode.field "detail" (TsDecode.map OnCompileResult compileResult)


compileResult : Decoder CompileResult
compileResult =
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
