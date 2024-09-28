module InteropDefinitions exposing
    ( CompileResult
    , Flags
    , FromElm(..)
    , ToElm(..)
    , compileResult
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
    = TriggerCompile String
    | RevokeObjectUrl String
    | ReplaceCodeWith String
    | WipJs


type ToElm
    = OnCompileResult CompileResult


type CompileResult
    = CompileSuccess { file : String, name : String }
    | CompileErrors { errors : List Json.Decode.Value }
    | CompileError { path : String, title : String, message : String }


type alias Flags =
    { file : String
    , theme : Theme
    }


fromElm : Encoder FromElm
fromElm =
    TsEncode.union
        (\vTriggerCompile vRevokeUrl vReplaceCodeWith vWipJs value ->
            case value of
                TriggerCompile filepath ->
                    vTriggerCompile filepath

                RevokeObjectUrl url ->
                    vRevokeUrl url

                ReplaceCodeWith source ->
                    vReplaceCodeWith source

                WipJs ->
                    vWipJs ()
        )
        |> TsEncode.variantTagged "compile" TsEncode.string
        |> TsEncode.variantTagged "revoke-object-url" TsEncode.string
        -- |> TsEncode.variantTagged "revoke-object-url"
        --     (TsEncode.object [ required "url" identity TsEncode.string ])
        |> TsEncode.variantTagged "replace-code" TsEncode.string
        |> TsEncode.variantTagged "wip-js" TsEncode.null
        |> TsEncode.buildUnion


toElm : Decoder ToElm
toElm =
    TsDecode.discriminatedUnion "tag"
        [ ( "compile-result", compileResultEvent )

        --   , TsDecode.field "detail" (TsDecode.map OnCompileResult compileResult)
        --   )
        ]


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
