module InteropDefinitions exposing
    ( CompileResult
    , Flags
    , FromElm(..)
    , ToElm(..)
    , compileResult
    , interop
    )

import Elm.Package
import Elm.Version
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
    | LoadPackageList
    | WipJs


type ToElm
    = OnCompileResult CompileResult
    | PackageListLoaded PackageListResult


type CompileResult
    = CompileSuccess { file : String, name : String }
    | CompileErrors { errors : List Json.Decode.Value }
    | CompileError { path : String, title : String, message : String }


type alias PackageListResult =
    Result (List ( String, String )) PackageList


type alias PackageList =
    List ( Elm.Package.Name, Elm.Version.Version )


type alias Flags =
    { file : String
    , theme : Theme
    }


fromElm : Encoder FromElm
fromElm =
    TsEncode.union
        (\vTriggerCompile vRevokeUrl vReplaceCodeWith vWipJs vLoadPackageList value ->
            case value of
                TriggerCompile filepath ->
                    vTriggerCompile filepath

                RevokeObjectUrl url ->
                    vRevokeUrl url

                ReplaceCodeWith source ->
                    vReplaceCodeWith source

                WipJs ->
                    vWipJs ()

                LoadPackageList ->
                    vLoadPackageList ()
        )
        |> TsEncode.variantTagged "compile" TsEncode.string
        |> TsEncode.variantTagged "revoke-object-url" TsEncode.string
        -- |> TsEncode.variantTagged "revoke-object-url"
        --     (TsEncode.object [ required "url" identity TsEncode.string ])
        |> TsEncode.variantTagged "replace-code" TsEncode.string
        |> TsEncode.variantTagged "wip-js" TsEncode.null
        |> TsEncode.variantTagged "load-package-list" TsEncode.null
        |> TsEncode.buildUnion


toElm : Decoder ToElm
toElm =
    TsDecode.discriminatedUnion "tag"
        [ ( "compile-result", compileResultEvent )
        , ( "get-packages", packageListEvent )

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


packageListEvent : Decoder ToElm
packageListEvent =
    TsDecode.discriminatedUnion "result"
        [ ( "ok", TsDecode.field "data" packageList |> TsDecode.map PackageListLoaded )
        ]


packageList : Decoder PackageListResult
packageList =
    TsDecode.keyValuePairs TsDecode.string
        |> TsDecode.map
            (\list ->
                let
                    result : { ok : PackageList, err : List ( String, String ) }
                    result =
                        List.foldl
                            (\( k, v ) { ok, err } ->
                                case ( Elm.Package.fromString k, Elm.Version.fromString v ) of
                                    ( Just pkg, Just version ) ->
                                        { ok = ( pkg, version ) :: ok, err = err }

                                    _ ->
                                        { ok = ok, err = ( k, v ) :: err }
                            )
                            { ok = [], err = [] }
                            list
                in
                if result.err == [] then
                    Ok result.ok

                else
                    Err result.err
            )


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
