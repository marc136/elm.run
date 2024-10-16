module InteropDefinitions exposing
    ( CompileResult
    , Flags
    , FromElm(..)
    , PackageAddedOk
    , PackageList
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
    | AddPackage Elm.Package.Name
    | RemovePackage Elm.Package.Name
    | WipJs


type ToElm
    = OnCompileResult CompileResult
    | PackageListLoaded PackageListResult
    | PackageAdded PackageAddedResult
    | PackageRemoved (Result String Elm.Package.Name)


type CompileResult
    = CompileSuccess { file : String, name : String }
    | CompileErrors { errors : List Json.Decode.Value }
    | CompileError { path : String, title : String, message : String }


type alias PackageListResult =
    Result (List ( String, String )) PackageList


type alias PackageList =
    List ( Elm.Package.Name, Elm.Version.Version )


type alias PackageAddedResult =
    Result ( String, Json.Decode.Value ) PackageAddedOk


type alias PackageAddedOk =
    { name : Elm.Package.Name
    , version : Elm.Version.Version
    , comment : String
    }


type alias Flags =
    { file : String
    , theme : Theme
    }


fromElm : Encoder FromElm
fromElm =
    TsEncode.union
        (\vTriggerCompile vRevokeUrl vReplaceCodeWith vWipJs vLoadPackageList vAddPackage vRemovePackage value ->
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

                AddPackage name ->
                    vAddPackage <| Elm.Package.toString name

                RemovePackage name ->
                    vRemovePackage <| Elm.Package.toString name
        )
        |> TsEncode.variantTagged "compile" TsEncode.string
        |> TsEncode.variantTagged "revoke-object-url" TsEncode.string
        -- |> TsEncode.variantTagged "revoke-object-url"
        --     (TsEncode.object [ required "url" identity TsEncode.string ])
        |> TsEncode.variantTagged "replace-code" TsEncode.string
        |> TsEncode.variantTagged "wip-js" TsEncode.null
        |> TsEncode.variantTagged "load-package-list" TsEncode.null
        |> TsEncode.variantTagged "add-package" TsEncode.string
        |> TsEncode.variantTagged "remove-package" TsEncode.string
        |> TsEncode.buildUnion


toElm : Decoder ToElm
toElm =
    TsDecode.discriminatedUnion "fn"
        [ ( "compile-result", compileResultEvent )
        , ( "getPackages", packageListEvent )
        , ( "addPackage", addPackageEvent )
        , ( "removePackage", removePackageEvent )

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
                    Ok <| List.reverse <| result.ok

                else
                    Err result.err
            )


addPackageEvent : Decoder ToElm
addPackageEvent =
    decodeResult addPackageOkEvent addPackageErrEvent


addPackageOkEvent : Decoder ToElm
addPackageOkEvent =
    TsDecode.map4
        (\added version comment value ->
            case ( Elm.Package.fromString added, Elm.Version.fromString version ) of
                ( Just name, Just v ) ->
                    Ok { name = name, version = v, comment = comment } |> PackageAdded

                ( Nothing, _ ) ->
                    Err ( "Could not decode package name '" ++ added ++ "'", value ) |> PackageAdded

                ( _, Nothing ) ->
                    Err ( "Could not decode package version '" ++ version ++ "'", value ) |> PackageAdded
        )
        (TsDecode.field "added" TsDecode.string)
        (TsDecode.field "version" TsDecode.string)
        (TsDecode.field "comment" TsDecode.string)
        TsDecode.value


addPackageErrEvent : Decoder ToElm
addPackageErrEvent =
    TsDecode.map2
        (\error value -> PackageAdded (Err ( error, value )))
        (TsDecode.field "error" TsDecode.string)
        (TsDecode.field "data" TsDecode.value)


removePackageEvent : Decoder ToElm
removePackageEvent =
    TsDecode.map
        (\removed ->
            Elm.Package.fromString removed
                |> Result.fromMaybe removed
                |> PackageRemoved
        )
        (TsDecode.field "removed" TsDecode.string)


decodeResult : Decoder ToElm -> Decoder ToElm -> Decoder ToElm
decodeResult okDecoder errDecoder =
    TsDecode.field "result" TsDecode.string
        |> TsDecode.andThen
            (TsDecode.andThenInit
                (\ok err result ->
                    case result of
                        "ok" ->
                            ok

                        "err" ->
                            err

                        _ ->
                            TsDecode.fail <| "Unexpected `result='" ++ result ++ "'`"
                )
                |> TsDecode.andThenDecoder (TsDecode.field "data" okDecoder)
                |> TsDecode.andThenDecoder errDecoder
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
