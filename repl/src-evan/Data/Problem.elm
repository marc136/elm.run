module Data.Problem exposing
    ( Location(..)
    , Problem
    , toIndexedProblems
    , viewChunks
    , viewList
    )

{-| Copyright 2021 Evan Czaplicki
<https://github.com/elm/elm-lang.org/blob/8193eb7552e26989d444167d6006eb50616ff554/editor/src/Data/Problem.elm>
Licensed under BSD-3-Clause License <https://github.com/elm/elm-lang.org/blob/8193eb7552e26989d444167d6006eb50616ff554/LICENSE>
-}

import Elm.Error
import Html exposing (Html)
import Html.Attributes
import Html.Events


type alias Problem =
    { index : Int
    , location : Location
    , title : String
    , message : List Elm.Error.Chunk
    }


type Location
    = General { path : Maybe String }
    | Specific { path : String, name : String, region : Elm.Error.Region }


toIndexedProblems : Elm.Error.Error -> List Problem
toIndexedProblems errors =
    case errors of
        Elm.Error.GeneralProblem problem ->
            [ { index = 0
              , location = General { path = problem.path }
              , title = problem.title
              , message = problem.message
              }
            ]

        Elm.Error.ModuleProblems modules ->
            let
                toModuleProblems : Elm.Error.BadModule -> ( Int, List Problem ) -> ( Int, List Problem )
                toModuleProblems module_ ( nextIndex, all ) =
                    List.foldr (toSingleProblem module_) ( nextIndex, all ) module_.problems

                toSingleProblem : Elm.Error.BadModule -> Elm.Error.Problem -> ( Int, List Problem ) -> ( Int, List Problem )
                toSingleProblem module_ problem ( nextIndex, all ) =
                    let
                        indexedProblem : Problem
                        indexedProblem =
                            { index = nextIndex
                            , location =
                                Specific
                                    { path = module_.path
                                    , name = module_.name
                                    , region = problem.region
                                    }
                            , title = problem.title
                            , message = problem.message
                            }
                    in
                    ( nextIndex + 1, indexedProblem :: all )
            in
            List.foldr toModuleProblems ( 0, [] ) modules
                |> Tuple.second



-- View


viewList : (Elm.Error.Region -> msg) -> List Problem -> Html msg
viewList onJumpToProblem problems =
    let
        viewProblem : Problem -> Html msg
        viewProblem problem =
            viewContainer
                [ viewHeader
                    [ viewTitle problem.title

                    -- TODO enable jump to problem again?
                    -- , viewNavigation [ viewLocation onJumpToProblem problem.location ]
                    ]
                , viewBody problem.message
                ]
    in
    Html.div
        [ Html.Attributes.id "problems" ]
        (List.map viewProblem problems)



-- PARTS


viewContainer : List (Html msg) -> Html msg
viewContainer =
    Html.div [ Html.Attributes.class "problem-container" ]


viewHeader : List (Html msg) -> Html msg
viewHeader =
    Html.div [ Html.Attributes.class "problem-header" ]


viewNavigation : List (Html msg) -> Html msg
viewNavigation =
    Html.nav [ Html.Attributes.class "problem-navigation" ]


viewTitle : String -> Html msg
viewTitle title =
    Html.div [ Html.Attributes.class "problem-title" ] [ Html.text title ]


viewLocation : (Elm.Error.Region -> msg) -> Location -> Html msg
viewLocation onJumpToProblem location =
    case location of
        General { path } ->
            viewModuleName path

        Specific { path, name, region } ->
            viewRegion onJumpToProblem name region


viewRegion : (Elm.Error.Region -> msg) -> String -> Elm.Error.Region -> Html msg
viewRegion onJumpToProblem name region =
    Html.a [ Html.Attributes.class "problem-region", Html.Events.onClick (onJumpToProblem region) ]
        [ Html.text "Jump to problem" ]


viewModuleName : Maybe String -> Html msg
viewModuleName name =
    Html.div [ Html.Attributes.class "problem-region" ] [ Html.text (Maybe.withDefault "" name) ]


viewBody : List Elm.Error.Chunk -> Html msg
viewBody =
    Html.div [ Html.Attributes.class "problem-body" ] << viewChunks


viewChunks : List Elm.Error.Chunk -> List (Html msg)
viewChunks chunks =
    case chunks of
        [] ->
            [ Html.text "\n\n\n" ]

        chunk :: others ->
            let
                htmlChunk =
                    case chunk of
                        Elm.Error.Unstyled string ->
                            Html.text string

                        Elm.Error.Styled style string ->
                            Html.span (styleToClasses style) [ Html.text string ]
            in
            htmlChunk :: viewChunks others


styleToClasses : Elm.Error.Style -> List (Html.Attribute msg)
styleToClasses { bold, underline, color } =
    [ Html.Attributes.classList
        [ ( "bold", bold )
        , ( "underline", underline )
        , ( Maybe.map colorToClass color |> Maybe.withDefault ""
          , True
          )
        ]
    ]


colorToClass : Elm.Error.Color -> String
colorToClass color =
    case color of
        Elm.Error.Red ->
            "red"

        Elm.Error.RED ->
            "red"

        Elm.Error.Magenta ->
            "magenta"

        Elm.Error.MAGENTA ->
            "magenta"

        Elm.Error.Yellow ->
            "yellow"

        Elm.Error.YELLOW ->
            "yellow"

        Elm.Error.Green ->
            "green"

        Elm.Error.GREEN ->
            "green"

        Elm.Error.Cyan ->
            "cyan"

        Elm.Error.CYAN ->
            "cyan"

        Elm.Error.Blue ->
            "blue"

        Elm.Error.BLUE ->
            "blue"

        Elm.Error.White ->
            "white"

        Elm.Error.WHITE ->
            "white"

        Elm.Error.Black ->
            "black"

        Elm.Error.BLACK ->
            "black"
