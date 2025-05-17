module RelativeDistance exposing (degreeOfSeparation)

import Dict exposing (Dict)
import Set exposing (Set)


degreeOfSeparation : Dict String (List String) -> String -> String -> Int
degreeOfSeparation familyTree personA personB =
    let
        graph =
            familyTree
                |> initialState
                |> withParents
                |> withSiblings

        degrees = Debug.todo "Implement BFS"

        _ =
            Debug.log "Final Graph" graph

        _ =
            Debug.log "Degrees" degrees
    in
    3


initialState : Dict String (List String) -> Dict String (Set String)
initialState familyTree =
    Dict.foldl
        (\parent children acc ->
            Dict.insert parent (Set.fromList children) acc
        )
        Dict.empty
        familyTree


withParents : Dict String (Set String) -> Dict String (Set String)
withParents familyTree =
    Dict.foldl
        (\parent children graph ->
            Set.foldl
                (\child acc ->
                    Dict.update child
                        (\existingRelatives ->
                            let
                                newLinks =
                                    Set.singleton parent
                            in
                            case existingRelatives of
                                Nothing ->
                                    Just newLinks

                                Just existingLinks ->
                                    Just (Set.insert parent existingLinks)
                        )
                        acc
                )
                graph
                children
        )
        familyTree
        familyTree


withSiblings : Dict String (Set String) -> Dict String (Set String)
withSiblings familyTree =
    Dict.foldl
        (\parent children graph ->
            Set.foldl
                (\child acc ->
                    Dict.update child
                        (\existingRelatives ->
                            let
                                newLinks =
                                    Set.remove child children
                            in
                            case existingRelatives of
                                Nothing ->
                                    Just newLinks

                                Just existingLinks ->
                                    Just (Set.union existingLinks newLinks)
                        )
                        acc
                )
                graph
                children
        )
        familyTree
        familyTree
