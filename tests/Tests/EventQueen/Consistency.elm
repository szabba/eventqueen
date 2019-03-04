module Tests.EventQueen.Consistency exposing (Config, concurrently, isStronglyEventuallyConsistent)

import Dict
import EventQueen.Node as Node exposing (Node)
import EventQueen.Node.Simulation as Simulation
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Result.Extra as Result
import Test exposing (Test)
import Tests.EventQueen.Simulation.History as History


type alias Config operation diff state =
    { simulation : Simulation.Config operation diff state
    , history : History.Config operation
    }


isStronglyEventuallyConsistent : Config operation diff state -> Test
isStronglyEventuallyConsistent config =
    Test.fuzz (history config) "is strongly eventually consistent" <|
        \events ->
            events
                |> Simulation.run config.simulation
                |> Dict.values
                |> List.map .state
                |> allEqual


concurrently :
    String
    -> Config operation diff state
    ->
        ({ root : String }
         ->
            { atRoot : Node diff state -> Result String (Node diff state)
            , atLeft : Node diff state -> Result String (Node diff state)
            , atRight : Node diff state -> Result String (Node diff state)
            , expectMerge : Node diff state -> Expectation
            }
        )
    -> Test
concurrently testName config f =
    let
        fuzzer =
            History.fuzzNode config.history config.simulation
    in
    Test.fuzz fuzzer testName <|
        \node ->
            let
                { atRoot, atLeft, atRight, expectMerge } =
                    f { root = "root" }
            in
            node
                |> switchNode "root"
                |> atRoot
                |> Result.map
                    (\root ->
                        ( root |> switchNode "left" |> atLeft |> Result.mapError (\err -> Expect.fail <| "at left node: " ++ err)
                        , root |> switchNode "right" |> atRight |> Result.mapError (\err -> Expect.fail <| "at right node: " ++ err)
                        )
                    )
                |> Result.mapError Expect.fail
                |> Result.andThen
                    (\( left, right ) ->
                        Result.map2 (merge config.simulation.node) left right
                    )
                |> Result.andThen identity
                |> Result.map (switchNode "merge" >> expectMerge)
                |> Result.merge


merge : Node.Config diff state -> Node diff state -> Node diff state -> Result Expectation (Node diff state)
merge config left right =
    let
        ( leftSync, rightSync ) =
            ( left |> Simulation.syncFrom config right
            , right |> Simulation.syncFrom config left
            )
    in
    if leftSync.state /= rightSync.state then
        (Debug.toString leftSync.state ++ "\n\t/=\n" ++ Debug.toString rightSync.state)
            |> Expect.fail
            |> Err

    else
        leftSync |> Ok


history : Config operation diff state -> Fuzzer (List (Simulation.Event operation))
history config =
    History.fuzz config.history
        |> Fuzz.map History.ensureFullSync


switchNode : String -> Node diff state -> Node diff state
switchNode newName node =
    { node | name = newName }


allEqual : List a -> Expectation
allEqual vals =
    let
        consMissing v acc =
            if acc |> List.member v then
                acc

            else
                v :: acc

        uniqueVals =
            vals |> List.foldl consMissing []

        failureMessage n =
            case n of
                0 ->
                    Nothing

                1 ->
                    Nothing

                _ ->
                    Just <| "There are several different values: " ++ Debug.toString uniqueVals ++ "."
    in
    uniqueVals
        |> List.length
        |> failureMessage
        |> Maybe.map Expect.fail
        |> Maybe.withDefault Expect.pass
