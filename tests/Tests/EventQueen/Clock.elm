module Tests.EventQueen.Clock exposing (suite)

import Dict
import EventQueen.Clock as Clock exposing (Clock)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "EventQueen.Clock"
        [ test "the zero clock corresponds to an empty dict" <|
            \() ->
                Clock.zero
                    |> Clock.toDict
                    |> Expect.equal Dict.empty
        , fuzz string "every node has 0 ticks on the zero clock" <|
            \nodeName ->
                Clock.zero
                    |> Clock.ticksOf { node = nodeName }
                    |> Expect.equal 0
        , fuzz (Fuzz.intRange 0 3) "the tick count of a node is the time the clock was ticked with it's name" <|
            \times ->
                Clock.zero
                    |> repeat times (Clock.tick { node = "node" })
                    |> Clock.ticksOf { node = "node" }
                    |> Expect.equal times
        , test "ticking one node does not change the tick count of another" <|
            \() ->
                Clock.zero
                    |> Clock.tick { node = "one" }
                    |> Clock.ticksOf { node = "another" }
                    |> Expect.equal 0
        , fuzz (randomClock { size = 3 }) "only has non-negative ticks" <|
            \clock ->
                clock
                    |> Clock.toDict
                    |> Dict.filter (\_ v -> v < 0)
                    |> Expect.equal Dict.empty
                    |> Expect.onFail "clock has negative ticks for some keys"
        , fuzz (randomClock { size = 3 }) "is simultaneous with itself" <|
            \clock ->
                Clock.compare ( clock, clock )
                    |> Expect.equal Clock.Simultaneous
        , fuzz (randomClock { size = 3 }) "is chronological with a clock with extra ticks" <|
            \clock ->
                Clock.compare ( clock, clock |> Clock.tick { node = "1" } )
                    |> Expect.equal Clock.Chronological
        , fuzz (randomClock { size = 3 }) "is reverse chronological with a clock with less ticks" <|
            \clock ->
                Clock.compare ( clock |> Clock.tick { node = "1" }, clock )
                    |> Expect.equal Clock.ReverseChronological
        , fuzz (randomClock { size = 3 }) "two clocks with extra ticks for different nodes are concurrent" <|
            \clock ->
                Clock.compare
                    ( clock |> Clock.tick { node = "1" }
                    , clock |> Clock.tick { node = "2" }
                    )
                    |> Expect.equal Clock.Concurrent
        ]


repeat : Int -> (a -> a) -> a -> a
repeat n f x =
    if n <= 0 then
        x

    else
        repeat (n - 1) f (f x)


randomClock : { size : Int } -> Fuzzer Clock
randomClock { size } =
    let
        randomDict nodeNo =
            if nodeNo <= 0 then
                Fuzz.constant Dict.empty

            else
                (nodeNo - 1)
                    |> randomDict
                    |> Fuzz.map2 (Dict.insert (String.fromInt size)) int
    in
    Fuzz.map Clock.fromDict <| randomDict size
