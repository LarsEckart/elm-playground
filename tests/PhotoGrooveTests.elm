module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode exposing (decodeString)
import Main exposing (..)
import Test exposing (..)


decoderTest : Test
decoderTest =
    test "title defaults to (untitled)" <|
        \_ ->
            """ {"url":"fruits.com", "size":5}"""
                |> decodeString Main.photoDecoder
                |> Result.map .title
                |> Expect.equal (Ok "(untitled)")
