module Tests exposing (all)

import Expect
import List.Zipper as Zipper exposing (Zipper)
import Main exposing (Prefecture, Status(..), getPrefectureStatus)
import Test exposing (Test, describe, test)


aomori : Prefecture
aomori =
    { name = "Aomori", kanji = "青森県", hiragana = "あおもりけん", id = 2, status = NotAsked }


hokkaido : Prefecture
hokkaido =
    { name = "Hokkaido", kanji = "北海道", hiragana = "ほっかいどう", id = 1, status = Correct }


iwate : Prefecture
iwate =
    { name = "Iwate", kanji = "岩手県", hiragana = "いわてけん", id = 3, status = Failed }


all : Test
all =
    describe "getPrefectureStatus tests"
        [ test "current country is focused -> color Accent" <|
            \_ ->
                let
                    zipper : Zipper Prefecture
                    zipper =
                        Zipper.singleton aomori
                in
                Expect.equal Accent (getPrefectureStatus 2 zipper)
        , test "country not yet asked -> color NotAsked" <|
            \_ ->
                let
                    zipper : Zipper Prefecture
                    zipper =
                        Zipper.singleton aomori
                in
                Expect.equal NotAsked (getPrefectureStatus 1 zipper)
        , test "country asked and answered correctly -> color Green" <|
            \_ ->
                let
                    zipper : Zipper Prefecture
                    zipper =
                        Zipper.from [ hokkaido ] aomori [ iwate ]
                in
                Expect.equal Correct (getPrefectureStatus 1 zipper)
        , test "country asked and answered incorrectly -> Red" <|
            \_ ->
                let
                    zipper : Zipper Prefecture
                    zipper =
                        Zipper.from [ hokkaido, iwate ] aomori []
                in
                Expect.equal Failed (getPrefectureStatus 3 zipper)
        ]
