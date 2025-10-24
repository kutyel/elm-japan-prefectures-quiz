module Main exposing (Model, Msg, Prefecture, Status(..), getPrefectureStatus, main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Events.Extra as Events
import List.Extra as List
import List.Zipper as Zipper exposing (Zipper)
import Maybe.Extra as Maybe
import Random exposing (Generator)
import Random.List exposing (shuffle)
import Svg
import Svg.Attributes as SvgAttr
import Task
import Toast


emptyTray : Toast.Tray Toast
emptyTray =
    Toast.tray


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = always Sub.none }


type Toast
    = Red Prefecture
    | Green


type alias Score =
    { correct : Int
    , failed : Int
    , streak : Int
    , maxStreak : Int
    }


type alias GameState =
    { prefectures : Zipper Prefecture
    , score : Score
    , guess : String
    }


type alias Prefecture =
    { name : String
    , kanji : String
    , hiragana : String
    , id : Int
    , status : Status
    }


type Status
    = NotAsked
    | Accent
    | Failed
    | Correct


type Model
    = Idle
    | Playing GameState (Toast.Tray Toast)
    | Finished Score (Toast.Tray Toast)


init : () -> ( Model, Cmd msg )
init () =
    ( Idle, Cmd.none )


type Msg
    = Start
    | Restart
    | ToastMsg Toast.Msg
    | AddToast Toast
    | OnInput GameState
    | CheckAnswer GameState (Toast.Tray Toast)
    | RandomPrefecture Score (List Prefecture)


addToTray :
    (Toast.Tray Toast -> m)
    -> Toast.Tray Toast
    -> Toast
    -> ( m, Cmd Msg )
addToTray stateFn oldTray content =
    Toast.expireIn 3000 content
        |> Toast.add oldTray
        |> (\( tray, cmd ) -> ( stateFn tray, Cmd.map ToastMsg cmd ))


updateTray :
    (Toast.Tray content -> m)
    -> Toast.Msg
    -> Toast.Tray content
    -> ( m, Cmd Msg )
updateTray stateFn msg oldTray =
    Toast.update msg oldTray
        |> (\( tray, cmd ) -> ( stateFn tray, Cmd.map ToastMsg cmd ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnInput gameState ->
            case model of
                Playing _ tray ->
                    ( Playing gameState tray, Cmd.none )

                _ ->
                    ( Playing gameState emptyTray, Cmd.none )

        Start ->
            let
                prefectureGenerator : Generator (List Prefecture)
                prefectureGenerator =
                    -- the whole game revolves around this!
                    shuffle allPrefectures
            in
            ( model, Random.generate (RandomPrefecture (Score 0 0 0 0)) prefectureGenerator )

        Restart ->
            ( Idle, Cmd.none )

        AddToast content ->
            case model of
                Playing state oldTray ->
                    addToTray (Playing state) oldTray content

                Finished score oldTray ->
                    addToTray (Finished score) oldTray content

                _ ->
                    ( model, Cmd.none )

        ToastMsg tmsg ->
            case model of
                Playing state oldTray ->
                    updateTray (Playing state) tmsg oldTray

                Finished score oldTray ->
                    updateTray (Finished score) tmsg oldTray

                _ ->
                    ( model, Cmd.none )

        RandomPrefecture score (p :: ps) ->
            ( Playing (GameState (Zipper.fromCons p ps) score "") emptyTray, Cmd.none )

        RandomPrefecture score [] ->
            ( Finished score emptyTray, Cmd.none )

        CheckAnswer { prefectures, score, guess } tray ->
            let
                answerWasCorrect : Bool
                answerWasCorrect =
                    (String.length (String.trim guess) >= 3)
                        && (String.contains
                                (String.toLower <| String.trim guess)
                                (String.toLower (Zipper.current prefectures).name)
                                || String.contains
                                    (String.toLower <| String.trim guess)
                                    (String.toLower (Zipper.current prefectures).kanji)
                                || String.contains
                                    (String.toLower <| String.trim guess)
                                    (String.toLower (Zipper.current prefectures).hiragana)
                           )

                updatedGameScore : Score
                updatedGameScore =
                    if answerWasCorrect then
                        { score
                            | correct = score.correct + 1
                            , streak = score.streak + 1
                            , maxStreak = max (score.streak + 1) score.maxStreak
                        }

                    else
                        { score
                            | failed = score.failed + 1
                            , streak = 0
                        }

                updatedCurrentPrefecture : Zipper Prefecture
                updatedCurrentPrefecture =
                    Zipper.mapCurrent
                        (\p ->
                            { p
                                | status =
                                    if answerWasCorrect then
                                        Correct

                                    else
                                        Failed
                            }
                        )
                        prefectures
            in
            ( case Zipper.next updatedCurrentPrefecture of
                Just remainingPrefectures ->
                    Playing (GameState remainingPrefectures updatedGameScore "") tray

                Nothing ->
                    -- if there is no Zipper.next, the game is over!
                    Finished updatedGameScore tray
            , if answerWasCorrect then
                Task.perform identity <| Task.succeed (AddToast Green)

              else
                Task.perform identity <| Task.succeed (AddToast <| Red (Zipper.current prefectures))
            )


viewToast : List (Html.Attribute Msg) -> Toast.Info Toast -> Html Msg
viewToast attributes toast =
    Html.div attributes <|
        case toast.content of
            Red correct ->
                [ Html.div
                    [ Attr.attribute "role" "alert"
                    , Attr.class "alert alert-error animate-in slide-in-from-top duration-500 animate-out slide-out-to-top mb-2.5"
                    , Events.onClick <| ToastMsg <| Toast.exit toast.id
                    ]
                    [ Svg.svg
                        [ SvgAttr.class "h-6 w-6 shrink-0 stroke-current animate-pulse"
                        , SvgAttr.fill "none"
                        , SvgAttr.viewBox "0 0 24 24"
                        ]
                        [ Svg.path
                            [ SvgAttr.strokeLinecap "round"
                            , SvgAttr.strokeLinejoin "round"
                            , SvgAttr.strokeWidth "2"
                            , SvgAttr.d "M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z"
                            ]
                            []
                        ]
                    , Html.span [ Attr.class "animate-in fade-in duration-300" ]
                        [ Html.text <| "Mistake! The prefecture was: " ++ correct.name ++ " ("
                        , Html.ruby []
                            [ Html.text correct.kanji
                            , Html.rt [] [ Html.text correct.hiragana ]
                            ]
                        , Html.text ")"
                        ]
                    ]
                ]

            Green ->
                [ Html.div
                    [ Attr.attribute "role" "alert"
                    , Attr.class "alert alert-success animate-in slide-in-from-top duration-500 animate-out slide-out-to-top mb-2.5"
                    , Events.onClick <| ToastMsg <| Toast.exit toast.id
                    ]
                    [ Svg.svg
                        [ SvgAttr.class "h-6 w-6 shrink-0 stroke-current animate-bounce"
                        , SvgAttr.fill "none"
                        , SvgAttr.viewBox "0 0 24 24"
                        ]
                        [ Svg.path
                            [ SvgAttr.strokeLinecap "round"
                            , SvgAttr.strokeLinejoin "round"
                            , SvgAttr.strokeWidth "2"
                            , SvgAttr.d "M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"
                            ]
                            []
                        ]
                    , Html.span [ Attr.class "animate-in fade-in duration-300" ]
                        [ Html.text "Correct!" ]
                    ]
                ]


isPlaying : Model -> Bool
isPlaying model =
    case model of
        Playing _ _ ->
            True

        _ ->
            False


allPrefectures : List Prefecture
allPrefectures =
    [ { name = "Hokkaido", kanji = "北海道", hiragana = "ほっかいどう", id = 1, status = NotAsked }
    , { name = "Aomori", kanji = "青森県", hiragana = "あおもりけん", id = 2, status = NotAsked }
    , { name = "Iwate", kanji = "岩手県", hiragana = "いわてけん", id = 3, status = NotAsked }
    , { name = "Miyagi", kanji = "宮城県", hiragana = "みやぎけん", id = 4, status = NotAsked }
    , { name = "Akita", kanji = "秋田県", hiragana = "あきたけん", id = 5, status = NotAsked }
    , { name = "Yamagata", kanji = "山形県", hiragana = "やまがたけん", id = 6, status = NotAsked }
    , { name = "Fukushima", kanji = "福島県", hiragana = "ふくしまけん", id = 7, status = NotAsked }
    , { name = "Ibaraki", kanji = "茨城県", hiragana = "いばらきけん", id = 8, status = NotAsked }
    , { name = "Tochigi", kanji = "栃木県", hiragana = "とちぎけん", id = 9, status = NotAsked }
    , { name = "Gunma", kanji = "群馬県", hiragana = "ぐんまけん", id = 10, status = NotAsked }
    , { name = "Saitama", kanji = "埼玉県", hiragana = "さいたまけん", id = 11, status = NotAsked }
    , { name = "Chiba", kanji = "千葉県", hiragana = "ちばけん", id = 12, status = NotAsked }
    , { name = "Tokyo", kanji = "東京都", hiragana = "とうきょうと", id = 13, status = NotAsked }
    , { name = "Kanagawa", kanji = "神奈川県", hiragana = "かながわけん", id = 14, status = NotAsked }
    , { name = "Niigata", kanji = "新潟県", hiragana = "にいがたけん", id = 15, status = NotAsked }
    , { name = "Toyama", kanji = "富山県", hiragana = "とやまけん", id = 16, status = NotAsked }
    , { name = "Ishikawa", kanji = "石川県", hiragana = "いしかわけん", id = 17, status = NotAsked }
    , { name = "Fukui", kanji = "福井県", hiragana = "ふくいけん", id = 18, status = NotAsked }
    , { name = "Yamanashi", kanji = "山梨県", hiragana = "やまなしけん", id = 19, status = NotAsked }
    , { name = "Nagano", kanji = "長野県", hiragana = "ながのけん", id = 20, status = NotAsked }
    , { name = "Gifu", kanji = "岐阜県", hiragana = "ぎふけん", id = 21, status = NotAsked }
    , { name = "Shizuoka", kanji = "静岡県", hiragana = "しずおかけん", id = 22, status = NotAsked }
    , { name = "Aichi", kanji = "愛知県", hiragana = "あいちけん", id = 23, status = NotAsked }
    , { name = "Mie", kanji = "三重県", hiragana = "みえけん", id = 24, status = NotAsked }
    , { name = "Shiga", kanji = "滋賀県", hiragana = "しがけん", id = 25, status = NotAsked }
    , { name = "Kyoto", kanji = "京都府", hiragana = "きょうとふ", id = 26, status = NotAsked }
    , { name = "Osaka", kanji = "大阪府", hiragana = "おおさかふ", id = 27, status = NotAsked }
    , { name = "Hyogo", kanji = "兵庫県", hiragana = "ひょうごけん", id = 28, status = NotAsked }
    , { name = "Nara", kanji = "奈良県", hiragana = "ならけん", id = 29, status = NotAsked }
    , { name = "Wakayama", kanji = "和歌山県", hiragana = "わかやまけん", id = 30, status = NotAsked }
    , { name = "Tottori", kanji = "鳥取県", hiragana = "とっとりけん", id = 31, status = NotAsked }
    , { name = "Shimane", kanji = "島根県", hiragana = "しまねけん", id = 32, status = NotAsked }
    , { name = "Okayama", kanji = "岡山県", hiragana = "おかやまけん", id = 33, status = NotAsked }
    , { name = "Hiroshima", kanji = "広島県", hiragana = "ひろしまけん", id = 34, status = NotAsked }
    , { name = "Yamaguchi", kanji = "山口県", hiragana = "やまぐちけん", id = 35, status = NotAsked }
    , { name = "Tokushima", kanji = "徳島県", hiragana = "とくしまけん", id = 36, status = NotAsked }
    , { name = "Kagawa", kanji = "香川県", hiragana = "かがわけん", id = 37, status = NotAsked }
    , { name = "Ehime", kanji = "愛媛県", hiragana = "えひめけん", id = 38, status = NotAsked }
    , { name = "Kochi", kanji = "高知県", hiragana = "こうちけん", id = 39, status = NotAsked }
    , { name = "Fukuoka", kanji = "福岡県", hiragana = "ふくおかけん", id = 40, status = NotAsked }
    , { name = "Saga", kanji = "佐賀県", hiragana = "さがけん", id = 41, status = NotAsked }
    , { name = "Nagasaki", kanji = "長崎県", hiragana = "ながさきけん", id = 42, status = NotAsked }
    , { name = "Kumamoto", kanji = "熊本県", hiragana = "くまもとけん", id = 43, status = NotAsked }
    , { name = "Oita", kanji = "大分県", hiragana = "おおいたけん", id = 44, status = NotAsked }
    , { name = "Miyazaki", kanji = "宮崎県", hiragana = "みやざきけん", id = 45, status = NotAsked }
    , { name = "Kagoshima", kanji = "鹿児島県", hiragana = "かごしまけん", id = 46, status = NotAsked }
    , { name = "Okinawa", kanji = "沖縄県", hiragana = "おきなわけん", id = 47, status = NotAsked }
    ]


statusToColor : Status -> List (Svg.Attribute msg)
statusToColor status =
    case status of
        NotAsked ->
            [ SvgAttr.fill "#808080" ]

        Accent ->
            [ SvgAttr.fill "#422ad5", SvgAttr.class "animate-pulse" ]

        Failed ->
            [ SvgAttr.fill "#ff627d" ]

        Correct ->
            [ SvgAttr.fill "#00d391" ]


fillColor : Int -> Zipper Prefecture -> List (Svg.Attribute msg)
fillColor id =
    getPrefectureStatus id >> statusToColor


getPrefectureStatus : Int -> Zipper Prefecture -> Status
getPrefectureStatus id zipper =
    if (Zipper.current zipper).id == id then
        Accent

    else
        Zipper.before zipper
            |> List.find (.id >> (==) id)
            |> Maybe.unwrap NotAsked .status


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.class "min-h-screen flex flex-col bg-base-200" ]
        [ Html.div
            [ Attr.class "navbar bg-base-100 shadow-md sticky top-0 z-50" ]
            [ Html.div
                [ Attr.class "flex-1" ]
                [ Html.a
                    [ Attr.class "btn btn-ghost text-lg md:text-xl normal-case"
                    ]
                    [ Html.text "🏯 Japan Prefectures Quiz" ]
                ]
            , Html.div
                [ Attr.class "flex-none" ]
                (if isPlaying model then
                    [ Html.button
                        [ Attr.class "btn btn-ghost btn-sm md:btn-md"
                        , Events.onClick Restart
                        ]
                        [ Html.text "🔄 Restart" ]
                    ]

                 else
                    []
                )
            ]
        , Html.div [ Attr.class "flex-1 flex items-center justify-center p-4 md:p-8" ] <|
            case model of
                Idle ->
                    [ Html.div [ Attr.class "text-center space-y-6 animate-in fade-in zoom-in duration-700" ]
                        [ Html.h1 [ Attr.class "text-4xl md:text-6xl font-bold mb-4" ]
                            [ Html.text "Japan Prefectures Quiz" ]
                        , Html.p [ Attr.class "text-lg md:text-xl text-base-content/70 mb-8" ]
                            [ Html.text " 🧠 Can you guess all the prefectures of Japan? 🇯🇵" ]
                        , Html.div [ Attr.class "flex flex-col gap-3 items-center" ]
                            [ Html.button
                                [ Attr.class "btn btn-primary btn-lg btn-wide text-lg"
                                , Events.onClick Start
                                ]
                                [ Html.text "🚀 Play!" ]
                            ]
                        ]
                    ]

                Playing ({ prefectures, score, guess } as gameState) tray ->
                    [ Html.div
                        [ Attr.class "toast toast-top toast-center z-50" ]
                        [ Toast.render viewToast tray (Toast.config ToastMsg) ]
                    , Html.div [ Attr.class "w-full max-w-4xl mx-auto flex flex-col gap-4 md:gap-6" ]
                        [ Html.div [ Attr.class "sticky top-16 z-40 bg-base-200 pb-2 md:relative md:top-0 md:order-2" ]
                            [ Html.input
                                [ Attr.type_ "text"
                                , Attr.placeholder "Type prefecture name... (漢字とひらがなもOK!)"
                                , Attr.class "input input-bordered input-lg w-full text-lg md:text-xl focus:input-primary shadow-lg"
                                , Attr.autocomplete False
                                , Attr.autofocus True
                                , Attr.attribute "autocapitalize" "words"
                                , Events.onInput <| \s -> OnInput { gameState | guess = s }
                                , Attr.value guess
                                , Events.onEnter <| CheckAnswer gameState tray
                                ]
                                []
                            ]
                        , Html.div
                            [ Attr.class "card bg-base-100 shadow-xl md:order-1" ]
                            [ Html.div
                                [ Attr.class "card-body flex items-center justify-center py-8 md:py-16" ]
                                [ Svg.svg
                                    [ SvgAttr.viewBox "0 0 1024 1024"
                                    ]
                                    [ Svg.defs []
                                        [ Svg.g
                                            [ SvgAttr.id "ground"
                                            ]
                                            [ Svg.g
                                                [ SvgAttr.fill "grey"
                                                , SvgAttr.stroke "black"
                                                , SvgAttr.strokeWidth "0.5"
                                                , SvgAttr.strokeLinejoin "round"
                                                ]
                                                [ Svg.g
                                                    [ SvgAttr.id "R1"
                                                    , SvgAttr.name "hokkaido region"
                                                    ]
                                                    [ Svg.g
                                                        ([ SvgAttr.id "1"
                                                         , SvgAttr.name "hokkaido"
                                                         ]
                                                            ++ fillColor 1 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M591 250l-1 6h-3v-9l6 -2zM620 271l-1 -11 -4 -7 -5 -1 -2 -5 -4 -1 -2 -5 4 -8 -1 -11 2 -3 8 -2 6 -9 3 4 2 -1 4 -9 6 -5 -9 -13v-7l7 -3 14 11 10 -3v3l7 2 6 -2 5 -10 -5 -24 2 -5 8 -4 4 -5 -1 -23 4 -8 1 -11 -3 -17 -8 -19 3 -10 -1 -6 2 3h5l6 -8 26 28 8 13 16 17 29 18 28 6 1 4 6 5h19l22 -27 2 5 -11 24v9l4 6 9 3 -2 2 2 -3h-6l4 13 6 6 5 1 6 -8 8 -1 -11 8 -1 7 -17 3 -2 6 -5 5h-5l-2 -2 1 -2 -2 -1 -4 6 3 2 -15 1 -8 -3 -15 8 -14 16 -8 14 -3 8v16l-2 8 -14 -12 -23 -8 -28 -18 -13 -2 4 -2 -16 8 -15 15 -4 -2 4 -1 -5 -1 -2 -6 -8 -7h-10l-6 8 -3 9 1 5 12 8 10 -1 10 12 8 4 2 3 -6 5 -4 1 -9 -4 -3 3v-5h-3l-2 5 -7 4v9l-8 3 -3 6 -8 -3 -2 -7 1 -9zM659 58l2 -2 4 1 2 5 -4 4 -4 -4zM653 45v-1l2 2 -1 9 -4 -10zM957 29l-2 -5h-2l-2 4 2 7 -1 3 -5 1 -2 8 -7 -2 1 7 -10 10 -1 4 -5 -1v3l4 1v4l-4 4 -4 -1 1 5 -3 -4 1 5 -3 7 4 1 4 -9 6 -1 4 -9 10 -9 3 -10 5 2 6 -1 15 -18 13 -9 9 -1 1 -4 -2 -3 2 -4 -5 -2 -6 2 -12 17 -11 3zM878 110l5 -6 8 -2 5 -6 3 1 3 -7 -11 3 -7 -5 -4 2 -3 11 -10 14v3l-13 15 2 7 5 -1v5l1 -14 8 -4 1 -6h3v-7zM910 136l11 -9 -4 -3 -4 3 1 2 -7 1v4l2 -1zM888 154l3 -5 -6 1zM878 157l2 -2h-4z"
                                                            ]
                                                            []
                                                        ]
                                                    ]
                                                , Svg.g
                                                    [ SvgAttr.id "R2"
                                                    , SvgAttr.name "tohoku region"
                                                    ]
                                                    [ Svg.g
                                                        ([ SvgAttr.id "2"
                                                         , SvgAttr.name "aomori"
                                                         ]
                                                            ++ fillColor 2 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M613 358v-7l-4 -4 7 -8h5l7 -4 2 -16 -3 -5 3 -1 1 -7 6 5 4 -3 4 2 2 15 3 7 6 -3 -1 -4 2 -4 11 8 3 -2 4 -16 -4 -7 -6 6 -14 3 6 -26 17 12 9 -5 -3 31 3 14 4 11 4 -1 5 5 -6 7 -5 -1 -4 2 -2 -2 -14 10 -3 -2 1 -11 -5 1v-6l-11 7 -3 -2 -2 2 -7 -5 -3 3 -13 -1 -2 3z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "3"
                                                         , SvgAttr.name "iwate"
                                                         ]
                                                            ++ fillColor 3 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M698 447l-7 -1 -2 13 -7 -2 -3 5 -7 -5 2 -3h-7l-11 -5 2 -6 -3 -5 2 -3 -8 -13 4 -11 4 -5 -2 -5 3 -4 -3 -5h4l-1 -19 4 -5 3 2 14 -10 2 2 4 -2 5 1 6 -7 7 12 -1 4 4 3 -2 5 7 5 2 14 -2 11 4 -5v5l2 2 -5 5 1 2 4 -3 -1 4h-3l-1 4 -2 1 4 -1 -5 3 2 3 -2 1h4l-6 7 4 2h-5l3 3h-3l2 2 -6 1v-3l1 4h-2v4l-1 -4z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "4"
                                                         , SvgAttr.name "miyagi"
                                                         ]
                                                            ++ fillColor 4 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M666 515l-4 1 1 5h-5l1 2 -5 -1 -1 -6h-9l-4 -5h-6l-1 -5 7 -1 3 -7v-5l7 -12 -5 -11 3 -1 2 -8 -5 -7h5l6 -5 11 5h7l-2 3 7 5 3 -5 7 2 2 -13 7 1 2 7 -5 -3 2 5 -4 4 2 5 -2 -2 -4 3 5 2 -4 6h4v3l-3 -2 2 5h-3l1 3h4l-3 1 3 3 -1 4 -4 -3 1 -2h-2l2 -1 -2 -2 -17 2 -2 4 3 1 -4 2h2l-5 10zM676 487l1 1h-2z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "5"
                                                         , SvgAttr.name "akita"
                                                         ]
                                                            ++ fillColor 5 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M656 449l-6 5h-5l-4 -2 -2 -4 -13 -4 -5 -5 -9 1 2 -9 5 -9 1 -23 -7 -7 -8 2 -3 -8 6 2 6 -8 4 -8v-8l-5 -6 4 1 2 -3 13 1 3 -3 7 5 2 -2 3 2 11 -7v6l5 -1 -1 11 -4 5 1 19h-4l3 5 -3 4 2 5 -4 5 -4 11 8 13 -2 3 3 5z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "6"
                                                         , SvgAttr.name "yamagata"
                                                         ]
                                                            ++ fillColor 6 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M612 440l9 -1 5 5 13 4 2 4 4 2 5 7 -2 8 -3 1 5 11 -7 12v5l-3 7 -7 1 1 5 -1 9 2 2 -3 4 -6 1 -3 -3 -3 1 -3 -4 -6 2 -7 -3 -2 -4 3 -9 -1 -7 9 -4 2 -4 -10 -6 1 -7 -9 -3 13 -22z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "7"
                                                         , SvgAttr.name "fukushima"
                                                         ]
                                                            ++ fillColor 7 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M661 580l-10 -5v5l-6 5 -10 -8 -1 -6 -6 -5 -12 -3 -25 16 -8 -2 1 -14 -5 -4 4 -7 -2 -4 1 -3h9l1 -4h8l-2 -8 9 -11 -3 -2 7 3 6 -2 3 4 3 -1 3 3 6 -1 3 -4 -2 -2 1 -9h6l4 5h9l1 6 5 1 -1 -2h5l-1 -5 4 -1 6 15v26l-2 17z"
                                                            ]
                                                            []
                                                        ]
                                                    ]
                                                , Svg.g
                                                    [ SvgAttr.id "R3"
                                                    , SvgAttr.name "kanto region"
                                                    ]
                                                    [ Svg.g
                                                        ([ SvgAttr.id "8"
                                                         , SvgAttr.name "ibaraki"
                                                         ]
                                                            ++ fillColor 8 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M611 630v0h-2l-2 -7 6 -3 2 -4 3 1 3 -5 11 -2 3 -7 -2 -11 3 -2 -2 -3 1 -10 10 8 6 -5v-5l10 5 -8 24 1 7 -3 6 2 11 5 8 -2 3 2 3 1 -5 7 13 -17 -13 -1 3 -17 5 -11 -6z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "9"
                                                         , SvgAttr.name "tochigi"
                                                         ]
                                                            ++ fillColor 9 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M607 623h-1l-2 -4h-9l-5 -7 6 -13 -8 -2 1 -9 3 -4 -3 -1 2 -4 25 -16 12 3 6 5 1 6 -1 10 2 3 -3 2 2 11 -3 7 -11 2 -3 5 -3 -1 -2 4z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "10"
                                                         , SvgAttr.name "gunma"
                                                         ]
                                                            ++ fillColor 10 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M606 623l-11 1 -5 -4 -11 -2 -5 10 -16 9 -4 -2v-6l-3 -3h3l-2 -7 3 -2 -1 -6h-9l-3 -5 1 -7 5 -4 -1 -2 9 -3 12 -6v-3l3 -1 -1 -5 4 -1 2 -4 7 7 8 2 -2 4 3 1 -3 4 -1 9 8 2 -6 13 5 7h9z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "11"
                                                         , SvgAttr.name "saitama"
                                                         ]
                                                            ++ fillColor 11 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M606 623h1l2 7h2v0l7 13v6l-7 -2 -1 2 -4 -1 -6 3v-3l-8 3 -4 -5 -15 -3 -4 2 -10 -3 -1 -5 16 -9 5 -10 11 2 5 4z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "12"
                                                         , SvgAttr.name "chiba"
                                                         ]
                                                            ++ fillColor 12 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M617 658l2 -4 -1 -5v-6l-7 -13 9 9 11 6 17 -5 1 -3 17 13 1 3 -10 1 -11 10 -3 22 -13 4 -11 14 -7 -4 5 -2 -3 -2 2 -3 -1 -6 2 -4 -4 -5 3 -4 3 1v-4l6 -2 5 -7 -7 -7 -5 4z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "13"
                                                         , SvgAttr.name "tokyo"
                                                         ]
                                                            ++ fillColor 13 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M756 1183l2 4h-2zM618 815v5l-5 -6zM603 756l-2 2 -3 -2 3 -3zM581 746l2 2 -3 1zM588 740l-2 -2 2 -4zM592 717v-6l4 2v5zM618 649l1 5 -2 4 -6 -1 2 5h-4h4v3l-14 -7 -4 2 3 2h-1l-1 5 -3 -5 -14 -5 -5 -3 -5 -9 4 -2 15 3 4 5 8 -3v3l6 -3 4 1 1 -2z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "14"
                                                         , SvgAttr.name "kanagawa"
                                                         ]
                                                            ++ fillColor 14 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M579 657l14 5 3 5 1 -5h1l-3 -2 4 -2 14 7 -4 4h-4l3 4 -3 -1v8l6 2 -5 5 2 2h-4l1 -4 -3 -5 -7 -2 -12 3 -3 3 1 6h-2l-7 -4 1 -12h-5l10 -9z"
                                                            ]
                                                            []
                                                        ]
                                                    ]
                                                , Svg.g
                                                    [ SvgAttr.id "R4"
                                                    , SvgAttr.name "hokuriku region"
                                                    ]
                                                    [ Svg.g
                                                        ([ SvgAttr.id "15"
                                                         , SvgAttr.name "niigata"
                                                         ]
                                                            ++ fillColor 15 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M510 589l-3 -11 -4 -3 14 -5 9 -7h7l15 -12 10 -15 5 -13 19 -13 1 3 -1 -2 7 -6 8 -29 9 3 -1 7 10 6 -2 4 -9 4 1 7 -3 9 2 4 3 2 -9 11 2 8h-8l-1 4h-9l-1 3 2 4 -4 7 5 4 -1 14 -7 -7 -2 4 -4 1 1 5 -3 1v3l-12 6v-7l-5 -4v-4l-3 -3 -7 2 -5 6v4l-4 -2 -8 5 -2 -2v-5l-7 -1v3zM539 522l-8 1 7 -9 -2 -3 -3 2v-5l4 -9 10 -8 -4 15 7 1 -4 9z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "16"
                                                         , SvgAttr.name "toyama"
                                                         ]
                                                            ++ fillColor 16 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M501 612l-10 -4 -4 2v-2l-5 2 -3 -2 -9 11 -1 -4 -4 -2 -4 5 -1 -9 2 -7 -2 -4 3 -4 -2 -3 3 -2 2 -10 8 -3 -3 7 12 7 8 -4 2 -8 10 -3 4 3 3 11 -1 11 -3 1 1 3z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "17"
                                                         , SvgAttr.name "ishikawa"
                                                         ]
                                                            ++ fillColor 17 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M470 567l-3 -3h7zM461 618l3 3 -5 10 -5 1 -5 -5 -10 -1 -6 -9 14 -14 12 -18 1 -11 -2 -4v-5l-3 -1 5 -13 25 -10 4 1 1 4 -6 1 -1 9 -4 -1 -7 7 -2 -3 -3 1 -2 9 6 2 3 -4v9l-8 3 -2 10 -3 2 2 3 -3 4 2 4 -2 7z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "18"
                                                         , SvgAttr.name "fukui"
                                                         ]
                                                            ++ fillColor 18 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M433 617l6 9 10 1 5 5 5 -1 -2 6 5 8 -2 4h-13l-1 3 -7 -3 -5 8 -6 -2 1 8 -3 -1 -1 3 -5 3 -3 -2 -3 7 -4 -1 -2 4 -12 -2 -4 -6 2 -5 2 4 7 -3 -4 3 6 1 3 -3h-2l-1 -3 6 3 -1 -3 3 -1 -2 -3 8 1 -1 -6 3 -3v6l3 1 1 -7 -7 -13 9 -13v-4z"
                                                            ]
                                                            []
                                                        ]
                                                    ]
                                                , Svg.g
                                                    [ SvgAttr.id "R5"
                                                    , SvgAttr.name "chubu region"
                                                    ]
                                                    [ Svg.g
                                                        ([ SvgAttr.id "19"
                                                         , SvgAttr.name "yamanasi"
                                                         ]
                                                            ++ fillColor 19 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M533 659l-2 -5 3 -2 -3 -3 9 -11 6 5 6 -2 4 4 3 -3 10 3 5 9 5 3 -1 8 -10 9 -12 3 -5 -6 -3 8 1 7 -5 2 -4 -9 -6 -1 1 -11z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "20"
                                                         , SvgAttr.name "nagano"
                                                         ]
                                                            ++ fillColor 20 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M556 590l-9 3 1 2 -5 4 -1 7 3 5h9l1 6 -3 2 2 7h-3l3 3v6l4 2 1 5 -3 3 -4 -4 -6 2 -6 -5 -9 11 3 3 -3 2 2 5 -4 5v12l-16 10 -8 -2 -5 3 -1 -6 2 -3 -1 -4h3l-2 -3 2 -4 -6 -6 2 -3 -5 -7 -7 -3 3 -5h5l6 -7 1 -4 -3 -4 5 -10 -3 -6 6 -8 -1 -3 3 -1 1 -11 5 -7v-3l7 1v5l2 2 8 -5 4 2v-4l5 -6 7 -2 3 3v4l5 4z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "21"
                                                         , SvgAttr.name "gifu"
                                                         ]
                                                            ++ fillColor 21 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M501 612l3 6 -5 10 3 4 -1 4 -6 7h-5l-3 5 7 3 5 7 -2 3 6 6 -2 4 2 3h-3l1 4 -2 3 -6 4 -7 -4 -6 2 -5 -2 -6 -8 -10 3 -5 8v6l-7 -7 -6 2 -2 -2 3 -9v-3l-3 -7h-3l-2 -7 5 -8 7 3 1 -3h13l2 -4 -5 -8 2 -6 5 -10 -3 -3 4 -5 4 2 1 4 9 -11 3 2 5 -2v2l4 -2z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "22"
                                                         , SvgAttr.name "shizuoka"
                                                         ]
                                                            ++ fillColor 22 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M568 674h5l-1 12 7 4 -3 5 5 6 -1 4 -7 10v5l-8 4 -5 -6 2 -14 -1 -3 1 -4 6 -1 -11 -6 -7 3 -3 4 1 3 -8 4 -3 9 -5 7 1 4 -13 -4 -25 -1v-8l8 -5 10 -20 16 -10v-12l4 -5 2 8 -1 11 6 1 4 9 5 -2 -1 -7 3 -8 5 6z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "23"
                                                         , SvgAttr.name "aichi"
                                                         ]
                                                            ++ fillColor 23 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M495 719l-24 6 3 -5 2 2 9 -6 1 3 3 -3 -1 -3 -6 -3 -2 4 -8 -1 -4 -3 2 -8 -4 11 3 4h-2l-5 -3 2 -5 -3 -3v-5l4 -7 -6 3 -5 -7v-6l5 -8 10 -3 6 8 5 2 6 -2 7 4 6 -4 1 6 5 -3 8 2 -10 20 -8 5z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "24"
                                                         , SvgAttr.name "mie"
                                                         ]
                                                            ++ fillColor 24 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M418 778l-5 -3 -2 -7 1 1v-3h1l6 -3 -1 -3 6 -1v-3l-1 -7 2 -4 -2 -3 2 -1 -3 -5 9 -6 -10 -6 2 -3 -2 -2h2l-1 -4 -2 -4 4 -1 1 -6 7 3 6 -3 4 -10 -1 -9 6 -2 7 7 5 7 -6 2v4l-7 12v8l16 7 2 5 2 -2v5h-5h5l-1 6h-3l1 -1 -3 -2v2h-6l2 -3h-3l-3 5 -1 -2 -2 2 -2 -1v3l-2 -2 -8 3 -2 3 1 4h-3l1 -2 -3 3 4 3 -2 1 1 3 -3 -2 1 3 -7 4z"
                                                            ]
                                                            []
                                                        ]
                                                    ]
                                                , Svg.g
                                                    [ SvgAttr.id "R6"
                                                    , SvgAttr.name "kinki region"
                                                    ]
                                                    [ Svg.g
                                                        ([ SvgAttr.id "25"
                                                         , SvgAttr.name "shiga"
                                                         ]
                                                            ++ fillColor 25 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M434 657l2 7h3l3 7v3l-3 9 2 2 1 9 -4 10 -6 3 -7 -3 -1 6 -4 1 -4 -6h-4l-2 -10 2 -15 -4 -4 2 -4 4 1 3 -7 3 2 5 -3 1 -3 3 1 -1 -8z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "26"
                                                         , SvgAttr.name "kyoto"
                                                         ]
                                                            ++ fillColor 26 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M405 712l-2 -8 -4 -2v-2l-3 3 -9 -8 1 -6 -9 -2 -2 -6 -5 1 -7 -5v-6l6 1v-7v-2l-6 2 -3 -5 1 -4 2 3 -1 -3 17 -7 4 7 -8 6 2 2 3 -4 -1 4 5 1 -1 4 2 -2 2 1 -3 -5 7 -3 1 3 -2 5 4 6 12 2 4 4 -2 15 2 10h4l4 6 2 4 -2 1 -5 -3 -4 3z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "27"
                                                         , SvgAttr.name "osaka"
                                                         ]
                                                            ++ fillColor 27 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M387 695l9 8 3 -3v2l4 2 2 8 -4 10 2 10 -2 4 -26 6 -3 -2 6 -2 7 -6 3 -10 3 3 -3 -9 3 -2 -2 -11 3 -1 -6 -2 -1 -5z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "28"
                                                         , SvgAttr.name "hyogo"
                                                         ]
                                                            ++ fillColor 28 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M360 744l-7 3 -1 -4 -3 1v-4l19 -19 1 3 -7 9 2 9zM333 713l1 -3 -4 -5 2 -3 -2 -7 8 -9v-5l6 -2v-5l-7 -17 9 -3 17 2 -1 4 3 5 6 -2v2v7l-6 -1v6l7 5 5 -1 2 6 9 2 -1 6h-2l1 5 6 2 -3 1 2 11 -3 2 -5 -1 -13 5 -15 -9h-14l-1 -3 -2 5 -3 -2z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "29"
                                                         , SvgAttr.name "nara"
                                                         ]
                                                            ++ fillColor 29 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M418 760l-4 2 -1 4h-1v3l-1 -1 -14 -2 2 -5 -4 -7 5 -7 4 -1 -3 -10 2 -4 -2 -10 4 -10 6 4 4 -3 5 3 2 -1 1 4h-2l2 2 -2 3 10 6 -9 6 3 5 -2 1 2 3 -2 4 1 7v3z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "30"
                                                         , SvgAttr.name "wakayama"
                                                         ]
                                                            ++ fillColor 30 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M411 768l2 7 5 3 -4 5 2 4 -9 5 -1 3h-1v-2l-16 -5 -6 -8 4 -3 -9 -4 -5 -6h-4l3 -5 -2 -1 5 -3 -4 -3 3 -2 -1 -2 5 -1 -8 -7 2 -3 3 2 26 -6 3 10 -4 1 -5 7 4 7 -2 5zM418 760l1 3 -6 3 1 -4z"
                                                            ]
                                                            []
                                                        ]
                                                    ]
                                                , Svg.g
                                                    [ SvgAttr.id "R7"
                                                    , SvgAttr.name "chugoku region"
                                                    ]
                                                    [ Svg.g
                                                        ([ SvgAttr.id "31"
                                                         , SvgAttr.name "tottori"
                                                         ]
                                                            ++ fillColor 31 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M280 690l-7 -2 3 -5 -2 -4 8 -2 1 -9 -6 -8 3 -1 2 4 6 2 8 -4 11 2 21 -1 9 -5 7 17v5l-6 2 -12 4 -1 -7 -7 -2 1 -3 -10 6 -4 -4 -8 -2 -2 6 -3 1 1 3 -6 -1v4l-6 1z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "32"
                                                         , SvgAttr.name "shimane"
                                                         ]
                                                            ++ fillColor 32 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M269 624l5 -2 -3 5 -3 -2 1 3 -3 -3zM275 626l-3 2v-4l3 -1 2 3zM279 615l5 -6 5 5 -1 4 -3 1 2 2h-5zM198 709l8 -2 14 -14 11 -6 6 -8 11 -5 2 -6 -2 -3 18 -5 6 -5 3 3h8l-3 1 -3 1 6 8 -1 9 -8 2 2 4 -3 5 -13 -2 -7 9 -6 3 3 4 -8 2 -6 -2 -2 3 -8 -2 -6 5 2 2 -2 6 -5 5 1 2 -3 2v4l-3 4 -2 -2 -5 2 -3 -4 2 -5h-5l-1 -4 3 -5z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "33"
                                                         , SvgAttr.name "okayama"
                                                         ]
                                                            ++ fillColor 33 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M338 681v5l-8 9 2 7 -2 3 4 5 -1 3 -7 -1 3 2 -7 7 -9 -1 5 2 -2 4 -2 -1 -2 4 -5 -2 -2 3 -3 -6v4l-3 -5 -4 4 -6 -3 2 4 -4 -2 -4 -16 2 -4 -4 -6 1 -7 -2 -3 1 -3 6 -1v-4l6 1 -1 -3 3 -1 2 -6 8 2 4 4 10 -6 -1 3 7 2 1 7z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "34"
                                                         , SvgAttr.name "hiroshima"
                                                         ]
                                                            ++ fillColor 34 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M287 726v2l-3 -2 3 3h-2l-2 4 -6 -2v-3l-9 2 -2 5h-13l-2 4 -6 3 -2 -2 -3 2 1 -3 -3 -6 1 -2h-8l-7 6 1 3 -6 -2v-6l-3 -2v-7l-1 -2 5 -5 2 -6 -2 -2 6 -5 8 2 2 -3 6 2 8 -2 -3 -4 6 -3 7 -9 13 2 7 2 2 3 -1 7 4 6 -2 4zM260 740l-3 2 -2 -2 5 -3zM236 740l-1 -4 3 1 -1 8h-3l1 -3 -4 -5zM229 734l1 2 -4 3zM249 744l-2 -2h4zM239 744h2l-2 3h3l-2 2 -5 -1 3 -6h2zM273 735l1 3 -3 -3zM271 736l-4 3 1 -3zM275 730l1 1 -3 2z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "35"
                                                         , SvgAttr.name "yamaguchi"
                                                         ]
                                                            ++ fillColor 35 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M198 709l1 6 -3 5 1 4h5l-2 5 3 4 5 -2 2 2 3 -4v-4l3 -2v7l3 2v6l6 2 -1 5h-2v8l-5 3 1 7 -4 -6h-5l-5 -6 -4 2 2 -3 -4 -3 -10 4 -4 -3 -2 3v-2l-2 2v-4l-2 5 -5 2 -2 -2 -2 2 -2 -5 -5 -4 -7 7v-7l-2 -4 4 -6 -3 -5 2 -4h7l-5 -2 2 -3 8 2 1 3h8l6 -2 -1 -3 11 -11 3 1zM217 763l-5 2 2 -4zM228 759l7 -2 -5 2v3l-4 -3 -5 3 -1 -5 2 -2zM171 724l-1 -2 5 1zM204 755h-3l3 -3z"
                                                            ]
                                                            []
                                                        ]
                                                    ]
                                                , Svg.g
                                                    [ SvgAttr.id "R8"
                                                    , SvgAttr.name "shikoku region"
                                                    ]
                                                    [ Svg.g
                                                        ([ SvgAttr.id "36"
                                                         , SvgAttr.name "tokushima"
                                                         ]
                                                            ++ fillColor 36 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M346 745l2 -1 -1 3zM338 745l7 -1v2l3 1 -4 9 7 7 -4 5 6 1 -19 11 -5 6 -5 -1 -3 -4 2 -3 -6 -1 -1 -9 -6 2 -13 -6 1 -8 10 -5 6 2 10 -6 12 2z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "37"
                                                         , SvgAttr.name "kagawa"
                                                         ]
                                                            ++ fillColor 37 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M298 756l-4 -2 3 -9 -5 -5 6 3 14 -10 7 3 4 -3 1 4 2 -2 3 2v3l9 5 -2 4 -12 -2 -10 6 -6 -2zM319 728l-1 -1h3zM333 723h2l-2 8 -3 -1 1 -2 -4 4 1 -4 -4 -2z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "38"
                                                         , SvgAttr.name "ehime"
                                                         ]
                                                            ++ fillColor 38 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M243 757l-2 -1 4 -3zM264 742h-4l4 -6 3 3zM269 741l2 1 -2 1 -3 -1zM267 746l-3 2 1 -5 4 1zM242 821l-8 -2 1 3h-3l1 -5 4 1 -6 -5 2 -1 -2 -4h4l-4 -1 2 -2 -3 -2 4 3 4 -4 -1 -3h-3l2 -3h-8l3 -2 -2 -2 2 -5 -6 -1 -8 6 -7 1 34 -20 7 -18 8 -4 -1 -4 2 -1 9 14 10 -4 10 2 5 -4 4 2 -1 8 -25 4 -6 9 -4 10 -11 1 5 9 -6 3 -5 8 -4 -3 3 13z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "39"
                                                         , SvgAttr.name "kochi"
                                                         ]
                                                            ++ fillColor 39 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M297 764l13 6 6 -2 1 9 6 1 -2 3 3 4 5 1 -7 19 -12 -16 -10 -3 -9 2 -1 -3 1 3 -12 4h6l-6 1 -1 3 -2 -3 -3 5 1 4 -2 6h-3l-4 8 -5 1v8l-3 3 3 7 -4 -5 -7 3 -5 -4 -4 2 2 -6 3 -1v-3h-3l2 -3 -3 -13 4 3 5 -8 6 -3 -5 -9 11 -1 4 -10 6 -9zM237 833l-2 2v-4z"
                                                            ]
                                                            []
                                                        ]
                                                    ]
                                                , Svg.g
                                                    [ SvgAttr.id "R9"
                                                    , SvgAttr.name "kyushu region"
                                                    ]
                                                    [ Svg.g
                                                        ([ SvgAttr.id "40"
                                                         , SvgAttr.name "fukuoka"
                                                         ]
                                                            ++ fillColor 40 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M148 804l-8 -5 -2 4 -4 -1 -4 4v3l-5 -1v-5l-2 -3 1 -7 9 -5v-6l-6 2 -7 -6h-12l6 -4 -3 -3 6 -5 3 6 7 -1 2 -5 -4 2 -3 -3 5 1 4 -3 1 -8 10 -1 1 -3 6 1 -3 3 3 1 1 -3 4 2 7 -4 -4 8 2 2 -1 1 5 10 5 1 -1 7 -8 -1 -7 4 -4 6 2 5 -2 1 3 4z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "41"
                                                         , SvgAttr.name "saga"
                                                         ]
                                                            ++ fillColor 41 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M108 778h12l7 6 6 -2v6l-9 5 -1 7 -6 -5 -5 5 2 10 -7 -2 -7 -7 1 -5 -6 -2 -3 -6 2 -3 4 4 -2 -4 3 -4 -5 -3 2 -2 2 2v-6l1 2h5v4z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "42"
                                                         , SvgAttr.name "nagasaki"
                                                         ]
                                                            ++ fillColor 42 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M81 700l3 -2 2 3 -4 3 2 4 -6 6 -1 4 3 -2 -3 4 3 -1 -3 4v-5l-2 3v-4l-2 3 -3 -1 3 -1v-4l2 -1 -2 -1 4 -3 -2 -3 2 -5 4 1zM75 727l-3 6 -6 1 3 -14 1 3 3 1 -1 -3 2 3 2 -1zM95 758l1 2 -4 2 -3 -5 2 1 -2 -3 3 -3 4 1 -2 3 3 1zM56 809l-3 5 2 -6 -4 -2 3 -1v-4h2l2 -9 -2 11 5 1 -2 3 -2 -2zM52 810h-3v-2l2 1 -1 -3zM47 812l1 -3v5l-4 -4zM35 817l3 -2 1 2 2 -3 3 10 -6 -1v4l-9 -3 1 -2 1 3 1 -2h-1l2 -2 -1 -7zM94 778l-3 2 2 -4zM96 781l2 2 -3 1zM58 786l2 -2 1 2zM83 775h-3l3 -2zM94 785l-2 3 3 6 6 2 -1 5 7 7 7 2 -7 6 13 1 2 5 -2 6 -10 5 -1 -6 4 -4 -1 -3h-5l-7 1 -3 6 -9 5 4 -5 -1 -2h3l2 -3 -3 1 -2 -6 -4 -1 -3 -6 3 -11 4 4 -1 3 1 -2 3 3 -2 7 11 3 -3 -6 1 -5 -4 -4 -3 2v-7l-4 1v-3l-3 4 2 -2 -4 -2 1 -4 -1 2 -4 -2 4 -5 -3 -1 1 -4 5 -1 1 3h5zM78 786l-1 5 -6 1 4 -2 -1 -2 2 -6 5 1 1 -4 1 4zM43 814l1 -3 2 3 -3 2zM92 799v4l-2 -4 2 -2z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "43"
                                                         , SvgAttr.name "kumamoto"
                                                         ]
                                                            ++ fillColor 43 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M108 836h4v11l-8 9 -4 1v-6l4 -1 -5 -2 5 -9 -2 -3zM119 846h-6l-1 -4 6 -4h8l-4 8zM123 832l3 1 -1 4 -2 -1zM137 866l-5 -6 -9 4 -4 -4 12 -16 -2 -5 2 1 -1 -2 6 -6 -10 1 8 -6 1 -5 -9 -8 -1 -6 5 1v-3l4 -4 4 1 2 -4 8 5 7 5 2 -4 -1 -6 6 1 7 13v6l3 3 -3 1 -9 14 -5 2 -1 7 5 7 -4 5 3 5z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "44"
                                                         , SvgAttr.name "oita"
                                                         ]
                                                            ++ fillColor 44 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M148 804l3 -5 -3 -4 2 -1 -2 -5 4 -6 7 -4 8 1 1 -7 12 4 5 -7 9 4 2 6 -2 8 -4 -1 1 3 -8 2v4l4 2 17 -1 -6 9h5l-3 2 3 2 5 -2 1 3h-5l-2 4 10 4h-6l2 2 -4 3 4 1 -7 1v4h-2l1 -5 -5 -1 -3 4h-8l-4 -5 -6 1 -2 -2 -3 -3v-6l-7 -13 -6 -1 1 6 -2 4z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "45"
                                                         , SvgAttr.name "miyazaki"
                                                         ]
                                                            ++ fillColor 45 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M172 822l2 2 6 -1 4 5h8l3 -4 5 1 -1 5h2l-11 10 -1 4 3 1 -4 1 -1 2 2 1 -5 8 -6 17 -3 23 -5 5 -2 12 -5 -2 -5 -5 2 -3v-7l-7 -1 -3 -9 -5 -2 2 -5 -6 -5 -5 -7 1 -2 21 -3 -3 -5 4 -5 -5 -7 1 -7 5 -2 9 -14z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "46"
                                                         , SvgAttr.name "kagoshima"
                                                         ]
                                                            ++ fillColor 46 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M4 1148l-4 2v-5l9 -1zM21 1126v-8l4 -1v5l3 3 -1 3 -5 3 -2 -4zM82 1097l-2 2 -1 -3 6 -3zM64 1089l-13 9 3 2 -4 4 -3 -2 1 4 -5 -5 1 -2 -8 -3h7l2 -1 -5 -2 14 -5 2 2 3 -5h4l-2 3 2 -2 2 2 2 -6 1 6zM43 1107l-1 -2 -3 2 -1 -7 5 2 -2 3 6 1 -2 3v-2zM103 965l3 2h-3zM113 976l-1 -6 7 -4 9 6 -1 5 -6 4 -6 -1zM144 969v5l-6 1v-8l3 -3 2 -9 7 -10 1 9zM119 860l4 4 9 -4 5 6 -1 2 5 7 6 5 -2 5 5 2 3 9 7 1v7l-2 3 -3 -1 -5 6 5 3 -2 3h3l-6 3 -4 5 -16 8v-5l5 -3 4 -13 -5 -8v-6l-6 -2 5 -2 1 4h3l4 -6 -7 -5 -4 2 -6 13 2 11 5 3 -2 5 -6 1 -3 -6 -13 -1 1 -3 -3 -2 2 -1 -5 -5 9 1 4 -11 -3 -8 -5 -5 3 -7 -2 -11 4 -2 3 2zM112 853l2 -1 -1 2zM107 863l-2 -5 4 -3 2 5zM85 884l2 -3 1 2 -6 8 -2 -1zM93 880l-4 -3h6zM75 1012l-3 4v-3zM81 1001l2 -2 2 4z"
                                                            ]
                                                            []
                                                        ]
                                                    , Svg.g
                                                        ([ SvgAttr.id "47"
                                                         , SvgAttr.name "okinawa"
                                                         ]
                                                            ++ fillColor 47 prefectures
                                                        )
                                                        [ Svg.path
                                                            [ SvgAttr.d "M-321 1283l3 1 -2 1 -5 -1zM-276 1295l6 3 -4 6 -11 -5 2 -2 3 2 1 -7zM-198 1281l-1 -4 3 2zM-192 1287l-3 -2h2l-1 -3 2 -2 -1 -5 3 8 7 6zM-100 1198v5l-5 -5 3 -2zM143 1255l-1 -3h2zM-45 1183v-2l3 2zM-31 1187v2l-2 -2zM-252 1292l5 -6 1 2 -6 6v7h-6l-2 -2 2 -2 -3 -3 8 1zM-37 1195l3 -3 -5 -2v-6l6 1 1 4 6 -1 -1 -2 10 -10 3 4 -1 5 -5 6 -5 -1 -1 5 -5 -1 1 2 -7 5 -6 -1 4 9 -4 -2 -5 7 4 3 -10 4 -1 -8 7 -6 -2 -8 5 1z"
                                                            ]
                                                            []
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    , Svg.g []
                                        [ Svg.use
                                            [ SvgAttr.clipPath "url(#main)"
                                            , SvgAttr.x "0"
                                            , SvgAttr.y "0"
                                            , SvgAttr.xlinkHref "#ground"
                                            ]
                                            []
                                        , Svg.use
                                            [ SvgAttr.clipPath "url(#sub)"
                                            , SvgAttr.x "384"
                                            , SvgAttr.y "-1024"
                                            , SvgAttr.xlinkHref "#ground"
                                            ]
                                            []
                                        ]
                                    , Svg.path
                                        [ SvgAttr.d "M512 0A512 512 0 0 1 0 512"
                                        , SvgAttr.fill "none"
                                        , SvgAttr.stroke "white"
                                        , SvgAttr.strokeWidth "5"
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        , Html.div [ Attr.class "stats stats-vertical sm:stats-horizontal bg-base-100 shadow-xl w-full md:order-3" ]
                            [ Html.div [ Attr.class "stat place-items-center py-2" ]
                                [ Html.div [ Attr.class "stat-title text-xs" ] [ Html.text "Correct" ]
                                , Html.div [ Attr.class "stat-value text-success text-xl md:text-4xl transition-all duration-300" ]
                                    [ Html.text <| "✅ " ++ String.fromInt score.correct ]
                                ]
                            , Html.div [ Attr.class "stat place-items-center py-2" ]
                                [ Html.div [ Attr.class "stat-title text-xs" ] [ Html.text "Incorrect" ]
                                , Html.div [ Attr.class "stat-value text-error text-xl md:text-4xl transition-all duration-300" ]
                                    [ Html.text <| "❌ " ++ String.fromInt score.failed ]
                                ]
                            , Html.div [ Attr.class "stat place-items-center py-2" ]
                                [ Html.div [ Attr.class "stat-title text-xs" ] [ Html.text "Streak" ]
                                , Html.div
                                    [ Attr.class <|
                                        "stat-value text-primary text-xl md:text-4xl transition-all duration-300 "
                                            ++ (if score.streak > 0 then
                                                    "animate-pulse scale-110"

                                                else
                                                    ""
                                               )
                                    ]
                                    [ Html.text <|
                                        (if score.streak > 2 then
                                            "🔥 "

                                         else
                                            ""
                                        )
                                            ++ String.fromInt score.streak
                                    ]
                                ]
                            , Html.div [ Attr.class "stat place-items-center py-2" ]
                                [ Html.div [ Attr.class "stat-title text-xs" ] [ Html.text "Best" ]
                                , Html.div
                                    [ Attr.class "stat-value text-secondary text-xl md:text-4xl transition-all duration-500" ]
                                    [ Html.text <|
                                        (if score.maxStreak > 2 then
                                            "🔥 "

                                         else
                                            ""
                                        )
                                            ++ String.fromInt score.maxStreak
                                    ]
                                ]
                            ]
                        ]
                    ]

                Finished { correct, failed, maxStreak } tray ->
                    [ Html.div
                        [ Attr.class "toast toast-top toast-center z-50" ]
                        [ Toast.render viewToast tray (Toast.config ToastMsg) ]
                    , Html.div
                        [ Attr.class "card w-full max-w-md bg-base-100 shadow-2xl animate-in zoom-in duration-500" ]
                        [ Html.div
                            [ Attr.class "card-body p-6 md:p-8"
                            ]
                            [ Html.h2
                                [ Attr.class "card-title text-2xl md:text-4xl text-center justify-center mb-6 animate-in slide-in-from-top duration-700"
                                ]
                                [ Html.text "Congratulations! 🎉" ]
                            , Html.div [ Attr.class "stats stats-vertical shadow w-full mb-4" ]
                                [ Html.div [ Attr.class "stat animate-in slide-in-from-left duration-500 delay-200" ]
                                    [ Html.div [ Attr.class "stat-figure text-success" ]
                                        [ Html.div [ Attr.class "text-4xl" ] [ Html.text "✅" ] ]
                                    , Html.div [ Attr.class "stat-title" ] [ Html.text "Correct" ]
                                    , Html.div [ Attr.class "stat-value text-success" ] [ Html.text <| String.fromInt correct ]
                                    ]
                                , Html.div [ Attr.class "stat animate-in slide-in-from-right duration-500 delay-300" ]
                                    [ Html.div [ Attr.class "stat-figure text-error" ]
                                        [ Html.div [ Attr.class "text-4xl" ] [ Html.text "❌" ] ]
                                    , Html.div [ Attr.class "stat-title" ] [ Html.text "Incorrect" ]
                                    , Html.div [ Attr.class "stat-value text-error" ] [ Html.text <| String.fromInt failed ]
                                    ]
                                , Html.div [ Attr.class "stat animate-in slide-in-from-bottom duration-500 delay-400" ]
                                    [ Html.div [ Attr.class "stat-figure text-warning" ]
                                        [ Html.div [ Attr.class "text-4xl animate-pulse" ] [ Html.text "🔥" ] ]
                                    , Html.div [ Attr.class "stat-title" ] [ Html.text "Best Streak" ]
                                    , Html.div [ Attr.class "stat-value text-warning" ] [ Html.text <| String.fromInt maxStreak ]
                                    ]
                                ]
                            , Html.div [ Attr.class "card-actions justify-center mt-4" ]
                                [ Html.button
                                    [ Attr.class "btn btn-primary btn-wide"
                                    , Events.onClick Restart
                                    ]
                                    [ Html.text "🚀 Play Again" ]
                                ]
                            ]
                        ]
                    ]
        , Html.footer [ Attr.class "footer footer-center p-4 bg-base-300 text-base-content rounded-b-lg mt-8" ]
            [ Html.div [ Attr.class "text-base-content/50" ]
                [ Html.text "Map SVG © Lincun (modifications by Erida539, YasutoTakenaka and Flavio Corpa)" ]
            ]
        ]
