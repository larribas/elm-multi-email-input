module Main exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import MultiEmailInput
import Test exposing (..)


suite : Test
suite =
    describe "MultiEmailInput"
        [ describe "isEmailValid"
            [ test "validates correct email addresses" <|
                \_ ->
                    let
                        validEmails =
                            [ "simple@email.com"
                            , "Abc\\@def@example.com"
                            , "Fred\\ Bloggs@example.com"
                            , "Joe.\\Blow@example.com"
                            , "\"Abc@def\"@example.com"
                            , "\"Fred Bloggs\"@example.com"
                            , "customer/department=shipping@example.com"
                            , "$A12345@example.com"
                            , "!def!xyz%abc@example.com"
                            , "_somename@example.com"
                            ]
                    in
                    all (Expect.true "" << MultiEmailInput.isEmailValid) (\x -> "Expected " ++ x ++ " to be valid") validEmails
            , test "does not validate incorrect email addresses" <|
                \_ ->
                    let
                        invalidEmails =
                            [ "invalid"
                            , "invalid@"
                            , "invalid @"
                            , "invalid@domainless"
                            , "domain.com"
                            , "@."
                            ]
                    in
                    all (Expect.false "" << MultiEmailInput.isEmailValid) (\x -> "Expected " ++ x ++ " not to be valid") invalidEmails
            ]
        , describe "update"
            [ test "a tab tokenizes the current email" <|
                \_ ->
                    let
                        email =
                            "current@email.com"

                        ( state, emails, _ ) =
                            MultiEmailInput.update (MultiEmailInput.KeyDown 9) { nextEmail = email, id = "id" } []
                    in
                    Expect.equal ( "", [ email ] ) ( state.nextEmail, emails )
            , test "a backspace removes the last token and goes into edit mode" <|
                \_ ->
                    let
                        ( state, emails, _ ) =
                            MultiEmailInput.update (MultiEmailInput.KeyDown 8) { nextEmail = "", id = "id" } [ "first@email.com", "previous@email.com" ]
                    in
                    Expect.equal ( "previous@email.com", [ "first@email.com" ] ) ( state.nextEmail, emails )
            , test "a backspace does nothing special when there are no tokens" <|
                \_ ->
                    let
                        ( state, emails, _ ) =
                            MultiEmailInput.update (MultiEmailInput.KeyDown 8) { nextEmail = "", id = "id" } []
                    in
                    Expect.equal ( "", [] ) ( state.nextEmail, emails )
            , test "a backspace does nothing special when there is a current email" <|
                \_ ->
                    let
                        ( state, emails, _ ) =
                            MultiEmailInput.update (MultiEmailInput.KeyDown 8) { nextEmail = "something", id = "id" } [ "other@email.com" ]
                    in
                    Expect.equal ( "something", [ "other@email.com" ] ) ( state.nextEmail, emails )
            , test "any nonspecial key means induces no changes" <|
                \_ ->
                    let
                        ( state, emails, _ ) =
                            MultiEmailInput.update (MultiEmailInput.KeyDown 4) { nextEmail = "something", id = "id" } []
                    in
                    Expect.equal ( "something", [] ) ( state.nextEmail, emails )
            , test "removing an email" <|
                \_ ->
                    let
                        ( state, emails, _ ) =
                            MultiEmailInput.update (MultiEmailInput.RemoveEmail "two@email.com") { nextEmail = "something", id = "id" } [ "one@email.com", "two@email.com", "three@email.com" ]
                    in
                    Expect.equal ( "something", [ "one@email.com", "three@email.com" ] ) ( state.nextEmail, emails )
            , test "the current email is tokenized when it loses focus" <|
                \_ ->
                    let
                        ( state, emails, _ ) =
                            MultiEmailInput.update (MultiEmailInput.TextareaBlurred "halfway") { nextEmail = "", id = "id" } []
                    in
                    Expect.equal ( "", [ "halfway" ] ) ( state.nextEmail, emails )
            , test "no new tokens are added when the current email is empty" <|
                \_ ->
                    let
                        messages =
                            [ MultiEmailInput.KeyDown 9
                            , MultiEmailInput.TextareaBlurred ""
                            ]

                        updateWhenEmpty msg =
                            let
                                ( state, emails, _ ) =
                                    MultiEmailInput.update msg { nextEmail = "", id = "id" } []
                            in
                            ( state.nextEmail, emails )
                    in
                    all (Expect.equal ( "", [] ) << updateWhenEmpty) (\_ -> "Expected that no new tokens were created") messages
            , test "when the input changes all the emails are tokenized and druplicates dropped" <|
                \_ ->
                    let
                        nextInput =
                            "one two\tthree\nfour, five,six,,,seven eight\n\n\neight\nnine"

                        ( state, emails, _ ) =
                            MultiEmailInput.update (MultiEmailInput.InputChanged nextInput) { nextEmail = "", id = "id" } [ "previous" ]

                        expectedEmails =
                            ( "nine", [ "previous", "one", "two", "three", "four", "five", "six", "seven", "eight" ] )
                    in
                    Expect.equal expectedEmails ( state.nextEmail, emails )
            ]
        ]


all : (a -> Expectation) -> (a -> String) -> List a -> Expectation
all expectation message cases =
    case List.head cases of
        Just head ->
            if expectation head == Expect.pass then
                all expectation message (Maybe.withDefault [] <| List.tail cases)
            else
                Expect.fail <| message head

        Nothing ->
            Expect.pass
