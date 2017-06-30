module MultiEmailInput exposing (Msg(..), State, init, isEmailValid, update, view)

{-| A component to input multiple emails and display/manage them comfortably. It tokenizes emails on tab, space, enter and blur. It also allows you to paste emails in bulk, remove existing emails or go back and ammend the previous one.


# Main workflow

@docs Msg, State, init, update, view


# Extras

@docs isEmailValid

-}

import Dom
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Json.Decode as Json
import Regex exposing (Regex)
import Set
import String
import Task


{-| Internal messages to manage the component's state.
-}
type Msg
    = FocusElement
    | TextareaFocused
    | TextareaBlurred String
    | KeyDown Int
    | RemoveEmail String
    | InputChanged String


{-| Component's internal state.
-}
type alias State =
    { nextEmail : String
    , id : String
    }


{-| Initialize the component's state. It needs the component's ID so that it can refocus the textarea when new emails are inserted. By default, we begin with an empty textarea.
-}
init : String -> State
init id =
    { nextEmail = ""
    , id = id
    }


{-| Updates the component's state and a supplied list of emails.

Given a particular change on the input (e.g. a list of emails have been pasted, the component has lost focus, a special key has been pressed...) it will update the list of distinct emails and the current state of the component.

-}
update : Msg -> State -> List String -> ( State, List String, Cmd Msg )
update msg state emails =
    let
        nextEmailIsEmpty =
            state.nextEmail == ""

        noChanges =
            ( state, emails, Cmd.none )

        refocus =
            Task.attempt (\_ -> TextareaFocused) (Dom.focus state.id)
    in
    case msg of
        FocusElement ->
            ( state, emails, refocus )

        KeyDown key ->
            case toSpecialKey key of
                Tab ->
                    if nextEmailIsEmpty then
                        noChanges
                    else
                        ( { state | nextEmail = "" }, dropDuplicates (emails ++ [ state.nextEmail ]), refocus )

                Backspace ->
                    if nextEmailIsEmpty then
                        case emails |> List.reverse |> List.head of
                            Just previousEmail ->
                                ( { state | nextEmail = previousEmail }, emails |> List.take (List.length emails - 1), refocus )

                            Nothing ->
                                noChanges
                    else
                        noChanges

                Other ->
                    noChanges

        InputChanged text ->
            let
                newElements =
                    text |> Regex.split Regex.All separator

                ( newTokens, nextToken ) =
                    ( newElements |> List.take (List.length newElements - 1) |> List.filter (not << String.isEmpty)
                    , newElements |> List.drop (List.length newElements - 1) |> List.head |> Maybe.withDefault ""
                    )
            in
            ( { state | nextEmail = nextToken }, dropDuplicates (emails ++ newTokens), refocus )

        RemoveEmail email ->
            ( state, List.filter ((/=) email) emails, Cmd.none )

        TextareaFocused ->
            noChanges

        TextareaBlurred email ->
            if email /= "" then
                ( { state | nextEmail = "" }, dropDuplicates (emails ++ [ email ]), Cmd.none )
            else
                noChanges


{-| Renders the component visually.

       MultiEmailInput.view MultiEmailInputMsg [] "Write a placeholder here" model.inputEmails model.inputEmailsState

See README for actual examples.

-}
view : (Msg -> msg) -> List (Html.Attribute msg) -> String -> List String -> State -> Html msg
view toOuterMsg customAttributes placeholder emails state =
    Html.div [ Attr.class "multi-email-input-container" ]
        [ Html.ul [ Attr.class "multi-email-input-list", Ev.onClick (toOuterMsg FocusElement) ]
            ((emails |> List.map (viewToken toOuterMsg))
                ++ [ Html.li [ Attr.class "multi-email-input-list-item" ] [ viewExpandingTextArea toOuterMsg customAttributes placeholder state ]
                   ]
            )
        ]


{-| Renders an expanding text area (that is, a textarea element inspired by [this article](https://alistapart.com/article/expanding-text-areas-made-elegant)) used to hold the next email
-}
viewExpandingTextArea : (Msg -> msg) -> List (Html.Attribute msg) -> String -> State -> Html msg
viewExpandingTextArea toOuterMsg customAttributes placeholder state =
    Html.div [ Attr.class "multi-email-input-expanding-area" ]
        [ Html.pre []
            [ Html.span []
                [ Html.text <|
                    if state.nextEmail /= "" then
                        state.nextEmail
                    else
                        placeholder
                ]
            , Html.br [] []
            ]
        , Html.textarea
            ([ Attr.value state.nextEmail
             , Attr.placeholder placeholder
             , Attr.rows 1
             , Attr.id state.id
             , Ev.onInput (toOuterMsg << InputChanged)
             , Ev.onBlur (toOuterMsg <| TextareaBlurred state.nextEmail)
             , onKeyDown (toOuterMsg << KeyDown)
             ]
                ++ customAttributes
            )
            []
        ]


{-| Describes a separate email (usually visualized as a capsule)
-}
viewToken : (Msg -> msg) -> String -> Html msg
viewToken toOuterMsg email =
    Html.li
        [ Attr.classList
            [ ( "multi-email-input-token", True )
            , ( "multi-email-input-token-invalid", not (isEmailValid email) )
            ]
        ]
        [ Html.p []
            [ Html.text email
            , Html.i [ Attr.class "multi-email-input-delete-button", Ev.onClick (toOuterMsg <| RemoveEmail email) ] [ Html.text "" ]
            ]
        ]


type SpecialKey
    = Tab
    | Backspace
    | Other


toSpecialKey : Int -> SpecialKey
toSpecialKey keyCode =
    case keyCode of
        8 ->
            Backspace

        9 ->
            Tab

        _ ->
            Other


{-| The regex that we use to separate an email from another
-}
separator : Regex
separator =
    Regex.regex ",| |\t|\n"


onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown toMsg =
    Ev.on "keydown" <| Json.map toMsg Ev.keyCode


{-| Drop the duplicates in a list. It preserves the original order, keeping only the first
-}
dropDuplicates : List comparable -> List comparable
dropDuplicates list =
    let
        step next ( set, acc ) =
            if Set.member next set then
                ( set, acc )
            else
                ( Set.insert next set, next :: acc )
    in
    List.foldl step ( Set.empty, [] ) list |> Tuple.second |> List.reverse


{-| Regex to validate emails
-}
validEmail : Regex
validEmail =
    Regex.regex ".+@.+\\..+"


{-| Returns true only if the email is valid. This component validates emails against quite a basic regular expression, following the dont-waste-your-time rule in [this article](http://www.regular-expressions.info/email.html) and many others.

If you have a use case for using a custom regular expression, we could have the component receive it as a parameter. PRs are welcome!

-}
isEmailValid : String -> Bool
isEmailValid =
    Regex.find (Regex.AtMost 1) validEmail
        >> (not << List.isEmpty)
