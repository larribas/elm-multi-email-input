module Demo exposing (main)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import MultiEmailInput


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { classicEmails : List String
    , classicInputState : MultiEmailInput.State
    , customStylesEmails : List String
    , customStylesInputState : MultiEmailInput.State
    , extraFunctionalityEmails : List String
    , extraFunctionalityInputState : MultiEmailInput.State
    }


type Msg
    = ClassicMultiEmailInputMsg MultiEmailInput.Msg
    | ClassicReset
    | CustomStylesMultiEmailInputMsg MultiEmailInput.Msg
    | CustomStylesReset
    | ExtraFunctionalityMultiEmailInputMsg MultiEmailInput.Msg
    | ExtraFunctionalityReset


init : ( Model, Cmd Msg )
init =
    ( { classicEmails = initialEmails
      , classicInputState = initClassicInput
      , customStylesEmails = initialEmails
      , customStylesInputState = initCustomStylesInput
      , extraFunctionalityEmails = initialEmails
      , extraFunctionalityInputState = initExtraFunctionalityInput
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClassicMultiEmailInputMsg subMsg ->
            let
                ( nextState, nextEmails, nextCmd ) =
                    MultiEmailInput.update subMsg model.classicInputState model.classicEmails
            in
            ( { model | classicEmails = nextEmails, classicInputState = nextState }, Cmd.map ClassicMultiEmailInputMsg nextCmd )

        ClassicReset ->
            ( { model | classicEmails = [], classicInputState = initClassicInput }, Cmd.none )

        CustomStylesMultiEmailInputMsg subMsg ->
            let
                ( nextState, nextEmails, nextCmd ) =
                    MultiEmailInput.update subMsg model.customStylesInputState model.customStylesEmails
            in
            ( { model | customStylesEmails = nextEmails, customStylesInputState = nextState }, Cmd.map CustomStylesMultiEmailInputMsg nextCmd )

        CustomStylesReset ->
            ( { model | customStylesEmails = [], customStylesInputState = initCustomStylesInput }, Cmd.none )

        ExtraFunctionalityMultiEmailInputMsg subMsg ->
            let
                ( nextState, nextEmails, nextCmd ) =
                    MultiEmailInput.update subMsg model.extraFunctionalityInputState model.extraFunctionalityEmails
            in
            ( { model | extraFunctionalityEmails = nextEmails, extraFunctionalityInputState = nextState }, Cmd.map ExtraFunctionalityMultiEmailInputMsg nextCmd )

        ExtraFunctionalityReset ->
            ( { model | extraFunctionalityEmails = [], extraFunctionalityInputState = initExtraFunctionalityInput }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "example-list" ]
        [ viewClassicExample model
        , viewCustomStylesExample model
        , viewExtraFunctionalityExample model
        ]


viewClassicExample : Model -> Html Msg
viewClassicExample model =
    Html.div [ Attr.class "example classic" ]
        [ Html.h2 [] [ Html.text "Classic" ]
        , Html.p [ Attr.class "explanation" ] [ Html.text "This is how the plugin would look like with the default stylesheet and no custom elements" ]
        , MultiEmailInput.view
            ClassicMultiEmailInputMsg
            []
            "Write an email here"
            model.classicEmails
            model.classicInputState
        , Html.button [ Attr.class "reset", Ev.onClick ClassicReset ] [ Html.text "Reset" ]
        ]


viewCustomStylesExample : Model -> Html Msg
viewCustomStylesExample model =
    Html.div [ Attr.class "example custom-styles" ]
        [ Html.h2 [] [ Html.text "Custom Styles" ]
        , Html.p [ Attr.class "explanation" ] [ Html.text "The default styles are build to be easily customizable. ", Html.a [ Attr.target "_blank", Attr.href "https://github.com/larribas/elm-multi-email-input/blob/master/demo/demo.css" ] [ Html.text "See the code for this example's styles here" ] ]
        , MultiEmailInput.view
            CustomStylesMultiEmailInputMsg
            []
            "Write an email here"
            model.customStylesEmails
            model.customStylesInputState
        , Html.button [ Attr.class "reset", Ev.onClick CustomStylesReset ] [ Html.text "Reset" ]
        ]


viewExtraFunctionalityExample : Model -> Html Msg
viewExtraFunctionalityExample model =
    let
        validEmails =
            List.filter MultiEmailInput.isEmailValid model.extraFunctionalityEmails

        nValidEmails =
            List.length validEmails

        maxValidEmails =
            10
    in
    Html.div [ Attr.class "example extra-functionality" ]
        [ Html.h2 [] [ Html.text "With Some Extra Functionality" ]
        , Html.p [ Attr.class "explanation" ] [ Html.text "You can also add some extra functionality to the default component. Here's an idea:" ]
        , MultiEmailInput.view
            ExtraFunctionalityMultiEmailInputMsg
            [ Attr.disabled (nValidEmails >= maxValidEmails) ]
            "Write an email here"
            model.extraFunctionalityEmails
            model.extraFunctionalityInputState
        , Html.p [ Attr.class "counter" ] [ Html.text <| "You've introduced (" ++ toString nValidEmails ++ "/" ++ toString maxValidEmails ++ ") valid emails" ]
        , Html.button [ Attr.class "reset", Ev.onClick ExtraFunctionalityReset ] [ Html.text "Reset" ]
        ]


initialEmails : List String
initialEmails =
    [ "valid@email.com", "another@email.com", "invalid" ]


initClassicInput =
    MultiEmailInput.init "classic-textarea"


initCustomStylesInput =
    MultiEmailInput.init "custom-styles-textarea"


initExtraFunctionalityInput =
    MultiEmailInput.init "extra-functionality-textarea"
