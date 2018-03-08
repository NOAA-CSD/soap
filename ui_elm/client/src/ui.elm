module Ui exposing (..)

import Material.Button as Button
import Material.Options exposing (onClick)

Button.render Mdl
        [ 1 ]
        model.mdl
        [ Button.raised
        , Button.ripple
        , Button.primary
        , onClick StopServer
        , css "margin-top" "10px"
        , css "width" "125px"
        ]
        [ Html.text "Stop Server" ]

textfield :
    Int -- mdl id
    -> Model
    -> Maybe String -- label for the text field
    -> String --value of text field
    -> (String -> Msg) -- message handled by onInput
    -> Maybe Msg -- possible message handled by onBlur
    -> Html Msg
textfield num model label value input_msg blur_msg =
    Textfield.render Mdl
        [ num ]
        model.mdl
        ([ Textfield.floatingLabel
         , css "width" "125px"
         , Textfield.maxlength 15
         , Textfield.value value
         , onInput input_msg
         ]
            ++ (label
                    |> Maybe.map (\lab -> [ Textfield.label lab ])
                    |> Maybe.withDefault []
               )
            ++ -- Handle onBlur events if there
               (blur_msg
                    |> Maybe.map
                        (\msg -> [ onBlur msg ])
                    |> Maybe.withDefault []
               )
        )
        []