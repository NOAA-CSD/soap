module NoaaMdl exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (property)
import Material
import Material.Options exposing (Style, css, onBlur, onInput)
import Material.Textfield as Textfield


type alias Model a =
    { a | mdl : Material.Model }


type Msg
    = Mdl (Material.Msg Msg)



--type Msg a =
-- TODO: Move MDL formatting to different file along with app.css for consistent formatting


textfield :
    Int -- mdl id
    -> Model a
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



-- TODO: Move MDL formatting to different file along with app.css for consistent formatting


floatTextfield :
    Int -- mdl id
    -> Model a
    -> Maybe String -- label for the text field
    -> Float --value of text field
    -> (String -> Msg) -- message handled by onInput
    -> Maybe Msg -- possible message handled by onBlur
    -> Html Msg
floatTextfield num model label value input_msg blur_msg =
    Textfield.render Mdl
        [ num ]
        model.mdl
        ([ Textfield.floatingLabel
         , css "width" "125px"
         , Textfield.maxlength 15
         , Textfield.value (toString value)
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
