module PhotoGroove exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

urlPrefix = 
    "https://elm-in-action.com/"

initialModel =
    { photos =
        [ { url = "1.jpeg"}
        , { url = "2.jpeg"}
        , { url = "3.jpeg"}
        ]
    , selectedUrl = "1.jpeg"
    }

update msg model =
    if msg.description == "ClickedPhoto" then
        { model | selectedUrl = msg.data }
    else
        model

view model =
    div [ class "content" ]
        [ h1 [] [text "Photo Groove" ]
        , div [ id "thumbnails" ] (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img [ class "large", src (urlPrefix ++ "large/" ++ model.selectedUrl) ] []
        ]

viewThumbnail selectedUrl thumb =
    img 
        [ classList [("selected", selectedUrl == thumb.url)]
        , src (urlPrefix ++ thumb.url)
        , onClick { description = "ClickedPhoto", data = thumb.url }
        ]
        []

main =
    Browser.sandbox { init=initialModel, view=view, update=update }
