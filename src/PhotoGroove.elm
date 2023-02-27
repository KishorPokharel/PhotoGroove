module PhotoGroove exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)

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

view model =
    div [ class "content" ]
        [ h1 [] [text "Photo Groove" ]
        , div [ id "thumbnails" ] 
            (List.map 
                (\photo -> viewThumbnail model.selectedUrl photo) model.photos)
        , img [ class "large", src (urlPrefix ++ "large/" ++ model.selectedUrl) ] []
        ]

viewThumbnail selectedUrl thumb =
    img 
        [ classList [("selected", selectedUrl == thumb.url)]
        , src (urlPrefix ++ thumb.url)
        ]
        []


main =
    view initialModel
