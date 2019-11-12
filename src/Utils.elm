module Utils exposing (h1text, h3text)

import Html exposing (Html, h1, h3, text)

h1text : String -> Html a
h1text t = h1 [] [ text t ]

h3text : String -> Html a
h3text t = h3 [] [ text t ]
