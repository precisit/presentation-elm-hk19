module Item exposing (Items, Msg(..), update, todoItems, addTask, empty)

import Html exposing
  ( Html
  , div
  , p
  , strong
  , text
  )
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Utils exposing (h3text)


type alias Item =
    { id : Int
    , title : String
    , done : Bool
    }


type alias Items =
    List Item

empty : Items
empty = []

type Msg
    = AddTask Int String
    | SetDone Int Bool

addTask : Int -> String -> Items -> Items
addTask id title = update (AddTask id title)

makeTask : Int -> String -> Item
makeTask id title =
    { id = id
    , title = title
    , done = False
    }


setDone : Bool -> Item -> Item
setDone value item =
    { item | done = value }


setDoneById : Int -> Bool -> Items -> Items
setDoneById id value items =
    items
        |> List.map
            (\item ->
                if item.id == id then
                    setDone value item

                else
                    item
            )


update : Msg -> Items -> Items
update msg items =
    case msg of
        AddTask id title ->
            makeTask id title :: items

        SetDone id value ->
            setDoneById id value items


todoItems : Bool -> Items -> Html Msg
todoItems showDone items =
    div []
        [ div [] []
        , div []
            ( items
                |> List.filter (\i -> showDone || not i.done)
                |> List.map todoItem
            )
        , div [] []
        ]


todoItem : Item -> Html Msg
todoItem item =
    div [ onClick (SetDone item.id (not item.done))
        , style "border-bottom" "1px solid grey"
        , style "padding" "10px"
        ]
        [ h3text item.title
        , p [] [ text "" ]
        , strong [ style "color" <| doneColor item ] [ doneText item ]
        ]


doneColor : Item -> String
doneColor { done } =
    if done then
        "green"
    else
        "gray"


doneText : Item -> Html Msg
doneText { done } =
    let
        t =
            if done then
                "DONE"

            else
                "NOT DONE"
    in
    text t
