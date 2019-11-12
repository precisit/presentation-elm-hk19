module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (placeholder, style, type_, value)
import Html.Events exposing (onClick, onSubmit)
import Item



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { items : Item.Items
    , newTodoText : String
    , showDone : Bool
    , nextId : Int
    }


type alias Cmd =
    Cmd.Cmd Msg


init : () -> ( Model, Cmd )
init _ =
    ( { items = Item.empty
      , newTodoText = ""
      , showDone = True
      , nextId = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Item Item.Msg
    | Submit String
    | ClearItems
    | ToggleVisibility
    | SetNewTodoText String


update : Msg -> Model -> ( Model, Cmd )
update msg model =
    case msg of
        Item itemMsg ->
            ( { model | items = Item.update itemMsg model.items }, Cmd.none )

        Submit title ->
            ( submitUpdate title model, Cmd.none )

        ClearItems ->
            ( { model | items = Item.empty }, Cmd.none )

        ToggleVisibility ->
            ( { model | showDone = not model.showDone }, Cmd.none )

        SetNewTodoText s ->
            ( { model | newTodoText = s }, Cmd.none )


submitUpdate : String -> Model -> Model
submitUpdate title model =
    if title == "" then
        model

    else
        let
            oldId =
                model.nextId

            nextId =
                oldId + 1

            nextItems =
                Item.addItem oldId title model.items
        in
        { model | newTodoText = "", nextId = nextId, items = nextItems }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Todo List"
    , body = body model
    }


body : Model -> List (Html Msg)
body model =
    [ div containerAttributes
        [ h1 [] [ text "Todo List" ]
        , p [] [ text "Add todos" ]
        , todoForm model
        , clearButton
        , toggleVisibilityButton
        , todoItems model
        ]
    ]


todoItems : Model -> Html Msg
todoItems { items, showDone } =
    div [] [ Html.map Item (Item.todoItems showDone items) ]


todoForm : Model -> Html Msg
todoForm { newTodoText } =
    form [ onSubmit (Submit newTodoText) ]
        [ input
            [ placeholder "New Todo Item..."
            , value newTodoText
            , Html.Events.onInput SetNewTodoText
            ]
            []
        , button [ type_ "submit" ] [ text "Add" ]
        ]


containerAttributes : List (Html.Attribute Msg)
containerAttributes =
    [ style "width" "400px"
    , style "font-size" "28px"
    , style "margin" "auto"
    , style "border" "1px solid"
    , style "margin-top" "2em"
    , style "padding-bottom" "2em"
    , style "padding-left" "1em"
    , style "padding-right" "1em"
    ]


clearButton : Html Msg
clearButton =
    div
        [ style "margin-top" "1em"
        , style "display" "block"
        , style "width" "100%"
        ]
        [ button [ onClick ClearItems ] [ text "Clear Tasks" ] ]


toggleVisibilityButton : Html Msg
toggleVisibilityButton =
    div
        [ style "margin-top" "0.5em"
        , style "display" "block"
        , style "width" "100%"
        ]
        [ button [ onClick ToggleVisibility ] [ text "Toggle Completed" ] ]
