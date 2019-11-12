module Main exposing ( main )

import Browser
import Html exposing 
    ( Html
    , button
    , h1
    , p
    , text
    , form
    , input
    , div
    )
import Html.Attributes exposing ( value, placeholder, type_, style )
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
    | ClearTasks
    | ToggleVisibility
    | SetNewTodoText String


update : Msg -> Model -> ( Model, Cmd )
update msg model =
    case msg of
        Item itemMsg ->
            ({ model | items = Item.update itemMsg model.items }, Cmd.none)
        
        Submit title -> if title == "" then (model, Cmd.none) else
            let
                oldId = model.nextId
                nextId = oldId + 1
                nextItems = Item.addTask oldId title model.items
            in
                ({ model | newTodoText = "", nextId = nextId, items = nextItems }, Cmd.none) 

        ClearTasks ->
            ({ model | items = Item.empty }, Cmd.none)

        ToggleVisibility ->
            ({ model | showDone = not model.showDone }, Cmd.none)

        SetNewTodoText s ->
            ({ model | newTodoText = s }, Cmd.none)


-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "Todo List"
    , body = body model
    }

body : Model -> List (Html Msg)
body model = 
    [ div [ style "width" "400px", style "margin" "auto" ]
      [   h1 [ style "margin-top" "2em" ] [ text "Todo List" ]
      ,   p [] [ text "Add todos" ]
      ,   todoForm model
      ,   div [ style "margin-top" "2em", style "display" "block", style "width" "100%"] [ button [ onClick ClearTasks ] [ text "Clear Tasks" ] ]
      ,   div [ style "margin-top" "1em", style "display" "block", style "width" "100%" ] [ button [ onClick ToggleVisibility ] [ text "Toggle Completed"] ]
      ,   todoItems model
      ]
    ]

todoItems : Model -> Html Msg
todoItems { items, showDone } =
  Html.map Item (Item.todoItems showDone items)


todoForm : Model -> Html Msg
todoForm { newTodoText } =
    form [ onSubmit (Submit newTodoText) ]
        [ input
            [ placeholder "New Todo Item..."
            , value newTodoText
            , Html.Events.onInput SetNewTodoText
            ] []
        ,   button [ type_ "submit" ] [ text "Add" ]
        ]
