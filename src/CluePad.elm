module CluePad exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, form, input, label, text, br, h4, h5)
import Html.Attributes exposing (type_, class, id, for, value)
import Html.Events exposing (keyCode, on, onBlur, onClick, onFocus, onInput)

-- MAIN
main =
    Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view 
    }


-- MODEL
type ItemCategory =
  Character
  | Room
  | Weapon

type GameObjectId
  = None
  | MissScarlett
  | ColMustard
  | MrsWhite
  | MrGreen
  | MrsPeacock
  | ProfPlum
  | Candlestick
  | Knife
  | LeadPipe
  | Revolver
  | Rope
  | Wrench
  | Ballroom
  | BilliardRoom
  | Conservatory
  | DiningRoom
  | Hall
  | Kitchen
  | Library
  | Lounge
  | Study

gameObjectIdToString : GameObjectId -> String
gameObjectIdToString gameObjectId =
  case gameObjectId of
     None ->
       "None"
     
     MissScarlett ->
       "MissScarlett"
  
     ColMustard ->
       "ColMustard" 
  
     MrsWhite ->
       "MrsWhite"
  
     MrGreen ->
       "MrGreen"

     MrsPeacock ->
       "MrsPeacock"
  
     ProfPlum ->
       "ProfPlum"

     Candlestick ->
       "Candlestick"

     Knife ->
       "Knife"

     LeadPipe ->
       "LeadPipe"
  
     Revolver ->
       "Revolver"

     Rope ->
       "Rope"
  
     Wrench ->
       "Wrench"

     Ballroom ->
       "Ballroom"

     BilliardRoom ->
       "BilliardRoom"
     
     Conservatory ->
       "Conservatory"
  
     DiningRoom ->
       "DiningRoom"
  
     Hall ->
       "Hall"
     
     Kitchen ->
       "Kitchen"

     Library ->
       "Library"

     Lounge ->
       "Lounge"
     
     Study ->
       "Study"

type alias Item =
  { category: ItemCategory
  , gameObjectId: GameObjectId
  , name: String
  , note: String
  }

type alias Model =
  { items: List Item
  , selectedItemGameObjectId: GameObjectId
  , selectedItemCategory: ItemCategory
  }

isItemInCategory : ItemCategory -> Item -> Bool
isItemInCategory itemCategory item =
  item.category == itemCategory

getItemsForCategory : ItemCategory -> List Item -> List Item
getItemsForCategory itemCategory items = 
  List.filter (isItemInCategory itemCategory) items




init : () -> (Model, Cmd Msg)
init _ =
  ( Model [ Item Character MissScarlett "Miss Scarlett" ""
    , Item Character ColMustard "Col. Mustard" ""
    , Item Character MrsWhite "Mrs. White" ""
    , Item Character MrGreen "Mr. Green" ""
    , Item Character MrsPeacock "Mrs. Peacock" ""
    , Item Character ProfPlum "Prof. Plum" ""
    , Item Room Ballroom "Ballroom" ""
    , Item Room BilliardRoom "Billiard Room" ""
    , Item Room Conservatory "Conservatory" ""
    , Item Room DiningRoom "Dining Room" ""
    , Item Room Hall "Hall" ""
    , Item Room Kitchen "Kitchen" ""
    , Item Room Library "Library" ""
    , Item Room Lounge "Lounge" ""
    , Item Room Study "Study" ""
    , Item Weapon Candlestick "Candlestick" ""
    , Item Weapon Knife "Knife" ""
    , Item Weapon LeadPipe "Lead Pipe" ""
    , Item Weapon Revolver "Revolver" ""
    , Item Weapon Rope "Rope" ""
    , Item Weapon Wrench "Wrench" ""
    ]
    None
    Character
  , Cmd.none
  )


-- UPDATE

type Msg
  = ItemNoteSelected GameObjectId
  | UpdateItemNote String
  | SaveAllNotes
  | ClearAllNotes
  | SwitchTab ItemCategory

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    
    ItemNoteSelected gameObjectId ->
      ( { model | selectedItemGameObjectId = gameObjectId }
      , Cmd.none  
      )

    UpdateItemNote note ->
      ( { model | items = updateItems model.items model.selectedItemGameObjectId note }
      , Cmd.none  
      )

    SaveAllNotes ->
      ( model
      , Cmd.none
      )

    ClearAllNotes ->
      ( { model | items = clearNotes model.items }
      , Cmd.none
      )

    SwitchTab itemCategory ->
      ( { model | selectedItemCategory = itemCategory }
      , Cmd.none
      )

updateItems : List Item -> GameObjectId -> String -> List Item
updateItems items gameObjectId note =
  List.map (updateItemNote gameObjectId note) items

updateItemNote : GameObjectId -> String -> Item -> Item
updateItemNote gameObjectId note item =
  if item.gameObjectId == gameObjectId then
    { item | note = note }
  else
    item

clearNote : Item -> Item
clearNote item =
  { item | note = "" }

clearNotes : List Item -> List Item
clearNotes items = 
  List.map clearNote items

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ div [ class "top-decoration" ] []
    , div [ class "main-content" ]
      [ form []
        [ h4 [] [ text "CluePad \u{1F575}\u{FE0F}\u{200D}\u{2640}\u{FE0F}" ]

        , div [ class "btn-group mb-3" ]
          [ viewButton "btn btn-sm btn-secondary mr-1" (SwitchTab Character) "\u{1F937}\u{200D}\u{2640}\u{FE0F} Suspects"
          , viewButton "btn btn-sm btn-secondary mr-1" (SwitchTab Weapon) "\u{1F5E1}\u{FE0F} Weapons"
          , viewButton "btn btn-sm btn-secondary" (SwitchTab Room) "\u{1F6AA} Rooms"
          ]
        , viewItemCategory model.selectedItemCategory model.items
        , div [ class "form-group" ]
          [ viewButton "btn btn-danger mb-4" ClearAllNotes "\u{1F5D1}\u{FE0F} Clear All Notes"
          ]
        ]
      ]
    ]

getNoteHtmlInputId : Item -> String
getNoteHtmlInputId item =
  (String.toLower (gameObjectIdToString item.gameObjectId)) ++ "note"

viewItem : Item -> Html Msg
viewItem item =
  div [ class "form-group row" ] 
    [ label [ for (getNoteHtmlInputId item), class "col-5 col-sm-3 col-lg-2 col-form-label" ] [ text item.name ] 
    , div [ class "col-7 col-sm-9 col-lg-10" ] [ input [ id (getNoteHtmlInputId item), type_ "text", class "form-control bg-transparent", value item.note, onInput UpdateItemNote, onFocus (ItemNoteSelected item.gameObjectId) ] [] ]
    ]

viewItems : List Item -> Html Msg
viewItems items =
  div [] (List.map viewItem items)

viewButton : String -> Msg -> String -> Html Msg
viewButton cls toMsg txt = 
  button [ type_ "button", class cls, onClick toMsg ] [ text txt ]

viewItemCategory : ItemCategory -> List Item -> Html Msg
viewItemCategory itemCategory items =
  viewItems (getItemsForCategory itemCategory items)
