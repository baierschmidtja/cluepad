module CluePad exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, form, input, label, text, br, h4, h5, hr)
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
  }

isSuspect : Item -> Basics.Bool
isSuspect item =
  item.category == Character

isWeapon : Item -> Basics.Bool
isWeapon item =
  item.category == Weapon

isRoom : Item -> Basics.Bool
isRoom item =
  item.category == Room

getSuspects : List Item -> List Item
getSuspects items =
  List.filter isSuspect items

getWeapons : List Item -> List Item
getWeapons items =
  List.filter isWeapon items

getRooms : List Item -> List Item
getRooms items =
  List.filter isRoom items


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
  , Cmd.none
  )


-- UPDATE

type Msg
  = ItemNoteSelected GameObjectId
  | UpdateItemNote String
  | SaveAllNotes
  | ClearAllNotes

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
    [ form [ class "pt-4" ]
      [ h4 [] [ text "CluePad" ]
      , hr [] []
      ]
    , h5[] [ text "Suspects" ]
    , viewItems (getSuspects model.items)
    , hr [] []
    , h5[] [ text "Weapons" ]
    , viewItems (getWeapons model.items)
    , hr [] []
    , h5[] [ text "Rooms" ]
    , viewItems (getRooms model.items)
    , br [] []
    , div [ class "form-group" ]
        [ viewButton "btn btn-primary mb-4" ClearAllNotes "Clear All Notes"
        ]
    ]

viewItem : Item -> Html Msg
viewItem item =
  div [ class "form-group row" ] 
    [ label [ for (getNoteHtmlInputId item), class "col-4 col-sm-3 col-lg-2 col-form-label" ] [ text item.name ] 
    , div [ class "col-8 col-sm-9 col-lg-10" ] [ input [ id (getNoteHtmlInputId item), type_ "text", class "form-control", value item.note, onInput UpdateItemNote, onFocus (ItemNoteSelected item.gameObjectId) ] [] ]
    ]

getNoteHtmlInputId : Item -> String
getNoteHtmlInputId item =
  (String.toLower (gameObjectIdToString item.gameObjectId)) ++ "note"

viewItems : List Item -> Html Msg
viewItems items =
  div [] (List.map viewItem items)

viewButton : String -> Msg -> String -> Html Msg
viewButton cls toMsg txt = 
  button [ type_ "button", class cls, onClick toMsg ] [ text txt ]

