-- Ports can only be used in a port module
port module Cluepad exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, form, input, label, text, br, h4, h5)
import Html.Attributes exposing (type_, class, id, for, value)
import Html.Events exposing (keyCode, on, onBlur, onClick, onFocus, onInput)
import Json.Encode as JsonE
import Json.Decode as JsonD


-- MAIN


main =
    Browser.element
    { init = init
    , update = updateWithStorage
    , subscriptions = \_ -> Sub.none
    , view = view 
    }


-- MODEL
type ItemCategory =
  Character
  | Room
  | Weapon

itemCategoryToString : ItemCategory -> String
itemCategoryToString itemCategory =
  case itemCategory of
    Character -> "Character"
    Room -> "Room"        
    Weapon -> "Weapon"


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


-- Here we use "flags" to load information in from localStorage. The
-- data comes in as a JS value, so we define a `decoder` at the bottom
-- of this file to turn it into an Elm value.
--
-- Check out index.html to see the corresponding code on the JS side.
--
init : JsonE.Value -> (Model, Cmd Msg)
init flags =
  let
    model = Model
      [ Item Character MissScarlett "Miss Scarlett" ""
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
  in
    ( case JsonD.decodeValue serializableStateDecoder flags of
        Ok serializableState ->
          { model | items = mapSerializableNotesToItems serializableState.serializableNotes model.items
          , selectedItemCategory =  serializableState.selectedItemCategory
          }
        Err _ -> model
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



-- VIEW


view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ div [ class "top-decoration" ] []
    , div [ class "main-content" ]
      [ form []
        [ h4 [] [ text "Cluepad \u{1F575}\u{FE0F}\u{200D}\u{2640}\u{FE0F}" ]

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
    [ label [ for (getNoteHtmlInputId item), class "col-5 col-sm-3 col-lg-2 col-form-label col-form-label-lg handwriting" ] [ text item.name ] 
    , div [ class "col-7 col-sm-9 col-lg-10" ] [ input [ id (getNoteHtmlInputId item), type_ "text", class "form-control form-control-lg bg-transparent handwriting", value item.note, onInput UpdateItemNote, onFocus (ItemNoteSelected item.gameObjectId) ] [] ]
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



-- PORTS


port setLocalStorage : JsonE.Value -> Cmd msg

-- We want to `setLocalStorage` on every update, so this function adds
-- the setLocalStorage command on each step of the update function.
--
-- Check out index.html to see how this is handled on the JS side.
--
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
  let
    ( newModel, cmds ) = update msg oldModel
  in
  ( newModel
  , Cmd.batch [ setLocalStorage (encodeSerializableState (modelToSerializableState newModel)), cmds ]
  )



-- PERSISTENCE

type alias SerializableNote =
  { gameObjectId: GameObjectId
  , note: String
  }

type alias SerializableState =
 { serializableNotes: List SerializableNote
 , selectedItemCategory: ItemCategory
 }


itemToSerializableNote : Item -> SerializableNote
itemToSerializableNote item =
  SerializableNote item.gameObjectId item.note


itemsToSerializableNotes : List Item -> List SerializableNote
itemsToSerializableNotes items =
  List.map itemToSerializableNote items


hasGameObjectId : GameObjectId -> SerializableNote -> Bool
hasGameObjectId gameObjectId serializableNote =
  serializableNote.gameObjectId == gameObjectId

mapSerializableNoteToItem : List SerializableNote -> Item -> Item
mapSerializableNoteToItem serializableNotes item  =
  let
    maybeSerializableNote = List.head (List.filter (hasGameObjectId item.gameObjectId) serializableNotes)
  in
    case maybeSerializableNote of
       
       Maybe.Just serializableNote ->
         { item | note = serializableNote.note }

       Maybe.Nothing ->
         item


mapSerializableNotesToItems : List SerializableNote -> List Item -> List Item
mapSerializableNotesToItems serializableNotes items  =
  List.map (mapSerializableNoteToItem serializableNotes) items

modelToSerializableState : Model -> SerializableState
modelToSerializableState model =
  SerializableState (itemsToSerializableNotes model.items) model.selectedItemCategory

-- JSON DECODERS


itemCategoryDecoder : JsonD.Decoder ItemCategory
itemCategoryDecoder =
  JsonD.string |> JsonD.andThen (\itemCategoryStr ->
    case itemCategoryStr of  
      "Character" ->
        JsonD.succeed Character
      
      "Weapon" ->
        JsonD.succeed Weapon

      "Room" ->
        JsonD.succeed Room

      _ ->
        JsonD.fail "invalid ItemCategory"
  )


gameObjectIdDecoder : JsonD.Decoder GameObjectId
gameObjectIdDecoder =
  JsonD.string |> JsonD.andThen (\gameObjectIdStr ->
    case gameObjectIdStr of
      "None" ->
        JsonD.succeed None
     
      "MissScarlett" ->
        JsonD.succeed MissScarlett
  
      "ColMustard" ->
        JsonD.succeed ColMustard 
  
      "MrsWhite" ->
        JsonD.succeed MrsWhite
  
      "MrGreen" ->
        JsonD.succeed MrGreen

      "MrsPeacock" ->
        JsonD.succeed MrsPeacock
  
      "ProfPlum" ->
        JsonD.succeed ProfPlum

      "Candlestick" ->
        JsonD.succeed Candlestick

      "Knife" ->
        JsonD.succeed Knife

      "LeadPipe" ->
        JsonD.succeed LeadPipe
  
      "Revolver" ->
        JsonD.succeed Revolver

      "Rope" ->
        JsonD.succeed Rope
  
      "Wrench" ->
        JsonD.succeed Wrench

      "Ballroom" ->
        JsonD.succeed Ballroom

      "BilliardRoom" ->
        JsonD.succeed BilliardRoom
     
      "Conservatory" ->
        JsonD.succeed Conservatory
  
      "DiningRoom" ->
        JsonD.succeed DiningRoom
  
      "Hall" ->
        JsonD.succeed Hall
     
      "Kitchen" ->
        JsonD.succeed Kitchen

      "Library" ->
        JsonD.succeed Library

      "Lounge" ->
        JsonD.succeed Lounge
     
      "Study" ->
        JsonD.succeed Study

      _ ->
        JsonD.fail "invalid GameObjectId"
  
  )


serializableNoteDecoder : JsonD.Decoder SerializableNote
serializableNoteDecoder =
  JsonD.map2 SerializableNote
    (JsonD.field "gameObjectId" gameObjectIdDecoder)
    (JsonD.field "note" JsonD.string)


serializableNotesDecoder : JsonD.Decoder (List SerializableNote)
serializableNotesDecoder =
  JsonD.list serializableNoteDecoder

serializableStateDecoder : JsonD.Decoder SerializableState
serializableStateDecoder =
  JsonD.map2 SerializableState
    (JsonD.field "serializableNotes" serializableNotesDecoder)
    (JsonD.field "selectedItemCategory" itemCategoryDecoder)

  

-- JSON ENCODE

encodeSerializableNote : SerializableNote -> JsonE.Value
encodeSerializableNote serializableNote =
  JsonE.object
    [ ("gameObjectId", JsonE.string (gameObjectIdToString serializableNote.gameObjectId))
    , ("note", JsonE.string serializableNote.note)
    ]


encodeSerializableNotes : List SerializableNote -> JsonE.Value
encodeSerializableNotes serializableNotes =
  JsonE.list encodeSerializableNote serializableNotes

encodeSerializableState : SerializableState -> JsonE.Value
encodeSerializableState serializableState =
  JsonE.object
    [ ("serializableNotes", encodeSerializableNotes serializableState.serializableNotes)
    , ("selectedItemCategory", JsonE.string (itemCategoryToString serializableState.selectedItemCategory))
    ]
