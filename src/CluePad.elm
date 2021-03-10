-- Ports can only be used in a port module
port module Cluepad exposing (..)

import Browser
import Html exposing (Html, a, ul, li, button, div, p, input, label, text)
import Html.Attributes exposing (type_, class, href, id, for, checked, target, value)
import Html.Events exposing (onCheck, onClick, onFocus, onInput)
import Json.Encode as JsonE
import Json.Decode as JsonD
import String


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
  , showingSettings: Bool
  , useCheckBoxes: Bool
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
      False
      True
  in
    ( case JsonD.decodeValue serializableStateDecoder flags of
        Ok serializableState ->
          { model | items = mapSerializableNotesToItems serializableState.serializableNotes model.items
          , selectedItemCategory =  serializableState.selectedItemCategory
          , showingSettings = serializableState.showingSettings
          , useCheckBoxes = serializableState.useCheckBoxes
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
  | ToggleSettings
  | CheckUseCheckBoxes Bool


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    
    ItemNoteSelected gameObjectId ->
      ( if model.useCheckBoxes then
          { model | selectedItemGameObjectId = gameObjectId, items = updateItems model.items gameObjectId (getNewCheckBoxValue model.items gameObjectId) }
        else
          { model | selectedItemGameObjectId = gameObjectId }
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
      ( { model | items = clearNotes model.items, selectedItemCategory = Character, showingSettings = False }
      , Cmd.none
      )

    SwitchTab itemCategory ->
      ( { model | selectedItemCategory = itemCategory }
      , Cmd.none
      )

    ToggleSettings ->
      ( { model | showingSettings = not model.showingSettings }
      , Cmd.none
      )

    CheckUseCheckBoxes useCheckBoxes ->
      ( { model | useCheckBoxes = useCheckBoxes }
      , Cmd.none
      )


getNewCheckBoxValue : List Item -> GameObjectId -> String
getNewCheckBoxValue items gameObjectId =
  let
    itemHasGameObjectId item = 
      item.gameObjectId == gameObjectId

    maybeItem = List.head (List.filter itemHasGameObjectId items)
  in 
    case maybeItem of
       Maybe.Nothing -> ""
       Maybe.Just item ->
        if String.trim item.note == "" then
          "X"
        else
          ""
  

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
  if model.showingSettings then
    viewSettings model.useCheckBoxes
  else
    div [ class "container" ]
      [ div [ class "top-decoration" ] []
      , div [ class "main-content" ]
        [ div []
          [ div [ class "btn-toolbar" ]
            [ viewSettingsButton
            , viewTabButtons model.selectedItemCategory
            ]
          , viewItemCategory model.selectedItemCategory model.items model.useCheckBoxes
          , div [] []
          ]
        ]
      ]


viewSettingsButton : Html Msg
viewSettingsButton = 
  div [ class "btn-group mb-1 mr-1" ] [ viewButton "btn btn-lg" ToggleSettings "\u{2699}\u{FE0F}" ]
  


viewSettings : Bool -> Html Msg
viewSettings useCheckBoxes =
  div [ class "container" ]
    [ div [ class "top-decoration" ] []
    , div [ class "main-content" ]
        [ div [ class "input-group" ]
            [ div [ class "input-group-prepend" ]
                [ viewSettingsButton
                ]
            , label [ class "form-control form-control-lg bg-transparent handwriting" ] [ text "Cluepad" ]
            ]
        , div [ class "handwriting"]
            [ p [] [ text "Save paper and keep track of notes on your favorite device while playing the classic Whodunnit game.  On a mobile device?  Be sure to Add to Home Screen to allow for a full-screen experience that works offline." ]
            , label [] [ text "Thanks:" ]
            , ul []
                [ li [] [ a [ href "https://elm-lang.org/", target "_blank" ] [ text "elm programming language" ] ]
                , li [] [ a [ href "https://fonts.google.com/specimen/Reenie+Beanie?preview.text_type=custom", target "_blank" ] [ text "Reenie Beanie Google font by James Grieshaber" ] ] 
                , li [] [ a [ href "https://www.myfreetextures.com/wp-content/uploads/2014/10/texture-seamless-wood-4.jpg", target "_blank" ] [ text "Woodgrain texture" ] ]
                ]
            , p []
                [ text "Source code can be found on GitHub " 
                , a [ href "https://github.com/baierschmidtja/cluepad/", target "_blank" ] [ text "here" ]
                ]
            , text "======================================"
            , div [ class "form-group form-check" ]
                [ input [ id "useCheckBoxes", class "form-check-input", type_ "checkbox", checked useCheckBoxes, onCheck CheckUseCheckBoxes ] []
                , label [ class "form-check-label", for "useCheckBoxes" ] [ text "Uncheck to use text notes instead of X marks" ]
                ]
            , div [ class "form-group" ]
                [ viewButton "btn btn-danger mt-4 mb-4" ClearAllNotes "\u{1F5D1}\u{FE0F} Erase All Notes!"
                ]
            ]
        ]
    ]

viewTabButton : ItemCategory -> ItemCategory -> Html Msg
viewTabButton itemCategory selectedItemCategory =
  let
    baseButtonClass = "btn btn-lg tab"
    
    buttonClass = 
      if (itemCategory == selectedItemCategory) then
        baseButtonClass ++ " tab-active"
      else
        baseButtonClass ++ " tab-inactive"

    buttonLabel = 
      case itemCategory of
         Character -> "\u{1F937}\u{200D}\u{2640}\u{FE0F}"
         Weapon -> "\u{1F5E1}\u{FE0F}"
         Room -> "\u{1F6AA}"
  
  in
    viewButton buttonClass (SwitchTab itemCategory) buttonLabel


viewTabButtons : ItemCategory -> Html Msg
viewTabButtons selectedItemCategory =
  div [ class "btn-group mb-1" ]
    [ viewTabButton Character selectedItemCategory
    , viewTabButton Weapon selectedItemCategory
    , viewTabButton Room selectedItemCategory
    ]


viewItem : Bool -> Int -> Item -> Html Msg
viewItem useCheckBox i item =
  let
    getHtmlId =
      (String.toLower (gameObjectIdToString item.gameObjectId)) ++ "note" ++ String.fromInt i
    baseOuterDivClass = "row"
    outerDivClass = 
      case item.gameObjectId of
         None -> baseOuterDivClass ++ " invisible"
         _ -> baseOuterDivClass
    noteElementColClass = "col-6 col-sm-9 col-lg-10"
    noteElementClass = "bg-transparent handwriting handwriting-xl"
    noteElement = 
      if useCheckBox then
        label [ id getHtmlId, class (noteElementColClass ++ " col-form-label " ++ noteElementClass) ] [ text item.note ]
      else    
        div [ class noteElementColClass ] [ input [ id getHtmlId, type_ "text", class ("form-control " ++ noteElementClass), value item.note, onInput UpdateItemNote, onFocus (ItemNoteSelected item.gameObjectId) ] [] ]
  in       
    div [ class outerDivClass, onClick (ItemNoteSelected item.gameObjectId) ] 
      [ label [ for getHtmlId, class "col-6 col-sm-3 col-lg-2 col-form-label bg-transparent handwriting handwriting-xl" ] [ text item.name ] 
      , noteElement
      ]


viewItems : List Item -> Bool -> Html Msg
viewItems items useCheckBoxes =
  div [] (List.indexedMap (viewItem useCheckBoxes) items)


viewButton : String -> Msg -> String -> Html Msg
viewButton cls toMsg txt = 
  button [ type_ "button", class cls, onClick toMsg ] [ text txt ]


viewItemCategory : ItemCategory -> List Item -> Bool -> Html Msg
viewItemCategory itemCategory items useCheckBoxes =
  viewItems (getItemsForCategory itemCategory items) useCheckBoxes



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
 , showingSettings: Bool
 , useCheckBoxes: Bool
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
  SerializableState (itemsToSerializableNotes model.items) model.selectedItemCategory model.showingSettings model.useCheckBoxes

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
  JsonD.map4 SerializableState
    (JsonD.field "serializableNotes" serializableNotesDecoder)
    (JsonD.field "selectedItemCategory" itemCategoryDecoder)
    (JsonD.field "showingSettings" JsonD.bool)
    (JsonD.field "useCheckBoxes" JsonD.bool)

  

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
    , ("showingSettings", JsonE.bool serializableState.showingSettings)
    , ("useCheckBoxes", JsonE.bool serializableState.useCheckBoxes)
    ]
