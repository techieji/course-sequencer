port module Main exposing (..)

import Browser
import Http
import Html exposing (Html, button, div, text, ul, li, input, p)
import Html.Attributes exposing (draggable, dropzone, value, placeholder, class)
import Html.Events exposing (on, preventDefaultOn, onInput, onClick)
import Dict exposing (Dict)
import Json.Decode as Json
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode exposing (at, map3)
import Maybe exposing (Maybe, andThen, withDefault)
import Text.Search as Search

main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }

-- Model and init

type alias Semester = String

-- simplified.json format:
type alias Class =
    { subject : String
    , courseNumber : String
    , title : String
--    , distReqs : String      -- TODO add back once data is preprocessed
--    , prereqs : String
--    , coreqs : String
    }

type alias IndependentClass =
    { class : Class, semester : Semester }

type alias ClassList = Dict Semester (List Class)
type ClassRosterResult
    = Roster (List Class)
    | Loading
    | Error (Http.Error)
type alias Model =
    { classes : ClassList
    , selectedClass : Maybe IndependentClass
    , searchText : String
    , classRoster : ClassRosterResult }

classToJson : Class -> Encode.Value
classToJson class =
    Encode.object
        [ ( "subject", Encode.string class.subject )
        , ( "catalogNbr", Encode.string class.courseNumber )
        , ( "titleLong", Encode.string class.title ) ]

modelToJsonString : Model -> String
modelToJsonString =
    .classes >> (Encode.dict identity <| Encode.list classToJson) >> Encode.encode 0

classesRecord : Json.Decoder ClassList
classesRecord = Decode.dict classRecord
-- Kinda stupid naming going on here: record is JSON -> Elm object

port saveModel : (String, String) -> Cmd msg
port loadModelReq : String -> Cmd msg
port loadModelSub : ((Maybe String) -> msg) -> Sub msg

classRecord : Json.Decoder (List Class)
classRecord =
    Decode.list
        <| map3 Class
            (at ["subject"] Decode.string)
            (at ["catalogNbr"] Decode.string)
            (at ["titleLong"] Decode.string)

toSearchString : Class -> String
toSearchString record = record.subject ++ record.courseNumber ++ ": " ++ record.title

init : () -> (Model, Cmd Msg)
init flags =
    ( Model
        (Dict.fromList [
            ("Fall 2025", []),
            ("Spring 2025", []),
            ("Fall 2026", []),
            ("Spring 2026", [])])
        Nothing
        ""
        Loading
    , Cmd.batch
        [ Http.get
            { url = "/data/simplified.json", expect = Http.expectJson LoadedClasses classRecord }
        , loadModelReq "plan1" ] )

-- Updating

type Msg
    = AddClass Class Semester
    | DelClass Class Semester
    | StartDrag Class Semester
    | MultiMsg (List Msg)
    | LoadedClasses (Result Http.Error (List Class))
    | NewSearch String
    | SaveModel String
    | LoadModel String
    | DoNothing

noCmd : Model -> (Model, Cmd Msg)
noCmd model = (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AddClass class semester ->
            noCmd { model | classes =
                (Dict.insert
                    semester
                    (List.append (withDefault [] (Dict.get semester model.classes)) [class])
                    model.classes) }
        DelClass class semester ->
            case (Dict.get semester model.classes) of
                Just semesterList ->
                    noCmd { model | classes =
                        (Dict.insert semester (List.filter (\c -> c /= class) semesterList) model.classes) }
                Nothing -> noCmd model    -- This is where classes from the roster would end up
        MultiMsg list ->
            -- This should probably provide the Cmd of the last Msg in the list
            noCmd <| List.foldl (\msg_ model_ -> Tuple.first <| update msg_ model_) model list
        StartDrag class semester ->
            noCmd { model | selectedClass = Just (IndependentClass class semester) }
        LoadedClasses (Ok roster) ->
            noCmd { model | classRoster = Roster roster }
        LoadedClasses (Err error) ->
            noCmd { model | classRoster = Error error }
        NewSearch str ->
            noCmd { model | searchText = str }
        SaveModel key ->
            ( model, saveModel (key, modelToJsonString model) )
        LoadModel json ->
            case (Decode.decodeString classesRecord json) of
                Ok classList ->
                    noCmd { model | classes = classList }
                Err err ->
                    noCmd model
        DoNothing ->
            noCmd model

-- View

onDrag : msg -> Html.Attribute msg
onDrag message =
    on "dragstart" (Json.succeed message)

onDragover : msg -> Html.Attribute msg
onDragover message =
    preventDefaultOn "dragover" <| Json.map (\msg -> (msg,True)) (Json.succeed message)

onDrop : msg -> Html.Attribute msg
onDrop message =
    preventDefaultOn "drop" <| Json.map (\msg -> (msg,True)) (Json.succeed message)

delIndependentClass : IndependentClass -> Msg
delIndependentClass class = DelClass class.class class.semester

dropHandler : String -> Maybe IndependentClass -> Msg
dropHandler semester modelClass =
    case (modelClass) of
        Just class -> MultiMsg [delIndependentClass class, AddClass class.class semester]
        Nothing -> DoNothing

classCard : Semester -> Class -> Html Msg
classCard semester class_ =
    div [ draggable "true", onDrag (StartDrag class_ semester), class "class" ]
        [ p [ class "class-code" ] [ text <| class_.subject ++ " " ++ class_.courseNumber ]
        , p [ class "class-name" ] [ text class_.title ]
        , p [ class "class-credits" ] [ text "? credits" ] ]     -- In enrollGroups, unitsMinimum

semesterToHtml : Model -> Semester -> Html Msg
semesterToHtml model semester =
    case (Dict.get semester model.classes) of
        Just classes ->
            classes
            |> List.sortBy .subject
            |> List.map (classCard semester)
            |> div [ class "classes" ]
            |> List.singleton
            |> (::) (p [ class "title" ] [ text semester ])
            |> div [ dropzone "true"
                   , onDragover DoNothing
                   , onDrop (dropHandler semester model.selectedClass)
                   , class "semester" ]
        Nothing ->
            text "Error"

classRosterHtml : Model -> Html Msg
classRosterHtml model =
    case (model.classRoster) of
        Roster roster ->
            Search.withQueryString toSearchString Search.NotCaseSensitive model.searchText roster
            |> List.map (classCard "None")
            |> div [ class "results"
                   , dropzone "true"
                   , onDragover DoNothing
                   , onDrop
                     <| withDefault DoNothing
                     <| Maybe.map delIndependentClass model.selectedClass ]
            |> List.singleton
            |> (::) (input [ placeholder "Class name", value model.searchText, onInput NewSearch ] [])
            |> div [ class "search" ]
        Loading -> text "Loading"
        Error err -> text (Debug.toString err)

toolbar : Html Msg
toolbar =
    div [ class "toolbar" ]
        [ div [ class "toolbar-item" ] [ text "Edit Semesters" ]
        , div [ class "toolbar-item" ] [ text "Edit Requirements" ]
        , div [ class "toolbar-item", onClick <| SaveModel "plan1" ] [ text "Save" ] ]

view : Model -> Browser.Document Msg
view model =
    let
        semesterList = List.map (semesterToHtml model) (Dict.keys model.classes)
        search = classRosterHtml model
    in
    search
    |> List.singleton
    |> (::) (div [ class "semesters" ] semesterList)
    |> div [ class "container" ]
    |> List.singleton
    |> (::) toolbar
    |> div []
    |> List.singleton
    |> Browser.Document "Course Sequencer"

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    loadModelSub <| withDefault DoNothing << Maybe.map LoadModel    -- jesus idk wtf functional programming is
