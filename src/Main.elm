module Main exposing (..)

-- component import example

import Browser
import Date exposing (Date)
import DatePicker exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Step exposing (Step)
import Task
import Time exposing (Posix)



-- APP


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = Step.asUpdateFunction update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { datePicker : DatePicker, date : Maybe Date }


init : () -> ( Model, Cmd Msg )
init flags =
    let
        ( datePicker, datePickerCmd ) =
            DatePicker.init
    in
    ( { datePicker = datePicker, date = Just <| Date.fromPosix Time.utc (Time.millisToPosix 0) }
    , Cmd.batch
        [ Task.perform NewTime Time.now
        , Cmd.map SetDatePicker datePickerCmd
        ]
    )



-- UPDATE


type Msg
    = SetDatePicker DatePicker.Msg
    | NewTime Posix


someSettings : DatePicker.Settings
someSettings =
    { defaultSettings
        | inputClassList = [ ( "form-control", True ) ]
        , inputId = Just "datePicker"
    }


update : Msg -> Model -> Step Model Msg a
update msg model =
    case msg of
        NewTime time ->
            Step.to { model | date = Just (Date.fromPosix Time.utc time) }

        SetDatePicker subMsg ->
            DatePicker.update someSettings subMsg model.datePicker
                |> datePickerStepWithin model


datePickerStepWithin model ( datePicker, dateCmd, dateEvent ) =
    let
        date =
            case dateEvent of
                Picked newDate ->
                    Just newDate

                _ ->
                    model.date
    in
    Step.fromUpdate ( datePicker, dateCmd )
        |> Step.within (\newDatePicker -> { model | datePicker = newDatePicker, date = date }) SetDatePicker



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "title"
    , body =
        [ DatePicker.view
            model.date
            someSettings
            model.datePicker
            |> Html.map SetDatePicker
        , node "style" [] [ text css ]
        ]
    }


css =
    """
.elm-datePicker--container {
  position: relative; }

.elm-datePicker--input:focus {
  outline: 0; }

.elm-datePicker--picker {
  position: absolute;
  border: 1px solid #CCC;
  z-index: 10;
  background-color: white; }

.elm-datePicker--picker-header,
.elm-datePicker--weekdays {
  background: #F2F2F2; }

.elm-datePicker--picker-header {
  display: flex;
  align-items: center; }

.elm-datePicker--prev-container,
.elm-datePicker--next-container {
  flex: 0 1 auto;
  cursor: pointer; }

.elm-datePicker--month-container {
  flex: 1 1 auto;
  padding: 0.5em;
  display: flex;
  flex-direction: column; }

.elm-datePicker--month,
.elm-datePicker--year {
  flex: 1 1 auto;
  cursor: default;
  text-align: center; }

.elm-datePicker--year {
  font-size: 0.6em;
  font-weight: 700; }

.elm-datePicker--prev,
.elm-datePicker--next {
  border: 6px solid transparent;
  background-color: inherit;
  display: block;
  width: 0;
  height: 0;
  padding: 0 0.2em; }

.elm-datePicker--prev {
  border-right-color: #AAA; }
  .elm-datePicker--prev:hover {
    border-right-color: #BBB; }

.elm-datePicker--next {
  border-left-color: #AAA; }
  .elm-datePicker--next:hover {
    border-left-color: #BBB; }

.elm-datePicker--table {
  border-spacing: 0;
  border-collapse: collapse;
  font-size: 0.8em; }
  .elm-datePicker--table td {
    width: 2em;
    height: 2em;
    text-align: center; }

.elm-datePicker--row {
  border-top: 1px solid #F2F2F2; }

.elm-datePicker--dow {
  border-bottom: 1px solid #CCC;
  cursor: default; }

.elm-datePicker--day {
  cursor: pointer; }
  .elm-datePicker--day:hover {
    background: #F2F2F2; }

.elm-datePicker--disabled {
  cursor: default;
  color: #DDD; }
  .elm-datePicker--disabled:hover {
    background: inherit; }

.elm-datePicker--picked {
  color: white;
  background: darkblue; }
  .elm-datePicker--picked:hover {
    background: darkblue; }

.elm-datePicker--today {
  font-weight: bold; }

.elm-datePicker--other-month {
  color: #AAA; }
  .elm-datePicker--other-month.elm-datePicker--disabled {
    color: #EEE; }
  .elm-datePicker--other-month.elm-datePicker--picked {
    color: white; }
"""
