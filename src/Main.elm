module Main exposing (..)

-- component import example

import Browser
import Date exposing (Date, format)
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


type alias DatePickerModel =
    { date : Maybe Date
    , datePicker : DatePicker
    }


type Model
    = MyPicking DatePickerModel
    | MyPicked Date


init : () -> ( Model, Cmd Msg )
init flags =
    let
        ( datePicker, datePickerCmd ) =
            DatePicker.init
    in
    ( MyPicking
        (DatePickerModel
            (Just <| Date.fromPosix Time.utc (Time.millisToPosix 0))
            datePicker
        )
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
    case model of
        MyPicking datePickerModel ->
            case msg of
                NewTime time ->
                    Step.to <| MyPicking { datePickerModel | date = Just (Date.fromPosix Time.utc time) }

                SetDatePicker subMsg ->
                    datePickerUpdate subMsg datePickerModel
                        |> Step.map MyPicking
                        |> Step.onExit (\date -> Step.to (MyPicked date))

        MyPicked _ ->
            Step.stay


datePickerUpdate : DatePicker.Msg -> DatePickerModel -> Step DatePickerModel Msg Date
datePickerUpdate subMsg datePickerModel =
    let
        ( newDatePicker, dpCmd, dpEvent ) =
            DatePicker.update someSettings subMsg datePickerModel.datePicker
    in
    case dpEvent of
        Picked newDate ->
            Step.exit newDate

        _ ->
            Step.to <| { datePickerModel | datePicker = newDatePicker }


datePickerToStep ( a, b, c ) =
    ( ( a, c ), b )
        |> Step.fromUpdate


dateFromDateEvent defaultDate dateEvent =
    case dateEvent of
        Picked newDate ->
            Just newDate

        _ ->
            defaultDate



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "title"
    , body =
        [ node "style" [] [ text css ]
        , case model of
            MyPicking datePickerModel ->
                DatePicker.view
                    datePickerModel.date
                    someSettings
                    datePickerModel.datePicker
                    |> Html.map SetDatePicker

            MyPicked date ->
                text <| format "MMMM ddd, y" date
        ]
    }


css =
    """
.elm-datepicker--container {
  position: relative; }

.elm-datepicker--input:focus {
  outline: 0; }

.elm-datepicker--picker {
  position: absolute;
  border: 1px solid #CCC;
  z-index: 10;
  background-color: white; }

.elm-datepicker--picker-header,
.elm-datepicker--weekdays {
  background: #F2F2F2; }

.elm-datepicker--picker-header {
  display: flex;
  align-items: center; }

.elm-datepicker--prev-container,
.elm-datepicker--next-container {
  flex: 0 1 auto;
  cursor: pointer; }

.elm-datepicker--month-container {
  flex: 1 1 auto;
  padding: 0.5em;
  display: flex;
  flex-direction: column; }

.elm-datepicker--month,
.elm-datepicker--year {
  flex: 1 1 auto;
  cursor: default;
  text-align: center; }

.elm-datepicker--year {
  font-size: 0.6em;
  font-weight: 700; }

.elm-datepicker--prev,
.elm-datepicker--next {
  border: 6px solid transparent;
  background-color: inherit;
  display: block;
  width: 0;
  height: 0;
  padding: 0 0.2em; }

.elm-datepicker--prev {
  border-right-color: #AAA; }
  .elm-datepicker--prev:hover {
    border-right-color: #BBB; }

.elm-datepicker--next {
  border-left-color: #AAA; }
  .elm-datepicker--next:hover {
    border-left-color: #BBB; }

.elm-datepicker--table {
  border-spacing: 0;
  border-collapse: collapse;
  font-size: 0.8em; }
  .elm-datepicker--table td {
    width: 2em;
    height: 2em;
    text-align: center; }

.elm-datepicker--row {
  border-top: 1px solid #F2F2F2; }

.elm-datepicker--dow {
  border-bottom: 1px solid #CCC;
  cursor: default; }

.elm-datepicker--day {
  cursor: pointer; }
  .elm-datepicker--day:hover {
    background: #F2F2F2; }

.elm-datepicker--disabled {
  cursor: default;
  color: #DDD; }
  .elm-datepicker--disabled:hover {
    background: inherit; }

.elm-datepicker--picked {
  color: white;
  background: darkblue; }
  .elm-datepicker--picked:hover {
    background: darkblue; }

.elm-datepicker--today {
  font-weight: bold; }

.elm-datepicker--other-month {
  color: #AAA; }
  .elm-datepicker--other-month.elm-datepicker--disabled {
    color: #EEE; }
  .elm-datepicker--other-month.elm-datepicker--picked {
    color: white; }
"""
