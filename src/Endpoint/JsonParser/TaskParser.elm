module Endpoint.JsonParser.TaskParser exposing (parseTaskFromListResult, parseTaskResult, taskEncoder, taskErrorsDecoder, tasksDecoder)

import Data.Task exposing (Task, emptyTask)
import Endpoint.JsonParser.DateTimeDecoder exposing (stringToDate, stringToDateTime)
import Endpoint.JsonParser.ResponseErrorDecoder exposing (ErrorResponse, errorDecoder)
import Http as Http
import Http.Detailed as HttpEx
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Json.Encode.Extra as Encode
import Maybe exposing (withDefault)


taskEncoder : Task -> Encode.Value
taskEncoder model =
    Encode.object
        [ ( "version", Encode.int model.version )
        , ( "title", Encode.string model.title )
        , ( "description", Encode.maybe Encode.string model.description )
        , ( "startTime", Encode.string (model.startDate ++ "T" ++ model.startTime ++ ":00.000000Z") )
        , ( "assignedUsers", Encode.list Encode.int [] )
        , case model.endDate of
            Just d ->
                case model.endTime of
                    Just t ->
                        ( "endTime", Encode.string (d ++ "T" ++ t ++ ":00.000000Z") )

                    Nothing ->
                        ( "endTime", Encode.maybe Encode.string Nothing )

            Nothing ->
                ( "endTime", Encode.maybe Encode.string Nothing )
        ]


tasksDecoder : Maybe Int -> Decode.Decoder Task
tasksDecoder calendarId =
    Decode.succeed Task
        |> hardcoded calendarId
        |> required "taskId" (Decode.nullable Decode.int)
        |> required "version" Decode.int
        |> required "title" Decode.string
        |> optional "description" (Decode.maybe Decode.string) Nothing
        |> required "startTime" stringToDate
        |> required "startTime" stringToDateTime
        |> optional "endTime" (Decode.maybe stringToDate) Nothing
        |> optional "endTime" (Decode.maybe stringToDateTime) Nothing


taskErrorsDecoder : HttpEx.Error String -> List String
taskErrorsDecoder responseError =
    errorDecoder responseError


parseTaskFromListResult : Maybe Int -> ( Http.Metadata, String ) -> Result String Task
parseTaskFromListResult calendarId ( meta, body ) =
    let
        decode =
            Decode.decodeString (Decode.list (tasksDecoder calendarId)) body
    in
    case decode of
        Ok tasks ->
            Ok (withDefault (emptyTask 0 "") (List.head tasks))

        Err error ->
            Err ("error when decoding task: " ++ Decode.errorToString error)


parseTaskResult : Maybe Int -> ( Http.Metadata, String ) -> Result String Task
parseTaskResult calendarId ( meta, body ) =
    let
        decode =
            Decode.decodeString (tasksDecoder calendarId) body
    in
    case decode of
        Ok task ->
            Ok task

        Err error ->
            Err ("error when decoding task: " ++ Decode.errorToString error)
