module Endpoint.TaskEndpoint exposing (copyTasks, createTask, taskResponse, taskUpdateResponse, updateTask)

import Data.CalendarEntry as CalnedarEntry
import Data.Task exposing (Model, Msg(..), Task)
import Data.UIMessages exposing (Messages(..))
import Endpoint.JsonParser.TaskParser exposing (parseTaskFromListResult, parseTaskResult, taskEncoder, taskErrorsDecoder)
import Env.Serverurl as Server
import Http as Http
import Http.Detailed as HttpEx
import Json.Encode as Encode
import Maybe exposing (withDefault)


updateTask : Task -> Cmd Msg
updateTask model =
    Http.riskyRequest
        { method = "PUT"
        , headers = []
        , url = Server.updateCalendarTask (withDefault 0 model.calendarEntryId) (withDefault 0 model.taskId)
        , body = Http.jsonBody (taskEncoder model)
        , expect = HttpEx.expectString UpdateTaskResult
        , timeout = Nothing
        , tracker = Nothing
        }


createTask : Task -> Cmd Msg
createTask model =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = Server.calendarTask (withDefault 0 model.calendarEntryId)
        , body = Http.jsonBody (Encode.list taskEncoder [ model ])
        , expect = HttpEx.expectString CreateTaskResult
        , timeout = Nothing
        , tracker = Nothing
        }


copyTasks : Int -> List Task -> Cmd CalnedarEntry.Msg
copyTasks calendarId models =
    let
        newTasks =
            List.map (\model -> { model | calendarEntryId = Just calendarId, taskId = Nothing, version = 0 }) models
    in
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = Server.calendarTask calendarId
        , body = Http.jsonBody (Encode.list taskEncoder newTasks)
        , expect = HttpEx.expectString CalnedarEntry.GetCalendarEntryTasksAfterCopyResult
        , timeout = Nothing
        , tracker = Nothing
        }


taskResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> Model -> Model
taskResponse response model =
    case response of
        Ok value ->
            let
                resp =
                    parseTaskFromListResult model.task.calendarEntryId value
            in
            case resp of
                Ok task ->
                    { model | task = task, messages = SuccessUpdate }

                Err error ->
                    { model | messages = Problems [ error ] }

        Err error ->
            { model | messages = Problems (taskErrorsDecoder error) }


taskUpdateResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> Model -> Model
taskUpdateResponse response model =
    case response of
        Ok value ->
            let
                resp =
                    parseTaskResult model.task.calendarEntryId value
            in
            case resp of
                Ok task ->
                    { model | task = task, messages = SuccessUpdate }

                Err error ->
                    { model | messages = Problems [ error ] }

        Err error ->
            { model | messages = Problems (taskErrorsDecoder error) }
