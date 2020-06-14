module Endpoint.TaskEndpoint exposing (createTask, taskResponse, updateTask)

import Data.Task exposing (Model, Msg(..), Task)
import Data.UIMessages exposing (Messages(..))
import Endpoint.JsonParser.TaskParser exposing (parseTaskResult, taskEncoder, taskErrorsDecoder)
import Env.Serverurl as Server
import Http as Http
import Http.Detailed as HttpEx
import Maybe exposing (withDefault)


updateTask : String -> Task -> Cmd Msg
updateTask token model =
    Http.request
        { method = "PUT"
        , headers = [Http.header "Authorization" ("Bearer " ++ token)]
        , url = Server.updateCalendarTask (withDefault 0 model.calendarEntryId) (withDefault 0 model.taskId)
        , body = Http.jsonBody (taskEncoder model)
        , expect = HttpEx.expectString CreateTaskResult
        , timeout = Nothing
        , tracker = Nothing
        }


createTask : String -> Task -> Cmd Msg
createTask token model =
    Http.request
        { method = "POST"
        , headers = [Http.header "Authorization" ("Bearer " ++ token)]
        , url = Server.calendarTask (withDefault 0 model.calendarEntryId)
        , body = Http.jsonBody (taskEncoder model)
        , expect = HttpEx.expectString CreateTaskResult
        , timeout = Nothing
        , tracker = Nothing
        }


taskResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> Model -> Model
taskResponse response model =
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
