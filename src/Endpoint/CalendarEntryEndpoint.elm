module Endpoint.CalendarEntryEndpoint exposing (calendarEntriesResponse, createCalendarEntry, getCalendarEntryResponse, loadCalendarEntries, loadCalendarEntry, saveCalendarEntry, saveCalendarEntryResponse)

import Data.CalendarEntry exposing (CalendarEntry, Model, Msg(..))
import Data.SimpleCalendarList as CalendarList
import Data.UIMessages exposing (Messages(..))
import Endpoint.JsonParser.CalendarEntryParser exposing (calendarEntryEncoder, calendarErrorsDecoder, parseCalendarEntriesResult, parseCalendarEntryResult)
import Env.Serverurl as Server
import Http as Http
import Http.Detailed as HttpEx
import Maybe exposing (withDefault)


saveCalendarEntry : String -> CalendarEntry -> Cmd Msg
saveCalendarEntry token model =
    Http.request
        { method = "PUT"
        , headers = [Http.header "Authorization" ("Bearer " ++ token)]
        , url = Server.calendarEntry (withDefault 0 model.entryId)
        , body = Http.jsonBody (calendarEntryEncoder model)
        , expect = HttpEx.expectString SaveCalendarResult
        , timeout = Nothing
        , tracker = Nothing
        }


createCalendarEntry : String -> CalendarEntry -> Cmd Msg
createCalendarEntry token model =
    Http.request
        { method = "POST"
        , headers = [Http.header "Authorization" ("Bearer " ++ token)]
        , url = Server.calendarEntries
        , body = Http.jsonBody (calendarEntryEncoder model)
        , expect = HttpEx.expectString SaveCalendarResult
        , timeout = Nothing
        , tracker = Nothing
        }


loadCalendarEntries : String -> Cmd CalendarList.Msg
loadCalendarEntries token =
    Http.request
        { method = "GET"
        , headers = [Http.header "Authorization" ("Bearer " ++ token)]
        , url = Server.calendarEntries
        , body = Http.emptyBody
        , expect = HttpEx.expectString CalendarList.GetCalendarEntriesResult
        , timeout = Nothing
        , tracker = Nothing
        }


loadCalendarEntry : String -> Int -> Cmd Msg
loadCalendarEntry token cId =
    Http.request
        { method = "GET"
        , headers = [Http.header "Authorization" ("Bearer " ++ token)]
        , url = Server.calendarEntry cId
        , body = Http.emptyBody
        , expect = HttpEx.expectString GetCalendarEntryResult
        , timeout = Nothing
        , tracker = Nothing
        }


calendarEntriesResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> CalendarList.Model -> CalendarList.Model
calendarEntriesResponse response model =
    case response of
        Ok value ->
            let
                resp =
                    parseCalendarEntriesResult value
            in
            case resp of
                Ok calendarEntries ->
                    { model | calendarEntries = calendarEntries }

                Err error ->
                    { model | problems = [ error ] }

        Err error ->
            { model | problems = calendarErrorsDecoder error }


calendarEntryResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> Model -> Messages -> Model
calendarEntryResponse response model sucessMessage =
    case response of
        Ok value ->
            let
                resp =
                    parseCalendarEntryResult value
            in
            case resp of
                Ok calendarEntry ->
                    { model | calendarEntry = calendarEntry, messages = sucessMessage }

                Err error ->
                    { model | messages = Problems [ error ] }

        Err error ->
            { model | messages = Problems (calendarErrorsDecoder error) }


getCalendarEntryResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> Model -> Model
getCalendarEntryResponse response model =
    calendarEntryResponse response model (Problems [])


saveCalendarEntryResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> Model -> Model
saveCalendarEntryResponse response model =
    calendarEntryResponse response model SuccessUpdate
