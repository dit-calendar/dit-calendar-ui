module Data.CalendarEntry exposing (CalendarDetailMsg(..), CalendarEntry, Model, Msg(..), emptyCalendarEntry)

import Data.Task exposing (Task)
import Data.UIMessages exposing (Messages)
import Http
import Http.Detailed as HttpEx


type alias CalendarEntry =
    { entryId : Maybe Int
    , version : Int
    , title : String
    , description : Maybe String
    , startDate : String
    , startTime : String
    , endDate : String
    , endTime : String
    }


type alias Model =
    { calendarEntry : CalendarEntry
    , tasks : List Task
    , messages : Messages
    }


emptyCalendarEntry : CalendarEntry
emptyCalendarEntry =
    { entryId = Nothing, version = 0, title = "", description = Nothing, startDate = "", startTime = "00:00", endDate = "", endTime = "23:59" }


type CalendarDetailMsg
    = Title String
    | Description String
    | StartDate String
    | StartTime String
    | EndDate String
    | EndTime String


type Msg
    = CalendarDetailEditMsg CalendarDetailMsg
    | GetCalendarEntryMsg
    | GetCalendarEntryResult (Result (HttpEx.Error String) ( Http.Metadata, String ))
    | GetCalendarEntryTasksResult (Result (HttpEx.Error String) ( Http.Metadata, String ))
    | GetCalendarEntryTasksAfterCopyResult (Result (HttpEx.Error String) ( Http.Metadata, String ))
    | SaveCalendarMsg
    | CopyCalendarMsg
    | SaveCalendarResult (Result (HttpEx.Error String) ( Http.Metadata, String ))
    | CopyCalendarResult (Result (HttpEx.Error String) ( Http.Metadata, String ))
    | OpenTaskDetailsViewMsg Task
