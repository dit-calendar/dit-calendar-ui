module Data.SimpleCalendarList exposing (Model, Msg(..), emptyModel)

import Data.CalendarEntry exposing (CalendarEntry)
import Http
import Http.Detailed as HttpEx


type alias Model =
    { calendarEntries : List CalendarEntry
    , problems : List String
    , token: String
    }


emptyModel : String -> Model
emptyModel jwt =
    { calendarEntries = [], problems = [], token = jwt }


type Msg
    = PerformGetCalendarEntries
    | GetCalendarEntriesResult (Result (HttpEx.Error String) ( Http.Metadata, String ))
    | OpenCalendarDetailsView CalendarEntry
