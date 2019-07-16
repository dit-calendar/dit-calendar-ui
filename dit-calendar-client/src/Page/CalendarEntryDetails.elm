module Page.CalendarEntryDetails exposing (CalendarEntry, Model, Msg(..), init, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup
import Endpoint.ResponseErrorDecoder exposing (calendarErrorDecoder)
import Env.Serverurl as Server
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http
import Http.Detailed as HttpEx
import Json.Decode as Decode exposing (Value)
import Tuple exposing (mapFirst)


type alias CalendarEntry =
    { entryId : Maybe Int
    , version : Int
    , description : String
    , startDate : String
    , endDate : String
    }


type Task
    = Task String -- Todo use Task.Model from Task.elm(page)


getBla : Task -> String
getBla (Task str) =
    str


type alias Model =
    { calendarEntry : CalendarEntry
    , tasks : List Task
    , problems : List String
    }


initModel : CalendarEntry -> Model
initModel cal =
    { calendarEntry = cal, tasks = [], problems = [] }


type CalendarDetialMsg
    = Description String
    | StartDate String
    | EndDate String


type Msg
    = CalendarDetialMsg CalendarDetialMsg
    | GetCalendarEntryTasks Int
    | GetCalendarEntryTasksResult (Result (HttpEx.Error String) ( Http.Metadata, String ))


init : Int -> CalendarEntry -> ( Model, Cmd Msg )
init calId cal =
    update (GetCalendarEntryTasks calId) (initModel cal)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CalendarDetialMsg calendarDetialMsg ->
            mapFirst (\x -> { model | calendarEntry = x }) (updateCalendarDetials calendarDetialMsg model.calendarEntry)

        GetCalendarEntryTasks taskId ->
            ( model, loadCalendarEntryTasks taskId )

        GetCalendarEntryTasksResult result ->
            ( calendarEntryTasksResponse result model, Cmd.none )


updateCalendarDetials : CalendarDetialMsg -> CalendarEntry -> ( CalendarEntry, Cmd Msg )
updateCalendarDetials msg model =
    case msg of
        Description des ->
            ( { model | description = des }, Cmd.none )

        StartDate startD ->
            ( { model | startDate = startD }, Cmd.none )

        EndDate endD ->
            ( { model | endDate = endD }, Cmd.none )


loadCalendarEntryTasks : Int -> Cmd Msg
loadCalendarEntryTasks taskId =
    Http.riskyRequest
        { method = "GET"
        , headers = []
        , url = Server.calendarTask taskId
        , body = Http.emptyBody
        , expect = HttpEx.expectString GetCalendarEntryTasksResult
        , timeout = Nothing
        , tracker = Nothing
        }


calendarEntryTasksResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> Model -> Model
calendarEntryTasksResponse response model =
    case response of
        Ok value ->
            let
                resp =
                    calendarEntryTasksDecoder value
            in
            case resp of
                Ok tasks ->
                    { model | tasks = tasks }

                Err error ->
                    { model | problems = [ error ] }

        Err error ->
            { model | problems = calendarErrorDecoder error }


calendarEntryTasksDecoder : ( Http.Metadata, String ) -> Result String (List Task)
calendarEntryTasksDecoder ( meta, body ) =
    let
        decode =
            Decode.decodeString (Decode.list tasksDecoder) body
    in
    case decode of
        Ok calendarEntries ->
            Ok calendarEntries

        Err error ->
            Err ("fehler beim decodieren der calendars taks" ++ Decode.errorToString error)


tasksDecoder : Decode.Decoder Task
tasksDecoder =
    Decode.map
        -- TODO reuse decoder from Tasks?
        Task
        (Decode.at [ "description" ] Decode.string)


view : Model -> Html Msg
view model =
    let
        calendarInfo =
            model.calendarEntry

        tasks =
            model.tasks
    in
    div []
        [ Grid.container []
            [ Grid.row []
                [ Grid.col [] [ text ("description: " ++ calendarInfo.description) ]
                , Grid.col [] [ text ("start date:" ++ calendarInfo.startDate) ]
                , Grid.col [] [ text ("end date:" ++ calendarInfo.endDate) ]
                ]
            , ListGroup.ul
                (List.map
                    (\task ->
                        ListGroup.li [] [ text ("task: " ++ getBla task) ]
                    )
                    tasks
                )
            ]
        , div [ class "error-messages" ]
            (List.map viewProblem model.problems)
        ]


viewProblem : String -> Html msg
viewProblem problem =
    Alert.simpleDanger [] [ text problem ]
