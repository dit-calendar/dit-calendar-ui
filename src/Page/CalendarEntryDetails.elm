module Page.CalendarEntryDetails exposing (init, initEmptyModelForPageReload, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button exposing (onClick)
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Utilities.Spacing as Spacing
import Browser.Navigation as Navigation
import Data.CalendarEntry exposing (CalendarDetailMsg(..), CalendarEntry, Model, Msg(..), emptyCalendarEntry)
import Data.Task exposing (emptyTask)
import Data.UIMessages exposing (Messages(..))
import Endpoint.CalendarEntryEndpoint exposing (copyCalendarEntry, createCalendarEntry, getCalendarEntryResponse, loadCalendarEntry, saveCalendarEntry, saveCalendarEntryResponse)
import Endpoint.CalendarTaskEndpoint exposing (calendarEntryTasksResponse, loadCalendarEntryTasks)
import Endpoint.TaskEndpoint exposing (copyTasks)
import Html exposing (Html, div, h4, q, text)
import Html.Attributes exposing (class)
import Html.Events as HtmlEvent
import Maybe exposing (withDefault)


init : CalendarEntry -> ( Model, Cmd Msg )
init cal =
    ( { calendarEntry = cal, tasks = [], messages = Problems [] }, Cmd.none )


initEmptyModelForPageReload : Int -> Model
initEmptyModelForPageReload cId =
    let
        cEmptyCalendarEntry =
            { emptyCalendarEntry | entryId = Just cId }
    in
    { calendarEntry = cEmptyCalendarEntry, tasks = [], messages = Problems [] }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CalendarDetailEditMsg calendarDetialMsg ->
            ( { model | calendarEntry = updateCalendarDetails calendarDetialMsg model.calendarEntry }, Cmd.none )

        GetCalendarEntryTasksResult result ->
            ( calendarEntryTasksResponse result model, Cmd.none )

        SaveCalendar ->
            ( { model | messages = Problems [] }
            , if not (model.calendarEntry.entryId == Nothing) then
                saveCalendarEntry model.calendarEntry

              else
                createCalendarEntry model.calendarEntry
            )

        SaveCalendarResult result ->
            ( saveCalendarEntryResponse result model, Cmd.none )

        OpenTaskDetailsView _ ->
            ( model, Cmd.none )

        GetCalendarEntryResult result ->
            let
                newCalendarModel =
                    getCalendarEntryResponse result model
            in
            ( newCalendarModel, loadCalendarEntryTasks (withDefault 0 newCalendarModel.calendarEntry.entryId) )

        GetCalendarEntry ->
            --TODO sobald id in url übergeben wird, werden tasks nachgeladen; selbst wenn calendar nicht existent
            ( model, loadCalendarEntry (withDefault 0 model.calendarEntry.entryId) )

        CopyCalendar ->
            let
                oldCalendarEntry =
                    model.calendarEntry

                newCalendarEntry =
                    { oldCalendarEntry | title = oldCalendarEntry.title ++ " (copy)", entryId = Nothing, version = 0 }
            in
            ( { model | messages = Problems [] }, Cmd.batch [ copyCalendarEntry newCalendarEntry ] )

        CopyCalendarResult result ->
            let
                newModel =
                    getCalendarEntryResponse result model
            in
            ( newModel, copyTasks (withDefault 0 newModel.calendarEntry.entryId) model.tasks )

        GetCalendarEntryTasksAfterCopyResult _ ->
            ( model, Navigation.load ("#calendar/" ++ String.fromInt (withDefault 0 model.calendarEntry.entryId)) )


updateCalendarDetails : CalendarDetailMsg -> CalendarEntry -> CalendarEntry
updateCalendarDetails msg model =
    case msg of
        Title title ->
            { model | title = title }

        Description des ->
            { model | description = Just des }

        StartDate startD ->
            { model | startDate = startD }

        StartTime startT ->
            { model | startTime = startT }

        EndDate endD ->
            { model | endDate = endD }

        EndTime endT ->
            { model | endTime = endT }


view : Model -> Html Msg
view model =
    let
        calendarInfo =
            model.calendarEntry

        tasks =
            model.tasks
    in
    div []
        [ Form.form []
            [ case calendarInfo.entryId of
                Just _ ->
                    h4 []
                        [ text "Kalendar Eintrag"
                        , Button.button
                            [ Button.outlineSecondary
                            , Button.onClick CopyCalendar
                            , Button.attrs [ Spacing.ml1 ]
                            ]
                            [ text "copy" ]
                        ]

                Nothing ->
                    h4 [] [ text "Kalendar Eintrag" ]
            , Form.formInline []
                [ Form.label [] [ text "title" ]
                , Input.text [ Input.value calendarInfo.title, Input.onInput (CalendarDetailEditMsg << Title) ]
                ]
            , Form.formInline []
                [ Form.label [] [ text "description" ]
                , Input.text [ Input.value (withDefault "" calendarInfo.description), Input.onInput (CalendarDetailEditMsg << Description) ]
                ]
            , Form.formInline []
                [ Form.label [] [ text "start date" ]
                , Input.date [ Input.value calendarInfo.startDate, Input.onInput (CalendarDetailEditMsg << StartDate) ]
                ]
            , Form.formInline []
                [ Form.label [] [ text "end date" ]
                , Input.date [ Input.value calendarInfo.endDate, Input.onInput (CalendarDetailEditMsg << EndDate) ]
                ]
            ]
        , ListGroup.custom
            (List.map
                (\task ->
                    ListGroup.button [ ListGroup.attrs [ HtmlEvent.onClick (OpenTaskDetailsView task) ] ] [ text ("task: " ++ task.title) ]
                )
                tasks
            )
        , Button.button [ Button.success, onClick SaveCalendar ] [ text "Speichern" ]

        -- hide button for create task if calendar entry is not existing yet
        , case calendarInfo.entryId of
            Just eId ->
                Button.button
                    [ Button.primary, Button.onClick (OpenTaskDetailsView (emptyTask eId calendarInfo.startDate)), Button.attrs [ Spacing.ml1 ] ]
                    [ text "Neuen Task Eintrag erstellen" ]

            Nothing ->
                div [] []
        , case calendarInfo.entryId of
            Just eId ->
                div [ Spacing.mt2 ]
                    [ Card.config [ Card.outlinePrimary, Card.attrs [ Spacing.mb3 ] ]
                        |> Card.header [] [ text "How to post this calendar entry in telegram?" ]
                        |> Card.block []
                            [ Block.text [] [ text "send in your telegram chat: ", q [] [ text ("/postcalendar " ++ String.fromInt eId) ] ]
                            ]
                        |> Card.view
                    ]

            Nothing ->
                div [] []
        , case model.messages of
            Problems errors ->
                div [ class "error-messages" ] (List.map viewProblem errors)

            SuccessUpdate ->
                div [ class "success-messages" ]
                    [ viewSuccess "Kalendereintrag erfolgreich gespeichert" ]
        ]


viewProblem : String -> Html msg
viewProblem problem =
    Alert.simpleDanger [] [ text problem ]


viewSuccess : String -> Html msg
viewSuccess success =
    Alert.simpleSuccess [] [ text success ]
