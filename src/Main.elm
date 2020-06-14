module Main exposing (Model, Msg(..), init, main, update, view)

import Bootstrap.Button as Button exposing (onClick)
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Data.CalendarEntry as CalendarEntryDetails
import Data.Login as Login
import Data.Logout as Logout
import Data.Register as Register exposing (RegisterModel)
import Data.SimpleCalendarList as CalendarList
import Data.Task as TaskDetail
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe
import Page.CalendarEntryDetails as CalendarEntryDetails
import Page.Login as Login
import Page.Logout as LogoutService
import Page.Register as Register
import Page.SimpleCalendarList as CalendarList
import Page.TaskDetail as TaskDetail
import ServerDeploy as ServerDeploy
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser)


type alias Flags =
    {}


type alias Model =
    { navKey : Navigation.Key
    , page : Page
    , navState : Navbar.State
    , serverStarUpLoadingState : ServerDeploy.Model
    , token : String
    }


type Page
    = LoginPage Login.Model
    | RegisterPage Register.Model
    | SimpleCalendarPage CalendarList.Model
    | CalendarDetailsPage CalendarEntryDetails.Model
    | TaskDetailPage TaskDetail.Model
    | NotFound


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChange
        }


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( serverStartUpState, serverStartUpCmd ) =
            ServerDeploy.update ServerDeploy.CheckHealth ServerDeploy.initModel

        ( model, urlCmd ) =
            urlUpdate url
                { navKey = key
                , navState = navState
                , page = LoginPage { name = "", password = "", problems = [], token = "" }
                , serverStarUpLoadingState = serverStartUpState
                , token = ""
                }
    in
    ( model, Cmd.batch [ urlCmd, navCmd, Cmd.map ServerHealthMsg serverStartUpCmd ] )


type Msg
    = UrlChange Url
    | ClickedLink UrlRequest
    | NavMsg Navbar.State
    | LoginMsg Login.Msg
    | RegisterMsg Register.Msg
    | CalendarMsg CalendarList.Msg
    | CalendarDetailMsg CalendarEntryDetails.Msg
    | TaskDetailMsg TaskDetail.Msg
    | LogoutMsg Logout.Msg
    | ServerHealthMsg ServerDeploy.Msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink req ->
            case req of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.navKey <| Url.toString url )

                Browser.External href ->
                    ( model, Navigation.load href )

        UrlChange url ->
            urlUpdate url model

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )

        LoginMsg loginMsg ->
            case model.page of
                LoginPage login ->
                    stepLogin model (Login.update loginMsg login)

                _ ->
                    --TODO kann das enfernt werden?
                    ( model, Cmd.none )

        RegisterMsg regMsg ->
            case model.page of
                RegisterPage register ->
                    stepRegister model (Register.update regMsg register)

                _ ->
                    ( model, Cmd.none )

        CalendarMsg calendarMsg ->
            case calendarMsg of
                CalendarList.OpenCalendarDetailsView entry ->
                    case entry.entryId of
                        Just eId ->
                            ( model, Navigation.load ("#calendar/" ++ String.fromInt eId) )

                        Nothing ->
                            stepCalendarDetails model (CalendarEntryDetails.init model.token entry)

                _ ->
                    case model.page of
                        SimpleCalendarPage calendar ->
                            stepCalendar model (CalendarList.update calendarMsg { calendar | token = model.token })

                        _ ->
                            ( model, Cmd.none )

        CalendarDetailMsg calendarDetailMsg ->
            case calendarDetailMsg of
                CalendarEntryDetails.OpenTaskDetailsView task ->
                    stepTaskDetails model (TaskDetail.init model.token task)

                _ ->
                    case model.page of
                        CalendarDetailsPage calendarDetails ->
                            stepCalendarDetails model (CalendarEntryDetails.update calendarDetailMsg { calendarDetails | token = model.token })

                        _ ->
                            ( model, Cmd.none )

        TaskDetailMsg tskMsg ->
            case model.page of
                TaskDetailPage task ->
                    stepTaskDetails model (TaskDetail.update tskMsg { task | token = model.token })

                _ ->
                    ( model, Cmd.none )

        LogoutMsg logoutMsg ->
            stepLogout model (LogoutService.update logoutMsg)

        ServerHealthMsg startUpMsg ->
            stepHealth model (ServerDeploy.update startUpMsg model.serverStarUpLoadingState)


stepHealth : Model -> ( ServerDeploy.Model, Cmd ServerDeploy.Msg ) -> ( Model, Cmd Msg )
stepHealth model ( startUpState, cmd ) =
    ( { model | serverStarUpLoadingState = startUpState }
    , Cmd.map ServerHealthMsg cmd
    )


stepLogin : Model -> ( Login.Model, Cmd Login.Msg ) -> ( Model, Cmd Msg )
stepLogin model ( login, cmds ) =
    ( { model | page = LoginPage login, token = login.token }
    , Cmd.map LoginMsg cmds
    )


stepLogout : Model -> Cmd Logout.Msg -> ( Model, Cmd Msg )
stepLogout model cmds =
    ( model
    , Cmd.map LogoutMsg cmds
    )


stepRegister : Model -> ( Register.Model, Cmd Register.Msg ) -> ( Model, Cmd Msg )
stepRegister model ( register, cmds ) =
    ( { model | page = RegisterPage register }
    , Cmd.map RegisterMsg cmds
    )


stepCalendar : Model -> ( CalendarList.Model, Cmd CalendarList.Msg ) -> ( Model, Cmd Msg )
stepCalendar model ( calendar, cmds ) =
    ( { model | page = SimpleCalendarPage calendar }
    , Cmd.map CalendarMsg cmds
    )


stepCalendarDetails : Model -> ( CalendarEntryDetails.Model, Cmd CalendarEntryDetails.Msg ) -> ( Model, Cmd Msg )
stepCalendarDetails model ( calendarDetail, cmds ) =
    ( { model | page = CalendarDetailsPage calendarDetail }
    , Cmd.map CalendarDetailMsg cmds
    )


stepTaskDetails : Model -> ( TaskDetail.Model, Cmd TaskDetail.Msg ) -> ( Model, Cmd Msg )
stepTaskDetails model ( task, cmds ) =
    ( { model | page = TaskDetailPage task }
    , Cmd.map TaskDetailMsg cmds
    )


urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case decode url of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            case route of
                SimpleCalendarPage _ ->
                    -- needed to perform request if url was changed
                    stepCalendar model (CalendarList.update CalendarList.PerformGetCalendarEntries (CalendarList.emptyModel model.token))

                CalendarDetailsPage calendar ->
                    -- needed to perform request if url was changed
                    stepCalendarDetails model (CalendarEntryDetails.update CalendarEntryDetails.GetCalendarEntry { calendar | token = model.token })

                _ ->
                    ( { model | page = route }, Cmd.none )


decode : Url -> Maybe Page
decode url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> UrlParser.parse routeParser


routeParser : Parser (Page -> a) a
routeParser =
    let
        reg =
            RegisterModel "" "" "" ""
    in
    UrlParser.oneOf
        [ UrlParser.map (LoginPage { name = "", password = "", problems = [], token = "" }) UrlParser.top
        , UrlParser.map (LoginPage { name = "", password = "", problems = [], token = "" }) (UrlParser.s "login")
        , UrlParser.map (RegisterPage { register = reg, problems = [] }) (UrlParser.s "register")
        , UrlParser.map (SimpleCalendarPage (CalendarList.emptyModel "")) (UrlParser.s "calendar")
        , UrlParser.map (\num -> CalendarDetailsPage (CalendarEntryDetails.initEmptyModelForPageReload num)) (UrlParser.s "calendar" </> UrlParser.int)
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "DIT Calendar"
    , body =
        [ div []
            [ menuView model
            , mainContent model
            ]
        ]
    }


menuView : Model -> Html Msg
menuView model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.brand [ href "#" ] [ text "DIT Calendar" ]
        |> Navbar.info
        |> Navbar.items
            [ Navbar.itemLink [ href "#login" ] [ text "Login" ]
            , Navbar.itemLink [ href "#register" ] [ text "Register" ]
            , Navbar.itemLink [ href "#calendar" ] [ text "Calendar" ]
            ]
        |> Navbar.customItems
            [ Navbar.formItem []
                [ Button.button
                    [ Button.success
                    , onClick (LogoutMsg Logout.TriggerLogout)
                    ]
                    [ text "logout" ]
                ]
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            LoginPage login ->
                [ Html.map LoginMsg (Login.view login)
                , Html.map ServerHealthMsg (ServerDeploy.view model.serverStarUpLoadingState)
                ]

            RegisterPage register ->
                [ Html.map RegisterMsg (Register.view register) ]

            NotFound ->
                pageNotFound

            SimpleCalendarPage calendars ->
                [ Html.map CalendarMsg (CalendarList.view calendars) ]

            CalendarDetailsPage calendarDetail ->
                [ Html.map CalendarDetailMsg (CalendarEntryDetails.view calendarDetail) ]

            TaskDetailPage taskDetails ->
                [ Html.map TaskDetailMsg (TaskDetail.view taskDetails) ]


pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "Sorry couldn't find that page"
    ]
