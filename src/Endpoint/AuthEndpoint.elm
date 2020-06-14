module Endpoint.AuthEndpoint exposing (login, logout, loginResponse, register, registerResponse)

import Data.Login as Login
import Data.Logout as Logout
import Data.Register as Register
import Endpoint.JsonParser.AuthParser exposing (authErrorDecoder, loginEncoder, registerEncoder)
import Env.Serverurl as Server
import Http as Http
import Http.Detailed as HttpEx


login : Login.Model -> Cmd Login.Msg
login model =
    Http.post
        { url = Server.loginUrl
        , body = Http.jsonBody (loginEncoder model)
        , expect = HttpEx.expectString Login.HttpLogin
        }

logout : Cmd Logout.Msg
logout =
    Http.post
        { url = Server.logoutUrl
        , body = Http.emptyBody
        , expect = HttpEx.expectString Logout.HttpLogout
        }


loginResponse : HttpEx.Error String -> Login.Model -> Login.Model
loginResponse error model =
    { model | problems = authErrorDecoder error }


register : Register.Model -> Cmd Register.Msg
register model =
    Http.post
        { url = Server.registerUrl
        , body = Http.jsonBody (registerEncoder model)
        , expect = HttpEx.expectString Register.RegisterResult
        }


registerResponse : Result (HttpEx.Error String) ( Http.Metadata, String ) -> Register.Model -> Register.Model
registerResponse response model =
    case response of
        Ok value ->
            model

        Err error ->
            { model | problems = authErrorDecoder error }
