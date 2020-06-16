module Endpoint.JsonParser.ResponseErrorDecoder exposing (ErrorResponse, errorDecoder, errorDecoderJson)

import Http.Detailed as HttpEx
import Json.Decode as Decode


errorDecoder : HttpEx.Error String -> List String
errorDecoder responseError =
    case responseError of
        HttpEx.BadStatus _ body ->
            [body]

        HttpEx.NetworkError ->
            [ "network error" ]

        _ ->
            [ "unknown error" ]

errorDecoderJson : HttpEx.Error String -> Decode.Decoder ErrorResponse -> List String
errorDecoderJson responseError responseDecoder =
    case responseError of
        HttpEx.BadStatus metadata body ->
            let
                decode =
                    Decode.decodeString responseDecoder body
            in
            case decode of
                Ok regResponse ->
                    [ regResponse.message ]

                Err error ->
                    --TODO beim decodieren des Fehlers ist was scheifgelaufen
                    [ "beim decodieren des Fehlers ist was scheifgelaufen. "
                        ++ "StatusCode: "
                        ++ String.fromInt metadata.statusCode
                        ++ ", StatusCode: "
                        ++ metadata.statusText
                        ++ ", error message: "
                        ++ Decode.errorToString error
                    ]

        HttpEx.NetworkError ->
            [ "network error" ]

        _ ->
            [ "unknown error" ]


type alias ErrorResponse =
    { message : String
    }
