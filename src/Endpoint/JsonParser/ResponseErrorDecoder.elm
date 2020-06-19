module Endpoint.JsonParser.ResponseErrorDecoder exposing (ErrorResponse, errorDecoder, errorDecoderJson)

import Http.Detailed as HttpEx
import Json.Decode as Decode


errorDecoder : HttpEx.Error String -> List String
errorDecoder responseError =
    case responseError of
        HttpEx.BadStatus _ body ->
            [body]

        HttpEx.NetworkError ->
            [ "network problem" ]

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
                    [ "error when decoding the error message"
                        ++ "StatusCode: "
                        ++ String.fromInt metadata.statusCode
                        ++ ", StatusCode: "
                        ++ metadata.statusText
                        ++ ", error message: "
                        ++ Decode.errorToString error
                    ]

        HttpEx.NetworkError ->
            [ "network problem" ]

        _ ->
            [ "unknown error" ]


type alias ErrorResponse =
    { message : String
    }
