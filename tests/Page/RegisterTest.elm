module Page.RegisterTest exposing (updateTest)

import Data.Register as RegisterPage exposing (Model)
import Expect
import Http.Detailed exposing (Error(..))
import Page.Register as RegisterPage
import Test exposing (Test, describe, test)
import Tuple exposing (first, second)


updateTest : Test
updateTest =
    describe "update"
        [ describe "registerResult"
            [ test "case error" <|
                \_ ->
                    let
                        registerResult =
                            RegisterPage.RegisterResult (Err (BadUrl "error1"))

                        startModel =
                            Model (RegisterPage.RegisterModel "" "" "" "") [ "start Fehler" ]

                        expectedModel =
                            Model (RegisterPage.RegisterModel "" "" "" "") [ "unknown error" ]
                    in
                    Expect.all
                        [ first >> Expect.equal expectedModel
                        , second >> Expect.equal Cmd.none
                        ]
                        (RegisterPage.update registerResult startModel)
            ]
        ]
