module Units.UtilsTest exposing (utilsTests)

import Test exposing (describe, test)
import Expect exposing (equal)
import Fuzz exposing (list, int, tuple, string)
import Utils exposing (demandPercentage)


utilsTests =
    [ describe "demandPercentage"
        [ test "it formats with 2 decimals any float" <|
            \() ->
                equal (demandPercentage 42.4242) "424.24"
        ]
    ]
