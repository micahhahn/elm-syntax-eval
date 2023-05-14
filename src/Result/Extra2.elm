module Result.Extra2 exposing (combineFold)


combineFold : (a -> state -> Result err state) -> state -> List a -> Result err state
combineFold fold state items =
    case items of
        [] ->
            Ok state

        item :: others ->
            case fold item state of
                Err err ->
                    Err err

                Ok nextState ->
                    combineFold fold nextState others
