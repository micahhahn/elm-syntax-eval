module List.Extra2 exposing (exactZip)

{-| -}


{-| Like List.Extra.zip, but fails if lists are not the same length.
-}
exactZip : List a -> List b -> Maybe (List ( a, b ))
exactZip left right =
    let
        go todoLeft todoRight done =
            case ( todoLeft, todoRight ) of
                ( leftHead :: otherLeft, rightHead :: otherRight ) ->
                    go otherLeft otherRight (( leftHead, rightHead ) :: done)

                ( [], [] ) ->
                    Just <| List.reverse done

                _ ->
                    Nothing
    in
    go left right []
