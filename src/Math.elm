module Math exposing (..)


lim : number -> number -> number -> number
lim a b v =
    max a (min v b)


minimum : comparable -> List comparable -> comparable
minimum x xs =
    case List.minimum xs of
        Just x_ ->
            if x < x_ then
                x

            else
                x_

        Nothing ->
            x


maximum : comparable -> List comparable -> comparable
maximum x xs =
    case List.maximum xs of
        Nothing ->
            x

        Just x_ ->
            if x > x_ then
                x

            else
                x_
