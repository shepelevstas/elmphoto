module Rect exposing (..)

-- import Point exposing (Point)

import Math exposing (lim)


type alias Rect =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }


width : Rect -> Float
width { w } =
    w


height : Rect -> Float
height { h } =
    h


restrict : Rect -> Rect -> Rect
restrict { x, y, w, h } src =
    let
        scale =
            min 1 (min (w / src.w) (h / src.h))

        new_w =
            scale * src.w

        new_h =
            scale * src.h

        scaled_x =
            src.x + (src.w - new_w) / 2

        scaled_y =
            src.y + (src.h - new_h) / 2

        new_x =
            lim x (x + w - new_w) scaled_x

        new_y =
            lim y (y + h - new_h) scaled_y
    in
    Rect new_x new_y new_w new_h


flipSize : Rect -> Rect
flipSize { x, y, w, h } =
    { x = x, y = y, w = h, h = w }


turn : Rect -> Rect
turn { x, y, w, h } =
    let
        w2 =
            w / 2

        h2 =
            h / 2

        c_x =
            x + w2

        c_y =
            y + h2

        new_x =
            c_x - h2

        new_y =
            c_y - w2
    in
    Rect new_x new_y h w


bottomRight : Rect -> ( Float, Float )
bottomRight { x, y, w, h } =
    ( x + w, y + h )
