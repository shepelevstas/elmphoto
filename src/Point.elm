module Point exposing (..)

import Math exposing (lim)
import Rect exposing (Rect)


type alias Point =
    ( Float, Float )


delta : Point -> Point -> Point
delta ( ax, ay ) ( bx, by ) =
    ( bx - ax, by - ay )


add : Point -> Point -> Point
add ( ax, ay ) ( bx, by ) =
    ( ax + bx, ay + by )


mul : Float -> Point -> Point
mul v ( x, y ) =
    ( v * x, v * y )


flip : Point -> Point
flip ( x, y ) =
    ( y, x )


len : Point -> Float
len ( x, y ) =
    sqrt (x * x + y * y)


left : Point -> Point -> Point
left (( ax, _ ) as a) (( bx, _ ) as b) =
    if ax < bx then
        a

    else
        b


isLeftTurn : Point -> Point -> Bool
isLeftTurn ( ax, ay ) ( bx, by ) =
    ax * by < ay * bx


fromRect : Rect -> Point
fromRect { x, y } =
    ( x, y )


toRect : Point -> Point -> Rect
toRect ( x, y ) ( w, h ) =
    { x = x, y = y, w = w, h = h }


rectSize : Rect -> Point
rectSize { w, h } =
    ( w, h )


pointSize : Rect -> ( Point, Point )
pointSize { x, y, w, h } =
    ( ( x, y ), ( w, h ) )


dot : Point -> Point -> Float
dot ( x1, y1 ) ( x2, y2 ) =
    x1 * x2 + y1 * y2


clamp : Rect -> Point -> Point
clamp { x, y, w, h } ( px, py ) =
    ( lim x (x + w) px
    , lim y (y + h) py
    )


hits : Rect -> Point -> Bool
hits { x, y, w, h } ( px, py ) =
    not (px < x || py < y || px > x + w || py > y + h)


fit : Point -> Point -> ( Point, Float )
fit ( trg_w, trg_h ) ( src_w, src_h ) =
    let
        scale =
            min (trg_w / src_w) (trg_h / src_h)
    in
    ( ( src_w * scale, src_h * scale ), scale )


ratio : Point -> Float
ratio ( w, h ) =
    w / h


toRelative : Point -> Point -> Point
toRelative ( w, h ) ( x, y ) =
    ( x / w, y / h )


fromRelative : Point -> Point -> Point
fromRelative ( w, h ) ( x_, y_ ) =
    ( x_ * w, y_ * h )
