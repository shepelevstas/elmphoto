port module Home exposing (main)

-- import List.Extra exposing (find)
-- import File.Select as Select
-- import Math as M exposing (lim)
-- import Debug exposing (log)

import Browser
import Canvas as C
import Canvas.Settings as CS
import Canvas.Settings.Advanced as CSA
import Canvas.Settings.Line
import Canvas.Texture exposing (Texture)
import Color
import File exposing (File)
import Html exposing (Attribute, Html, button, div, {- img, -} input, label, span, text)
import Html.Attributes exposing (accept, class, multiple, {- src, -} style, type_, value)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Html.Events.Extra.Mouse as Mouse
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Json.Decode as D exposing (Value)
import List exposing (filter, map)
import Point as P exposing (Point)
import Rect as R exposing (Rect)
import Tuple exposing (first)


size_handle_offset : Point
size_handle_offset =
    ( -6, -6 )


size_handle_size : Point
size_handle_size =
    ( 12, 12 )


default_cropmode : CropMode
default_cropmode =
    Fill


default_printSize_mm : PrintSize
default_printSize_mm =
    ( 102, 152 )


type alias PrintSize =
    ( Int, Int )


type alias Photo =
    { id : Int
    , texture : Texture
    , size : PrintSize
    , horizontal : Bool
    , file : File
    , name : String
    , cur : Cur
    , prints :
        { list : List Print
        , count : Int
        }
    , print : Print
    }


type alias Print =
    { id : Int
    , size : PrintSize -- in mm: 102,152
    , cropmode : CropMode -- Fil | Fill
    , q : Int
    , turn : Turn
    , lim : Rect
    , img : Rect
    , imgscale : Float
    , horizontal : Bool -- crop horizontal image or not
    , cropratio : Float
    , crop : Rect
    , sz : Rect
    }


calc : CropMode -> PrintSize -> Point -> Turn -> Maybe Rect -> Bool -> { cropratio : Float, lim : Rect, img : Rect, imgscale : Float, crop : Rect, sz : Rect }
calc mode print_size_mm img_size turn mb_prev_crop max_crop =
    let
        lim =
            P.toRect lim_P lim_size

        lim_P =
            P.mul 0.5 (P.delta lim_size ( 276, 276 ))

        ( short, long ) =
            Tuple.mapBoth toFloat toFloat print_size_mm

        print_size_mm_ =
            if P.ratio img_size_with_turn > 1 then
                ( long, short )

            else
                ( short, long )

        lim_size =
            if mode == Fit then
                P.fit ( 256, 256 ) print_size_mm_ |> first

            else
                P.fit ( 256, 256 ) img_size_with_turn |> first

        img =
            P.toRect img_P img_size_in_lim

        img_P =
            P.mul 0.5 (P.delta img_size_in_lim lim_size) |> P.add lim_P

        ( img_size_in_lim, img_scale ) =
            P.fit lim_size img_size_with_turn

        img_size_with_turn =
            if turn == TurnUp || turn == TurnDown then
                img_size

            else
                P.flip img_size

        ( crop, crop_P, crop_size ) =
            case mb_prev_crop of
                Nothing ->
                    let
                        crop_size_ =
                            P.fit lim_size print_size_mm_ |> first

                        crop_P_ =
                            P.mul 0.5 (P.delta crop_size_ lim_size) |> P.add lim_P

                        crop_ =
                            P.toRect crop_P_ crop_size_
                    in
                    ( crop_, crop_P_, crop_size_ )

                Just prev_crop ->
                    let
                        crop_ =
                            R.restrict lim prev_crop max_crop

                        crop_P_ =
                            P.fromRect crop_

                        crop_size_ =
                            P.rectSize crop_
                    in
                    ( crop_, crop_P_, crop_size_ )

        crop_ratio =
            P.ratio crop_size

        sz =
            P.toRect sz_P size_handle_size

        sz_P =
            P.add size_handle_offset (P.add crop_P crop_size)
    in
    { cropratio = crop_ratio, imgscale = img_scale, lim = lim, img = img, crop = crop, sz = sz }


type alias Model =
    { hover : Bool
    , photos : List Photo
    , photoCount : Int
    , dragging : Drag
    }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( Model False [] 0 DragNone, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> recvImage recvFileDecoder
        }


port sendValues : List Value -> Cmd msg


port recvImage : (( D.Value, D.Value, ( Int, Int ) ) -> msg) -> Sub msg


recvFileDecoder : ( D.Value, D.Value, ( Int, Int ) ) -> Msg
recvFileDecoder ( fileValue, imgValue, originalSize ) =
    case
        ( D.decodeValue File.decoder fileValue
        , Canvas.Texture.fromDomImage imgValue
        )
    of
        ( Ok file, Just img ) ->
            RecvImage ( file, img, originalSize )

        _ ->
            NoOp


type Cur
    = Null
    | Move
    | Cross


curStyle : Cur -> Attribute msg
curStyle cur =
    let
        val =
            case cur of
                Cross ->
                    "crosshair"

                Move ->
                    "move"

                Null ->
                    ""
    in
    style "cursor" val


type Drag
    = DragNone
    | DragMove Photo (Point -> Point)
    | DragSize Photo (Point -> Point)


type CropMode
    = Fill -- cut off what does not fit
    | Fit -- fit in the whole picture, add white padding


type Turn
    = TurnUp -- Original, no turn
    | TurnRight -- 90 deg clockwise turn
    | TurnLeft -- 90 deg counter-clockwise turn
    | TurnDown -- upside down turn, 180 deg


nextTurn : Turn -> Turn
nextTurn turn =
    case turn of
        TurnUp ->
            TurnRight

        TurnRight ->
            TurnDown

        TurnDown ->
            TurnLeft

        TurnLeft ->
            TurnUp


prevTurn : Turn -> Turn
prevTurn turn =
    case turn of
        TurnUp ->
            TurnLeft

        TurnLeft ->
            TurnDown

        TurnDown ->
            TurnRight

        TurnRight ->
            TurnUp



--
--    ####          ####
--    ####          ####
--    ####          ####
--    ####          ####
--    ####          ####
--    ####          ####
--    ####          ####
--     ####        ####
--      ##############
--        ##########
--
-- UPDATE


type Msg
    = NoOp
      -- dragging
    | DragEnter
    | DragLeave
      -- mouse
    | MouseDown Photo Point
    | MouseMove Photo Point
    | MouseUp
      -- editing
    | TurnPrev Photo
    | TurnNext Photo
    | ToggleCropMode Photo
    | TurnCrop Photo
    | Rename Photo String
    | Delete Int
      -- internal
    | GotFileValues (List Value)
    | RecvImage ( File, Texture, ( Int, Int ) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MouseDown ({ print } as photo) p ->
            if P.hits print.sz p then
                let
                    proj_p_in_crop np =
                        let
                            (( x_, y_ ) as new_mp) =
                                P.delta crop_P np

                            above =
                                P.isLeftTurn crop_size new_mp
                        in
                        if above then
                            ( x_, x_ / print.cropratio )

                        else
                            ( y_ * print.cropratio, y_ )

                    ( crop_P, crop_size ) =
                        P.pointSize print.crop

                    ( lim_P, ( lim_w, lim_h ) ) =
                        P.pointSize print.lim

                    ( crop_x, crop_y ) =
                        P.delta lim_P crop_P

                    diag_x_right =
                        let
                            x_ =
                                lim_w - crop_x
                        in
                        ( x_, x_ / print.cropratio )

                    diag_x_bottom =
                        let
                            y_ =
                                lim_h - crop_y
                        in
                        ( y_ * print.cropratio, y_ )

                    lim_x_p =
                        P.left diag_x_right diag_x_bottom

                    new_crop_size np =
                        P.left lim_x_p (proj_p_in_crop np)

                    dragging =
                        DragSize photo new_crop_size
                in
                ( { model | dragging = dragging }, Cmd.none )

            else if P.hits print.crop p then
                let
                    ( crop_P, crop_size ) =
                        P.pointSize print.crop

                    ( lim_P, lim_size ) =
                        P.pointSize print.lim

                    -- return new crop_P from new mouse point
                    delta_with_crop_P =
                        P.delta (P.delta crop_P p)

                    clamp =
                        P.clamp (P.toRect lim_P (P.delta crop_size lim_size))

                    new_crop_P =
                        delta_with_crop_P
                            >> clamp

                    dragging =
                        DragMove photo new_crop_P
                in
                ( { model | dragging = dragging }, Cmd.none )

            else
                ( model, Cmd.none )

        MouseMove { print, id } p ->
            case model.dragging of
                DragNone ->
                    let
                        cur =
                            if P.hits print.sz p then
                                Cross

                            else if P.hits print.crop p then
                                Move

                            else
                                Null

                        update_photo ph =
                            if ph.id == id then
                                { ph | cur = cur }

                            else
                                ph
                    in
                    ( { model
                        | photos =
                            map update_photo model.photos
                      }
                    , Cmd.none
                    )

                DragMove curPhoto xy ->
                    if curPhoto.id == id then
                        let
                            -- _ =
                            --     log "DragMove shapes" curPhoto.print.canv.shapes
                            crop_size =
                                P.rectSize print.crop

                            crop_P =
                                xy p

                            crop_bottom_right_P =
                                P.add crop_P crop_size

                            crop =
                                P.toRect crop_P crop_size

                            sz =
                                P.toRect (P.add size_handle_offset crop_bottom_right_P) size_handle_size

                            update_photo : Photo -> Photo
                            update_photo ph =
                                if ph.id == id then
                                    { curPhoto
                                        | print =
                                            { print
                                                | crop = crop
                                                , sz = sz
                                            }
                                    }

                                else
                                    ph
                        in
                        ( { model | photos = map update_photo model.photos }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                DragSize curPhoto wh ->
                    if curPhoto.id == id then
                        let
                            -- _ =
                            --     log "DragSize shapes" curPhoto.print.canv.shapes
                            curPrint =
                                curPhoto.print

                            crop_P =
                                P.fromRect print.crop

                            crop_size =
                                wh p

                            crop_bottom_right_P =
                                P.add crop_P crop_size

                            sz =
                                P.toRect (P.add crop_bottom_right_P size_handle_offset) size_handle_size

                            crop =
                                P.toRect crop_P crop_size

                            update_wh ph =
                                if ph.id == id then
                                    { ph | print = { curPrint | crop = crop, sz = sz } }

                                else
                                    ph
                        in
                        ( { model | photos = map update_wh model.photos }, Cmd.none )

                    else
                        ( model, Cmd.none )

        MouseUp ->
            ( { model | dragging = DragNone }, Cmd.none )

        DragEnter ->
            ( { model | hover = True }, Cmd.none )

        DragLeave ->
            ( { model | hover = False }, Cmd.none )

        GotFileValues values ->
            ( { model | hover = False }, sendValues values )

        Rename photo newName ->
            ( { model
                | photos =
                    map
                        (\ph ->
                            if ph.id == photo.id then
                                { photo | name = newName }

                            else
                                ph
                        )
                        model.photos
              }
            , Cmd.none
            )

        TurnNext ({ print, id } as photo) ->
            let
                { cropratio, imgscale, lim, img, crop, sz } =
                    calc print.cropmode print.size img_size turn (Just print.crop) (floor print.crop.w == floor print.lim.w || floor print.crop.h == floor print.lim.h)

                turn =
                    nextTurn print.turn

                img_size =
                    imageSize photo.texture

                new_print =
                    { print
                        | turn = turn
                        , cropratio = cropratio
                        , imgscale = imgscale
                        , lim = lim
                        , img = img
                        , crop = crop
                        , sz = sz
                    }

                new_photo =
                    { photo | print = new_print }

                update_photo ph =
                    if ph.id == id then
                        new_photo

                    else
                        ph
            in
            ( { model | photos = map update_photo model.photos }, Cmd.none )

        TurnPrev ({ print, id } as photo) ->
            let
                turn =
                    prevTurn print.turn

                img_size =
                    imageSize photo.texture

                { cropratio, imgscale, lim, img, crop, sz } =
                    calc print.cropmode print.size img_size turn (Just print.crop) (floor print.crop.w == floor print.lim.w || floor print.crop.h == floor print.lim.h)

                new_print =
                    { print
                        | turn = turn
                        , cropratio = cropratio
                        , imgscale = imgscale
                        , lim = lim
                        , img = img
                        , crop = crop
                        , sz = sz
                    }

                new_photo =
                    { photo | print = new_print }

                update_photo ph =
                    if ph.id == id then
                        new_photo

                    else
                        ph
            in
            ( { model | photos = map update_photo model.photos }, Cmd.none )

        ToggleCropMode ({ print } as photo) ->
            let
                cropmode =
                    if print.cropmode == Fit then
                        Fill

                    else
                        Fit

                img_size =
                    imageSize photo.texture

                -- _ =
                --     log "print.crop" print.crop
                -- _ =
                --     log "print.lim" print.lim
                max_crop =
                    let
                        crop_w_ =
                            floor print.crop.w

                        crop_h_ =
                            floor print.crop.h

                        lim_w_ =
                            floor print.lim.w

                        lim_h_ =
                            floor print.lim.h
                    in
                    crop_w_ == lim_w_ || crop_h_ == lim_h_

                { cropratio, imgscale, lim, img, crop, sz } =
                    calc cropmode print.size img_size print.turn (Just print.crop) max_crop

                new_print =
                    { print
                        | cropmode = cropmode
                        , cropratio = cropratio
                        , imgscale = imgscale
                        , lim = lim
                        , img = img
                        , crop = crop
                        , sz = sz
                    }

                new_photo =
                    { photo | print = new_print }

                update_photo ph =
                    if ph.id == photo.id then
                        new_photo

                    else
                        ph
            in
            ( { model | photos = map update_photo model.photos }, Cmd.none )

        TurnCrop ({ print } as photo) ->
            let
                horizontal =
                    not print.horizontal

                cropratio =
                    if horizontal then
                        P.ratio <| P.flip (Tuple.mapBoth toFloat toFloat print.size)

                    else
                        P.ratio (Tuple.mapBoth toFloat toFloat print.size)

                crop =
                    R.restrict print.lim (R.turn print.crop) False

                bottomRight =
                    R.bottomRight crop

                sz =
                    P.toRect (P.add size_handle_offset bottomRight) size_handle_size

                new_print =
                    { print
                        | horizontal = horizontal
                        , cropratio = cropratio
                        , crop = crop
                        , sz = sz
                    }

                new_photo =
                    { photo | print = new_print }

                update_photo ph =
                    if ph.id == photo.id then
                        new_photo

                    else
                        ph
            in
            ( { model | photos = map update_photo model.photos }, Cmd.none )

        Delete id ->
            ( { model
                | photos =
                    filter (.id >> (/=) id) model.photos
              }
            , Cmd.none
            )

        RecvImage ( file, image, ( originalWidth, originalHeight ) ) ->
            let
                img_size =
                    imageSize image

                horizontal =
                    originalWidth > originalHeight

                { cropratio, imgscale, lim, img, crop, sz } =
                    calc default_cropmode default_printSize_mm img_size TurnUp Nothing True

                print =
                    { id = 0
                    , size = default_printSize_mm
                    , q = 1
                    , turn = TurnUp
                    , horizontal = horizontal
                    , cropmode = default_cropmode
                    , cropratio = cropratio
                    , lim = lim
                    , img = img
                    , imgscale = imgscale
                    , crop = crop
                    , sz = sz
                    }

                photo =
                    { id = model.photoCount
                    , texture = image
                    , size = ( originalWidth, originalHeight )
                    , horizontal = horizontal
                    , file = file
                    , name = File.name file
                    , cur = Null
                    , prints = { count = 1, list = [ print ] }
                    , print = print
                    }
            in
            ( { model
                | photoCount = model.photoCount + 1
                , photos = model.photos ++ [ photo ]
              }
            , Cmd.none
            )



--
--   ####              ####
--    ####            ####
--     ####          ####
--      ####        ####
--       ####      ####
--        ####    ####
--         ####  ####
--          ########
--           ######
--            ####
--
-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "elm"
        , Mouse.onUp (always MouseUp)
        ]
        [ Keyed.node "div"
            [ class "photos" ]
            (map photoEditorKeyed model.photos ++ [ dropboxKeyed model ])
        ]


fileInputDecoder : (List D.Value -> value) -> D.Decoder value
fileInputDecoder msg =
    D.at [ "target", "files" ] (D.list D.value) |> D.map msg



-- parts


photoEditorKeyed : Photo -> ( String, Html Msg )
photoEditorKeyed photo =
    ( String.fromInt photo.id, lazy photoEditor photo )


photoEditor : Photo -> Html Msg
photoEditor ({ prints } as photo) =
    -- let
    --     id =
    --         log "draw photo" (String.fromInt photo.id)
    -- in
    div [ class "photo-editor" ]
        [ input
            [ value photo.name
            , class "photo-name"
            , onInput (Rename photo)
            ]
            []

        -- THE CANVAS
        , div [ class "photo-center" ] [ canvas photo ]

        -- BUTTONS
        , div [ class "photo-buttons" ]
            [ button [ onClick (Delete photo.id) ] [ text "delete" ]
            , text ("№ " ++ String.fromInt (photo.id + 1))
            , button [ onClick (TurnPrev photo) ] [ text "left" ]
            , button [ onClick (TurnNext photo) ] [ text "right" ]
            , button [ onClick (TurnCrop photo) ] [ text "turncrop" ]
            , button [ onClick (ToggleCropMode photo) ] [ text "mode" ]
            ]

        -- PRINTS
        , div [ class "prints" ] (map printCtrl prints.list)
        ]


canvas : Photo -> Html Msg
canvas ({ texture, print, cur } as photo) =
    let
        ( lim_P, ( lim_w, lim_h ) ) =
            P.pointSize print.lim

        img_P =
            P.fromRect print.img

        ( crop_P, ( crop_w, crop_h ) as crop_size ) =
            P.pointSize print.crop

        ( crop_x, crop_y ) =
            P.delta lim_P crop_P

        lim5 =
            P.add lim_P ( 0, crop_y )

        lim6 =
            P.add lim5 ( 0, crop_h )

        img2 =
            P.add crop_P ( crop_w, 0 )

        fog =
            C.shapes
                [ CS.fill (Color.rgba 1 0.2 0.2 0.6) ]
                [ -- top
                  C.rect lim_P lim_w crop_y
                , -- left
                  C.rect lim5 crop_x crop_h
                , -- right
                  C.rect img2 (lim_w - crop_x - crop_w) crop_h
                , -- bottom
                  C.rect lim6 lim_w (lim_h - crop_y - crop_h)
                ]

        crop_rect =
            C.shapes
                [ CS.stroke (Color.rgb255 108 113 196) ]
                [ C.rect crop_P crop_w crop_h ]

        crop_size_handle0 =
            C.shapes
                [ CS.fill (Color.rgb255 33 150 243) ]
                [ C.rect (P.add size_handle_offset (P.add crop_P crop_size)) (first size_handle_size) (first size_handle_size) ]

        crop_size_handle =
            C.shapes
                [ CS.fill Color.white ]
                [ C.circle (P.add crop_P crop_size) 3 ]

        dim =
            Canvas.Texture.dimensions photo.texture

        ( rotation, draw_offset ) =
            case print.turn of
                TurnUp ->
                    ( degrees 0, ( 0, 0 ) )

                TurnRight ->
                    ( degrees 90, ( dim.height * print.imgscale, 0 ) )

                TurnDown ->
                    ( degrees 180, ( dim.width * print.imgscale, dim.height * print.imgscale ) )

                TurnLeft ->
                    ( degrees -90, ( 0, dim.width * print.imgscale ) )

        ( trans_x, trans_y ) =
            P.add draw_offset img_P

        img =
            C.texture
                [ CSA.transform
                    [ CSA.translate trans_x trans_y
                    , CSA.rotate rotation
                    , CSA.scale print.imgscale print.imgscale
                    ]
                ]
                ( 0, 0 )
                texture

        lim_border =
            C.shapes
                [ CS.stroke Color.black
                , Canvas.Settings.Line.lineWidth 0.25
                ]
                [ C.rect lim_P lim_w lim_h ]

        clear =
            C.shapes
                [ CS.fill (Color.rgb 0.75 0.75 0.75) ]
                [ C.rect ( 0, 0 ) 276 276 ]

        page =
            C.shapes
                [ CS.fill Color.white ]
                [ C.rect lim_P lim_w lim_h ]

        render =
            [ clear
            , page
            , img
            , fog
            , lim_border
            , crop_rect
            , crop_size_handle0
            , crop_size_handle
            ]
    in
    div
        [ class "photo-padd"
        , Mouse.onMove (.offsetPos >> MouseMove photo)
        , Mouse.onDown (.offsetPos >> MouseDown photo)
        ]
        [ C.toHtml ( 276, 276 )
            [ class "photo", curStyle cur ]
            render
        ]


printCtrl : Print -> Html Msg
printCtrl { size, cropmode, q } =
    let
        ( print_width, print_height ) =
            size
    in
    div []
        [ text
            (String.concat
                [ String.fromInt q
                , "шт.  - "
                , String.fromInt print_width
                , "mm x "
                , String.fromInt print_height
                , "mm - "
                , if cropmode == Fill then
                    "без полей"

                  else
                    "с полями"
                ]
            )
        ]


dropboxKeyed : Model -> ( String, Html Msg )
dropboxKeyed model =
    ( "dropbox", lazy dropbox model )


dropbox : Model -> Html Msg
dropbox model =
    div
        ([ class "dropbox"
         , hijackOn "dragenter" (D.succeed DragEnter)
         , hijackOn "dragover" (D.succeed DragEnter)
         , hijackOn "dragleave" (D.succeed DragLeave)
         , hijackOn "drop" (dropDecoder GotFileValues)
         ]
            ++ hoverStyle model.hover
        )
        [ label
            []
            [ input
                [ type_ "file"
                , accept "image/*"
                , multiple True
                , on "change" (fileInputDecoder GotFileValues)
                , value ""
                ]
                []
            , span
                [ class "msg" ]
                [ text "click to add files or drop them here" ]
            ]
        ]



-- help funcs


dropDecoder : (List Value -> value) -> D.Decoder value
dropDecoder msg =
    D.at [ "dataTransfer", "files" ] (D.list D.value) |> D.map msg


hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (D.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )


hoverStyle : Bool -> List (Attribute Msg)
hoverStyle hover =
    if hover then
        [ style "border-color" "purple"
        , style "background-color" "thistle"
        ]

    else
        []


imageSize : Texture -> Point
imageSize tex =
    let
        dim =
            Canvas.Texture.dimensions tex
    in
    ( dim.width, dim.height )
