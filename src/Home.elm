port module Home exposing (main)

-- import List.Extra exposing (find)
-- import File.Select as Select
-- import Debug exposing (log)

import Browser
import Canvas
import Canvas.Settings
import Canvas.Settings.Advanced
import Canvas.Texture exposing (Texture, fromDomImage)
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
import Tuple exposing (first, second)


type alias Point =
    ( Float, Float )



{-
   type alias Size =
       { w : Float
       , h : Float
       }
-}


type alias Rect =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }


type alias PrintSize =
    ( Int, Int )



{-
   type alias Print =
       { id : Int
       , q : Int
       , mode : CropMode -- Fill | Fit
       , size : PrintSize -- in mm
       , crop : Rect -- in percent
       }
-}
{-
   type alias PhotoData =
       { id : Int
       , texture : Texture
       , crop : { p : Point, size : Size }

       -- , crop : Point
       -- , size : Size
       , prints : List Print
       , file : File
       , name : String
       , cur : Cur
       }
-}
-- a new try to draw canvas editor


type alias Photo =
    { id : Int
    , texture : Texture
    , size : PrintSize
    , horizontal : Bool
    , file : File
    , name : String
    , cur : Cur
    , prints :
        { list : List Print2
        , count : Int
        }
    , print : Print2
    }


type alias Print2 =
    { id : Int
    , size : PrintSize
    , q : Int
    , turn : Turn
    , horizontal : Bool
    , cropmode : CropMode
    , xy : Point
    , wh : Point
    , canv :
        { img : Point
        , scale : Float
        , print : Maybe Rect
        , lims : Rect
        }
    }


type alias PrintConf =
    { texture : Texture
    , cropmode : CropMode
    , printsize : PrintSize
    }


type alias CanvConf =
    { img : Point
    , scale : Float
    , print : Maybe Rect
    , lims : Rect
    }


calcCanv : PrintConf -> CanvConf



-- Photo
-- -> Print2
-- ->
--     { img : Point
--     , scale : Float
--     , print : Maybe Rect
--     , lims : Rect
--     }
-- calcCanv ({ horizontal } as ph) ({ cropmode } as pr) =


calcCanv { texture, cropmode, printsize } =
    let
        dim =
            -- Canvas.Texture.dimensions ph.texture
            Canvas.Texture.dimensions texture

        horizontal =
            dim.width > dim.height
    in
    case cropmode of
        Fill ->
            let
                lims =
                    if horizontal then
                        { x = 10, y = (276 - dim.height) / 2, w = 256, h = dim.height }

                    else
                        { x = (276 - dim.width) / 2, y = 10, w = dim.width, h = 256 }
            in
            { img = ( lims.x, lims.y )
            , scale = 1
            , print = Nothing
            , lims = lims
            }

        Fit ->
            let
                ( print_w, print_h ) =
                    if horizontal then
                        -- ( second pr.size, first pr.size )
                        ( second printsize, first printsize )

                    else
                        -- pr.size
                        printsize

                print_ratio =
                    toFloat print_w / toFloat print_h

                img_ratio =
                    dim.width / dim.height

                ( lims, scale, img ) =
                    if horizontal then
                        let
                            page_h =
                                256 / print_ratio

                            ( scale_, img_ ) =
                                if img_ratio > print_ratio then
                                    -- img is wider than print
                                    ( 1, ( 10, (276 - dim.height) / 2 ) )

                                else
                                    -- print is wider than img
                                    let
                                        s =
                                            page_h / dim.height
                                    in
                                    ( s, ( (276 - dim.width * s) / 2, (276 - page_h) / 2 ) )
                        in
                        ( { x = 10
                          , y = (276 - page_h) / 2
                          , w = 256
                          , h = page_h
                          }
                        , scale_
                        , img_
                        )

                    else
                        -- vertical
                        let
                            page_w =
                                256 * print_ratio

                            ( scale_, img_ ) =
                                if img_ratio > print_ratio then
                                    -- img is wider than print
                                    let
                                        s =
                                            page_w / dim.width
                                    in
                                    ( s
                                    , ( (276 - page_w) / 2, (276 - dim.height * s) / 2 )
                                    )

                                else
                                    -- print is wider than img
                                    ( 1, ( (276 - dim.width) / 2, 10 ) )
                        in
                        ( { x = (276 - page_w) / 2
                          , y = 10
                          , w = page_w
                          , h = 256
                          }
                        , scale_
                        , img_
                        )
            in
            { img = img
            , scale = scale
            , lims = lims
            , print = Just lims
            }



{-
   if horizontal then
       let
           page_h =
               256 / print_ratio

           lims =
               { x = 10, y = (276 - page_h) / 2, w = 256, h = page_h }
       in
       if img_ratio > print_ratio then
           { img = ( 10, (276 - dim.height) / 2 )
           , s = 1
           , print = Just lims
           , lims = lims
           }

       else
           let
               img_h =
                   page_h

               scale =
                   page_h / dim.height

               img_w =
                   dim.width * scale
           in
           { img = ( (276 - img_w) / 2, (276 - img_h) / 2 )
           , s = scale
           , print = Just lims
           , lims = lims
           }

   else
       let
           page_w =
               256 * print_ratio

           lims =
               { x = (276 - page_w) / 2, y = 10, w = page_w, h = 256 }
       in
       if img_ratio > print_ratio then
           let
               img_w =
                   page_w

               scale =
                   page_w / dim.width

               img_h =
                   dim.height * scale
           in
           { img = ( (276 - img_w) / 2, (276 - img_h) / 2 )
           , s = scale
           , print = Just lims
           , lims = lims
           }

       else
           { img = ( (276 - dim.width) / 2, 10 )
           , s = 1
           , print = Just lims
           , lims = lims
           }
-}


type alias Model =
    { hover : Bool

    -- , photos : List PhotoData
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



-- PORTS
-- port sendFiles : ( Int, List String ) -> Cmd msg


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


curToStyle : Cur -> String
curToStyle cur =
    case cur of
        Cross ->
            "crosshair"

        -- Pointer ->
        --     "pointer"
        Move ->
            "move"

        Null ->
            ""


type Drag
    = DragNone
    | DragMove Photo Point
    | DragSize Photo Point



-- ( width in mm, height in mm )
{-
   printSizes =
       [ ( 102, 152, "10x15" )
       , ( 152, 203, "15x20" )
       , ( 152, 210, "15x21" )
       , ( 152, 230, "15x23" )
       , ( 203, 305, "20x30" )
       , ( 210, 305, "21x30" )
       ]
-}


type CropMode
    = Fill -- cut off what does not fit
    | Fit -- fit in the whole picture, add white padding



{-
   type Orient
       = Album -- Horizontal, width > height
       | Port -- Vertical, width < height
-}


type Turn
    = TurnUp -- Original, no turn



-- | TurnRight -- 90 deg clockwise turn
-- | TurnLeft -- 90 deg counter-clockwise turn
-- | TurnDown -- upside down turn, 180 deg
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
      -- drag n drop
    | DragEnter
    | DragLeave
      -- mouse
    | MouseDown Photo Point
    | MouseMove Photo Point
    | MouseUp
      -- editing
    | Turn Photo
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
            let
                canv =
                    print.canv

                lims =
                    canv.lims

                crop_x =
                    first print.xy * lims.w + lims.x

                crop_y =
                    second print.xy * lims.h + lims.y

                crop_w =
                    first print.wh * lims.w

                crop_h =
                    second print.wh * lims.h

                crop_p =
                    ( crop_x, crop_y )

                crop_size =
                    ( crop_w, crop_h )

                size_handle_p =
                    addPoint crop_p ( crop_w - 4, crop_h - 4 )
            in
            if inBox size_handle_p ( 8, 8 ) p then
                ( { model | dragging = DragSize photo (deltaPoint size_handle_p p) }, Cmd.none )

            else if inBox crop_p crop_size p then
                ( { model | dragging = DragMove photo (deltaPoint crop_p p) }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        MouseMove ({ print, id } as photo) (( x, y ) as p) ->
            case model.dragging of
                DragNone ->
                    let
                        crop_x =
                            first print.xy * print.canv.lims.w + print.canv.lims.x

                        crop_y =
                            second print.xy * print.canv.lims.h + print.canv.lims.y

                        crop_w =
                            first print.wh * print.canv.lims.w

                        crop_h =
                            second print.wh * print.canv.lims.h

                        crop =
                            { p = ( crop_x, crop_y )
                            , size = { w = crop_w, h = crop_h }
                            }

                        size_handle_p =
                            addPoint crop.p ( crop.size.w - 4, crop.size.h - 4 )

                        cur =
                            if inBox size_handle_p ( 8, 8 ) p then
                                Cross

                            else if inBox crop.p ( crop.size.w, crop.size.h ) p then
                                Move

                            else
                                Null
                    in
                    ( { model
                        | photos =
                            map
                                (\ph ->
                                    if ph.id == id then
                                        { ph | cur = cur }

                                    else
                                        ph
                                )
                                model.photos
                      }
                    , Cmd.none
                    )

                DragMove curPhoto dp ->
                    if curPhoto.id == id then
                        let
                            -- crop_w =
                            --     first print.wh * print.canv.lims.w
                            -- crop_h =
                            --     second print.wh * print.canv.lims.h
                            -- dim =
                            --     Canvas.Texture.dimensions photo.texture
                            ( new_crop_x, new_crop_y ) =
                                deltaPoint dp p
                                    |> clampPoint
                                        { x = 0
                                        , y = 0
                                        , w = (1 - first print.wh) * print.canv.lims.w
                                        , h = (1 - second print.wh) * print.canv.lims.h
                                        }

                            update_photo : Photo -> Photo
                            update_photo ph =
                                if ph.id == id then
                                    { ph | print = { print | xy = ( new_crop_x / print.canv.lims.w, new_crop_y / print.canv.lims.h ) } }

                                else
                                    ph
                        in
                        ( { model | photos = map update_photo model.photos }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                DragSize curPhoto ( dx, dy ) ->
                    if curPhoto.id == id then
                        let
                            -- ( crop_x, crop_y ) =
                            -- crop.p
                            crop_x =
                                first print.xy * print.canv.lims.w

                            crop_y =
                                second print.xy * print.canv.lims.h

                            crop_w =
                                first print.wh * print.canv.lims.w

                            crop_h =
                                second print.wh * print.canv.lims.h

                            s =
                                -- ( photo.crop.size.w, photo.crop.size.h )
                                ( crop_w, crop_h )

                            -- dim =
                            --     Canvas.Texture.dimensions photo.texture
                            max_x =
                                -- dim.width - crop_x
                                print.canv.lims.x + print.canv.lims.w

                            max_y =
                                -- dim.height - crop_y
                                print.canv.lims.y + print.canv.lims.h

                            (( sx, _ ) as sp) =
                                let
                                    ratio =
                                        -- photo.crop.size.w / photo.crop.size.h
                                        crop_w / crop_h

                                    sx_cross_max_y =
                                        ratio * max_y
                                in
                                if sx_cross_max_y > max_x then
                                    ( max_x, 1 / ratio * max_x )

                                else
                                    ( sx_cross_max_y, max_y )

                            (( proj_x, _ ) as proj) =
                                mul (dot s ( x - crop_x - dx + 4, y - crop_y - dy + 4 ) / dot s s) s

                            ( new_w, new_h ) =
                                if proj_x < sx then
                                    proj

                                else
                                    sp

                            -- _ =
                            --     log "aspect" (Debug.toString <| new_w / new_h)
                        in
                        ( { model
                            | photos =
                                map
                                    (\ph ->
                                        if ph.id == id then
                                            { photo | print = { print | wh = ( new_w / print.canv.lims.w, new_h / print.canv.lims.h ) } }

                                        else
                                            ph
                                    )
                                    model.photos
                          }
                        , Cmd.none
                        )

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

        Turn _ ->
            ( model, Cmd.none )

        Delete id ->
            ( { model
                | photos =
                    filter (.id >> (/=) id) model.photos
              }
            , Cmd.none
            )

        RecvImage ( file, img, ( originalWidth, originalHeight ) ) ->
            let
                canvConf =
                    calcCanv
                        { texture = img
                        , cropmode = Fit
                        , printsize = ( 102, 152 )
                        }

                horizontal =
                    originalWidth > originalHeight

                print =
                    { id = 0
                    , size = ( 102, 152 )
                    , q = 1
                    , turn = TurnUp
                    , cropmode = Fit
                    , horizontal = horizontal
                    , canv = canvConf
                    , xy = ( 0, 0 )
                    , wh = ( 0.5, 0.5 )
                    }

                photo =
                    { id = model.photoCount
                    , texture = img
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
                , photos =
                    model.photos
                        ++ [ photo ]
              }
            , Cmd.none
            )



{-
   let
       { width, height } =
           Canvas.Texture.dimensions img

       horizontal =
           width > height

       image_ratio =
           toFloat originalWidth / toFloat originalHeight

       -- print horizontal image horizontaly
       -- and vertical image verticaly
       print_size =
           iif horizontal ( 152, 102 ) ( 102, 152 )

       print_ratio =
           first print_size / second print_size

       -- Print id q cropMode printSize cropRect
       defaultPrint =
           Print 0 1 Fill print_size

       -- add crop info
       print =
           if horizontal then
               -- image is wider than print
               -- Fill == Fit print(crop) into image
               if image_ratio > print_ratio then
                   -- for Fill max crop box is inside
                   -- image. crop_h = img.height
                   -- crop_w = calc
                   let
                       -- in pixel
                       w =
                           height * print_ratio

                       -- in percent
                       x =
                           (width - w) / (2 * width)
                   in
                   defaultPrint
                       { x = x
                       , y = 0
                       , w = w / width -- in percent
                       , h = 1 -- full height
                       }

               else
                   -- image is taller than print
                   -- fit print(crop) into image
                   -- image is taller than print
                   let
                       -- in pixel
                       h =
                           width / print_ratio

                       -- in percent
                       y =
                           (height - h) / (2 * height)
                   in
                   defaultPrint
                       { x = 0
                       , y = y
                       , w = 1 -- full width
                       , h = h / height -- in percent
                       }

           else if print_ratio > image_ratio then
               -- image is vertical
               -- image is taller than print
               -- fit print into image
               -- print width == image width
               let
                   -- in pixel
                   h =
                       width / print_ratio

                   -- in percent
                   y =
                       (height - h) / (2 * height)
               in
               defaultPrint
                   { x = 0
                   , y = y
                   , w = 1 -- full width
                   , h = h / height -- in percent
                   }

           else
               -- image is wider than print
               let
                   -- in pixel
                   w =
                       height * print_ratio

                   -- in percent
                   x =
                       (width - w) / (2 * width)
               in
               defaultPrint
                   { x = x
                   , y = 0
                   , w = w / width -- in percent
                   , h = 1 -- full height
                   }
   in
   ( { model
       | photoCount = model.photoCount + 1
       , photos =
           model.photos
               ++ [ { id = model.photoCount
                    , texture = img
                    , crop =
                       { p =
                           ( print.crop.x * width
                           , print.crop.y * height
                           )
                       , size =
                           { w = print.crop.w * width
                           , h = print.crop.h * height
                           }
                       }

                    --  , crop =
                    --     ( print.crop.x * width
                    --     , print.crop.y * height
                    --     )
                    --  , size =
                    --     { w = print.crop.w * width
                    --     , h = print.crop.h * height
                    --     }
                    , prints = [ print ]
                    , file = file
                    , name = File.name file
                    , cur = Null
                    }
                  ]
     }
   , Cmd.none
   )
-}
-- type alias PrintConf =
--    { mm : PrintSize -- 102 x 152
--    , mode : CropMode -- Fill / Fit
--    , orient : Orient -- Port / Album
--    , croporient : Orient -- Port / Album
--    ,
--    }
-- calc : PrintSize -> CropMode -> CropTurn -> PrintTurn -> ImageSize -> Rect
--    calc print_size mode crop_turn turn imgage_size =
--        let
--            image_aspect = imgage_size.w / imgage_size.h
--            horizontal = image_aspect > 1
--            (print_w, print_h) = if horizontal then (print_size.h, print_size.w) else (print_size.w, print_size.h)
--            print_aspect = print_w / print_h
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
            , button [ onClick (Turn photo) ] [ text "turn" ]
            ]

        -- PRINTS
        , div [ class "prints" ] (map printCtrl prints.list)
        ]


canvas : Photo -> Html Msg
canvas ({ texture, print, cur } as photo) =
    let
        -- dim =
        --     Canvas.Texture.dimensions texture
        canv =
            print.canv

        ( crop_x, crop_y ) =
            -- crop.p
            ( first print.xy * canv.lims.w + canv.lims.x
            , second print.xy * canv.lims.h + canv.lims.y
            )

        ( crop_w, crop_h ) =
            ( first print.wh * canv.lims.w, second print.wh * canv.lims.h )

        fog =
            Canvas.shapes
                [ Canvas.Settings.fill (Color.rgba 0.5 0.5 0.5 0.65) ]
                [ Canvas.rect ( canv.lims.x, canv.lims.y ) canv.lims.w (crop_y - canv.lims.y)
                , Canvas.rect ( canv.lims.x, crop_y ) (crop_x - canv.lims.x) crop_h
                , Canvas.rect ( crop_x + crop_w, crop_y ) (canv.lims.w - crop_w - first print.xy * canv.lims.w) crop_h
                , Canvas.rect
                    ( canv.lims.x, crop_y + crop_h )
                    canv.lims.w
                    (canv.lims.h - crop_h - second print.xy * canv.lims.h)
                ]

        crop_rect =
            Canvas.shapes
                [ Canvas.Settings.stroke (Color.rgb255 108 113 196)
                ]
                [ Canvas.rect ( crop_x, crop_y ) crop_w crop_h
                ]

        crop_size_handle =
            Canvas.shapes
                [ Canvas.Settings.fill (Color.rgb255 33 150 243) ]
                [ Canvas.circle (addPoint ( crop_x, crop_y ) ( crop_w, crop_h )) 5
                ]

        img =
            Canvas.texture
                [ Canvas.Settings.Advanced.transform
                    [ Canvas.Settings.Advanced.scale canv.scale canv.scale ]
                ]
                ( first canv.img / canv.scale
                , second canv.img / canv.scale
                )
                texture

        render =
            case print.canv.print of
                Nothing ->
                    [ Canvas.clear ( 0, 0 ) 276 276
                    , img
                    , fog
                    , crop_rect
                    , crop_size_handle
                    ]

                Just pageRect ->
                    [ Canvas.clear ( 0, 0 ) 276 276
                    , Canvas.shapes
                        [ Canvas.Settings.stroke (Color.rgb255 0 0 0)
                        ]
                        [ Canvas.rect ( pageRect.x, pageRect.y ) pageRect.w pageRect.h
                        ]
                    , img
                    , fog
                    , crop_rect
                    , crop_size_handle
                    ]
    in
    div
        [ class "photo-padd"
        , Mouse.onMove (.offsetPos >> MouseMove photo)
        , Mouse.onDown (.offsetPos >> MouseDown photo)
        ]
        [ Canvas.toHtml ( 276, 276 )
            [ class "photo", style "cursor" (curToStyle cur) ]
            render
        ]


printCtrl : Print2 -> Html Msg
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
                , iif (cropmode == Fill) "Без полей" "С полями"
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



--
--
--
--
--
--
--
--
--
--
--
--
-- UTIL
-- LOGIC


iif : Bool -> a -> a -> a
iif bool v x =
    if bool then
        v

    else
        x



-- GEOMETRY


inBox : Point -> Point -> Point -> Bool
inBox ( left, top ) ( width, height ) ( x, y ) =
    not (x < left || y < top || x > left + width || y > top + height)


deltaPoint : Point -> Point -> Point
deltaPoint ( x0, y0 ) ( x1, y1 ) =
    ( x1 - x0, y1 - y0 )


addPoint : Point -> Point -> Point
addPoint ( x0, y0 ) ( x1, y1 ) =
    ( x0 + x1, y0 + y1 )


clampPoint : Rect -> Point -> Point
clampPoint { x, y, w, h } ( px, py ) =
    ( lim x (x + w) px
    , lim y (y + h) py
    )


dot : Point -> Point -> Float
dot ( x1, y1 ) ( x2, y2 ) =
    x1 * x2 + y1 * y2


mul : Float -> Point -> Point
mul m ( x, y ) =
    ( m * x, m * y )



-- MATH


lim : number -> number -> number -> number
lim a b v =
    max a (min v b)
