port module Home exposing (main)

-- import List.Extra exposing (find)

import Browser
import Canvas
import Canvas.Settings
import Canvas.Texture exposing (Texture, fromDomImage)
import Color
import Debug exposing (log)
import File exposing (File)
import File.Select as Select
import Html exposing (Attribute, Html, button, div, img, input, label, span, text)
import Html.Attributes exposing (accept, class, multiple, src, style, type_, value)
import Html.Events exposing (on, onClick, preventDefaultOn)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as D exposing (Value)
import List exposing (map)
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port sendFiles : ( Int, List String ) -> Cmd msg


port sendValues : List Value -> Cmd msg


port recvImage : (( D.Value, D.Value ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    recvImage recvFileDecoder


recvFileDecoder : ( D.Value, D.Value ) -> Msg
recvFileDecoder ( fileValue, imgValue ) =
    let
        fileRes =
            D.decodeValue File.decoder fileValue

        imgMaybe =
            Canvas.Texture.fromDomImage imgValue
    in
    case fileRes of
        Ok file ->
            case imgMaybe of
                Just img ->
                    RecvImage ( file, img )

                _ ->
                    NoOp

        _ ->
            NoOp


type alias Model =
    { hover : Bool
    , loading : Bool
    , files : List File
    , previews : List String
    , photos : List ( Int, File )
    , textures : List PhotoData
    , cursor : Cur
    , textureCount : Int
    , dragging : Drag
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model False False [] [] [] [] Null 0 DragNone, Cmd.none )


type alias PhotoData =
    { id : Int
    , texture : Texture
    , crop : Point
    , size :
        { w : Float
        , h : Float
        }
    , prints : List Print
    , file : File
    }


type Cur
    = Move
      -- | Pointer
    | Null
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


type alias Point =
    ( Float, Float )


type alias Rect =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }


type Drag
    = DragNone
    | DragMove Point
    | DragSize Point


type alias PrintSize =
    ( Int, Int )



-- ( width in mm, height in mm )


printSizes =
    [ ( 102, 152, "10x15" )
    , ( 152, 203, "15x20" )
    , ( 152, 210, "15x21" )
    , ( 152, 230, "15x23" )
    , ( 203, 305, "20x30" )
    , ( 210, 305, "21x30" )
    ]


type CropMode
    = Fill -- cut off what does not fit
    | Fit -- fit in the whole picture, add white padding


type Turn
    = N -- Original, no turn
    | E -- 90 deg clockwise turn
    | W -- 90 deg counter clockwise turn
    | S -- upside down turn, 180 deg


type alias Print =
    { q : Int
    , size : PrintSize
    , crop : Rect
    , mode : CropMode
    }



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
    = Pick
    | DragEnter
    | DragLeave
    | GotFiles File (List File)
      -- | LoadingFiles (List File)
      -- | GotPreviews (List String)
    | DeleteFile Int
    | GotFilesUrls ( List File, List String )
      -- | GotValues Value (List Value)
    | GotValues2 (List Value)
    | RecvImage ( File, Texture )
      -- | Log Mouse.Event
    | MouseDown PhotoData Point
    | MouseMove PhotoData Point
    | MouseUp
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MouseDown photo p ->
            let
                size_p =
                    addPoint photo.crop ( photo.size.w - 4, photo.size.h - 4 )
            in
            if inBox size_p ( 8, 8 ) p then
                ( { model | dragging = DragSize (deltaPoint size_p p) }, Cmd.none )

            else if inBox photo.crop ( photo.size.w, photo.size.h ) p then
                ( { model | dragging = DragMove (deltaPoint photo.crop p) }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        MouseMove ({ crop } as photo) (( x, y ) as p) ->
            let
                ( crop_x, crop_y ) =
                    crop
            in
            case model.dragging of
                DragNone ->
                    let
                        size_p =
                            addPoint crop ( photo.size.w - 4, photo.size.h - 4 )

                        cur =
                            if inBox size_p ( 8, 8 ) p then
                                Cross

                            else if inBox crop ( photo.size.w, photo.size.h ) p then
                                Move

                            else
                                Null
                    in
                    ( { model | cursor = cur }, Cmd.none )

                DragMove dp ->
                    let
                        dim =
                            Canvas.Texture.dimensions photo.texture
                    in
                    ( { model
                        | textures =
                            map
                                (\({ id } as ph) ->
                                    if id == photo.id then
                                        { photo | crop = deltaPoint dp p |> clampPoint { x = 0, y = 0, w = dim.width - photo.size.w, h = dim.height - photo.size.h } }

                                    else
                                        ph
                                )
                                model.textures
                      }
                    , Cmd.none
                    )

                DragSize ( dx, dy ) ->
                    let
                        {-
                           vector s : y = (102/152)*x === (size.h/size.w)*x
                           max point : y = (size.h/size.w) * (dim.width-crop_x)
                        -}
                        s =
                            ( photo.size.w, photo.size.h )

                        dim =
                            Canvas.Texture.dimensions photo.texture

                        max_x =
                            dim.width - crop_x

                        max_y =
                            dim.height - crop_y

                        ( s_x, s_y ) =
                            let
                                ratio =
                                    photo.size.w / photo.size.h

                                sx_cross_max_y =
                                    ratio * max_y
                            in
                            if sx_cross_max_y > max_x then
                                ( max_x, 1 / ratio * max_x )

                            else
                                ( sx_cross_max_y, max_y )

                        ( proj_w, proj_h ) =
                            mul (dot s ( x - crop_x - dx + 4, y - crop_y - dy + 4 ) / dot s s) s

                        ( new_w, new_h ) =
                            if proj_w < s_x then
                                ( proj_w, proj_h )

                            else
                                ( s_x, s_y )

                        _ =
                            log "aspect" (Debug.toString <| new_w / new_h)
                    in
                    ( { model
                        | textures =
                            map
                                (\({ id } as ph) ->
                                    if id == photo.id then
                                        { photo | size = { w = new_w, h = new_h } }

                                    else
                                        ph
                                )
                                model.textures
                      }
                    , Cmd.none
                    )

        MouseUp ->
            ( { model | dragging = DragNone }, Cmd.none )

        Pick ->
            ( model, Select.files [ "image/*" ] GotFiles )

        DragEnter ->
            ( { model | hover = True }, Cmd.none )

        DragLeave ->
            ( { model | hover = False }, Cmd.none )

        -- GotValues v vs ->
        --     let
        --         values =
        --             v :: vs
        --     in
        --     ( { model | hover = False }, sendValues values )
        GotValues2 values ->
            ( { model | hover = False }, sendValues values )

        GotFiles f fs ->
            let
                files =
                    f :: fs

                _ =
                    log "GotFiles" files

                -- maxIndex : List ( Int, File ) -> Int
                -- maxIndex photos =
                --     List.map Tuple.first photos
                --         |> List.maximum
                --         |> Maybe.withDefault 0
                -- newIndices : List ( Int, File ) -> List Int
                -- newIndices photos =
                --     List.range (maxIndex photos + 1) (List.length files - 1)
                -- newPhotos : List ( Int, File ) -> List ( Int, File )
                -- newPhotos photos =
                --     zip (newIndices photos) files
                -- foo : ( Int, Task.Task x String ) -> Task.Task x ( Int, String )
                -- foo ( idx, task ) =
                --     task |> Task.map (Tuple.pair idx)
                -- _ =
                --     Debug.log "photos" model.photos
            in
            -- ( { model
            --     | hover = False
            --     , loading = True
            -- , files = model.files ++ files
            -- , photos = log "model.photos" model.photos ++ (newPhotos model.photos |> log "newPhotos")
            --   }
            -- , List.map File.toUrl files
            --     |> Task.sequence
            --     |> Task.map (Tuple.pair files)
            --     |> Task.perform GotFilesUrls
            -- , update (LoadingFiles files) model
            -- )
            -- update (LoadingFiles files) { model | hover = False, loading = True }
            ( { model
                | hover = False
                , loading = True
              }
            , List.map File.toUrl files
                |> Task.sequence
                |> Task.map (Tuple.pair files)
                |> Task.perform GotFilesUrls
            )

        -- LoadingFiles files ->
        --     ( model
        --     , List.map File.toUrl files
        --         |> Task.sequence
        --         |> Task.map (Tuple.pair files)
        --         |> Task.perform GotFilesUrls
        --     )
        GotFilesUrls ( files, urls ) ->
            -- ( model, sendFiles t )
            let
                maxIndex =
                    map Tuple.first model.photos
                        |> maxOr 0
            in
            ( { model
                | photos = model.photos ++ zip (List.range (maxIndex + 1) (maxIndex + List.length files)) files
                , loading = False
              }
            , sendFiles ( maxIndex + 1, urls )
            )

        -- GotPreviews urls ->
        --     ( { model | previews = model.previews ++ urls }, Cmd.none )
        DeleteFile n ->
            ( { model
                | files = removeAt n model.files
                , previews = removeAt n model.previews
              }
            , Cmd.none
            )

        RecvImage ( file, img ) ->
            let
                { width, height } =
                    Canvas.Texture.dimensions img

                _ =
                    log "dims" (Debug.toString ( width, height ))

                print =
                    if width > height then
                        if width / height > 152 / 102 then
                            let
                                w =
                                    152 * height / 102

                                x =
                                    (width - w) / (2 * width)
                            in
                            Print 1
                                ( 152, 102 )
                                { x = x
                                , y = 0
                                , w = w / width
                                , h = 1
                                }
                                Fill

                        else
                            let
                                h =
                                    102 * width / 152

                                y =
                                    (height - h) / (2 * height)
                            in
                            Print 1
                                ( 152, 102 )
                                { x = 0
                                , y = y
                                , w = 1
                                , h = h / height
                                }
                                Fill

                    else if height / width > 152 / 102 then
                        let
                            h =
                                152 * width / 102

                            y =
                                (height - h) / (2 * height)
                        in
                        Print 1
                            ( 102, 152 )
                            { x = 0
                            , y = y
                            , w = 1
                            , h = h / height
                            }
                            Fill

                    else
                        let
                            w =
                                102 * height / 152

                            x =
                                (width - w) / (2 * width)
                        in
                        Print 1 ( 102, 152 ) { x = x, y = 0, w = w / width, h = 1 } Fill

                -- x = if width/height > 152/102 then
            in
            ( { model
                | textureCount = model.textureCount + 1
                , textures =
                    model.textures
                        ++ [ { id = model.textureCount
                             , texture = img
                             , crop =
                                ( print.crop.x * width
                                , print.crop.y * height
                                )
                             , size =
                                { w = print.crop.w * width
                                , h = print.crop.h * height
                                }
                             , prints = [ print ]
                             , file = file
                             }
                           ]
              }
            , Cmd.none
            )


maxOr : comparable -> List comparable -> comparable
maxOr v list =
    List.maximum list |> Maybe.withDefault v



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
    div [ class "elm", Mouse.onUp (always MouseUp) ]
        [ filesList model.files model.previews
        , div [] (List.map (photoEditor model.cursor) model.textures)
        , dropbox model
        , div [ class "dropbox-wrap" ]
            [ label []
                [ input
                    [ type_ "file"
                    , accept "image/*"
                    , multiple True
                    , style "display" "none"

                    -- , hijackOn "change" changeDecoder
                    -- , on "change" (D.at [ "target", "files" ] (D.list D.value) |> D.map GotValues2)
                    , on "change" (fileInputDecoder GotValues2)
                    , value ""
                    ]
                    []
                , div [ class "dropbox-cont" ]
                    [ text "Choose file" ]
                ]
            ]
        , span [ style "font-weight" "bold" ]
            [ text
                (if model.loading then
                    "LOADING..."

                 else
                    ""
                )
            ]
        ]



-- filesDecoder : D.Decoder (List Value)
-- filesDecoder =
--     D.at [ "target", "files" ] (D.list D.value)
-- filesToValues : (a -> Msg) -> D.Decoder Msg


fileInputDecoder : (List D.Value -> value) -> D.Decoder value
fileInputDecoder msg =
    D.at [ "target", "files" ] (D.list D.value) |> D.map msg



-- parts


photoEditor : Cur -> PhotoData -> Html Msg
photoEditor cur ({ texture, crop, size } as photo) =
    let
        dim =
            Canvas.Texture.dimensions texture

        ( crop_x, crop_y ) =
            crop
    in
    Canvas.toHtml ( round dim.width, round dim.height )
        [ Mouse.onMove (.offsetPos >> MouseMove photo)
        , Mouse.onDown (.offsetPos >> MouseDown photo)

        -- , Mouse.onUp MouseUp
        , class "photo"
        , style "cursor" (curToStyle cur)
        ]
        [ Canvas.texture [] ( 0, 0 ) texture
        , Canvas.shapes [ Canvas.Settings.fill (Color.rgba 1 1 1 0.65) ]
            [ Canvas.rect ( 0, 0 ) dim.width crop_y
            , Canvas.rect ( 0, crop_y ) crop_x size.h
            , Canvas.rect ( crop_x + size.w, crop_y ) dim.width size.h
            , Canvas.rect ( 0, crop_y + size.h ) dim.width dim.height
            ]
        , Canvas.shapes [ Canvas.Settings.fill (Color.rgb255 33 150 243) ]
            [ Canvas.rect (addPoint crop ( size.w - 4, size.h - 4 )) 8 8
            ]
        ]


filesList : List File -> List String -> Html Msg
filesList files previews =
    let
        filesAndUrls =
            zip files previews

        getItem i ( f, p ) =
            div []
                [ viewPreview p
                , text (String.fromInt (i + 1) ++ ". " ++ Debug.toString f)
                , button [ onClick (DeleteFile i) ] [ text "delete" ]
                ]

        items =
            -- List.indexedMap fileToDiv files
            List.indexedMap getItem filesAndUrls
    in
    div [] items


dropbox : Model -> Html Msg
dropbox model =
    div
        ([ class "dropbox"
         , hijackOn "dragenter" (D.succeed DragEnter)
         , hijackOn "dragover" (D.succeed DragEnter)
         , hijackOn "dragleave" (D.succeed DragLeave)

         --  , hijackOn "drop" dropDecoder2
         , hijackOn "drop" (dropDecoder GotValues2)
         , onClick Pick
         ]
            ++ hoverStyle model.hover
        )
        [ span [ style "color" "#ccc" ] [ text "click to add files or drop them here" ]
        ]



-- help funcs
-- dropDecoder : D.Decoder Msg
-- dropDecoder =
--     D.at [ "dataTransfer", "files" ] (D.oneOrMore GotFiles File.decoder)
-- dropDecoder2 : D.Decoder Msg
-- dropDecoder2 =
--     D.at [ "dataTransfer", "files" ] (D.oneOrMore GotValues D.value)


dropDecoder : (List Value -> value) -> D.Decoder value
dropDecoder msg =
    D.at [ "dataTransfer", "files" ] (D.list D.value) |> D.map msg



-- changeDecoder : D.Decoder Msg
-- changeDecoder =
--     D.at [ "target", "files" ] (D.oneOrMore GotValues D.value)


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


viewPreview : String -> Html msg
viewPreview url =
    img
        [ style "width" "60px"
        , style "height" "60px"
        , src url
        ]
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


removeAt : Int -> List a -> List a
removeAt index l =
    if index < 0 then
        l

    else
        let
            head =
                List.take index l

            tail =
                List.drop index l |> List.tail
        in
        case tail of
            Nothing ->
                l

            Just t ->
                List.append head t


zip : List a -> List b -> List ( a, b )
zip a b =
    List.map2 Tuple.pair a b


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


lim : number -> number -> number -> number
lim a b v =
    max a (min v b)


dot : Point -> Point -> Float
dot ( x1, y1 ) ( x2, y2 ) =
    x1 * x2 + y1 * y2


mul : Float -> Point -> Point
mul m ( x, y ) =
    ( m * x, m * y )



-- filesDecoder =
--     D.at [ "target", "files" ] (D.list File.decoder)
