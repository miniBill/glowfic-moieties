module Route.Index exposing (ActionData, Data, Hsla, Model, Msg, RouteParams, route)

import AssocList
import BackendTask exposing (BackendTask)
import BackendTask.Http as BHttp
import Color exposing (Color)
import Color.Oklch
import Dict
import Effect exposing (Effect)
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Hex
import Html
import Html.Attributes
import Html.Events
import Json.Decode exposing (Decoder)
import List.Extra
import LowLevel.Command
import Pages.Url
import PagesMsg exposing (PagesMsg)
import Path
import RouteBuilder exposing (App, StatefulRoute)
import Segment
import Shared
import SubPath
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Attributes.InPx as SAPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..))
import UrlPath
import View exposing (View)



--


type alias ColorSpace =
    { distance : Color -> Float
    , angle : Color -> Float
    , sorting : Color -> ( Float, Float )
    , lightness : Color -> Float
    }


oklch : ColorSpace
oklch =
    { toColor = Color.Oklch.toColor
    , fromColor = Color.Oklch.fromColor
    , distance = \{ chroma } -> logBase 2 (1 + chroma / 0.27)
    , angle = .hue
    , sorting = \{ lightness, hue } -> ( hue, lightness )
    }
        |> toColorSpace


lch : ColorSpace
lch =
    { toColor = Color.fromHsla
    , fromColor = Color.toHsla
    , distance = \{ saturation } -> saturation
    , angle = .hue
    , sorting = \{ lightness, hue } -> ( hue, lightness )
    }
        |> toColorSpace


toColorSpace :
    { toColor : { a | lightness : Float } -> Color
    , fromColor : Color -> { a | lightness : Float }
    , distance : { a | lightness : Float } -> Float
    , angle : { a | lightness : Float } -> Float
    , sorting : { a | lightness : Float } -> ( Float, Float )
    }
    -> ColorSpace
toColorSpace config =
    { distance = config.fromColor >> config.distance
    , angle = config.fromColor >> config.angle
    , sorting = config.fromColor >> config.sorting
    , lightness = config.fromColor >> .lightness
    }



--


type alias Model =
    { colorSpace : ColorSpace }


type Msg
    = SwitchToColorSpace ColorSpace


type alias RouteParams =
    {}


type alias Data =
    { moieties : AssocList.Dict Color (List String)
    }


type alias Hsla =
    { hue : Float
    , saturation : Float
    , lightness : Float
    , alpha : Float
    }


type alias ActionData =
    {}


route : StatefulRoute RouteParams Data ActionData Model Msg
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildWithLocalState
            { init = \_ _ -> ( init, Effect.none )
            , view = view
            , update = \_ _ -> update
            , subscriptions = \_ _ _ _ -> Sub.none
            }


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SwitchToColorSpace colorSpace ->
            ( { model | colorSpace = colorSpace }, Effect.none )


init : Model
init =
    { colorSpace = lch }


data : BackendTask FatalError Data
data =
    getAllPages "users"
        (Json.Decode.map2
            (\username maybeMoiety ->
                maybeMoiety
                    |> Maybe.andThen (String.toLower >> Hex.fromString >> Result.toMaybe)
                    |> Maybe.map (\moiety -> ( username, moiety ))
            )
            (Json.Decode.field "username" Json.Decode.string)
            (Json.Decode.field "moiety" <| Json.Decode.nullable Json.Decode.string)
        )
        |> BackendTask.map (List.filterMap identity)
        |> BackendTask.map
            (\moieties ->
                { moieties =
                    List.foldl
                        (\( username, moiety ) acc ->
                            let
                                color : Color
                                color =
                                    rgbToColor moiety
                            in
                            AssocList.insert color
                                (username :: Maybe.withDefault [] (AssocList.get color acc))
                                acc
                        )
                        AssocList.empty
                        moieties
                }
            )


rgbToColor : Int -> Color.Color
rgbToColor rgb =
    Color.rgb255 (rgb // 65536) (modBy 256 <| rgb // 256) (modBy 256 rgb)


getAllPages : String -> Decoder a -> BackendTask FatalError (List a)
getAllPages api decoder =
    let
        lastLinkPrefix : String
        lastLinkPrefix =
            "<https://glowfic.com/api/v1/" ++ api ++ "?page="

        lastLinkSuffix : String
        lastLinkSuffix =
            ">; rel=\"last\""
    in
    BHttp.get ("https://glowfic.com/api/v1/" ++ api)
        (BHttp.expectWhatever ()
            |> BHttp.withMetadata always
        )
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\{ headers } ->
                case Dict.get "link" headers of
                    Nothing ->
                        BackendTask.fail <| FatalError.fromString "Links not found"

                    Just link ->
                        case
                            List.filter
                                (\fragment ->
                                    String.endsWith lastLinkSuffix fragment
                                        && String.startsWith lastLinkPrefix fragment
                                )
                                (String.split "," link)
                        of
                            [ last ] ->
                                let
                                    sliced =
                                        String.slice
                                            (String.length lastLinkPrefix)
                                            -(String.length lastLinkSuffix)
                                            last
                                in
                                case String.toInt sliced of
                                    Just i ->
                                        BackendTask.succeed i

                                    Nothing ->
                                        fail <| "Last page should be \"" ++ sliced ++ "\", but that's not an int"

                            [] ->
                                fail "Could not find link to the last page"

                            _ ->
                                fail "Ambiguous link to the last page"
            )
        |> BackendTask.andThen
            (\lastPage ->
                List.range 1 lastPage
                    |> List.map
                        (\pageId ->
                            BHttp.getJson
                                ("https://glowfic.com/api/v1/" ++ api ++ "?page=" ++ String.fromInt pageId)
                                (Json.Decode.field "results" <| Json.Decode.list decoder)
                                |> BackendTask.allowFatal
                        )
                    |> BackendTask.combine
                    |> BackendTask.map List.concat
            )


fail : String -> BackendTask FatalError a
fail message =
    BackendTask.fail <| FatalError.fromString message


head :
    App Data ActionData RouteParams
    -> List Head.Tag
head _ =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "Glowfic Moieties"
        , image =
            { url = [ "images", "icon-png.png" ] |> UrlPath.join |> Pages.Url.fromPath
            , alt = "Glowfic logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "Display glowfic moieties in a nice way"
        , locale = Nothing
        , title = "Moieties"
        }
        |> Seo.website


segmentHeight : Float
segmentHeight =
    2 / ringCount


ringCount : number
ringCount =
    8


angleCount : number
angleCount =
    12


rectCount : Float
rectCount =
    ringCount


rectHeight : Float
rectHeight =
    2 / rectCount


borderWidth : Float
borderWidth =
    1 / 400


view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> Model
    -> View (PagesMsg Msg)
view app _ model =
    { title = "Moieties"
    , body =
        [ app.data.moieties
            |> AssocList.toList
            |> gatherEqualsBy (\( moiety, _ ) -> toDistance model.colorSpace moiety)
            |> List.sortBy Tuple.first
            |> List.map
                (\( distance, ring ) ->
                    ring
                        |> gatherEqualsBy (\( moiety, _ ) -> toAngle model.colorSpace moiety)
                        |> List.sortBy Tuple.first
                        |> List.map
                            (if distance == 0 then
                                viewGrayscale model.colorSpace

                             else
                                viewSegment model.colorSpace distance
                            )
                        |> S.g [ SA.id <| "ring-" ++ String.fromFloat distance ]
                )
            |> S.svg
                [ SA.viewBox -1 -1 (2 + segmentHeight / 2) 2
                , Html.Attributes.style "width" "calc(100vw - 16px)"
                , Html.Attributes.style "height" "calc(100vh - 16px)"
                ]
        , Html.div
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "left" "8px"
            , Html.Attributes.style "top" "8px"
            ]
            [ Html.button
                [ Html.Events.onClick <| PagesMsg.fromMsg <| SwitchToColorSpace lch
                ]
                [ Html.text "LCH" ]
            , Html.text " "
            , Html.button
                [ Html.Events.onClick <| PagesMsg.fromMsg <| SwitchToColorSpace oklch
                ]
                [ Html.text "OKLCH" ]
            ]
        ]
    }


viewGrayscale : ColorSpace -> ( Float, List ( Color, List String ) ) -> Svg (PagesMsg Msg)
viewGrayscale colorSpace ( _, list ) =
    list
        |> List.map
            (\( moiety, usernames ) ->
                S.rect
                    [ SAPx.x <| 1 + segmentHeight / 6
                    , SAPx.y <| (2 - rectHeight - 2 * borderWidth) * quantize (rectCount - 1) (colorSpace.lightness moiety) - 1 + segmentHeight / 20
                    , SAPx.width <| segmentHeight / 2 - segmentHeight / 6 - borderWidth
                    , SAPx.height rectHeight
                    , SA.fill <| Paint moiety
                    , SA.stroke <| Paint Color.black
                    , SAPx.strokeWidth borderWidth
                    ]
                    [ S.title [] [ Html.text <| String.join ", " usernames ] ]
            )
        |> S.g [ SA.id "grayscale" ]


viewSegment : ColorSpace -> Float -> ( Float, List ( Color, List String ) ) -> Svg (PagesMsg Msg)
viewSegment colorSpace distance ( angle, list ) =
    let
        outerDistance : Float
        outerDistance =
            distance

        innerDistance : Float
        innerDistance =
            outerDistance - segmentHeight / 2

        fragmentCount : Float
        fragmentCount =
            toFloat (List.length list)

        fragmentAngle : Float
        fragmentAngle =
            2 * pi / angleCount / fragmentCount

        fromPolar : ( Float, Float ) -> ( Float, Float )
        fromPolar ( alpha, dist ) =
            ( dist * cos alpha
            , dist * sin alpha
            )
    in
    list
        |> List.sortBy (\( color, _ ) -> colorSpace.sorting color)
        |> List.indexedMap
            (\i ( moiety, usernames ) ->
                let
                    fi : Float
                    fi =
                        toFloat i

                    outerBefore : ( Float, Float )
                    outerBefore =
                        fromPolar ( angle + fragmentAngle * fi, outerDistance )

                    outerAfter : ( Float, Float )
                    outerAfter =
                        fromPolar ( angle + fragmentAngle * (fi + 1), outerDistance )

                    innerAfter : ( Float, Float )
                    innerAfter =
                        fromPolar ( angle + fragmentAngle * (fi + 1), innerDistance )

                    innerBefore : ( Float, Float )
                    innerBefore =
                        fromPolar ( angle + fragmentAngle * fi, innerDistance )
                in
                S.path
                    [ SA.d <|
                        Path.toString
                            [ SubPath.fromSegments
                                [ Segment.ellipticalArc outerBefore
                                    { target = outerAfter
                                    , radii = ( outerDistance, outerDistance )
                                    , xAxisRotate = 0
                                    , arcFlag = LowLevel.Command.largestArc
                                    , direction = LowLevel.Command.clockwise
                                    }
                                ]
                                |> SubPath.connect
                                    (if innerDistance < segmentHeight / 2 then
                                        SubPath.fromSegments
                                            [ Segment.line innerBefore innerAfter
                                            ]

                                     else
                                        SubPath.fromSegments
                                            [ Segment.ellipticalArc innerAfter
                                                { target = innerBefore
                                                , radii = ( innerDistance, innerDistance )
                                                , xAxisRotate = 0
                                                , arcFlag = LowLevel.Command.largestArc
                                                , direction = LowLevel.Command.counterClockwise
                                                }
                                            ]
                                    )
                            ]
                    , SA.fill <| Paint moiety
                    ]
                    [ S.title [] [ Html.text <| String.join ", " usernames ] ]
            )
        |> S.g [ SA.id <| "angle-" ++ String.fromFloat angle ]


toDistance : ColorSpace -> Color -> Float
toDistance colorSpace moiety =
    quantize ringCount (colorSpace.distance moiety)


toAngle : ColorSpace -> Color -> Float
toAngle colorSpace moiety =
    2 * pi * quantize (angleCount - 1) (colorSpace.angle moiety) * (angleCount - 1) / angleCount


quantize : Float -> Float -> Float
quantize count value =
    (toFloat <| round <| count * value) / count


gatherEqualsBy : (a -> b) -> List a -> List ( b, List a )
gatherEqualsBy f list =
    list
        |> List.Extra.gatherEqualsBy f
        |> List.map (\( first, rest ) -> ( f first, first :: rest ))
