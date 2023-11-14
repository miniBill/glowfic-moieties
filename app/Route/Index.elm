module Route.Index exposing (ActionData, Data, Hsla, Model, Msg, RouteParams, route)

import AssocList
import BackendTask exposing (BackendTask)
import BackendTask.Http as BHttp
import Color exposing (Color)
import Dict
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Hex
import Html
import Html.Attributes
import Json.Decode exposing (Decoder)
import List.Extra
import LowLevel.Command
import Pages.Url
import PagesMsg exposing (PagesMsg)
import Path
import RouteBuilder exposing (App, StatelessRoute)
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


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    {}


type alias Data =
    { moieties : AssocList.Dict Hsla (List String)
    }


type alias Hsla =
    { hue : Float
    , saturation : Float
    , lightness : Float
    , alpha : Float
    }


type alias ActionData =
    {}


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildNoState { view = view }


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
                                hsla : Hsla
                                hsla =
                                    Color.toHsla <| rgbToColor moiety
                            in
                            AssocList.insert hsla
                                (username :: Maybe.withDefault [] (AssocList.get hsla acc))
                                acc
                        )
                        AssocList.empty
                        moieties
                }
            )


rgbToColor : Int -> Color
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
    -> View (PagesMsg Msg)
view app _ =
    { title = "Moieties"
    , body =
        [ app.data.moieties
            |> AssocList.toList
            |> gatherEqualsBy (\( moiety, _ ) -> toDistance moiety)
            |> List.sortBy Tuple.first
            |> List.map
                (\( distance, ring ) ->
                    ring
                        |> gatherEqualsBy (\( moiety, _ ) -> toAngle moiety)
                        |> List.sortBy Tuple.first
                        |> List.map
                            (if distance == 0 then
                                viewGrayscale

                             else
                                viewSegment distance
                            )
                        |> S.g [ SA.id <| "ring-" ++ String.fromFloat distance ]
                )
            |> S.svg
                [ SA.viewBox -1 -1 (2 + segmentHeight / 2) 2
                , Html.Attributes.style "width" "calc(100vw - 16px)"
                , Html.Attributes.style "height" "calc(100vh - 16px)"
                ]
        ]
    }


viewGrayscale : ( Float, List ( Hsla, List String ) ) -> Svg (PagesMsg Msg)
viewGrayscale ( _, list ) =
    list
        |> List.map
            (\( moiety, usernames ) ->
                S.rect
                    [ SAPx.x <| 1 + segmentHeight / 6
                    , SAPx.y <| (2 - rectHeight - 2 * borderWidth) * quantize (rectCount - 1) moiety.lightness - 1 + segmentHeight / 20
                    , SAPx.width <| segmentHeight / 2 - segmentHeight / 6 - borderWidth
                    , SAPx.height rectHeight
                    , SA.fill <| Paint <| Color.fromHsla moiety
                    , SA.stroke <| Paint Color.black
                    , SAPx.strokeWidth borderWidth
                    ]
                    [ S.title [] [ Html.text <| String.join ", " usernames ] ]
            )
        |> S.g [ SA.id "grayscale" ]


viewSegment : Float -> ( Float, List ( Hsla, List String ) ) -> Svg (PagesMsg Msg)
viewSegment distance ( angle, list ) =
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
        |> List.sortBy (\( { lightness, hue }, _ ) -> lightness + hue)
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
                    , SA.fill <| Paint <| Color.fromHsla moiety
                    ]
                    [ S.title [] [ Html.text <| String.join ", " usernames ] ]
            )
        |> S.g [ SA.id <| "angle-" ++ String.fromFloat angle ]


toDistance : Hsla -> Float
toDistance moiety =
    quantize ringCount moiety.saturation


toAngle : Hsla -> Float
toAngle moiety =
    2 * pi * quantize (angleCount - 1) moiety.hue * (angleCount - 1) / angleCount


quantize : Float -> Float -> Float
quantize count value =
    (toFloat <| round <| count * value) / count


gatherEqualsBy : (a -> b) -> List a -> List ( b, List a )
gatherEqualsBy f list =
    list
        |> List.Extra.gatherEqualsBy f
        |> List.map (\( first, rest ) -> ( f first, first :: rest ))
