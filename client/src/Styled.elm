module Styled
    exposing
        ( contentAnnotation
        , contentLink
        , detailCheckbox
        , detailContainer
        , detailContent
        , detailList
        , detailTitle
        , horizontalList
        , horizontalListItem
        , li
        , mainSearchInput
        , ol
        , pageExtra
        , pageFooter
        , pageFrame
        , pageHeader
        , pageLogo
        , pageMain
        , pageSecondary
        , sectionContent
        , sectionTitle
        , subSectionTitle
        , table
        , tbody
        , td
        , th
        , thead
        , topicContainer
        , topicItem
        , tr
        , ul
        )

import Css exposing (..)
import Html.Styled as Html
    exposing
        ( Attribute
        , Html
        , button
        , div
        , h1
        , h2
        , img
        , input
        , select
        , styled
        )
import Html.Styled.Attributes as Attr


type alias Styled msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


theme =
    { borderColor = hex "dddddd"
    , contentMaxWidth = px 1200
    , contentSpacing = px 20
    , defaultSpacing = px 20
    , fontSize = px 16
    , smallSpacing = px 10
    }


contentAnnotation : Styled msg
contentAnnotation =
    styled Html.p
        [ fontStyle italic
        , margin2 theme.smallSpacing zero
        ]


contentLink : Styled msg
contentLink =
    styled Html.a
        [ color (hex "1184ce") ]


-- TODO: implement content placeholder for detail view
detailPlaceholder : Styled msg
detailPlaceholder =
    styled Html.a
        [ color (hex "1184ce") ]


detailCheckbox : Styled msg
detailCheckbox attrs children =
    styled Html.li []
        []
        [ styled Html.label
            [ display block
            , padding2 (px 3) zero
            ]
            []
            [ styled input
                []
                ([ Attr.type_ "checkbox" ] ++ attrs)
                children
            ]
        ]


detailContainer : Styled msg
detailContainer =
    styled div []


detailContent : Styled msg
detailContent =
    styled div
        [ marginBottom theme.smallSpacing
        ]


detailList : Styled msg
detailList =
    styled ul
        [ listStyle none
        , margin zero
        , paddingLeft theme.smallSpacing
        ]


detailTitle : Styled msg
detailTitle =
    styled h2
        [ fontSize (px 16)
        , margin4 zero zero theme.smallSpacing zero
        ]


horizontalList : Styled msg
horizontalList =
    styled Html.ul
        [ listStyle none
        , margin zero
        , padding zero
        ]


horizontalListItem : Styled msg
horizontalListItem =
    styled Html.li
        [ display inlineBlock
        , marginRight theme.defaultSpacing
        ]


li : Styled msg
li =
    styled Html.li
        [ marginBottom theme.smallSpacing
        , marginTop theme.smallSpacing
        ]


mainSearchInput : Styled msg
mainSearchInput =
    styled input
        [ border3 (px 1) solid theme.borderColor
        , display inlineBlock
        , fontSize theme.fontSize
        , lineHeight (px 50)
        , padding4 zero (px 15) zero (px (40 + 10))
        , verticalAlign top
        , width (pct 100)
        ]


ol : Styled msg
ol =
    styled Html.ol []


pageExtra : Styled msg
pageExtra attrs children =
    styled div
        [ width (pct 100)
        ]
        []
        [ styled div
            [ borderTop3 (px 1) solid (hex "f0f0f0")
            , margin2 zero auto
            , maxWidth theme.contentMaxWidth
            , padding2 zero theme.defaultSpacing
            , position relative
            , width (pct 100)
            ]
            attrs
            children
        ]


pageFooter : Styled msg
pageFooter attrs children =
    styled div
        [ backgroundImage
            (linearGradient
                (stop <| hex "f7f7f7")
                (stop2 (hex "ffffff") (px 20))
                []
            )
        , borderTop3 (px 1) solid (hex "f3f3f3")
        , width (pct 100)
        ]
        []
        [ styled div
            [ margin2 zero auto
            , maxWidth theme.contentMaxWidth
            , padding2 zero theme.defaultSpacing
            , position relative
            , width (pct 100)
            ]
            attrs
            children
        ]


pageFrame : Styled msg
pageFrame attrs children =
    styled div
        [ boxSizing borderBox
        , fontFamilies [ "Arial" ]
        , fontSize theme.fontSize
        , width (pct 100)
        ]
        attrs
        ([ Html.node "style" []
            [ Html.text
                """
html {
    box-sizing: border-box;
    height: 100%;
}
body {
    font-size: 16px;
    height: 100%;
}
*, *:before, *:after {
    box-sizing: inherit;
}
                 """
            ]
         ]
            ++ children
        )


pageHeader : Styled msg
pageHeader attrs children =
    styled div
        --[ backgroundColor (hex "eeeeee")
        [ backgroundImage
            (linearGradient2
                toBottomRight
                (stop (hex "60b5cc"))
                (stop (hex "7fd13b"))
                []
            )
        , borderBottom3 (px 2) solid (hex "60b5cc")
        , width (pct 100)
        ]
        []
        [ styled div
            --[ backgroundColor (hex "eeeeee")
            [ margin2 zero auto
            , maxWidth theme.contentMaxWidth
            , padding theme.defaultSpacing
            , position relative
            , width (pct 100)
            ]
            attrs
            children
        ]


pageLogo : Styled msg
pageLogo attrs children =
    styled Html.a [] [ Attr.href "/" ]
        [ styled img
            [ display inlineBlock
            , height (px 30)
            , lineHeight (px 42)
            , marginLeft (px 11)
            , marginTop (px 11)
            , position absolute
            , textAlign center
            , verticalAlign top
            , width (px 30)
            ]
            ([ Attr.src "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAB4AAAAeCAYAAAA7MK6iAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAABSQAAAUkBRDzYzwAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAALvSURBVEiJvdbtS1NRHAfw79m92nyY2BScXcWVSBI4abICNcsXEZoGgQq2xwqEBvbsW90fkG80jIyIwMIUfBNLEgI1JIsoRYIoTVeKi3CBAx9iu6cXetd1be7cpZ1359zfj8/vcM8Tqepo3nO04t19SmCG8nbri7fLxQUCboAeV5LImbIre2fWMx05eYv7QWBQCJfuTXUn+lZqm1QiLQOgZ4YNxmN9am9C4f/GVQBACKlLn0jueT1qvEgoHimEAeDmAZ3TFeT50wAdZYbDcQCP48Hzdc62IJ9YzYKr5B0JfzNivBAPToEWVpwPH9jE4dEUWPWHp6cAMV1pAXk6Z47H21XNBX49A0gFEyzh6y+zMOzRNz41uLRBqmzBcQQ/288Wr84MTUbFI8ISnv11FQ+0VRabMHgewDkmlWLg7bLeTKeEh/k65+JMlJlHhSX81YQBACw2YRAxcYqB9369ebhEuAcKCwWQr3MiEs4VGctdMfBD37xZhb6UNHtx2vQBAEXboSPGfd0Ascq+lGlT3QlLK2eaVGKwHCB5TDATLkPpVjQqzgRL+Px33cGlZI1jCx4b3YL/8J+8xEFVue0/jtDqxyeLAcBqEwYBCjUjulkjWgqEG/i80H5KKRzCNZpVmx5zYEXDcVXs0Ijp3IjnBLpSHCIF4ZRmixT+OGZMBxJzk8ww1XanEKr2LWitWuEJwLjPKUXr9fmx2wrhPyhAraCAn5QCC2DCKUXrtdmxTogYUgCHoaFh1LPgEkpEDAEwMcJRUEacULRdlaFAjCMTAAjgXs5Nsmaaau7SSGgEPEPoJRSkUUKvzI51EBHPJTQmvIGqGzJNNXcoYItVZBgOUPJRhh6Rh0a/nZSiYXgG77Nc/uBKIhxeACgJD4u4j+NGZficp6ZnPLV5DQSfIoX8/QL5V1SG9xE7oIa1dK0ToGiMCu8YyoCH4B1HY+D8rqIyvB92CjVsEs7vOhqy0dBP7ZBwPiFXXbdxOOweGo6nB7yO3/TojrVr58QKAAAAAElFTkSuQmCC" ] ++ attrs)
            children
        ]


pageMain : List (Html msg) -> List (Html msg) -> Html msg
pageMain topics details =
    styled div
        [ width (pct 100)
        ]
        []
        [ styled div
            [ margin4 (px -23) auto (px 6) auto
            , maxWidth theme.contentMaxWidth
            , padding2 zero theme.defaultSpacing
            , width (pct 100)
            ]
            []
            [ styled div
                [ borderTop3 (px 1) solid theme.borderColor
                , boxShadow4 (px 0) (px 2) (px 4) (hex "bbbbbb")
                , displayFlex
                ]
                []
                [ styled div
                    [ backgroundColor (hex "ffffff")
                    , borderRight3 (px 1) solid theme.borderColor
                    , displayFlex
                    , flexDirection column
                    , flexShrink (num 0)
                    , position relative
                    , width (px 400)
                    ]
                    []
                    topics
                , styled div
                    [ backgroundColor (hex "eeeeee")
                    , flexGrow (num 1)
                    , padding theme.smallSpacing
                    , width (pct 100)
                    ]
                    []
                    details
                ]
            ]
        ]


pageSecondary : Styled msg
pageSecondary attrs children =
    styled div
        [ width (pct 100)
        ]
        []
        [ styled div
            [ margin2 zero auto
            , maxWidth theme.contentMaxWidth
            , padding theme.defaultSpacing
            , position relative
            , width (pct 100)
            ]
            attrs
            children
        ]


sectionContent : Styled msg
sectionContent =
    styled div
        [ marginBottom theme.defaultSpacing
        ]


sectionTitle : Styled msg
sectionTitle =
    styled h1
        [ color (hex "60b5cc")
        , fontSize (px 22)
        , margin2 theme.defaultSpacing zero
        ]


subSectionTitle : Styled msg
subSectionTitle =
    styled h2
        [ color (hex "f0ad00")
        , fontSize (px 18)
        , margin2 theme.defaultSpacing zero
        ]


table : Styled msg
table =
    styled Html.table
        [ margin4 theme.contentSpacing (px -1) theme.contentSpacing (px -1)
        ]


tbody : Styled msg
tbody =
    styled Html.tbody []


td : Styled msg
td =
    styled Html.td
        [ borderBottom3 (px 1) solid (hex "eeeeee")
        , padding2 theme.defaultSpacing zero
        , paddingRight theme.defaultSpacing
        , verticalAlign top
        ]


tfoot : Styled msg
tfoot =
    styled Html.tfoot []


th : Styled msg
th =
    styled Html.th
        [ borderBottom3 (px 1) solid (hex "f0ad00")
        , color (hex "f0ad00")
        , padding2 theme.smallSpacing zero
        , textAlign left
        ]


thead : Styled msg
thead =
    styled Html.thead []


tr : Styled msg
tr =
    styled Html.tr []


topicContainer : Html msg -> Styled msg
topicContainer topic attrs children =
    styled div
        [ backgroundColor (hex "eeeeee")
        , displayFlex
        ]
        []
        [ styled div
            [ alignSelf flexStart
            , borderTop3 (px 1) solid theme.borderColor
            , color (hex "888888")
            , marginTop (px -1)
            , maxWidth (px 125)
            , padding theme.smallSpacing
            , textAlign right
            , width (pct 100)
            ]
            []
            [ topic
            ]
        , styled ul
            [ alignSelf flexStart
            , backgroundColor (hex "ffffff")
            , borderLeft3 (px 1) solid theme.borderColor
            , listStyle none
            , flexGrow (num 1)
            , margin zero
            , padding zero
            ]
            attrs
            children
        ]


topicItem : Styled msg
topicItem attrs children =
    styled li
        [ borderTop3 (px 1) solid theme.borderColor
        , marginBottom zero
        , marginTop (px -1)
        ]
        []
        [ styled button
            [ backgroundColor (hex "ffffff")
            , border zero
            , display block
            , cursor pointer
            , fontSize theme.fontSize
            , padding theme.smallSpacing
            , textAlign left
            , width (pct 100)
            ]
            attrs
            children
        ]


ul : Styled msg
ul =
    styled Html.ul []
