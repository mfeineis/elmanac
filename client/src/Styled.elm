module Styled
    exposing
        ( detailCheckbox
        , detailContainer
        , detailContent
        , detailList
        , detailTitle
        , mainSearchInput
        , pageExtra
        , pageFooter
        , pageFrame
        , pageHeader
        , pageLogo
        , pageMain
        , pageSecondary
        , sectionContent
        , sectionTitle
        , topicContainer
        , topicItem
        )

import Css exposing (..)
import Html
    exposing
        ( Html
        , button
        , div
        , h1
        , h2
        , img
        , input
        , label
        , li
        , select
        , ul
        )
import Html.Attributes as Attr


styles : List Style -> Html.Attribute msg
styles =
    Css.asPairs >> Attr.style


type alias Styled msg =
    List (Html.Attribute msg) -> List (Html msg) -> Html msg


theme =
    { borderColor = hex "dddddd"
    , contentMaxWidth = px 1200
    , defaultSpacing = px 10
    , fontSize = px 16
    }


detailCheckbox : Styled msg
detailCheckbox attrs children =
    label
        [ styles
            [ display block
            , padding2 (px 3) zero
            ]
        ]
        (input
            (styles
                []
                :: [ Attr.type_ "checkbox" ]
                ++ attrs
            )
            []
            :: children
        )


detailContainer : Styled msg
detailContainer attrs children =
    div
        (styles
            []
            :: attrs
        )
        children


detailContent : Styled msg
detailContent attrs children =
    div
        (styles
            [ marginBottom theme.defaultSpacing
            ]
            :: attrs
        )
        children


detailList : Styled msg
detailList attrs children =
    ul
        (styles
            [ listStyle none
            , margin zero
            , paddingLeft theme.defaultSpacing
            ]
            :: attrs
        )
        children


detailTitle : Styled msg
detailTitle attrs children =
    h2
        (styles
            [ fontSize (px 16)
            , margin4 zero zero theme.defaultSpacing zero
            ]
            :: attrs
        )
        children


mainSearchInput : Styled msg
mainSearchInput attrs children =
    input
        (styles
            [ border3 (px 1) solid theme.borderColor
            , display inlineBlock
            , fontSize theme.fontSize
            , lineHeight (px 50)
            , padding4 zero (px 15) zero (px (40 + 10))
            , verticalAlign top
            , width (pct 100)
            ]
            :: attrs
        )
        children


pageExtra : Styled msg
pageExtra attrs children =
    div
        [ styles
            [ width (pct 100)
            ]
        ]
        [ div
            (styles
                [ borderTop3 (px 1) solid (hex "f0f0f0")
                , margin2 zero auto
                , maxWidth theme.contentMaxWidth
                , padding theme.defaultSpacing
                , position relative
                , width (pct 100)
                ]
                :: attrs
            )
            children
        ]


pageFooter : Styled msg
pageFooter attrs children =
    div
        [ styles
            [ backgroundImage
                (linearGradient
                    (stop <| hex "f7f7f7")
                    (stop2 (hex "ffffff") (px 20))
                    []
                )
            , borderTop3 (px 1) solid (hex "f3f3f3")
            , width (pct 100)
            ]
        ]
        [ div
            (styles
                [ margin2 zero auto
                , maxWidth theme.contentMaxWidth
                , padding theme.defaultSpacing
                , position relative
                , width (pct 100)
                ]
                :: attrs
            )
            children
        ]


pageFrame : Styled msg
pageFrame attrs children =
    div
        (styles
            [ boxSizing borderBox
            , fontFamilies [ "Arial" ]
            , fontSize theme.fontSize
            , width (pct 100)
            ]
            :: attrs
        )
        ([ Html.node "style"
            []
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
    div
        [ styles
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
        ]
        [ div
            (styles
                --[ backgroundColor (hex "eeeeee")
                [ margin2 zero auto
                , maxWidth theme.contentMaxWidth
                , padding theme.defaultSpacing
                , position relative
                , width (pct 100)
                ]
                :: attrs
            )
            children
        ]


pageLogo : Styled msg
pageLogo attrs children =
    img
        (styles
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
            :: ([ Attr.src "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAB4AAAAeCAYAAAA7MK6iAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAABSQAAAUkBRDzYzwAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAALvSURBVEiJvdbtS1NRHAfw79m92nyY2BScXcWVSBI4abICNcsXEZoGgQq2xwqEBvbsW90fkG80jIyIwMIUfBNLEgI1JIsoRYIoTVeKi3CBAx9iu6cXetd1be7cpZ1359zfj8/vcM8Tqepo3nO04t19SmCG8nbri7fLxQUCboAeV5LImbIre2fWMx05eYv7QWBQCJfuTXUn+lZqm1QiLQOgZ4YNxmN9am9C4f/GVQBACKlLn0jueT1qvEgoHimEAeDmAZ3TFeT50wAdZYbDcQCP48Hzdc62IJ9YzYKr5B0JfzNivBAPToEWVpwPH9jE4dEUWPWHp6cAMV1pAXk6Z47H21XNBX49A0gFEyzh6y+zMOzRNz41uLRBqmzBcQQ/288Wr84MTUbFI8ISnv11FQ+0VRabMHgewDkmlWLg7bLeTKeEh/k65+JMlJlHhSX81YQBACw2YRAxcYqB9369ebhEuAcKCwWQr3MiEs4VGctdMfBD37xZhb6UNHtx2vQBAEXboSPGfd0Ascq+lGlT3QlLK2eaVGKwHCB5TDATLkPpVjQqzgRL+Px33cGlZI1jCx4b3YL/8J+8xEFVue0/jtDqxyeLAcBqEwYBCjUjulkjWgqEG/i80H5KKRzCNZpVmx5zYEXDcVXs0Ijp3IjnBLpSHCIF4ZRmixT+OGZMBxJzk8ww1XanEKr2LWitWuEJwLjPKUXr9fmx2wrhPyhAraCAn5QCC2DCKUXrtdmxTogYUgCHoaFh1LPgEkpEDAEwMcJRUEacULRdlaFAjCMTAAjgXs5Nsmaaau7SSGgEPEPoJRSkUUKvzI51EBHPJTQmvIGqGzJNNXcoYItVZBgOUPJRhh6Rh0a/nZSiYXgG77Nc/uBKIhxeACgJD4u4j+NGZficp6ZnPLV5DQSfIoX8/QL5V1SG9xE7oIa1dK0ToGiMCu8YyoCH4B1HY+D8rqIyvB92CjVsEs7vOhqy0dBP7ZBwPiFXXbdxOOweGo6nB7yO3/TojrVr58QKAAAAAElFTkSuQmCC"] ++ attrs)
        )
        children


pageMain : List (Html msg) -> List (Html msg) -> Html msg
pageMain topics details =
    div
        [ styles
            [ width (pct 100)
            ]
        ]
        [ div
            [ styles
                [ margin4 (px -13) auto (px 6) auto
                , maxWidth theme.contentMaxWidth
                , padding2 zero (px 10)
                , width (pct 100)
                ]
            ]
            [ div
                [ styles
                    [ borderTop3 (px 1) solid theme.borderColor
                    , boxShadow4 (px 0) (px 2) (px 4) (hex "bbbbbb")
                    , displayFlex
                    ]
                ]
                [ div
                    [ styles
                        [ backgroundColor (hex "ffffff")
                        , borderRight3 (px 1) solid theme.borderColor
                        , displayFlex
                        , flexDirection column
                        , flexShrink (num 0)
                        , position relative
                        , width (px 400)
                        ]
                    ]
                    topics
                , div
                    [ styles
                        [ backgroundColor (hex "eeeeee")
                        , flexGrow (num 1)
                        , padding theme.defaultSpacing
                        , width (pct 100)
                        ]
                    ]
                    details
                ]
            ]
        ]


pageSecondary : Styled msg
pageSecondary attrs children =
    div
        [ styles
            [ width (pct 100)
            ]
        ]
        [ div
            (styles
                [ margin2 zero auto
                , maxWidth theme.contentMaxWidth
                , padding theme.defaultSpacing
                , position relative
                , width (pct 100)
                ]
                :: attrs
            )
            children
        ]


sectionContent : Styled msg
sectionContent attrs children =
    div
        (styles
            [ marginBottom theme.defaultSpacing
            ]
            :: attrs
        )
        children


sectionTitle : Styled msg
sectionTitle attrs children =
    h1
        (styles
            [ fontSize (px 18)
            , margin2 theme.defaultSpacing zero
            ]
            :: attrs
        )
        children


topicContainer : Html msg -> Styled msg
topicContainer topic attrs children =
    div
        [ styles
            [ backgroundColor (hex "eeeeee")
            , displayFlex
            ]
        ]
        [ div
            [ styles
                [ alignSelf flexStart
                , borderTop3 (px 1) solid theme.borderColor
                , color (hex "888888")
                , marginTop (px -1)
                , maxWidth (px 125)
                , padding theme.defaultSpacing
                , textAlign right
                , width (pct 100)
                ]
            ]
            [ topic
            ]
        , ul
            (styles
                [ alignSelf flexStart
                , backgroundColor (hex "ffffff")
                , borderLeft3 (px 1) solid theme.borderColor
                , listStyle none
                , flexGrow (num 1)
                , margin zero
                , padding zero
                ]
                :: attrs
            )
            children
        ]


topicItem : Styled msg
topicItem attrs children =
    li
        [ styles
            [ borderTop3 (px 1) solid theme.borderColor
            , marginTop (px -1)
            ]
        ]
        [ button
            (styles
                [ backgroundColor (hex "ffffff")
                , border zero
                , display block
                , cursor pointer
                , fontSize theme.fontSize
                , padding theme.defaultSpacing
                , textAlign left
                , width (pct 100)
                ]
                :: attrs
            )
            children
        ]
