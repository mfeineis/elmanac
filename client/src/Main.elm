module Main exposing (main)

import Browser
import Html.Styled as Html exposing (Html, toUnstyled)
import Html.Styled.Attributes as Attr
import Styled
import Url exposing (Url)


-- App setup


type alias Model =
    {}


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


type alias Flags =
    {}


defaultFlags : Flags
defaultFlags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( {}, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange : Url -> msg
        , onUrlRequest : UrlRequest -> msg
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- Staying in contact in the world


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- State management


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- Displaying the current state


view : Model -> Browser.Document Msg
view model =
    let
        body =
            Styled.pageFrame []
                [ Styled.pageHeader []
                    [ Styled.pageLogo []
                        [ Html.text "Elm"
                        ]
                    , Styled.mainSearchInput
                        [ Attr.placeholder "Do you want to know more?"
                        ]
                        []
                    ]
                , Styled.pageMain
                    [ Styled.topicContainer (Html.text "Booleans")
                        []
                        [ Styled.topicItem [] [ Html.text "Blubb" ]
                        , Styled.topicItem [] [ Html.text "Bla" ]
                        , Styled.topicItem [] [ Html.text "Plisch" ]
                        ]
                    , Styled.topicContainer (Html.text "Numbers")
                        []
                        [ Styled.topicItem [] [ Html.text "Int" ]
                        , Styled.topicItem [] [ Html.text "Float" ]
                        , Styled.topicItem [] [ Html.text "Double" ]
                        , Styled.topicItem [] [ Html.text "Complex" ]
                        ]
                    , Styled.topicContainer (Html.text "Maybe")
                        []
                        [ Styled.topicItem [] [ Html.text "Nothing" ]
                        , Styled.topicItem [] [ Html.text "Just something" ]
                        ]
                    ]
                    [ settingsView model
                    ]
                , elmLinksView model
                , toolbeltView model
                , contributingView model
                , faqView model
                , learningElmView model
                , externalToolsLinksView model
                , editorPluginView model
                , elmsImpactView model
                , Styled.pageFooter []
                    [ Styled.sectionTitle [] [ Html.text "About Elmanac" ]
                    , Styled.sectionContent []
                        [ Html.text
                            """
                            Elmanac strives to be a comprehensive information hub
                            for the Elm ecosystem.
                            """
                        ]
                    ]
                ]
    in
    { title = "Elmanac"
    , body =
        [ toUnstyled body
        ]
    }


elmsImpactView : Model -> Html Msg
elmsImpactView model =
    Styled.pageExtra []
        [ Styled.sectionTitle [] [ Html.text "Elm's Impact" ]
        , Styled.sectionContent []
            [ Styled.ul []
                  --
                [ Styled.li []
                    [ Styled.contentLink [ Attr.href "https://github.com/wende/elchemy" ]
                        [ Html.text "Elmchemy - Write Elixir code using statically-typed Elm-like syntax (compatible with Elm tooling)" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "https://fable-elmish.github.io/elmish/" ]
                        [ Html.text "Elmish - Elm style web framework in F#" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "https://github.com/dmjio/miso" ]
                        [ Html.text "Miso - Elm style isomorphic web framework in Haskell" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "http://redux.js.org/" ]
                        [ Html.text "Redux JavaScript state management" ]
                    ]
                ]
            ]
        ]


learningElmView : Model -> Html Msg
learningElmView model =
    Styled.pageExtra []
        [ Styled.sectionTitle [] [ Html.text "Learning Elm" ]
        , Styled.sectionContent []
            [ Styled.subSectionTitle [] [ Html.text "Talks" ]
            , Styled.ul []
                [ Styled.li []
                    [ Styled.contentLink [ Attr.href "https://www.youtube.com/watch?v=DfLvDFxcAIA" ]
                        [ Html.text "Evan Czaplicki - Accidentally Concurrent (2015)" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "https://www.youtube.com/watch?v=DSjbTC-hvqQ" ]
                        [ Html.text "Evan Czaplicki - Code is the Easy Part (2016)" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "https://www.youtube.com/watch?v=jl1tGiUiTtI" ]
                        [ Html.text "Evan Czaplicki - Convergent Evolution (2017)" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "https://www.youtube.com/watch?v=Bv8elmoComE" ]
                        [ Html.text "Evan Czaplicki - Elm's Future (2013)" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "http://www.elmbark.com/2016/03/16/mainstream-elm-user-focused-design" ]
                        [ Html.text "Evan Czaplicki - Let's be mainstream (2015)" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "https://www.youtube.com/watch?v=XpDsk374LDE" ]
                        [ Html.text "Evan Czaplicki - The Life of a File (2017)" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "https://www.youtube.com/watch?v=6EdXaWfoslc" ]
                        [ Html.text "Richard Feldman - Effects as Data (2015)" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "https://www.youtube.com/watch?v=3FNKaGm3gk0" ]
                        [ Html.text "Richard Feldman - Elm and React in Production (2016)" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "https://www.youtube.com/watch?v=ar3TakwE8o0" ]
                        [ Html.text "Richard Feldman - Elm and Web Components (2016)" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "https://www.youtube.com/watch?v=IcgmSRJHu_8" ]
                        [ Html.text "Richard Feldman - Making impossible states impossible (2016)" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "https://www.youtube.com/watch?v=DoA4Txr4GUs" ]
                        [ Html.text "Richard Feldman - Scaling Elm Apps (2017)" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "https://www.youtube.com/watch?v=G-GhUxeYc1U" ]
                        [ Html.text "Richard Feldman - Teaching Elm to Beginners (2017)" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "https://www.youtube.com/watch?v=n5faeSW71ko" ]
                        [ Html.text "Richard Feldman - The Design Evolution of elm-css and elm-test (2017)" ]
                    ]
                ]
            ]
        ]


externalToolsLinksView : Model -> Html Msg
externalToolsLinksView model =
    Styled.pageExtra []
        [ Styled.sectionTitle [] [ Html.text "Other Elm Tools" ]
        , Styled.sectionContent []
            [ Styled.ul []
                [ Styled.li []
                    [ Styled.contentLink [ Attr.href "http://elm-news.com/" ]
                        [ Html.text "http://elm-news.com"
                        ]
                    , Styled.contentAnnotation []
                        [ Html.text "A news aggregator for all things Elm."
                        ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "http://klaftertief.github.io/elm-search/" ]
                        [ Html.text "http://klaftertief.github.io/elm-search/"
                        ]
                    , Styled.contentAnnotation []
                        [ Html.text "A fancy type search in Elm."
                        ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "http://builtwithelm.co/" ]
                        [ Html.text "Built with Elm"
                        ]
                    , Styled.contentAnnotation []
                        [ Html.text "A collection of apps built with Elm."
                        ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "https://libraries.io/search?order=desc&platforms=Elm&sort=latest_release_published_at" ]
                        [ Html.text "Elm on libraries.io"
                        ]
                    , Styled.contentAnnotation []
                        [ Html.text "A continuously updated and sortable list of Elm packages."
                        ]
                    ]
                ]
            ]
        ]


toolbeltView : Model -> Html Msg
toolbeltView model =
    Styled.pageExtra []
        [ Styled.sectionTitle [] [ Html.text "Elm Toolbelt" ]
        , Styled.sectionContent []
            [ Html.text "Here are some tools you probably want to use."
            , Styled.ul []
                [ Styled.li []
                    [ Styled.contentLink [ Attr.href "https://ellie-app.com" ]
                        [ Html.text "Ellie - the Elm Live Editor" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "https://github.com/avh4/elm-format" ]
                        [ Html.text "avh4/elm-format" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink [ Attr.href "https://github.com/eeue56/elm-static-html" ]
                        [ Html.text "eeue56/elm-static-html" ]
                    ]
                ]
            ]
        ]


elmLinksView : Model -> Html Msg
elmLinksView model =
    Styled.pageSecondary []
        [ Styled.horizontalList []
            [ Styled.horizontalListItem []
                [ Styled.contentLink
                    [ Attr.href "http://elm-lang.org/" ]
                    [ Html.text "Elm Website" ]
                ]
            , Styled.horizontalListItem []
                [ Styled.contentLink
                    [ Attr.href "https://guide.elm-lang.org/" ]
                    [ Html.text "Elm Guide" ]
                ]
            , Styled.horizontalListItem []
                [ Styled.contentLink
                    [ Attr.href "http://package.elm-lang.org/" ]
                    [ Html.text "Elm Packages" ]
                ]
            , Styled.horizontalListItem []
                [ Styled.contentLink
                    [ Attr.href "http://elm-lang.org/community" ]
                    [ Html.text "Elm Community" ]
                ]
            , Styled.horizontalListItem []
                [ Styled.contentLink
                    [ Attr.href "https://github.com/elm-lang/projects/blob/master/roadmap.md" ]
                    [ Html.text "Elm Roadmap" ]
                ]
            , Styled.contentLink
                [ Attr.href "https://github.com/elm-lang/elm-platform/tree/master/upgrade-docs" ]
                [ Html.text "Elm Upgrade Guide" ]
            ]
        ]


faqView : Model -> Html Msg
faqView model =
    Styled.pageExtra []
        [ Styled.sectionTitle [] [ Html.text "Frequently Asked Questions" ]
        , Styled.sectionContent []
            [ Styled.ul []
                [ Styled.li []
                    [ Styled.contentLink
                        [ Attr.href "https://github.com/elm-lang/projects/blob/master/roadmap.md" ]
                        [ Html.text "Can I use Elm on servers?" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink
                        [ Attr.href "https://guide.elm-lang.org/interop/" ]
                        [ Html.text "Does Elm have a foreign function interface? No, but it has message passing." ]
                    ]
                , Styled.li []
                    [ Styled.contentLink
                        [ Attr.href "http://faq.elm-community.org/" ]
                        [ Html.text "I have a specific question about Elm, is there an FAQ for that?" ]
                    ]
                , Styled.li []
                    [ Html.text "I encounter huge compile times in Elm 0.18, what should I do?"
                    , Styled.ul []
                        [ Styled.li []
                            [ Styled.contentLink
                                [ Attr.href "https://github.com/elm-lang/elm-make/issues" ]
                                [ Html.text "Find the corresponding issue or open a new one on Github." ]
                            ]
                        , Styled.li []
                            [ Html.text "There is also a #compile-time slack channel."
                            ]
                        ]
                    ]
                , Styled.li []
                    [ Styled.contentLink
                        [ Attr.href "https://ellie-app.com" ]
                        [ Html.text "I want to share an Elm code snippet, is there something like JSFiddle? Yes, there is Ellie." ]
                    ]
                , Styled.li []
                    [ Styled.contentLink
                        [ Attr.href "https://gist.github.com/evancz" ]
                        [ Html.text "Evan shared a gist some time ago, where may I find it?" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink
                        [ Attr.href "https://github.com/elm-lang/projects/blob/master/roadmap.md" ]
                        [ Html.text "How do I create a Single Page App (a.k.a. SPA) in Elm?" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink
                        [ Attr.href "http://package.elm-lang.org/packages/elm-lang/core/latest" ]
                        [ Html.text "What modules get imported by default in Elm, is there a Haskell-style Prelude?" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink
                        [ Attr.href "https://gist.github.com/evancz/1c5f2cf34939336ecb79b97bb89d9da6" ]
                        [ Html.text "Why do I have to write JSON decoders by hand?" ]
                    ]
                ]
            ]
        ]


contributingView : Model -> Html Msg
contributingView model =
    Styled.pageExtra []
        [ Styled.sectionTitle [] [ Html.text "Contributing to Elm" ]
        , Styled.sectionContent []
            [ Html.text
                """
                Development on Elm works a little different than most
                open source projects, so before picking up an issue
                make sure you get in touch via slack to avoid
                confusion and possible frustration.
                """
            , Styled.ol []
                [ Styled.li []
                    [ Styled.contentLink
                        [ Attr.href "http://elm-lang.org/community#contribute" ]
                        [ Html.text "Read the contribution guide first" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink
                        [ Attr.href "https://github.com/elm-lang/projects" ]
                        [ Html.text "Take on a suggested project" ]
                    ]
                , Styled.li []
                    [ Styled.contentLink
                        [ Attr.href "https://github.com/elm-lang/error-message-catalog" ]
                        [ Html.text "Improve your friendly compiler error messages even further" ]
                    ]
                ]
            ]
        ]


editorPluginView : Model -> Html Msg
editorPluginView model =
    Styled.pageExtra []
        [ Styled.sectionTitle [] [ Html.text "Editor Support for Elm" ]
        , Styled.sectionContent []
            [ Html.text
                """
                As Elm continues to pick up steam there is a growing number
                of plugins to support Elm in all kinds of editors.
                """
            ]
        , Styled.sectionContent []
            [ Styled.table []
                [ Styled.thead []
                    [ Styled.tr []
                        [ Styled.th [] [ Html.text "Editor" ]
                        , Styled.th [] [ Html.text "Plugin" ]
                        , Styled.th [] [ Html.text "Details" ]
                        ]
                    ]
                , Styled.tbody []
                    [ Styled.tr []
                        [ Styled.td [] [ Html.text "Atom" ]
                        , Styled.td []
                            [ Styled.contentLink
                                [ Attr.href "https://atom.io/packages/language-elm"
                                ]
                                [ Html.text "language-elm" ]
                            ]
                        , Styled.td [] [ Html.text "Syntax highlighting and autocompletion for the language Elm" ]
                        ]
                    , Styled.tr []
                        [ Styled.td [] [ Html.text "JetBrains" ]
                        , Styled.td []
                            [ Styled.contentLink
                                [ Attr.href "https://plugins.jetbrains.com/plugin/8192-elm-language-plugin"
                                ]
                                [ Html.text "Elm Language Plugin" ]
                            ]
                        , Styled.td []
                            [ Html.text "Compatible with IntelliJ IDEA, PhpStorm, WebStorm, PyCharm, RubyMine, AppCode, CLion, Gogland, DataGrip, Rider, MPS, Android Studio"
                            ]
                        ]
                    , Styled.tr []
                        [ Styled.td [] [ Html.text "" ]
                        , Styled.td []
                            [ Styled.contentLink
                                [ Attr.href "https://plugins.jetbrains.com/plugin/7669-elm-language-support"
                                ]
                                [ Html.text "Elm Language Support" ]
                            ]
                        , Styled.td []
                            [ Html.text "Compatible with IntelliJ IDEA, PhpStorm, WebStorm, PyCharm, RubyMine, AppCode, CLion, Gogland, DataGrip, Rider, MPS, Android Studio"
                            ]
                        ]
                    , Styled.tr []
                        [ Styled.td [] [ Html.text "Light Table" ]
                        , Styled.td []
                            [ Styled.contentLink
                                [ Attr.href "https://rundis.gitbooks.io/elm-light-guide"
                                ]
                                [ Html.text "Elm Light" ]
                            ]
                        , Styled.td [] [ Html.text "This plugin aims to provide you with 1st. class support for the Elm programming language and associated platform." ]
                        ]
                    , Styled.tr []
                        [ Styled.td [] [ Html.text "Spacemacs" ]
                        , Styled.td []
                            [ Styled.contentLink
                                [ Attr.href "https://github.com/syl20bnr/spacemacs/tree/master/layers/+lang/elm"
                                ]
                                [ Html.text "spacemacs/layers/+lang/elm" ]
                            ]
                        , Styled.td [] [ Html.text "This layer adds support for Elm. It relies on elm-mode and flycheck-elm." ]
                        ]
                    , Styled.tr []
                        [ Styled.td [] [ Html.text "Sublime" ]
                        , Styled.td []
                            [ Styled.contentLink
                                [ Attr.href "https://packagecontrol.io/packages/Elm%20Language%20Support"
                                ]
                                [ Html.text "Elm Language Support" ]
                            ]
                        , Styled.td [] [ Html.text "Elm language syntax highlighting and tool integration for ST2/3." ]
                        ]
                    , Styled.tr []
                        [ Styled.td [] [ Html.text "Visual Studio Code" ]
                        , Styled.td []
                            [ Styled.contentLink
                                [ Attr.href "https://marketplace.visualstudio.com/items?itemName=sbrink.elm"
                                ]
                                [ Html.text "elm" ]
                            ]
                        , Styled.td [] [ Html.text "Elm Language Support for Visual Studio Code" ]
                        ]
                    , Styled.tr []
                        [ Styled.td [] [ Html.text "-" ]
                        , Styled.td []
                            [ Styled.contentLink
                                [ Attr.href "https://github.com/ElmCast/elm-oracle"
                                ]
                                [ Html.text "elm-oracle" ]
                            ]
                        , Styled.td [] [ Html.text "An Elm language server being used by some editor plugins." ]
                        ]
                    ]
                ]
            ]
        ]


settingsView : Model -> Html Msg
settingsView model =
    Styled.detailContainer []
        [ Styled.detailTitle [] [ Html.text "Settings" ]
        , Styled.detailContent []
            [ Html.text
                """
                Here you may choose the settings for your
                personal Elmanac experience.
                """
            ]
        , Styled.detailContent []
            [ Styled.detailCheckbox []
                [ Html.text "I am an Elm developer"
                ]
            , Styled.detailList []
                [ Styled.detailCheckbox [ Attr.checked True ]
                    [ Html.text "Enabled Code Search" ]
                , Styled.detailCheckbox [ Attr.checked True ]
                    [ Html.text "Include Doc comments" ]
                , Styled.detailCheckbox []
                    [ Html.text "Include type signatures" ]
                , Styled.detailCheckbox []
                    [ Html.text "Include migration topics" ]
                ]
            ]
        , Styled.detailContent []
            [ Styled.detailCheckbox []
                [ Html.text "I am new to web development"
                ]
            , Styled.detailList []
                [ Styled.detailCheckbox [ Attr.checked True ]
                    [ Html.text "Prioritize basic constructs" ]
                , Styled.detailCheckbox [ Attr.checked True ]
                    [ Html.text "Show \"Getting Started\" content" ]
                ]
            ]
        , Styled.detailContent []
            [ Styled.detailCheckbox []
                [ Html.text "I am a manager or CTO"
                ]
            , Styled.detailList []
                [ Styled.detailCheckbox [ Attr.checked True ]
                    [ Html.text "Include Roadmap" ]
                , Styled.detailCheckbox [ Attr.checked True ]
                    [ Html.text "Include non-technical sources" ]
                ]
            ]
        , Styled.detailContent []
            [ Html.text "Your settings will be saved in `localStorage`." ]
        , Styled.detailCheckbox
            [ Attr.checked True ]
            [ Html.text "Save Settings" ]
        ]
