# Room for improvement
This document is part two of [this repo's design documentation](../README.md)
that aims to determine what challenges an advanced elm package search might
have to tackle. 

In [part one](01-status-quo.md) we examined what the current user experience is
when searching on [package.elm-lang.org](package.elm-lang.org) and how it compares
to a normal search engine in the wild. We've seen a breakdown of the categories
of users we might encounter and what they might be looking for on the Elm package
site.

In this part we're going to have a look at how other web frameworks are dealing 
with discoverability of their APIs and packages. 

# Honorable Mention
The Aurelia docs are carried by the concept of "personas" - categories of users 
to tailor the experience to. This takes into account what the user might be 
interested in the most. In this way information overload for non-technical people 
is avoided and developers aren't treated to guides that are typically only 
relevant for management. The approach taken here is worth being explored as a 
valuable basis for the user experience of our new advanced package search.

![Aurelia personas](img/02-room-for-improvement/img-0100-aurelia-personas.png?raw=true)

Apart from that the search feature doesn't prove very useful if you don't already
know what you are looking for.

# Package registries around the web
Here we have a quick look at additional package registries that didn't end up in
the comparison case studies later in the document.

## Atom Editor Packages
[Atom Packages](https://atom.io/packages) has a basic search feature that has
a ranking system based on the number of downloads and number of github stars. This
is quite common to almost all package registries in use right now.

![Atom editor packages](img/02-room-for-improvement/img-0112-atom-packages.png?raw=true)

## Haskell has Hoogle
[Hoogle](https://www.haskell.org/hoogle/) is a search tool being used to 
search for Haskell APIs although one could argue that it is not a terrible user 
friendly one for newcomers. This style of search is already covered supremely
by [elm-search](https://klaftertief.github.io/elm-search/).

![Hoogle search](img/02-room-for-improvement/img-0113-hoogle.png?raw=true)

## JavaScript has JS Coach
[JS.COACH](https://js.coach) is the successor service of [React Parts](https://react.parts/web)
that in addition to the usual "newest" and "popular" rankings has a curated 
part Elm could adapt to packages that essentially emerged as the standard way 
of doing what they do - at least at the time. Worth noting is also the activity
graph of the github repo as the background image of the list entry to give the
user feeling for the quality of the package.

![JS.COACH search](img/02-room-for-improvement/img-0114-js-coach.png?raw=true)

## JavaScript mainly has NPM
[NPM](https://www.npmjs.com/) is the most popular package system for JavaScript 
modules. The search box provides the user with a nice fuzzy interface. The ranking
criteria include commendable attributes like "Quality" and "Maintenance" and
there is an advanced search where the user can combine categories that looks
pretty power user-ish.

![NPM search](img/02-room-for-improvement/img-0115-npm.png?raw=true)

## PHP has Packagist
[Packagist](https://packagist.org/) provides basically the same functionality for 
PHP what NPM is for JavaScript. The search is pretty basic but supports viewing
the download count and github stars of the package. It has a thorough detail view
that lists all kinds of dependencies, what the package provides and even which 
packages/language quirks it is supposed to replace.

![Packagist search](img/02-room-for-improvement/img-0116-packagist-search.png?raw=true)

![Packagist detail view](img/02-room-for-improvement/img-0117-packagist-details.png?raw=true)

## Python has PIP
The search function of Python's package manager [PIP](https://pypi.python.org/pypi/pip) 
is pretty basic although they have some kind of "weighting" metric to give you *the best*
results. Another aspect of note is the [Python Infrastructure Status Page](https://status.python.org/)
that is fairly normal for commercial services but has the potential of being a
nice addition to Elm's infrastructure.

![PIP search](img/02-room-for-improvement/img-0118-pip.png?raw=true)

## React had React parts
[React Parts](https://react.parts/web) is a registry for [React](https://facebook.github.io/react/)
whose search feature is pretty standard in this listing. What stands out is the
very obvious separation of "web" and "native" components that every user will
understand immediately.

![React parts search](img/02-room-for-improvement/img-0119-react-parts.png?raw=true)

## Sublime has Package Control
[Package Control](https://packagecontrol.io/stats) is the package manager for 
the [Sublime Text Editor](https://www.sublimetext.com/). In the spirit of the 
editor itself the website sports keyboard shortcuts. They also have quite a bit
of [geek stats](https://packagecontrol.io/stats).

![Package control home](img/02-room-for-improvement/img-0120-package-control-index.png?raw=true)

![Package control search](img/02-room-for-improvement/img-0121-package-control-search.png?raw=true)

![Package control stats](img/02-room-for-improvement/img-0122-package-control-stats.png?raw=true)

# Case study "Components"
NPM has taken the JavaScript web development world by storm, so virtually no major 
anything is still maintaining their own package infrastructure. We now take a look 
at some examples of the documentation that are available for well known frameworks 
and libraries.

Most of the big frameworks have some kind of search for their core APIs and they 
all have quite extensive guides for their core concepts but they don't focus
very much on their community contributions. Users mainly rely on external 
package registries for their building blocks.

* [Angular](https://angular.io/docs)
* [Aurelia](https://aurelia.io/hub.html)
* [Ember](https://www.emberjs.com/learn/)
* [React](https://facebook.github.io/react/docs/)

## "Component" Docs
Almost all modern web frameworks have some notion of components so this is what
people expect to find in Elm too. Let's examine how our study subjects present
themselves regarding finding information on components.

### Angular Components
As we see here [Angular](https://angular.io) has a nice integrated experience for 
their docs search. The panel presents the results categorized by API and Guide. The 
results we're looking for is there but the user has to skim a surprising number of 
entries to get to one of the core concepts of the framework.

![Angular component search](img/02-room-for-improvement/img-0101-angular-component.png?raw=true)

### Aurelia Components
The [Aurelia](https://aurelia.io) docs might not be very forthcoming making general
concepts searchable but finding out about components is clearly not a problem, the 
first entry is probably what we're looking for. Maybe they provide too much information
for newcomers, it feels a little overwhelming.

![Aurelia component search](img/02-room-for-improvement/img-0102-aurelia-component.png?raw=true)

### Ember Components
[Ember](https://www.emberjs.com) goes a different route than most of the other 
frameworks by integrating an external search engine provider - 
[Algolia](https://www.algolia.com/) - that powers their interactivity with the user. 
As we can see for "components" it's quite successful in bringing up relevant results, 
as is for "route" - the core concepts of Ember. 

![Ember component search](img/02-room-for-improvement/img-0103-ember-component.png?raw=true)

![Ember route search](img/02-room-for-improvement/img-0104-ember-route.png?raw=true)

For those newcomers who don't really know what they're looking for exactly, the quality
of the search results deteriorates quite remarkably.

![Ember route search](img/02-room-for-improvement/img-0105-ember-http.png?raw=true)

![Ember route search](img/02-room-for-improvement/img-0106-ember-date.png?raw=true)

![Ember route search](img/02-room-for-improvement/img-0107-ember-flexbox.png?raw=true)

### React Components
[React](https://facebook.github.io/react) takes the same approach as Ember in involving 
[Algolia](https://www.algolia.com/) as the external search provider and exposes similar 
strengths and weeknesses. A search for "component" or "immutable" is rewarding but anything 
that doesn't hit some keyword provokes unproductive result sets.

![React component search](img/02-room-for-improvement/img-0108-react-component.png?raw=true)

![React component search](img/02-room-for-improvement/img-0109-react-immutable.png?raw=true)

And again it goes downhill from here.

![React component search](img/02-room-for-improvement/img-0110-react-date.png?raw=true)

![React component search](img/02-room-for-improvement/img-0111-react-partial.png?raw=true)

# What did we learn?
Surprisingly only one of the registries covered here has more useful search abilities than 
keywords and the occasional ranking by github stars and download count - despite having
an external search provider in the mix. [Angular](https://angular.io/docs) comes out at the
top in this regard. Some of the libraries' documentation pages go pretty deep beneath 
their respective API surfaces which makes the information cumbersome at times and other 
times overwhelming bordering on being useless. Nevertheless, we've got a whole buffet of 
features to choose from and improve upon:

1) A search tailored to the user in the style of the [Aurelia Docs](https://aurelia.io/hub.html) could be a very powerful feature for Elm.
2) A specialized search for both APIs, concepts and packages under one roof would be a unique feature.
3) The "I depend on/I provide this" details view of [Packagist](https://packagist.org/) might prove useful for Elm as well.
4) The infrastructure status page of [PIP](https://status.python.org/) might be a source of inspiration for us.
5) People love [geek stats](https://packagecontrol.io/stats) stats, maybe Elm could use some of that.
6) It would be awesome, if we could manage to produce a clean and focused presentation for the results of a potential full-text search feature. We may have to try on our own to come up with a good algorithm that is clever about Elm, the results of [Algolia](https://www.algolia.com/) that power both Ember's and React's docs search don't seem that impressive as of now.
7) Common ranking metrics sported by almost every package registry should definitely be included:

    * Github stars
    * Download counts
    * Sort by newest
    * Recently updated 
    * Most popular
    * Quality <3
    * Maintenance <3

8) Plain categories for the user to choose from are an easy win for usability.
9) Curated content might prove useful. This is especially true for Elm since the nice thing about the language is that it strives to have *one* best way to do something. The sidebar content currently employed is a good stepping stone, making it searchable would be even nicer.
10) Incorporating some of the graphs from Github in a similar way as [JS.COACH](https://js.coach) does wouldn't be a detriment to good user experience.
11) Fuzzy search is king.

Hopefully we can leverage Elm's type system to make even more specific searches possible.

## Random ideas

* It would be nice if we could bring existing/future services like [elm-search](https://klaftertief.github.io/elm-search/) on board. 
* Keyboard shortcuts are not that popular but power users might appreciate them.

# What's next
In this part we have seen what other citizens of the web development sphere
are doing to help their users discover APIs and packages. We'll take a brush 
of inspiration and try to build upon that basis in [part three](03-harmonic-convergence.md) 
where the ideas we've gathered will hopefully converge into something beautiful.