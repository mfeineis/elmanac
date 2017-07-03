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

# Honorable Mentions

## Dojo
The [Dojo Toolkit](https://dojotoolkit.org/documentation/) has a lot of 
documentation but it provides no dynamic search functionality.

## Personas in the Aurelia docs
The Aurelia docs are carried by the concept of "personas" - categories of users 
to tailor the experience to. This takes into account what the user might be 
interested in the most. In this way information overload for non-technical people 
is avoided and developers aren't treated to guides that are typically only 
relevant for management. The approach taken here is worth being explored as a 
valuable basis for the user experience of our new advanced package search.

![Aurelia personas](img/02-room-for-improvement/img-0100-aurelia-personas.png?raw=true)

Apart from that the search feature doesn't prove very useful if you don't know
what you are looking for.

## Haskell has Hoogle
Hoogle is a command line tool being used to search for Haskell APIs although one
could argue that it is not a terrible user friendly one. Here we only want to
include services that a newcomer can use without installing a platform and 
spinning up a terminal emulator.

# Case studies
NPM has taken the web development world by storm, so virtually no major anything 
is still maintaining their own package infrastructure. We now take a look at some 
examples of the documentation and package registries that are available for well 
known frameworks/libraries:

## Frameworks and their APIs
Most of the big frameworks have some kind of search for their core APIs and they 
all have quite extensive guides for their core concepts but they don't focus
very much on their community contributions. Users mainly rely on external 
package registries for their building blocks.

* [Angular](https://angular.io/docs)
* [Aurelia](https://aurelia.io/hub.html)
* [Ember](https://www.emberjs.com/learn/)
* [React](https://facebook.github.io/react/docs/)

## Package Registries/Search services
In addition to the frameworks' main docs we also take a look at package registries
in use in conjunction with them.

* [JS.COACH](https://js.coach)
* [NPM](https://www.npmjs.com/)
* [React Parts](https://react.parts/web)

## Component Docs
Almost all modern web frameworks have some notion of components so this is what
people expect to find in Elm too. Let's examine how our study subjects present
themselves regarding components.

### Angular Components
As we see here Angular
![Angular component](img/02-room-for-improvement/img-0101-angular-component.png?raw=true)

# What did we learn?
1) A search tailored to the user could be a very powerful feature for Elm.
2) A specialized search for both APIs, concepts and packages under one roof would be a unique feature.