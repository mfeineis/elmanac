# Elmanach - formerly known as "Elm Advanced Package Search"
This is the home of a hypothetical new advanced search interface for [Elm](http://www.elm-lang.org) [Packages](http://package.elm-lang.org/).

## Mission Statement
The [original mission statement](https://github.com/elm-lang/projects#package-search) is reproduced here verbatim:

> The search feature of package.elm-lang.org is quite rudimentary. Community members have already created “type search” like this which is really cool, but I think we would benefit from a more traditional search feature as well.
>
> Here are some ideas:
>
> 1) Explore full-text search. Does searching through everything in the docs give better results than just searching the name and summary?
> 2) Include secondary information in rankings. How complete are the docs? How many examples? How many infix operators? This would allow transparent mechanisms for incentivizing better package design and documentation.
> 3) Many searches indicate a conceptual mismatch between JS and Elm. When someone searches for “components” the best outcome is that they find some documentation, not a package that claims that this is a thing. So it might be good to detect certain search terms and give structured information above the search results. For example, a link to the relevant parts of guide.elm-lang.org or some other relevant documentation.
>
> Ideally this service can live on its own server. It would take in JSON and give out JSON. So if the search server goes down, it does not take down the package website. This makes it a great project because it minimizes technical coordination in the early phases.
>
> I think it makes the most sense to focus on 1 and 2, and to not get hung up on the particular details of rankings. For any official use those details will need to be carefully tweaked, so perhaps the best system is one that makes it easy to analyze docs in various ways.

### Goals
We therefore conclude that our new solution (in no particular order) ...

* Should complement the (search-) facilities that are currently available
* Should be tailored to the target audience
* Should take advantage of the unique strengths that Elm packages provide
* Should incentivize better package design and documentation
* Should require minimal effort for package authors
* Should be able to run as independently as possible from existing infrastructure
* Should provide an extensibility model for actual rankings

### Non-Goals
* Should *not* try to replicate Your Personal Search Engine Favorite

## Basic Design Analysis
Before we commit to anything concrete we need to do some research first.
The corresponding documents live in the [design-docs](design-docs/) directory.

1) [How do people in the real world look for Elm packages and features](design-docs/01-status-quo.md)?
    * [x] Who is our target audience?
    * [x] Does our target audience find what it is looking for (easily)?
    * [x] What are the challenges we're facing?

2) [What is the rest of the web doing regarding interactivity in their docs](design-docs/02-room-for-improvement.md)?
    * [x] Prior Art - how does the rest of the web solve these problems?
    * [x] What are the challenges of the solutions employed?
    * [x] What can we learn from all this?

3) [Can we actually do better than the rest of the web](design-docs/03-harmonic-convergence.md)?
    * [x] Initial Mockups
    * [ ] Can the challenges discussed in part two be addressed by:
        + [ ] Full-text search in docs or even code?
        + [ ] Clever ranking based on secondary data?
        + [ ] Supplying the user with conceptual help, e.g. `component`, `TEA`?
        + [ ] ...
    * [ ] Is it actually worth it?

## Roadmap
There is no static roadmap right now.

## Changelog
* 2017-07-19 Basic Elm frontend and Haskell backend stubs
* 2017-07-10 Initial mockups for Harmonic Convergence document
* 2017-07-04 First draft of the Room for Improvement document
* 2017-07-03 First draft of the Status Quo design document
* 2017-07-02 Initial research started
