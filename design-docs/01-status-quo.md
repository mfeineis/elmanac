# Status Quo
This document is part one of [this repo's design documentation](../README.md)
that aims to determine what challenges an advanced elm package search might
have to tackle. [Part two is here](02-room-for-improvement.md).

Many search engine results involve links to specific Elm packages so that not
everybody who visits [package.elm-lang.org](package.elm-lang.org) will be 
aware that the search box is only for the packages. That's probably why the
package page has a sidebar with common links.

![Elm Package Sidebar](img/01-status-quo/snippet-0001-elmpackage-sidebar.png?raw=true)

## Elm's potential audience and what they get from package.elm-lang.org
Elm is a relatively young language that has attracted interest from
many development communities. Here is a feeble attempt at a broad categorization
of Elm's potential audience.

|  Category  | Background                       | Developer? | 
| ---------- | -------------------------------- | ---------- | 
| A          | Newcomers to web development     | yes/no     | 
| B          | Frontend Devs (JS/Compile-to-JS) | yes        |
| C          | Backend Devs (dynamic languages) | yes        |
| D          | Backend Devs (static languages)  | yes        | 
| E          | Manager/CTO                      | no/maybe   | 

We'll now have a look at these and how [package.elm-lang.org](package.elm-lang.org) 
is currently delivering value to them. This is complemented by what a similar
search would look like in a popular search engine.

### A) Newcomers to web development
Why are they considering Elm?
* Get into web development in the first place
* What is all the hype about?

What answers will they seek?
* I want to learn the basics of web development
* I want to learn the basics of Elm
* Why should I learn Elm instead of React/Ember/Angular?

Typical results may involve:

| Term                | Search Engine | Elm Package Search |
| ------------------- | ------------- | ------------------ |
| elm css             | ![Google elm css](img/01-status-quo/img-0001-google-search-css.png?raw=true) | ![Elm Package css](img/01-status-quo/img-0002-elmpackage-search-css.png?raw=true) |
| elm html            | ![Google elm html](img/01-status-quo/img-0003-google-search-html.png?raw=true) | ![Elm Package html](img/01-status-quo/img-0004-elmpackage-search-html.png?raw=true) |
| elm vs              | ![Google elm vs](img/01-status-quo/img-0005-google-search-elm-vs.png?raw=true) | ![Elm Package elm vs](img/01-status-quo/img-0006-elmpackage-search-elm-vs.png?raw=true) |
| why choose elm      | ![Google why choose elm](img/01-status-quo/img-0007-google-search-why-choose-elm.png?raw=true) | ![Elm Package why choose elm](img/01-status-quo/img-0008-elmpackage-search-why-choose-elm.png?raw=true) |

### B) Frontend devs
Why are they considering Elm?
* I want a better JavaScript
* What is all the hype about?

What answers will they seek?
* How do I do components in Elm?
* What sets Elm apart from the JavaScript frameworks I know?
* What is the syntax for this in Elm?
* How do I do this in Elm?
* How do I achieve JavaScript Interop with Elm?
* Why is Elm so different than JavaScript?
* How long will this trend survive?
* Where is feature X that I'm used to?
* Can I try Elm on a small part of my existing app?

Typical results may involve:

| Term                | Search Engine | Elm Package Search |
| ------------------- | ------------- | ------------------ |
| elm div             | ![Google elm div](img/01-status-quo/img-0033-google-search-elm-div.png?raw=true) | ![Elm Package div](img/01-status-quo/img-0034-elmpackage-search-div.png?raw=true) |
| elm component       | ![Google elm component](img/01-status-quo/img-0009-google-search-elm-component.png?raw=true) | ![Elm Package component](img/01-status-quo/img-0010-elmpackage-search-component.png?raw=true) |
| elm cheat sheet     | ![Google elm cheat sheet](img/01-status-quo/img-0011-google-search-elm-cheat-sheet.png?raw=true) | ![Elm Package cheat sheet](img/01-status-quo/img-0012-elmpackage-search-cheat-sheet.png?raw=true) |
| elm flexbox         | ![Google elm flexbox](img/01-status-quo/img-0015-google-search-elm-flexbox.png?raw=true) | ![Elm Package flexbox](img/01-status-quo/img-0016-elmpackage-search-flexbox.png?raw=true) |
| elm settimeout      | ![Google elm settimeout](img/01-status-quo/img-0017-google-search-elm-settimeout.png?raw=true) | ![Elm Package settimeout](img/01-status-quo/img-0018-elmpackage-search-settimeout.png?raw=true) |
| elm random          | ![Google elm random](img/01-status-quo/img-0019-google-search-elm-random.png?raw=true) | ![Elm Package random](img/01-status-quo/img-0020-elmpackage-search-random.png?raw=true) |
| elm date            | ![Google elm date](img/01-status-quo/img-0021-google-search-elm-date.png?raw=true) | ![Elm Package date](img/01-status-quo/img-0022-elmpackage-search-date.png?raw=true) |
| elm immutable       | ![Google elm immutable](img/01-status-quo/img-0027-google-search-elm-immutable.png?raw=true) | ![Elm Package immutable](img/01-status-quo/img-0028-elmpackage-search-immutable.png?raw=true) |

### C) Backend devs with background in dynamic languages 
Why are they considering Elm?
* I want a managable frontend replacement for my backend language of choice
* I need dynamic features but JavaScript is quirky in other ways than *my* dynamic language
* What is all the hype about?

What answers will they seek?
* What is the syntax for this in Elm?
* Why is Elm so different than my dynamic language?
* Why all the type hassle in Elm?
* What is all the hype about?

Typical results may involve:

| Term                | Search Engine | Elm Package Search |
| ------------------- | ------------- | ------------------ |
| elm partial         | ![Google elm partial](img/01-status-quo/img-0023-google-search-elm-partial.png?raw=true) | ![Elm Package partial](img/01-status-quo/img-0024-elmpackage-search-partial.png?raw=true) |
| elm route           | ![Google elm route](img/01-status-quo/img-0025-google-search-elm-route.png?raw=true) | ![Elm Package route](img/01-status-quo/img-0026-elmpackage-search-route.png?raw=true) |

### D) Backend devs with background in static languages
Why are they considering Elm?
* I want a managable frontend replacement for my backend language of choice
* JavaScript is tedious

What answers will they seek?
* Where are my type classes/OCaml modules/protocols/interfaces?
* Why is Elm so different than my language?

Typical results may involve:

| Term                     | Search Engine | Elm Package Search |
| ------------------------ | ------------- | ------------------ |
| elm http                 | ![Google elm http](img/01-status-quo/img-0029-google-search-elm-http.png?raw=true) | ![Elm Package http](img/01-status-quo/img-0030-elmpackage-search-http.png?raw=true) |
| elm dependency injection | ![Google elm dependency injection](img/01-status-quo/img-0031-google-search-elm-dependency-injection.png?raw=true) | ![Elm Package dependency injection](img/01-status-quo/img-0032-elmpackage-search-dependency-injection.png?raw=true) |

### E) Manager/CTO
Why are they considering Elm?
* I heard Elm has feature X?
* What is all the hype about?
* Employee Y wants to use Elm in my company

What answers will they seek?
* Does Elm really have feature X and how is it achieved?
* Can I use Elm in my team without a full rewrite?
* How stable is Elm and what is the SLA?
* Who else is using this?
* When should I not be using Elm right now?

Typical results may involve:

| Term                     | Search Engine | Elm Package Search |
| ------------------------ | ------------- | ------------------ |
| elm roadmap              | ![Google elm roadmap](img/01-status-quo/img-0013-google-search-elm-roadmap.png?raw=true) | ![Elm Package roadmap](img/01-status-quo/img-0014-elmpackage-search-roadmap.png?raw=true) |

## So, how useful is package.elm-lang.org right now?
As seen above there are many cases, especially for people new to the
language, where the current rudimentary search is less than optimal.
Search is so ubiquitous in the web world that seeing no results 
or suggestions will put off potentially interested people pretty
quickly. 

### Elm beginners
Beginning Elm adventurers might have a hard time finding things they
are looking for. Often the results shown don't reflect which packages
would be best, if you're just looking for a concept or how to do something
in Elm that you know from the JavaScript world.

Links to sources where one can find these conceptual articles are always 
there, in the form of the sidebar on [package.elm-lang.org](package.elm-lang.org), 
newcomers might not make that connection immediately without the proper nudge, 
though.

It would be nice if we could offer an experience where the search is also
aware of the available external sources and could provide appropriate 
recommendations and deep links. So it seems like the proposed conceptual 
search feature will be a big step with helping to onboard new users.

### Somewhat experienced Elm users
As long as you know what you are looking for the search in the package 
name, title and summary is pretty spot on. This means that if you're 
a somewhat experienced Elm user you probably won't *need* much more than 
is already there. 

However, that doesn't mean that there is no room for improvement. The 
potential benefit of the proposals for including full-text search and 
fine grained rankings will be explored in one of the next parts.

## What's next
 
[Part two](02-room-for-improvement.md) will have a look at how other
citizens of the web development environment are coping with these 
issues.
