# Status Quo
This document is part one of [this repo's design documentation](../README.md)
that aims to determine what challenges an advanced elm package search might
have to tackle. 

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

| Term                | Search Engine | Elm Package Search |
| ------------------- | ------------- | ------------------ |
| css                 | ![alt text](img/img-0001-google-search-css.png?raw=true) | ![alt text](img/img-0002-elmpackage-search-css.png?raw=true) |
| html                | ![alt text](img/img-0003-google-search-html.png?raw=true) | ![alt text](img/img-0004-elmpackage-search-html.png?raw=true) |

### B) Frontend devs
Why are they considering Elm?
* I want a better JavaScript
* What is all the hype about?

What answers will they seek?
* How do I do components in Elm?
* What sets Elm apart from the JavaScript frameworks I know?
* What is the syntax for this in Elm?
* How do I achieve JavaScript Interop with Elm?
* Why is Elm so different than JavaScript?
* How long will this trend survive?
* Where is feature X that I'm used to?
* Can I try Elm on a small part of my existing app?

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

### D) Backend devs with background in static languages
Why are they considering Elm?
* I want a managable frontend replacement for my backend language of choice
* JavaScript is tedious

What answers will they seek?
* Where are my type classes/OCaml modules/protocols/interfaces?
* Why is Elm so different than my language?

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

## How useful is package.elm-lang.org right now?


## What 