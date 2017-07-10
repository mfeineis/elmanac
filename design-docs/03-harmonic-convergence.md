# Harmonic Convergence
This document is part three of [this repo's design documentation](../README.md)
that aims to determine what challenges an advanced elm package search might
have to tackle. 

In [part two](02-room-for-improvement.md) we've seen what the rest of the web
is doing to provide users with the information they're looking for.

This part discusses how the information we gathered in the last parts could
converge into a concrete idea of a service to be implemented.

## Mockups
To get an idea of how the advanced search could look and feel we present 
some basic image mockups.

The initial view would suggest some basic information and features the settings
sub-view that configures the component. It'd make sense to give the user the
option to save her settings - possibly in `localstorage`.

![Elm Advanced Package Search Initial View](img/03-harmonic-convergence/img-0200-mockup-initial-view.png?raw=true)

While the user proceeds to type, the suggestions keep on adjusting using some
clever ranking system that is yet to be defined.

![Elm Advanced Package Search component search](img/03-harmonic-convergence/img-0201-mockup-component-search.png?raw=true)

## Prioritization

TODO