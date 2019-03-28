# GraphML
Graph utility written in Standard ML

## Use Case

### I have provided an example folder to walk through a use case of the library.

**example/graphbuilder.sml** reads **example/basicgraph.txt** using the format described in **example/basicgraph.format**. Running CM.make frmo the top level will compile this builder. 

First compile and bind to the environment from the top level run:

```CM.make "sources.cm";```

Then you can run the example:

```use "example/example.sml";```

## Extension

All graph composition changes should go in graph.sig and graph.sml. All additional graph algorithms can be placed in eithe graphalg.{sml, sig}, or you can of course define your own graph alg helpers.

## Bug Fixes

If any bugs are encountered, reach out to me and I will fix as soon as I can.


## TODO

1. Add unit testing
2. Implement additional algorithms in graphalg
