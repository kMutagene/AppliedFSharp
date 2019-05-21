# Primer design for qPCR with F# and bioContainers - my submission for the applied F# challenge

[_Kevin Schneider_]()

_This submission focuses on the power of F# regarding quick establishment of bioinformatic workflows.
In fact, the code in this repository was written in about 3 days. We use F# in our (computer)lab @CSBiology for all 
kinds of data analysis (scripting) and software development._


This Library is the result of a refactored script I developed for qPCR primer design. It uses 4 parameters to access if the primers are usable:

  - interaction free energy of the fwd and rev primer with each other
  - interaction free energy of the primer with itself
  - interaction free energy of intramolecular loop formation
  - the "badness" of the best blast result of the primer against the cDNA library that is not a match with the sequence itself

I leveraged [BioFSharp.BioTools]() to make two bioinformatic tools usable in F# from docker container to calculate these features:

  - [IntaRNA]() - a tool for predicting interactions between nuctleotides (this includes various hybridization energy calculations)
  - [BlastN]() - The classic Basic Local Alignment Search Tool

For exploratory data analysis, i used [Deedle]().

The output is a frame containing the features for fwd and rev primer sets:

![ExampleResult]((https://raw.githubusercontent.com/kMutagene/AppliedFSharp/master/src/docsrc/files/img/ExampleOutput..png)

## Prerequisites

  - [F# installation]()
  - [.Net core SDK]()
  - [fake cli]()
  - [Docker for windows]()
  - [IntaRNA BioContainer]()
  - [Blast BioContainer]()


