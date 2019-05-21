(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r @"..\..\bin\AppliedFSharp\netstandard2.0\AppliedFSharp.dll"
#r @"..\..\packages\System.Buffers\lib\netstandard2.0\System.Buffers.dll"
#r @"..\..\packages\Docker.DotNet\lib\netstandard2.0\Docker.DotNet.dll"
#r @"..\..\packages\FSharpAux\lib\netstandard2.0\FSharpAux.dll"
#r @"..\..\packages\FSharpAux.IO\lib\netstandard2.0\FSharpAux.IO.dll"
#r @"..\..\packages\BioFSharp\lib\netstandard2.0\BioFSharp.dll"
#r @"..\..\packages\BioFSharp.IO\lib\netstandard2.0\BioFSharp.IO.dll"
#r @"..\..\packages\BioFSharp.BioTools\lib\netstandard2.0\BioFSharp.BioTools.dll"
#r @"..\..\packages\Deedle\lib\netstandard2.0\Deedle.dll"
#r @"..\..\packages\SharpZipLib\lib\netstandard2.0\ICSharpCode.SharpZipLib.dll"
#r @"..\..\packages\BioFSharp\lib\netstandard2.0\BioFSharp.dll"
#r @"..\..\packages\FSharp.Stats\lib\netstandard2.0\FSharp.Stats.dll"
#r "netstandard"

#load @"..\..\packages\Deedle\Deedle.fsx"

open System.IO
open System

let dependencies = 
    [
        @"..\..\..\packages\Docker.DotNet\lib\netstandard2.0\Docker.DotNet.dll"
        @"..\..\..\packages\FSharpAux.IO\lib\netstandard2.0\FSharpAux.IO.dll"
        @"..\..\..\packages\SharpZipLib\lib\netstandard2.0\ICSharpCode.SharpZipLib.dll"
    ]

let resolveDockerDotnetDependecies () =
    Environment.SetEnvironmentVariable("Path",
        Environment.GetEnvironmentVariable("Path") + ";" + __SOURCE_DIRECTORY__ )
    dependencies 
    |> Seq.iter (fun dep -> 
        let path = Path.Combine(__SOURCE_DIRECTORY__,dep)
        Environment.SetEnvironmentVariable("Path",
            Environment.GetEnvironmentVariable("Path") + ";" + path)
        )    

resolveDockerDotnetDependecies()


(**
Pipeline walkthrough
====================
The pipeline used in the `Pipeline.DefaultConfig.runDefaultPipeline` function resembles the steps i used while scripting this tool.
The original script can be found [here](https://github.com/kMutagene/AppliedFSharp/blob/master/src/AppliedFSharp/Scripts/Script.fsx) (its a bit messy dont me). The besic steps are:

  1. Creation of all possible primer pairs of length n flanking a template of length m for the input cDNA/gene
  2. Creating a blast search database from the cDNA source/transcriptome/genome using the ][Blast BioContainer]()
  3. Blasting all primer pairs against the search database using the [Blast BioContainer]()
  4. Parsing the blast results in a deedle frame to handle grouping and filtering steps of the data
  5. Calculating self hybridization/internal Loop/fwd-rev primer hybridization energy using the [IntaRNA BioContainer]()

I added an example dataset in form of Chlamydomonas reinhardtii cDNA [here]().

to test the pipeline on this dataset you can use the following script:

*)
open AppliedFSharp
open BioFSharp
open BioFSharp.BioTools
open BioFSharp.IO



//Docker client pipe
let client = Docker.connect "npipe://./pipe/docker_engine"

//IntaRNA image name. can differ for you. path should be absolute so make sure you change this unless your name is Kevin ;)
let IntaRNAImage =  Docker.ImageName @"quay.io/biocontainers/intarna:2.4.1--pl526hfac12b2_0"

//Keep IntaRNA container up and running
let intaRNAContext = 
    BioContainer.initBcContextWithMountAsync client IntaRNAImage @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data"
    |> Async.RunSynchronously

//Blast image name. can differ for you.
let ImageBlast = Docker.DockerId.ImageId "blast"

//Keep Blast container up and running. path should be absolute so make sure you change this unless your name is Kevin ;)
let blastContext = 
    BioContainer.initBcContextWithMountAsync client ImageBlast @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data"
    |> Async.RunSynchronously


//Arbitrary cDNA from the cDNA pool
let testGene = 
    FastA.fromFile (BioArray.ofNucleotideString)(__SOURCE_DIRECTORY__ +  @"..\..\..\..\docsrc\content\data\Chlamydomonas_reinhardtii.Chlamydomonas_reinhardtii_v5.5.cdna.all.fa")
    |> Seq.item 1337

(***do-not-eval***)
let testResult = 
    Pipeline.DefaultConfig.runDefaultPipeline 
        // Primer generation parameters
        20 100 
        blastContext 
        // paths for saving outputs. again, as these are mounted into the containers, absolute paths should be used
        @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\Chlamydomonas_reinhardtii.Chlamydomonas_reinhardtii_v5.5.cdna.all.fa"
        @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\PipeLineQueryTest.fasta"
        @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\PipeLineTestBlastOutput.fasta"
        @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\PipeLineTestBlastOutputCleaned.fasta"
        intaRNAContext
        testGene


(** 
#Step by step


###Creation of all possible primer pairs of length n flanking a template of length m for the input cDNA/gene

the `generatePrimerPairs` function creates these primer pairs by moving over a sliding window of size 2*n+m and taking the flanking regions of length n

*)
(*** do-not-eval ***)
let testPairs = Pipeline.generatePrimerPairs 10 100 testGene

(** 
###Creating a blast search database from the cDNA source/transcriptome/genome using the Blast BioContainer

The `preparePrimerBlastSearch` prepares a blast database for subsequent blast searches. For best feature calculation, use the full cDNA transcriptome/Genome of the organism
*)
(*** do-not-eval ***)
let _ = 
    Pipeline.preparePrimerBlastSearch 
        blastContext 
        "C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\Chlamydomonas_reinhardtii.Chlamydomonas_reinhardtii_v5.5.cdna.all.fa"
        @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\PipeLineQueryTest.fasta"
        testPairs

(**
###Blasting all primer pairs against the search database using the Blast BioContainer

the `blastPrimerPairs` blasts all generated primer pairs against the previously generated database. results are written to a file of choice.
*)
(*** do-not-eval ***)
let _ = 
    Pipeline.blastPrimerPairs
        blastContext
        @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\Chlamydomonas_reinhardtii.Chlamydomonas_reinhardtii_v5.5.cdna.all.fa"
        @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\PipeLineQueryTest.fasta"
        @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\PipeLineTestBlastOutput.fasta"
        @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\PipeLineTestBlastOutputCleaned.fasta"

(**
###Parsing the blast results in a deedle frame to handle grouping and filtering steps of the data

###Calculating self hybridization/internal Loop/fwd-rev primer hybridization energy using the IntaRNA BioContainer

this is both handled by the `getResultFrame` function, which parses the blast Results, Calculates hybridization energy features for the given blast results and groups them by query id and direction(fwd/rev)

The result of this function is a frame that contains the features for all primer pairs.
*)
(*** do-not-eval ***)
let result2 =
    Pipeline.getResultFrame 
        intaRNAContext 
        @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\PipeLineTestBlastOutputCleaned.fasta"
        //this is unique for my case, you may want to add another converting function here. This converter gets the query id from the fasta files.
        (fun (x:string) -> x.Split(' ').[0].Trim())
        testPairs


(**
#Short Conclusion

While this may be not the flashiest algorithm, i think my post highlights the strengths of F# in data science pretty well. In a little more than 3 days i was able to
predict oligonucleotide interactions, blast sequences against genomes and group the results in a safe and visually acessible way during th exploratory data analysis. 

Furthermore, the script was easily transferable to .fs files and therefore compiled as library in no time. I think F# has great applications in research and me and my group 
aswell will continue to use it for all kinds of (bioinformatic) workflows

*)

