#r "netstandard"
#r @"..\..\..\packages\Deedle\lib\netstandard2.0\Deedle.dll"
#r @"..\..\..\packages\SharpZipLib\lib\netstandard2.0\ICSharpCode.SharpZipLib.dll"
#r @"..\..\..\packages\System.Buffers\lib\netstandard2.0\System.Buffers.dll"
#r @"..\..\..\packages\Docker.DotNet\lib\netstandard2.0\Docker.DotNet.dll"
#r @"..\..\..\packages\FSharpAux\lib\netstandard2.0\FSharpAux.dll"
#r @"..\..\..\packages\FSharpAux.IO\lib\netstandard2.0\FSharpAux.IO.dll"
#r @"..\..\..\packages\BioFSharp\lib\netstandard2.0\BioFSharp.dll"
#r @"..\..\..\packages\BioFSharp.IO\lib\netstandard2.0\BioFSharp.IO.dll"
#r @"..\..\..\packages\BioFSharp.BioTools\lib\netstandard2.0\BioFSharp.BioTools.dll"
#r @"..\..\..\packages\FSharp.Stats\lib\netstandard2.0\FSharp.Stats.dll"
#r @"..\..\..\packages\FSharp.Plotly\lib\netstandard2.0\FSharp.Plotly.dll"

#load @"..\..\..\packages\Deedle\Deedle.fsx"

#load "..\ContainerAPIs.fs"
#load "..\DeedleOperations.fs"
#load "..\Pipeline.fs"

open AppliedFSharp.Pipeline
open AppliedFSharp
open Docker.DotNet
open BioFSharp
open BioFSharp.BioTools
open BioFSharp.IO
open AppliedFSharp.ContainerAPIs


let IntaRNAImage =  Docker.ImageName @"quay.io/biocontainers/intarna:2.4.1--pl526hfac12b2_0"

let client = Docker.connect "npipe://./pipe/docker_engine"

Docker.Image.exists client IntaRNAImage

let intaRNAContext = 
    BioContainer.initBcContextWithMountAsync client IntaRNAImage @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data"
    |> Async.RunSynchronously

let ImageBlast = Docker.DockerId.ImageId "blast"

let blastContext = 
    BioContainer.initBcContextWithMountAsync client ImageBlast @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data"
    |> Async.RunSynchronously

let testGene = 
    FastA.fromFile (BioArray.ofNucleotideString)(__SOURCE_DIRECTORY__ +  @"..\..\..\..\docsrc\content\data\Chlamydomonas_reinhardtii.Chlamydomonas_reinhardtii_v5.5.cdna.all.fa")
    |> Seq.item 1337



let testResult = 
    Pipeline.DefaultConfig.runDefaultPipeline 
        20 100 
        blastContext 
        @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\Chlamydomonas_reinhardtii.Chlamydomonas_reinhardtii_v5.5.cdna.all.fa"
        @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\PipeLineQueryTest.fasta"
        @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\PipeLineTestBlastOutput.fasta"
        @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\PipeLineTestBlastOutputCleaned.fasta"
        intaRNAContext
        testGene
