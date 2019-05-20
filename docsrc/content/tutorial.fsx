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
#r @"..\..\packages\FSharp.Plotly\lib\netstandard2.0\FSharp.Plotly.dll"
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
Introducing your project
========================

Say more

*)

open BioFSharp.BioTools

let IntaRNAImage =  Docker.ImageName @"quay.io/biocontainers/intarna:2.4.1--pl526hfac12b2_0"

let client = Docker.connect "npipe://./pipe/docker_engine"

Docker.Image.exists client IntaRNAImage

let intaRNAContext = 
    BioContainer.initBcContextWithMountAsync client IntaRNAImage @"C:\Users\Kevin\source\repos\CsbScaffold\Ramping(Challenge)\results"
    |> Async.RunSynchronously

(**
Some more info
*)
