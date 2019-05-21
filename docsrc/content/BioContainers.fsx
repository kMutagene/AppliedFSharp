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
Bioinformatic tools in docker container using BioFSharp.BioTools 
================================================================

BioFSharp.BioTools is an experimental f# wrapper for Docker.DotNet that I and members of my group are working on. In a nutshell, it starts a docker image and keeps it running, and therefore accessible for I/O.
All that is needed is the name of the image. this is acessible for example in the docker cli:

![Docker Images](img/DockerImages.png)

*)

open BioFSharp.BioTools

(***do-not-eval***)
//create a DockerId with the target image name, in this case the IntaRNA container
let IntaRNAImage =  Docker.ImageName @"quay.io/biocontainers/intarna:2.4.1--pl526hfac12b2_0"

(**this is the standard pipe used for the docker engine*)
(***do-not-eval***)
let client = Docker.connect "npipe://./pipe/docker_engine"

(**initializing ta BcContext keeps the target container running and accessible for I/O. Aditionally, a folder from your harddrive can be mounted into the container*)
(***do-not-eval***)
let intaRNAContext = 
    BioContainer.initBcContextWithMountAsync client IntaRNAImage @"C:\Users\Kevin\source\repos\CsbScaffold\Ramping(Challenge)\results"
    |> Async.RunSynchronously

(**
Modelling the arguments of the process in the container
=======================================================

I modelled the IntaRNA parameters the same way I did with other tools in BioFSharp.BioTools: nested union cases. 
Because i wanted to mount a folder into the container for I/O operations, those types need two functions that either create the
argument string directly or shape the mounted path in a way that it is interpretable by the unix based system in the container. These functions are called
`make`/`makeCmd` and `makeWith`/`makeCmdWith` , the second set of functions getting a `MountInfo` parameter to ensure path safety.

Here is an example of a set of parameters from the IntaRNA BioContainer API:
*)

open BioFSharp.BioTools.BioContainer

type QueryInputOptions =
    ///RNA sequence string
    |RNASequence of string
    ///stream/file name from where to read the query sequences
    |File of string
    //returns the argument string for the cases above
    static member make = function
        |RNASequence s  -> ["-q"; s]
        |File f         -> ["-q"; f]
    //returns the argument string for the cases above, ensuring BioContainer path conventions
    static member makeWith (m:MountInfo) = 
        let cPath p = (MountInfo.containerPathOf m p)
        function
        |RNASequence s  -> ["-q"; s]        
        |File f         -> ["-q"; cPath f]  

(** 
Running the Container from F# interactive
=========================================
The following function uses the `makeCmdWith` functions of the `IntaRNAParams` type to create a List of correctly formatted command line argument strings.

The `BioContainer.execReturnAsync` function passes these arguments to the running container and returns the stdOut of the container when it has finished the comptation.
*)

open AppliedFSharp.ContainerAPIs.IntaRNA

let runIntaRNAAsync (bcContext:BioContainer.BcContext) (opt:IntaRNAParams list) = 
    //create command line argument strings
    let cmds = opt |> List.map (IntaRNAParams.makeCmdWith bcContext.Mount)
    //prepend the command befor the arguments
    let tp = "IntaRNA"::(cmds |> List.concat)
    printfn "starting process IntaRNA\r\nparameters:"
    cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))
    //Send command to the container and await result
    async {
        let! res = BioContainer.execReturnAsync bcContext tp
        return res
    }


