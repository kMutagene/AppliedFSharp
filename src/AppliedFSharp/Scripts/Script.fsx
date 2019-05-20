
#r @"..\..\..\packages\System.Buffers\lib\netstandard2.0\System.Buffers.dll"
#r @"..\..\..\packages\Docker.DotNet\lib\netstandard2.0\Docker.DotNet.dll"
#r @"..\..\..\packages\FSharpAux\lib\netstandard2.0\FSharpAux.dll"
#r @"..\..\..\packages\FSharpAux.IO\lib\netstandard2.0\FSharpAux.IO.dll"
#r @"..\..\..\packages\BioFSharp\lib\netstandard2.0\BioFSharp.dll"
#r @"..\..\..\packages\BioFSharp.IO\lib\netstandard2.0\BioFSharp.IO.dll"
#r @"..\..\..\packages\BioFSharp.BioTools\lib\netstandard2.0\BioFSharp.BioTools.dll"
#r @"..\..\..\packages\Deedle\lib\netstandard2.0\Deedle.dll"
#r @"..\..\..\packages\SharpZipLib\lib\netstandard2.0\ICSharpCode.SharpZipLib.dll"
#r @"..\..\..\packages\BioFSharp\lib\netstandard2.0\BioFSharp.dll"
#r @"..\..\..\packages\FSharp.Stats\lib\netstandard2.0\FSharp.Stats.dll"
#r @"..\..\..\packages\FSharp.Plotly\lib\netstandard2.0\FSharp.Plotly.dll"
#r "netstandard"

#load @"..\..\..\packages\Deedle\Deedle.fsx"

open System.IO
open System

let dependencies = 
    [
        @"../../..\packages\Docker.DotNet\lib\netstandard2.0\Docker.DotNet.dll"
        @"..\..\..\packages\FSharpAux.IO\lib\netstandard2.0\FSharpAux.IO.dll"
        @"../../..\packages\SharpZipLib\lib\netstandard2.0\ICSharpCode.SharpZipLib.dll"
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

open BioFSharp
open BioFSharp.IO
open Docker.DotNet
open Docker.DotNet
open ICSharpCode.SharpZipLib
open BioFSharp.BioTools
open BioFSharp.BioTools.BioContainer
open FSharpAux


open Blast
open Deedle

///Query

///  -q [ --query ] arg           either an RNA sequence or the stream/file name
///                               from where to read the query sequences (should
///                               be the shorter sequences to increase
///                               efficiency); use 'STDIN' to read from standard
///                               input stream; sequences have to use IUPAC
///                               nucleotide encoding
type QueryInputOptions =
    |RNASequence of string
    |File of string

    static member make = function
        |RNASequence s  -> ["-q"; s]
        |File f         -> ["-q"; f]

    static member makeWith (m:MountInfo) = 
        let cPath p = (MountInfo.containerPathOf m p)
        function
        |RNASequence s  -> ["-q"; s]        
        |File f         -> ["-q"; cPath f]  

///  --qAcc arg (=C)              accessibility computation :
///                                'N' no accessibility contributions
///                                'C' computation of accessibilities
///                                'P' unpaired probabilities in RNAplfold format
///                               from --qAccFile
///                                'E' ED values in RNAplfold Pu-like format from
///                               --qAccFile

type QueryAcessibilityComputationTypeOptions =
    |NoContributions
    |Compute
    |UnpairedFromFile
    |EDValuesFromFile

    static member make = function
        |NoContributions    -> ["--qAcc=N"]
        |Compute            -> ["--qAcc=C"]
        |UnpairedFromFile   -> ["--qAcc=P"]
        |EDValuesFromFile   -> ["--qAcc=E"]


///  --qAccW arg (=150)           accessibility computation : sliding window size
///                               for query accessibility computation (arg in
///                               range [0,99999]; 0 will use to the full sequence
///                               length). Note, this also restricts the maximal
///                               interaction length (see --qIntLenMax).
///  --qAccL arg (=100)           accessibility computation : maximal loop length
///                               (base pair span) for query accessibility
///                               computation (arg in range [0,99999]; 0 will use
///                               to sliding window size 'qAccW')

type QueryAcessibilityComputationOptions =
    |QueryAcessibilityComputationType of QueryAcessibilityComputationTypeOptions
    |SlidingWindowSize of int
    |MaximalLoopLength of int

    static member make = function
        |QueryAcessibilityComputationType t ->  QueryAcessibilityComputationTypeOptions.make t
        |SlidingWindowSize i                ->  [sprintf "--qAccW=%i" i]
        |MaximalLoopLength i                ->  [sprintf "--qAccL=%i" i]
    


type QueryOptions = 
    |QueryInput of QueryInputOptions
    |QueryAcessibilityComputation of QueryAcessibilityComputationOptions list

    static member make = function
        |QueryInput qi                      -> QueryInputOptions.make qi
        |QueryAcessibilityComputation cList -> cList |> List.map QueryAcessibilityComputationOptions.make |> List.concat

    static member makeWith (m:MountInfo) = 
        let cPath p = (MountInfo.containerPathOf m p)
        function        
        |QueryInput qi                      -> (QueryInputOptions.makeWith m) qi
        |QueryAcessibilityComputation cList -> cList |> List.map QueryAcessibilityComputationOptions.make |> List.concat



//Target:
//  -t [ --target ] arg          either an RNA sequence or the stream/file name
//                               from where to read the target sequences (should
//                               be the longer sequences to increase efficiency);
//                               use 'STDIN' to read from standard input stream;
//                               sequences have to use IUPAC nucleotide encoding

type TargetInputOptions =
    |RNASequence of string
    |File of string

    static member make = function
        |RNASequence s  -> ["-t"; s] 
        |File f         -> ["-t"; f] 

    static member makeWith (m:MountInfo) = 
        let cPath p = (MountInfo.containerPathOf m p)
        function
        |RNASequence s  -> ["-t"; s]        
        |File f         -> ["-t"; cPath f]  

//  --tAcc arg (=C)              accessibility computation :
//                                'N' no accessibility contributions
//                                'C' computation of accessibilities
//                                'P' unpaired probabilities in RNAplfold format
//                               from --tAccFile
//                                'E' ED values in RNAplfold Pu-like format from
//                               --tAccFile

type TargetAcessibilityComputationTypeOptions =
    |NoContributions
    |Compute
    |UnpairedFromFile
    |EDValuesFromFile

    static member make = function
        |NoContributions    -> ["--tAcc=N"]
        |Compute            -> ["--tAcc=C"]
        |UnpairedFromFile   -> ["--tAcc=P"]
        |EDValuesFromFile   -> ["--tAcc=E"]

//  --tAccW arg (=150)           accessibility computation : sliding window size
//                               for query accessibility computation (arg in
//                               range [0,99999]; 0 will use the full sequence
//                               length) Note, this also restricts the maximal
//                               interaction length (see --tIntLenMax).

//  --tAccL arg (=100)           accessibility computation : maximal loop size
//                               (base pair span) for query accessibility
//                               computation (arg in range [0,99999]; 0 will use
//                               the sliding window size 'tAccW')

type TargetAcessibilityComputationOptions =
    |TargetAcessibilityComputationType of TargetAcessibilityComputationTypeOptions
    |SlidingWindowSize of int
    |MaximalLoopLength of int

    static member make = function
        |TargetAcessibilityComputationType t ->  TargetAcessibilityComputationTypeOptions.make t
        |SlidingWindowSize i                ->  [sprintf "--tAccW=%i" i]
        |MaximalLoopLength i                ->  [sprintf "--tAccL=%i" i]



type TargetOptions = 
    |TargetInput of TargetInputOptions
    |TargetAcessibilityComputation of TargetAcessibilityComputationOptions list

    static member make = function
        |TargetInput ti                      -> TargetInputOptions.make ti
        |TargetAcessibilityComputation cList -> cList |> List.map TargetAcessibilityComputationOptions.make |> List.concat

    static member makeWith (m:MountInfo) = 
        let cPath p = (MountInfo.containerPathOf m p)
        function        
        |TargetInput ti                      -> (TargetInputOptions.makeWith m) ti
        |TargetAcessibilityComputation cList -> cList |> List.map TargetAcessibilityComputationOptions.make |> List.concat 

//Helix (only if --model=H):
//  --helixMinBP arg (=2)        minimal number of base pairs inside a helix (arg
//                               in range [2,4])
//  --helixMaxBP arg (=10)       maximal number of base pairs inside a helix (arg
//                               in range [2,20])
//  --helixMaxIL arg (=0)        maximal size for each internal loop size in a
//                               helix (arg in range [0,2]).
//  --helixMaxED arg (=999)      maximal ED-value allowed (per sequence) during
//                               helix computation (arg in range [-999,999]).
//  --helixMaxE arg (=0)         maximal energy considered during helix
//                               computation (arg in range [-999,999]).
//  --helixWithED                if present, ED-values will be used within the
//                               energy evaluation of a helix

type HelixOptions =
    |Default
    |MinBP              of int
    |MaxBP              of int
    |MaxInternalLoop    of float
    |MaxEDValue         of float
    |MaxEnergy          of float
    |WithED             

    static member make = function
        |Default            ->[""]
        |MinBP            i ->[sprintf "--helixMinBP=%i" i]
        |MaxBP            i ->[sprintf "--helixMaxBP=%i" i]
        |MaxInternalLoop  f ->[sprintf "--helixMaxIL=%f" f]
        |MaxEDValue       f ->[sprintf "--helixMaxED=%f" f]
        |MaxEnergy        f ->[sprintf "--helixMaxE=%f"  f]
        |WithED             ->["--helixWithED"]

//Interaction:
//  -m [ --mode ] arg (=H)       prediction mode :
//                                'H' = heuristic (fast and low memory),
//                                'M' = exact and low memory,
//                                'E' = exact (high memory)

type PredictionModeOptions =
    |Heuristic of HelixOptions list
    |ExactLowMemory
    |Exact

    static member make = function
        |Heuristic hList->  ("--mode=H":: (hList |> List.map HelixOptions.make |> List.concat))
        |ExactLowMemory ->  ["--mode=M"]
        |Exact          ->  ["--mode=E"]

//Seed:
//  --noSeed                     if present, no seed is enforced within the
//                               predicted interactions
//  --seedTQ arg                 comma separated list of explicit seed base pair
//                               encoding(s) in the format startTbpsT&startQbpsQ,
//                               e.g. '3|||.|&7||.||', where startT/Q are the
//                               indices of the 5' seed ends in target/query
//                               sequence and 'bps' the dot-bar base pair
//                               encodings. This disables all other seed
//                               constraints and seed identification.
//  --seedBP arg (=7)            number of inter-molecular base pairs within the
//                               seed region (arg in range [2,20])
//  --seedMaxUP arg (=0)         maximal overall number (query+target) of
//                               unpaired bases within the seed region (arg in
//                               range [0,20])

type SeedOptions = 
    |NoSeed
    |SeedList of string
    |BPAmount of int
    |MaxUnpairedBases of int
    
    static member make = function
        |NoSeed             -> ["--noSeed"]
        |SeedList sL        -> [sprintf "--seedTQ=%s" sL    ]
        |BPAmount i         -> [sprintf "--seedBP=%i" i     ]
        |MaxUnpairedBases i -> [sprintf "--seedMaxUP=%i" i  ]

//  --outMode arg (=N)           output mode :
//                                'N' normal output (ASCII char + energy),
//                                'D' detailed output (ASCII char +
//                               energy/position details),
//                                'C' CSV output (see --outCsvCols),
//                                '1' backward compatible IntaRNA v1.* normal
//                               output,
//                                'O' backward compatible IntaRNA v1.* detailed
//                               output (former -o)

type OutputModeOptions = 
    |Normal
    |Detailed
    |CSV

    static member make = function
        |Normal     -> ["--outMode=N"]
        |Detailed   -> ["--outMode=D"]
        |CSV        -> ["--outMode=C"]


//Output:
//  --out arg (=STDOUT)          output (multi-arg) : provide a file name for
//                               output (will be overwritten) or 'STDOUT/STDERR'
//                               to write to the according stream (according to
//                               --outMode).
//                               Use one of the following PREFIXES
//                               (colon-separated) to generate ADDITIONAL output:
//                                'qMinE:' (query) for each position the minimal
//                               energy of any interaction covering the position
//                               (CSV format)
//                                'qSpotProb:' (query) for each position the
//                               probability that is is covered by an interaction
//                               covering (CSV format)
//                                'qAcc:' (query) ED accessibility values
//                               ('qPu'-like format).
//                                'qPu:' (query) unpaired probabilities values
//                               (RNAplfold format).
//                                'tMinE:' (target) for each position the minimal
//                               energy of any interaction covering the position
//                               (CSV format)
//                                'tSpotProb:' (target) for each position the
//                               probability that is is covered by an interaction
//                               covering (CSV format)
//                                'tAcc:' (target) ED accessibility values
//                               ('tPu'-like format).
//                                'tPu:' (target) unpaired probabilities values
//                               (RNAplfold format).
//                                'pMinE:' (target+query) for each index pair the
//                               minimal energy of any interaction covering the
//                               pair (CSV format)
//                                'spotProb:' (target+query) tracks for a given
//                               set of interaction spots their probability to be
//                               covered by an interaction. If no spots are
//                               provided, probabilities for all index
//                               combinations are computed. Spots are encoded by
//                               comma-separated 'idxT&idxQ' pairs
//                               (target-query). For each spot a probability is
//                               provided in concert with the probability that
//                               none of the spots (encoded by '0&0') is covered
//                               (CSV format). The spot encoding is followed
//                               colon-separated by the output stream/file name,
//                               eg. '--out="spotProb:3&76,59&2:STDERR"'. NOTE:
//                               value has to be quoted due to '&' symbol!
//                               For each, provide a file name or STDOUT/STDERR
//                               to write to the respective output stream.

type OutputFileOptions =
    |A

//  -n [ --outNumber ] arg (=1)  number of (sub)optimal interactions to report
//                               (arg in range [0,1000])
//  --outOverlap arg (=Q)        suboptimal output : interactions can overlap
//                                'N' in none of the sequences,
//                                'T' in the target only,
//                                'Q' in the query only,
//                                'B' in both sequences



type IntaRNAParams =
    | Query of QueryOptions list
    | Target of TargetOptions list
    | PredictionMode of PredictionModeOptions list
    | Seed of SeedOptions list
    | OutputMode of OutputModeOptions

    static member makeCmd = function
        | Query             qList -> qList |> List.map QueryOptions.make            |> List.concat 
        | Target            tList -> tList |> List.map TargetOptions.make           |> List.concat 
        | PredictionMode    pList -> pList |> List.map PredictionModeOptions.make   |> List.concat 
        | Seed              sList -> sList |> List.map SeedOptions.make             |> List.concat 
        | OutputMode        o     -> OutputModeOptions.make o

    static member makeCmdWith (m:MountInfo) = 
        let cPath p = (MountInfo.containerPathOf m p)
        function
        | Query             qList -> qList |> List.map (QueryOptions.makeWith m)    |> List.concat 
        | Target            tList -> tList |> List.map (TargetOptions.makeWith m)   |> List.concat 
        | PredictionMode    pList -> pList |> List.map PredictionModeOptions.make   |> List.concat 
        | Seed              sList -> sList |> List.map SeedOptions.make             |> List.concat 
        | OutputMode        o     -> OutputModeOptions.make o


let runIntaRNAAsync (bcContext:BioContainer.BcContext) (opt:IntaRNAParams list) = 
    let cmds = opt |> List.map (IntaRNAParams.makeCmdWith bcContext.Mount)
    let tp = "IntaRNA"::(cmds |> List.concat)
    printfn "starting process IntaRNA\r\nparameters:"
    cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

    async {
        let! res = BioContainer.execReturnAsync bcContext tp
        return res
    }

let runIntaRNA (bcContext:BioContainer.BcContext) (opt:IntaRNAParams list) = 
    runIntaRNAAsync bcContext opt
    |> Async.RunSynchronously

let IntaRNAImage =  Docker.ImageName @"quay.io/biocontainers/intarna:2.4.1--pl526hfac12b2_0"

let client = Docker.connect "npipe://./pipe/docker_engine"

Docker.Image.exists client IntaRNAImage

let intaRNAContext = 
    BioContainer.initBcContextWithMountAsync client IntaRNAImage @"C:\Users\Kevin\source\repos\CsbScaffold\Ramping(Challenge)\results"
    |> Async.RunSynchronously


let paramz = 
    [
        Query [
            QueryInput (QueryInputOptions.RNASequence "GTTCCTACTTAATTAAAAATTAAGCAGCTTTAGCTTGAGATTTAAATTCTGTTCCTACTTAATTAAAAATTAAGCAGCTTTAGCTTGAGATTTAAATTCT")
        ]
        Target [
            TargetInput (TargetInputOptions.RNASequence ("CAAGGAUGAAUUAAUUUUUAAUUCGUCGAAAUCGAACUCUAAAUUUAAGACAAGGAUGAAUUAAUUUUUAAUUCGUCGAAAUCGAACUCUAAAUUUAAGA" |> String.rev) )
        ]

    ]

let paramz2 =
        [
            Query [
                QueryInput (QueryInputOptions.RNASequence "GTTCCTACTTAATTAAAAATTAAGCAGCTTTAGCTTGAGATTTAAATTCT")
                QueryAcessibilityComputation [
                    QueryAcessibilityComputationType QueryAcessibilityComputationTypeOptions.NoContributions
                    ]
            ]
            Target [
                TargetInput (TargetInputOptions.RNASequence ("CAAGGAUGAAUUAAUUUUUAAUUCGUCGAAAUCGAACUCUAAAUUUAAGA"|> String.rev))
                TargetAcessibilityComputation [
                    TargetAcessibilityComputationType TargetAcessibilityComputationTypeOptions.NoContributions
                ]
            ]
            PredictionMode[
                Heuristic [HelixOptions.Default]
            ]
            Seed [
                MaxUnpairedBases 0
            ]
        ]


let paramz3 =
        [
            Query [
                QueryInput (QueryInputOptions.RNASequence "GTTCCTACTTAATTAAAAATTAAGCAGCTTTAGCTTGAGATTTAAATTCTGTTCCTACTTAATTAAAAATTAAGCAGCTTTAGCTTGAGATTTAAATTCT")
                QueryAcessibilityComputation [
                    QueryAcessibilityComputationOptions.MaximalLoopLength 0
                    QueryAcessibilityComputationOptions.SlidingWindowSize 0
                    ]
            ]
            Target [
                TargetInput (TargetInputOptions.RNASequence ("CAAGGAUGAAUUAAUUUUUAAUUCGUCGAAAUCGAACUCUAAAUUUAAGACAAGGAUGAAUUAAUUUUUAAUUCGUCGAAAUCGAACUCUAAAUUUAAGA" |> String.rev))
                TargetAcessibilityComputation [
                    TargetAcessibilityComputationOptions.MaximalLoopLength 0
                    TargetAcessibilityComputationOptions.SlidingWindowSize 0
                ]
            ]
            PredictionMode[
                ExactLowMemory
            ]
            Seed [
                MaxUnpairedBases 0
            ]
        ]

type IntaRNAResult = 
    {
        InteractionChart : string
        InteractionEnergy: float
    }

type DetailedIntaRNAResult = 
    {
        InteractionChart : string
        InteractionEnergy: float
        LoopEnergy : float
    }
"  + E(loops)       ="

let detailedIntaRNAResultofString (res: string) =
    try 
        let lines = res.Replace("\r\n","\n").Split('\n')
        let chart = 
            lines
            |> String.concat "\r\n"

        let energy = 
            lines
            |> Array.filter (fun x -> (x.Contains"interaction energy"))
            |> fun x -> x.[0].Split(' ').[3]
            |> float

        let loopEnergy = 
            lines
            |> Array.filter (fun x -> (x.Contains"E(loops)"))
            |> fun x -> 
                let splt = x.[0].Split('=').[1].Trim().Split(' ').[0]
                printfn "%s" splt
                splt
            |> float
        {
            InteractionChart = chart
            InteractionEnergy = energy
            LoopEnergy = loopEnergy
        }
    with _ -> 
        {
            InteractionChart = ""
            InteractionEnergy = nan
            LoopEnergy = nan
        }


let intaRNAResultofString (res: string) =
    let lines = res.Replace("\r\n","\n").Split('\n')
    let chart = 
        lines
        |> Array.filter (fun x -> (not (x.Contains"interaction energy")))
        |> String.concat "\r\n"


    let energy = 
        lines
        |> Array.filter (fun x -> (x.Contains"interaction energy"))
        |> fun x -> x.[0].Split(' ').[3]
        |> float

    {InteractionChart = chart; InteractionEnergy=energy}

let testResult = runIntaRNA intaRNAContext paramz |> intaRNAResultofString
let testResult2 = runIntaRNA intaRNAContext paramz2 |> intaRNAResultofString
let testResult3 = runIntaRNA intaRNAContext paramz3 |> intaRNAResultofString

testResult.InteractionChart

let intaRNASimple probe target = 
    let paramz = 
        [
            Query [
                QueryInput (QueryInputOptions.RNASequence probe)
            ]
            Target [
                TargetInput (TargetInputOptions.RNASequence target)
            ]
        ]
    try
        let testResult = runIntaRNA intaRNAContext paramz |> intaRNAResultofString
        testResult.InteractionEnergy
    with e as exn -> printfn "FAIL"
                     nan

let intaRNADetailed probe target = 
    let paramz = 
        [
            Query [
                QueryInput (QueryInputOptions.RNASequence probe)
            ]
            Target [
                TargetInput (TargetInputOptions.RNASequence target)
            ]
            OutputMode OutputModeOptions.Detailed
        ]

    runIntaRNA intaRNAContext paramz |> detailedIntaRNAResultofString

let t = (intaRNADetailed "CTGCAGCAACATATGTGAGG" "CTGCAGCAACATATGTGAGG")


type BlastNParams = 
    |MaxEval of int
    |WordSize of int
    |GapOpenCost of int
    |GapExtendCost of int
    |MatchScore of int
    |MissMatchPenalty of int

    static member makeCmd =
        function
        |MaxEval          i -> ["-"; string i]
        |WordSize         i -> ["-word_size"; string i ]
        |GapOpenCost      i -> ["-"; string i ]
        |GapExtendCost    i -> ["-"; string i ]
        |MatchScore       i -> ["-"; string i ]
        |MissMatchPenalty i -> ["-"; string i ]

open FSharpAux.IO.FileIO
open BioFSharp.IO

let chlamyCDNAs = 
    FastA.fromFile (BioArray.ofNucleotideString)(__SOURCE_DIRECTORY__ +  @"..\..\..\..\docsrc\content\data\Chlamydomonas_reinhardtii.Chlamydomonas_reinhardtii_v5.5.cdna.all.fa")
    |> Array.ofSeq

let ImageBlast = Docker.DockerId.ImageId "blast"

let blastContext = 
    BioContainer.initBcContextWithMountAsync client ImageBlast @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data"
    |> Async.RunSynchronously

let makeBlastDBParams =
    [
        MakeDbParams.DbType Nucleotide
        MakeDbParams.Input  @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\Chlamydomonas_reinhardtii.Chlamydomonas_reinhardtii_v5.5.cdna.all.fa"
        MakeDbParams.Output @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\Chlamydomonas_reinhardtii.Chlamydomonas_reinhardtii_v5.5.cdna.all.fa"
    ]

let outputFormat= 
    
    [   
        OutputCustom.Query_SeqId; 
        OutputCustom.Subject_SeqId;
        OutputCustom.Query_Length;
        OutputCustom.Subject_Length;
        OutputCustom.AlignmentLength;
        OutputCustom.MismatchCount;
        OutputCustom.IdentityCount;
        OutputCustom.PositiveScoringMatchCount;
        OutputCustom.Evalue;
        OutputCustom.Bitscore;
    ] 

let blastNParamz = [
    BlastParams.SearchDB    @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\Chlamydomonas_reinhardtii.Chlamydomonas_reinhardtii_v5.5.cdna.all.fa"
    BlastParams.Query       @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\BlastQueries.fasta"
    BlastParams.Output      @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\BlastQueriesResults.fasta"
    OutputTypeCustom
        (
             OutputType.TabularWithComments,
             [   
                OutputCustom.Query_SeqId; 
                OutputCustom.Subject_SeqId;
                OutputCustom.Query_Length;
                OutputCustom.Subject_Length;
                OutputCustom.AlignmentLength;
                OutputCustom.MismatchCount;
                OutputCustom.IdentityCount;
                OutputCustom.PositiveScoringMatchCount;
                OutputCustom.Evalue;
                OutputCustom.Bitscore;
             ] 
        )
]

let testItem = 
    chlamyCDNAs
    |> Seq.item 1337

let generatePrimerPairs (length:int) (templateSpan:int) (item:FastA.FastaItem<BioArray.BioArray<Nucleotides.Nucleotide>>) =
    let header = item.Header
    item.Sequence
    |> Array.windowed ((2 * length) + templateSpan)
    |> Array.mapi
        (fun i flankedTemplate -> 
            let fwdPrimer = flankedTemplate.[0 .. length-1]
            let revPrimer = 
                // reverse primer is reversed complementary strand
                flankedTemplate.[length - 1 + templateSpan .. (2 * length) + templateSpan - 2]
                |> Array.map (Nucleotides.complement)
                |> Array.rev
            [|
                (sprintf "%i_fwd_%s" i header),fwdPrimer
                (sprintf "%i_rev_%s" i header),revPrimer
            |]
            )
    |> Array.concat
    |> Array.map (fun (header,sequence) -> FastA.createFastaItem header sequence)

let testPrimers = 
    testItem
    |> generatePrimerPairs 20 100

testPrimers
|> FastA.write (BioItem.symbol) @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\BlastQueries.fasta" 


let runBlastNShortAsync (bcContext:BioContainer.BcContext) (opt:BlastParams list) = 
    let cmds = (opt |> List.map (BlastParams.makeCmdWith bcContext.Mount))
    let tp = ["blastn"; "-task"; "blastn-short"] @(cmds |> List.concat)

    printfn "Starting process blastn\r\nparameters:"
    cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

    async {
            let! res = BioContainer.execAsync bcContext tp           
            return res
 
    }

let runBlastNShort (bcContext:BioContainer.BcContext) (opt:BlastParams list) =
    runBlastNShortAsync bcContext opt
    |> Async.RunSynchronously


Blast.runMakeBlastDB blastContext makeBlastDBParams 

runBlastNShort blastContext blastNParamz

let headerString = ["query id\tsubject id\tquery length\tsubject length\talignment length\tmismatches\tidentical\tpositives\tevalue\tbit score"]

let BlastStream = 
    FSharpAux.IO.FileIO.readFile @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\BlastQueriesResults.fasta" |> Seq.filter (fun x -> not (x.StartsWith("#")))
    |> fun x -> Seq.append headerString x
    |> FSharpAux.IO.FileIO.writeToFile false  @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\BlastQueriesResultsCleaned.txt" 


let primerSequenceMap =
    testPrimers
    |> Array.map (fun x -> (x.Header.Split(' ').[0].Trim())  => (x.Sequence |> BioArray.toString))
    |> fun x -> series ["Sequence" => series x]
    |> Frame.ofColumns





let results = 
    let resultSchema = "evalue=float,bit score=float"
    Frame.ReadCsv(path = @"C:\Users\Kevin\source\repos\AppliedFSharp\docsrc\content\data\BlastQueriesResultsCleaned.txt" , separators = "\t",schema=resultSchema)
    |> Frame.filterRows (fun _ os -> os.GetAs<int>("query length") <> os.GetAs<int>("alignment length") &&  os.GetAs<int>("query length") <> os.GetAs<int>("identical")) 


let sortedFrameForHybridization : Frame<(string*(string*(string*int))),string>=
    
    let groupedByHQueryId : Frame<string*int,string> =  
        results
        |> Frame.groupRowsBy "query id"

    groupedByHQueryId
    |> Frame.mapRowKeys 
        (fun (seqHeader,index) -> 
            let splitHeader = seqHeader.Split('_')
            (splitHeader.[2],(splitHeader.[0],(splitHeader.[1],index)))
        )
    |> Frame.dropCol "query id"
    |> Frame.dropCol "subject id"
    |> Frame.dropCol "subject length"
    |> Frame.dropCol "query length"
    |> Frame.dropCol "alignment length"
    |> Frame.dropCol "mismatches"
    |> Frame.dropCol "identical"
    |> Frame.dropCol "positives"
    |> Frame.dropCol "bit score"

let HybridizationCandidates = 
    sortedFrameForHybridization
    |> Frame.applyLevel 
        (fun (targetGene,(simNum,(direction,index))) -> (targetGene,(simNum,direction)))
        Stats.min
    |> Frame.mapRowKeys (fun (targetGene,(simNum,direction)) -> (sprintf "%s_%s_%s" simNum direction targetGene))
    |> Frame.join JoinKind.Inner primerSequenceMap
    |> Frame.mapRowKeys 
        (fun rk -> 
            let splitHeader = rk.Split('_')
            (splitHeader.[2],(splitHeader.[0],splitHeader.[1])))


let reverseSet =
    HybridizationCandidates
    |> Frame.filterRows (fun (_,(_,direction)) _ -> direction = "rev")

let fwdSet =
    HybridizationCandidates
    |> Frame.filterRows (fun (_,(_,direction)) _ -> direction = "fwd")

let revSetHybridizationScored =
    reverseSet
    |> fun x ->
        let selfAlignCol =
            x
            |> Frame.mapRows 
                (fun _ os -> 
                    let sequence = os.GetAs<string>("Sequence")
                    intaRNASimple sequence sequence
             
                    )
        let LoopCol =
            x
            |> Frame.mapRows 
                (fun _ os -> 
                    let sequence = os.GetAs<string>("Sequence")
                    (intaRNADetailed sequence sequence).LoopEnergy
             
                    )       
        
        x
        |> Frame.addCol "SelfAlign" selfAlignCol
        |> Frame.addCol "Loop" LoopCol


let fwdSetHybridizationScored =
    fwdSet
    |> fun x ->
        let selfAlignCol =
            x
            |> Frame.mapRows 
                (fun _ os -> 
                    let sequence = os.GetAs<string>("Sequence")
                    intaRNASimple sequence sequence
             
                    )
        let LoopCol =
            x
            |> Frame.mapRows 
                (fun _ os -> 
                    let sequence = os.GetAs<string>("Sequence")
                    (intaRNADetailed sequence sequence).LoopEnergy
             
                    )       
        
        x
        |> Frame.addCol "SelfAlign" selfAlignCol
        |> Frame.addCol "Loop" LoopCol


let forwardReverseAligned =
    let fwdSeqs = 
        fwdSetHybridizationScored
        |> Frame.getCol "Sequence"
        |> Series.mapKeys (fun (a,(b,c)) -> a,b)
    let revSeqs =
        revSetHybridizationScored
        |> Frame.getCol "Sequence"
        |> Series.mapKeys (fun (a,(b,c)) -> a,b)
    let alSeries = 
        Series.zipInner fwdSeqs revSeqs
        |> Series.mapValues (fun (fwd,rev) -> intaRNASimple fwd rev)
    fwdSetHybridizationScored
    |> Frame.addCol "PrimerCrossHybridization" (alSeries |> Series.mapKeys (fun (a,b) -> a,(b,"fwd")))
    |> Frame.merge (revSetHybridizationScored |> Frame.addCol "PrimerCrossHybridization" (alSeries |> Series.mapKeys (fun (a,b) -> a,(b,"rev"))))



forwardReverseAligned
|> Frame.sortRowsByKey