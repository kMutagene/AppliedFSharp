
#r @"..\..\..\packages\System.Buffers\lib\netstandard2.0\System.Buffers.dll"
#r @"..\..\..\packages\Docker.DotNet\lib\netstandard2.0\Docker.DotNet.dll"
#r @"..\..\..\packages\BioFSharp.IO\lib\netstandard2.0\BioFSharp.IO.dll"
#r @"..\..\..\packages\BioFSharp.BioTools\lib\netstandard2.0\BioFSharp.BioTools.dll"
#r @"..\..\..\packages\FSharpAux\lib\netstandard2.0\FSharpAux.dll"
#r @"..\..\..\packages\Deedle\lib\netstandard2.0\Deedle.dll"
#r @"..\..\..\packages\SharpZipLib\lib\netstandard2.0\ICSharpCode.SharpZipLib.dll"
#r @"..\..\..\packages\BioFSharp\lib\netstandard2.0\BioFSharp.dll"
#r @"..\..\..\packages\FSharp.Stats\lib\netstandard2.0\FSharp.Stats.dll"
#r @"..\..\..\packages\FSharp.Plotly\lib\netstandard2.0\FSharp.Plotly.dll"
#r "netstandard"

open System.IO
open System
let dependencies = 
    [
        @"../../..\packages\Docker.DotNet\lib\netstandard2.0\Docker.DotNet.dll"
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

    static member makeCmd = function
        | Query             qList -> qList |> List.map QueryOptions.make            |> List.concat 
        | Target            tList -> tList |> List.map TargetOptions.make           |> List.concat 
        | PredictionMode    pList -> pList |> List.map PredictionModeOptions.make   |> List.concat 
        | Seed              sList -> sList |> List.map SeedOptions.make             |> List.concat 

    static member makeCmdWith (m:MountInfo) = 
        let cPath p = (MountInfo.containerPathOf m p)
        function
        | Query             qList -> qList |> List.map (QueryOptions.makeWith m)    |> List.concat 
        | Target            tList -> tList |> List.map (TargetOptions.makeWith m)   |> List.concat 
        | PredictionMode    pList -> pList |> List.map PredictionModeOptions.make   |> List.concat 
        | Seed              sList -> sList |> List.map SeedOptions.make             |> List.concat 


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

let dataframe : Frame<string,string>= 
    let numericKeys = 
        Frame.ReadCsv(path = @"C:\Users\Kevin\source\repos\CsbScaffold\Ramping(Challenge)\data\nac2Probes.txt",separators="\t") 
        |> Frame.indexRows "Probe name"
        |> Frame.getNumericCols
        |> Frame.ofColumns
        |> Frame.dropCol "Probe position1"
        |> Frame.dropCol "Probe length [nt]" 
        |> Frame.dropCol "Probe GC content [%]" 
        |> fun x -> 
            x.ColumnKeys 
            |> Seq.map (fun x ->    printfn "%s" x
                                    sprintf "%s=float" x) 
            |> String.concat ","

    Frame.ReadCsv(path = @"C:\Users\Kevin\source\repos\CsbScaffold\Ramping(Challenge)\data\nac2Probes.txt",separators="\t",schema=numericKeys)     
    |> Frame.indexRows "Probe name"

let geneNameMapping : Frame<string,string>= 
    Frame.ReadCsv(path = @"C:\Users\Kevin\source\repos\CsbScaffold\Ramping(Challenge)\data\Chloro ORFs sortiert byDavid.csv",separators = ",")
    |> Frame.dropCol "Column5"
    |> Frame.dropCol "Column6"
    |> Frame.dropCol "Column7"
    |> Frame.dropCol "Column8"
    |> Frame.indexRows "Name"


let cleanedData : Frame<string,string> =
    dataframe
    |> Frame.dropCol "Probe position1"
    |> Frame.dropCol "Probe length [nt]"
    |> Frame.dropCol "Probe GC content [%]"
    |> Frame.dropCol "Probe within ORF2"
    |> Frame.filterCols (fun ck _ -> not (ck.Contains("Probe counts")))
    |> Frame.filterCols (fun ck _ -> not (ck.Contains("Normalized ORF Median")))

let cleanedDataWithGenes : Frame<string*(string*string),string>=
    cleanedData
    |> Frame.join JoinKind.Inner geneNameMapping
    |> Frame.groupRowsBy "strand"
    |> Frame.dropCol "strand"
    |> Frame.dropCol "Strand"
    |> Frame.dropCol "position"
    |> Frame.groupRowsBy "GeneName"
    |> Frame.dropCol "GeneName"
    |> Frame.sortRowsByKey


let rnaCleaned = 
    cleanedDataWithGenes
    |> Frame.filterCols (fun ck _ -> ck = "Probe sequence" || not(ck.Contains("Ribosome footprints")))

let profilesCleaned =
    cleanedDataWithGenes
    |> Frame.filterCols (fun ck _ -> ck = "Probe sequence" || (ck.Contains("Ribosome footprints")))


let rnaCleanedWithHybridisation =
    rnaCleaned
    |> fun x ->
        let hCol = 
            rnaCleaned
            |> Frame.mapRowValues 
                (fun os -> 
                    let probeDNA = os.GetAs<string>("Probe sequence") |> BioArray.ofNucleotideString
                    let targetRNA = probeDNA |> BioArray.transcribeCodeingStrand
                    let probeRNA = targetRNA |> Array.map Nucleotides.complement
                    let hybridEnergy = intaRNASimple (probeRNA |> BioArray.toString) (targetRNA |> BioArray.toString |> String.rev)
                    hybridEnergy
                    )
        x
        |> Frame.addCol "HybridizationEnergy" hCol

let profilesCleanedWithHybridisation =
    profilesCleaned
    |> fun x ->
        let hCol = 
            rnaCleaned
            |> Frame.mapRowValues 
                (fun os -> 
                    let probeDNA = os.GetAs<string>("Probe sequence") |> BioArray.ofNucleotideString
                    let targetRNA = probeDNA |> BioArray.transcribeCodeingStrand
                    let probeRNA = targetRNA |> Array.map Nucleotides.complement
                    let hybridEnergy = intaRNASimple (probeRNA |> BioArray.toString) (targetRNA |> BioArray.toString |> String.rev)
                    hybridEnergy
                    )
        x
        |> Frame.addCol "HybridizationEnergy" hCol


//let rnaCleanedWithHybridisation2 =
//    rnaCleaned
//    |> fun x ->
//        let hCol = 
//            rnaCleaned
//            |> Frame.mapRowValues 
//                (fun os -> 
//                    let probeDNA = os.GetAs<string>("Probe sequence") |> BioArray.ofNucleotideString
//                    let targetRNA = probeDNA |> BioArray.transcribeTemplateStrand
//                    let hybridEnergy = intaRNASimple (probeDNA |> BioArray.toString) (targetRNA |> BioArray.toString)
//                    hybridEnergy
//                    )
//        x
//        |> Frame.addCol "HybridizationEnergy" hCol

open FSharp.Stats
open FSharp.Plotly

let profilesData : Frame<string*(string*string),string>= 
    profilesCleanedWithHybridisation
    |> Frame.dropSparseRows
    |> Frame.filterCols (fun ck _ -> not (ck.Contains("_Replicate_3")))
    |> Frame.groupRowsBy "Probe sequence"
    |> Frame.mapRowKeys (fun (pSeq,(gene,(strand,pName))) -> (gene,(strand,pSeq)))
    |> Frame.dropCol "Probe sequence"
    |> Frame.filterCols (fun ck _ -> ck.Contains("(F635 Median - B635") || ck = "HybridizationEnergy")
    |> Frame.sortRowsByKey

let profileRegression1,profileRegression2 =
    let energy : Series<string*(string*string),float> = profilesData.GetColumn "HybridizationEnergy"
    let rep1 : Series<string*(string*string),float> = profilesData.GetColumn "Ribosome footprints_Replicate_1_Median (F635 Median - B635)"
    let rep2 : Series<string*(string*string),float> = profilesData.GetColumn "Ribosome footprints_Replicate_2_Median (F635 Median - B635)"

    Series.zipInner energy rep1
    |> Series.applyLevel 
        (fun (gene,(strand,pSeq)) -> gene)
        (fun ser -> 
            ser 
            |> Series.values 
            |> Seq.unzip
            |> fun (a,b) -> 
                let coeffs = FSharp.Stats.Fitting.LinearRegression.OrdinaryLeastSquares.Linear.Univariable.coefficient (vector (a)) (vector b)
                let fitFunc = FSharp.Stats.Fitting.LinearRegression.OrdinaryLeastSquares.Linear.Univariable.fit coeffs
                let rSquared = FSharp.Stats.Fitting.GoodnessOfFit.calulcateSumOfSquares fitFunc a b |> FSharp.Stats.Fitting.GoodnessOfFit.calulcateDetermination
                series [
                    
                    "Intercept" => coeffs.[0]
                    "Slope"     => coeffs.[1]
                    "R2"        => rSquared
                ]
                )
    |> Frame.ofRows
    |> fun x -> 
    let dataCol =
        Series.zipInner energy rep1
        |> Series.applyLevel 
            (fun (gene,(strand,pSeq)) -> gene)
            (fun ser -> 
                ser 
                |> Series.values 
                |> Array.ofSeq)
    Frame.addCol "XY" dataCol x
    ,

        
    Series.zipInner energy rep2
    |> Series.applyLevel 
        (fun (gene,(strand,pSeq)) -> gene)
        (fun ser -> 
            ser 
            |> Series.values 
            |> Seq.unzip
            |> fun (a,b) -> 
                let coeffs = FSharp.Stats.Fitting.LinearRegression.OrdinaryLeastSquares.Linear.Univariable.coefficient (vector a) (vector b)
                let fitFunc = FSharp.Stats.Fitting.LinearRegression.OrdinaryLeastSquares.Linear.Univariable.fit coeffs
                let rSquared = FSharp.Stats.Fitting.GoodnessOfFit.calulcateSumOfSquares fitFunc a b |> FSharp.Stats.Fitting.GoodnessOfFit.calulcateDetermination
                series [
                    "Intercept" => coeffs.[0]
                    "Slope"     => coeffs.[1]
                    "R2"        => rSquared
                ]
                )
        |> Frame.ofRows
        |> fun x -> 
            let dataCol =
                Series.zipInner energy rep2
                |> Series.applyLevel 
                    (fun (gene,(strand,pSeq)) -> gene)
                    (fun ser -> 
                        ser 
                        |> Series.values 
                        |> Array.ofSeq)
            Frame.addCol "XY" dataCol x

    
let rnaData : Frame<string*(string*string),string>= 
    rnaCleanedWithHybridisation
    |> Frame.dropSparseRows
    |> Frame.filterCols (fun ck _ -> not (ck.Contains("_Replicate_3")))
    |> Frame.groupRowsBy "Probe sequence"
    |> Frame.mapRowKeys (fun (pSeq,(gene,(strand,pName))) -> (gene,(strand,pSeq)))
    |> Frame.dropCol "Probe sequence"
    |> Frame.filterCols (fun ck _ -> ck.Contains("(F532 Median - B532") || ck = "HybridizationEnergy")
    |> Frame.sortRowsByKey


let rnaRegression1,rnaRegression2 =
    let energy : Series<string*(string*string),float> = rnaData.GetColumn "HybridizationEnergy"
    let rep1 : Series<string*(string*string),float> = rnaData.GetColumn "RNA_Replicate_1_Median (F532 Median - B532)"
    let rep2 : Series<string*(string*string),float> = rnaData.GetColumn "RNA_Replicate_2_Median (F532 Median - B532)"

    Series.zipInner energy rep1
    |> Series.applyLevel 
        (fun (gene,(strand,pSeq)) -> gene)
        (fun ser -> 
            ser 
            |> Series.values 
            |> Seq.unzip
            |> fun (a,b) -> 
                let coeffs = FSharp.Stats.Fitting.LinearRegression.OrdinaryLeastSquares.Linear.Univariable.coefficient (vector a) (vector b)
                let fitFunc = FSharp.Stats.Fitting.LinearRegression.OrdinaryLeastSquares.Linear.Univariable.fit coeffs
                let rSquared = FSharp.Stats.Fitting.GoodnessOfFit.calulcateSumOfSquares fitFunc a b |> FSharp.Stats.Fitting.GoodnessOfFit.calulcateDetermination
                series [
                    "Intercept" => coeffs.[0]
                    "Slope"     => coeffs.[1]
                    "R2"        => rSquared
                ]
                )
    |> Frame.ofRows
    |> fun x -> 
        let dataCol =
            Series.zipInner energy rep1
            |> Series.applyLevel 
                (fun (gene,(strand,pSeq)) -> gene)
                (fun ser -> 
                    ser 
                    |> Series.values 
                    |> Array.ofSeq)
        Frame.addCol "XY" dataCol x
    ,

        
    Series.zipInner energy rep2
    |> Series.applyLevel 
        (fun (gene,(strand,pSeq)) -> gene)
        (fun ser -> 
            ser 
            |> Series.values 
            |> Seq.unzip
            |> fun (a,b) -> 
                let coeffs = FSharp.Stats.Fitting.LinearRegression.OrdinaryLeastSquares.Linear.Univariable.coefficient (vector a) (vector b)
                let fitFunc = FSharp.Stats.Fitting.LinearRegression.OrdinaryLeastSquares.Linear.Univariable.fit coeffs
                let rSquared = FSharp.Stats.Fitting.GoodnessOfFit.calulcateSumOfSquares fitFunc a b |> FSharp.Stats.Fitting.GoodnessOfFit.calulcateDetermination
                series [
                    "Intercept" => coeffs.[0]
                    "Slope"     => coeffs.[1]
                    "R2"        => rSquared
                ]
                )
        |> Frame.ofRows
        |> fun x -> 
            let dataCol =
                Series.zipInner energy rep2
                |> Series.applyLevel 
                    (fun (gene,(strand,pSeq)) -> gene)
                    (fun ser -> 
                        ser 
                        |> Series.values 
                        |> Array.ofSeq)
            Frame.addCol "XY" dataCol x

let a = 
    [rnaRegression1;rnaRegression2]
    |> List.mapi 
        (fun i x ->
            x
            |> Frame.mapRows 
                (fun rk os ->
                    let coeffs = [os.GetAs<float>("Intercept");os.GetAs<float>("Slope")] |> vector
                    let fitFunc = FSharp.Stats.Fitting.LinearRegression.OrdinaryLeastSquares.Linear.Univariable.fit coeffs
                    let xy = os.GetAs<((float*float) [])>("XY")
                    let x = xy |> Array.map fst
                    let fitY = [|-100. .. 100.|] |> Array.map fitFunc
                    let plots =
                        [
                        Chart.Point(xy)
                        |> Chart.withTraceName (sprintf "%s_Values" rk)

                        Chart.Line(Seq.zip [|-100. .. 0.|] fitY)
                        |> Chart.withTraceName (sprintf "%s_Fit" rk)
                        ]
                        |> Chart.Combine 

                    plots
                )
        )

a
|> List.map (fun x -> x |> Series.values |> Chart.Combine |> Chart.withSize (1000.,1000.) |> Chart.Show) 

[profileRegression1;profileRegression2]
|> List.mapi 
    (fun i x ->
        x
        |> Frame.mapRows 
            (fun rk os ->
                let coeffs = [os.GetAs<float>("Intercept");os.GetAs<float>("Slope")] |> vector
                let fitFunc = FSharp.Stats.Fitting.LinearRegression.OrdinaryLeastSquares.Linear.Univariable.fit coeffs
                let xy = os.GetAs<((float*float) [])>("XY")
                let x = xy |> Array.map fst
                let fitY = [|-100. .. 100.|] |> Array.map fitFunc
                let plots =
                    [
                    Chart.Point(xy)
                    |> Chart.withTraceName (sprintf "%s_Values" rk)

                    Chart.Line(Seq.zip [|-100. .. 0.|] fitY)
                    |> Chart.withTraceName (sprintf "%s_Fit" rk)
                    ]
                    |> Chart.Combine 

                plots
            )
    )
|> List.map (fun x -> x |> Series.values |> Chart.Combine |> Chart.withSize (1000.,1000.) |> Chart.Show) 
