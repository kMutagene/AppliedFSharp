namespace AppliedFSharp

module ContainerAPIs =
    
    open BioFSharp.BioTools
    open BioFSharp.BioTools.BioContainer

    ///API for a bioContainer conform docker container containing the IntaRNA() software package
    module IntaRNA =

        ///  -q [ --query ] arg           
        ///
        ///either an RNA sequence or the stream/file name from where to read the query sequences (shouldbe the shorter sequences to increaseefficiency); 
        ///sequences have to use IUPACnucleotide encoding
        type QueryInputOptions =
            ///RNA sequence string
            |RNASequence of string
            ///stream/file name from where to read the query sequences
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
            |NotImplemented

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

        module DefaultCofig =  

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


                
            let intaRNASimple (bcContext:BioContainer.BcContext) probe target = 
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
                    let testResult = runIntaRNA bcContext paramz |> intaRNAResultofString
                    testResult.InteractionEnergy
                with e as exn -> printfn "FAIL"
                                 nan

            let intaRNADetailed (bcContext:BioContainer.BcContext) probe target = 
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

                runIntaRNA bcContext paramz |> detailedIntaRNAResultofString

    module BlastExtension =

        open BioFSharp.BioTools.Blast

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