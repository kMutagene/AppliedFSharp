namespace AppliedFSharp

module Pipeline =

    open ContainerAPIs
    open Deedle
    open DeedleOperations
    open BioFSharp
    open BioFSharp.IO
    open BioFSharp.BioTools

    ///Generates all possible primer pairs of lenght n flanking a sequence of length m from an input gene
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


    ///Prepare a blast database for subsequent blast searches. For best feature calculation, use the full cDNA transcriptome of the organism
    let preparePrimerBlastSearch (blastContext: BioContainer.BcContext) (dbPath:string) (queryFastaOutputPath:string) (queries: FastA.FastaItem<Nucleotides.Nucleotide[]>[]) =
        //
        try
            ContainerAPIs.BlastExtension.DefaultConfig.runMakeBlastDBDefault blastContext dbPath
        with e as exn ->
            failwithf "creating Blast database failed with:\r\n%s" e.Message
        //
        queries
        |> FastA.write (BioItem.symbol) queryFastaOutputPath
        //


    ///Blast all generated primer pairs against the previously generated database. results are written to a file of choice.
    let blastPrimerPairs (blastContext: BioContainer.BcContext) (dbPath:string) (queryFastaPath:string) (blastResultOutputPath:string)  (cleanedBlastResultOutputPath:string)=
        try 
            ContainerAPIs.BlastExtension.DefaultConfig.runBlastNShortDefault blastContext dbPath queryFastaPath blastResultOutputPath
            printfn "WUT"
        with e as exn ->
            failwithf "BlastN failed with:\r\n%s" e.Message


        let headerString = ["query id\tsubject id\tquery length\tsubject length\talignment length\tmismatches\tidentical\tpositives\tevalue\tbit score"]
        printfn "SO\r\nO\r\nO\r\n=\r\n"
        FSharpAux.IO.FileIO.readFile blastResultOutputPath |> Seq.filter (fun x -> not (x.StartsWith("#")))
        |> fun x -> Seq.append headerString x
        |> FSharpAux.IO.FileIO.writeToFile false cleanedBlastResultOutputPath

    ///Calculates hybridization energy features for the given blast results
    let getResultFrame (intaRNAContext: BioContainer.BcContext) (cleanedBlastResultPath:string) (fastaHeaderConverter: string -> string) (queries: FastA.FastaItem<Nucleotides.Nucleotide[]>[]) =
  
        let primerSequenceMap =
            queries
            |> Array.map (fun x -> x.Header |> fastaHeaderConverter => (x.Sequence |> BioArray.toString))
            |> fun x -> series ["Sequence" => series x]
            |> Frame.ofColumns

        readCleanedBlastResultFrame cleanedBlastResultPath
        |> sortFrameForHybridization
        |> getHybridizationCandidatesFrame primerSequenceMap
        |> getResultFrameWithHybridisationEnergies intaRNAContext

    /// The pipeline settings i used for my original script
    module DefaultConfig =
        
        let runDefaultPipeline (primerLength:int) (templateLength:int) (blastContext: BioContainer.BcContext) (dbPath:string) (queryFastaOutputPath:string) (blastResultOutputPath:string)  (cleanedBlastResultOutputPath:string) (intaRNAContext: BioContainer.BcContext) (gene:FastA.FastaItem<BioArray.BioArray<Nucleotides.Nucleotide>>)=
            //
            let queries =  generatePrimerPairs primerLength templateLength gene
            //
            queries
            |> preparePrimerBlastSearch blastContext dbPath queryFastaOutputPath
            //
            blastPrimerPairs blastContext dbPath queryFastaOutputPath blastResultOutputPath cleanedBlastResultOutputPath
            //
            getResultFrame intaRNAContext cleanedBlastResultOutputPath (fun x -> x.Split(' ').[0].Trim()) queries







