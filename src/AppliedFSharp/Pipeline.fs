namespace AppliedFSharp

module Pipeline =

    open ContainerAPIs
    open Deedle
    open DeedleOperations
    open BioFSharp
    open BioFSharp.IO
    open BioFSharp.BioTools

    
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


    ///
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

    /// (x.Header.Split(' ').[0].Trim())
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







