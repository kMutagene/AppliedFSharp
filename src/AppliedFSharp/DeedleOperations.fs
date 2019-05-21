namespace AppliedFSharp

module DeedleOperations =

    open ContainerAPIs
    open Deedle
    open BioFSharp.BioTools
    open BioContainer

    ///reads a cleaned (no comment lines) blast result and filters identical matches
    let readCleanedBlastResultFrame path = 
        printfn "Reading cleaned Blast result"

        Frame.ReadCsv(path = path , separators = "\t")
        |> Frame.filterRows (fun _ os -> os.GetAs<int>("query length") <> os.GetAs<int>("alignment length") &&  os.GetAs<int>("query length") <> os.GetAs<int>("identical")) 

    ///removes unnecessary columns from the blast result frame and groups primers by query id, generation index, direction (fwd/rev) and row index
    let sortFrameForHybridization (cleanBlastResultFrame : Frame<int,string>) : Frame<(string*(string*(string*int))),string>=
        printfn "sorting frame before calculating hybridisation ernergy"
        let groupedByHQueryId : Frame<string*int,string> =  
            cleanBlastResultFrame
            |> Frame.groupRowsBy "query id"
        //split composite keys ("string1_string2_string3") into actual keys
        groupedByHQueryId
        |> Frame.mapRowKeys 
            (fun (seqHeader,index) -> 
                let splitHeader = seqHeader.Split('_')
                (splitHeader.[2],(splitHeader.[0],(splitHeader.[1],index)))
            )
        //drop unnecessary columns
        |> Frame.dropCol "query id"
        |> Frame.dropCol "subject id"
        |> Frame.dropCol "subject length"
        |> Frame.dropCol "query length"
        |> Frame.dropCol "alignment length"
        |> Frame.dropCol "mismatches"
        |> Frame.dropCol "identical"
        |> Frame.dropCol "positives"
        |> Frame.dropCol "bit score"


    ///Returns a frame with the blast results with the lowest evalue for all primer candidates. This corresponds to the first match with no 100% identity
    let getHybridizationCandidatesFrame (primerSequenceMap:Frame<string,string>) (frameForHybridisation: Frame<(string*(string*(string*int))),string>) =
        printfn "calculating hybridization energy features"
        frameForHybridisation
        //keep candidates with the lowest e value. That way, the best non-identical hits are chosen.
        |> Frame.applyLevel 
            (fun (targetGene,(simNum,(direction,index))) -> (targetGene,(simNum,direction)))
            Stats.min
        |> Frame.mapRowKeys (fun (targetGene,(simNum,direction)) -> (sprintf "%s_%s_%s" simNum direction targetGene))
        //join with actual primer sequences
        |> Frame.join JoinKind.Inner primerSequenceMap
        |> Frame.mapRowKeys 
            (fun rk -> 
                let splitHeader = rk.Split('_')
                (splitHeader.[2],(splitHeader.[0],splitHeader.[1])))

    ///returns a frame containing the selfalign, internal loop, and cross hybridization energy for the input primer candidates using IntaRNA for prediction.
    let getResultFrameWithHybridisationEnergies (bcContext:BioContainer.BcContext) (hybridizationCandidatesFrame : Frame<(string*(string*string)),string>)  =
        let reverseSet =
            hybridizationCandidatesFrame
            |> Frame.filterRows (fun (_,(_,direction)) _ -> direction = "rev")

        let fwdSet =
            hybridizationCandidatesFrame
            |> Frame.filterRows (fun (_,(_,direction)) _ -> direction = "fwd")
        
        let revSetHybridizationScored =

            reverseSet
            |> fun x ->
                let selfAlignCol =
                    x
                    |> Frame.mapRows 
                        (fun _ os -> 
                            let sequence = os.GetAs<string>("Sequence")
                            IntaRNA.DefaultConfig.intaRNASimple bcContext sequence sequence
             
                            )
                let LoopCol =
                    x
                    |> Frame.mapRows 
                        (fun _ os -> 
                            let sequence = os.GetAs<string>("Sequence")
                            (IntaRNA.DefaultConfig.intaRNADetailed bcContext sequence sequence).LoopEnergy
             
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
                            IntaRNA.DefaultConfig.intaRNASimple bcContext sequence sequence
             
                            )
                let LoopCol =
                    x
                    |> Frame.mapRows 
                        (fun _ os -> 
                            let sequence = os.GetAs<string>("Sequence")
                            (IntaRNA.DefaultConfig.intaRNADetailed bcContext sequence sequence).LoopEnergy
             
                            )       
        
                x
                |> Frame.addCol "SelfAlign" selfAlignCol
                |> Frame.addCol "Loop" LoopCol

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
            |> Series.mapValues (fun (fwd,rev) -> IntaRNA.DefaultConfig.intaRNASimple bcContext fwd rev)
        fwdSetHybridizationScored
        |> Frame.addCol "PrimerCrossHybridization" (alSeries |> Series.mapKeys (fun (a,b) -> a,(b,"fwd")))
        |> Frame.merge (revSetHybridizationScored |> Frame.addCol "PrimerCrossHybridization" (alSeries |> Series.mapKeys (fun (a,b) -> a,(b,"rev"))))
