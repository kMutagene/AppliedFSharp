namespace AppliedFSharp

module DeedleOperations =

    open ContainerAPIs
    open Deedle
    open BioFSharp.BioTools
    open BioContainer

    let readCleanedBlastResultFrame path = 
        let resultSchema = "evalue=float,bit score=float"
        Frame.ReadCsv(path = path , separators = "\t",schema=resultSchema)
        |> Frame.filterRows (fun _ os -> os.GetAs<int>("query length") <> os.GetAs<int>("alignment length") &&  os.GetAs<int>("query length") <> os.GetAs<int>("identical")) 


    let sortFrameForHybridization (cleanBlastResultFrame : Frame<int,string>) : Frame<(string*(string*(string*int))),string>=
    
        let groupedByHQueryId : Frame<string*int,string> =  
            cleanBlastResultFrame
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



    let getHybridizationCandidatesFrame (primerSequenceMap:Frame<string,string>) (frameForHybridisation: Frame<(string*(string*(string*int))),string>) =
        frameForHybridisation
        |> Frame.applyLevel 
            (fun (targetGene,(simNum,(direction,index))) -> (targetGene,(simNum,direction)))
            Stats.min
        |> Frame.mapRowKeys (fun (targetGene,(simNum,direction)) -> (sprintf "%s_%s_%s" simNum direction targetGene))
        |> Frame.join JoinKind.Inner primerSequenceMap
        |> Frame.mapRowKeys 
            (fun rk -> 
                let splitHeader = rk.Split('_')
                (splitHeader.[2],(splitHeader.[0],splitHeader.[1])))


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
            |> Series.mapValues (fun (fwd,rev) -> IntaRNA.DefaultConfig.intaRNASimple fwd rev)
        fwdSetHybridizationScored
        |> Frame.addCol "PrimerCrossHybridization" (alSeries |> Series.mapKeys (fun (a,b) -> a,(b,"fwd")))
        |> Frame.merge (revSetHybridizationScored |> Frame.addCol "PrimerCrossHybridization" (alSeries |> Series.mapKeys (fun (a,b) -> a,(b,"rev"))))
