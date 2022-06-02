open System

// let convertMilesToYards (milesPointYards : float) : float = 
//     let wholeMiles = milesPointYards |> floor
//     let fraction = milesPointYards - float(wholeMiles)
//     wholeMiles + (fraction / 0.1760)

// // val decimalMiles : float = 1.5
// let decimalMiles = 1.0880 |> convertMilesToYards
// printfn "%f" decimalMiles

type MilesYards = MilesYards of wholeMiles : int * yards : int

module MilesYards =
    let private (~~) = float

    let fromMilesPointYards (milesPointYards : float) : MilesYards = 
        let wholeMiles = milesPointYards |> floor |> int
        if wholeMiles < 0 then
            raise <| ArgumentOutOfRangeException(nameof(milesPointYards),
                "Whole part must be >= 0")
        let fraction = milesPointYards - float(wholeMiles)
        if fraction > 0.1759 then
            raise <| ArgumentOutOfRangeException(nameof(milesPointYards),
                "Fractional part must be <= 0.1759")
        let yards = fraction * 10_000. |> round |> int
        MilesYards(wholeMiles, yards)

    // let toDecimalMiles (milesYards : MilesYards) : float = 
    //     match milesYards with
    //     | MilesYards(wholeMiles, yards) -> 
    //         (float wholeMiles) + ((float yards) / 1760.)
    let toDecimalMiles (MilesYards(wholeMiles, yards)) : float = 
        ~~wholeMiles + (~~yards / 1760.)

// 4.5
printfn "%A"
    (MilesYards.fromMilesPointYards(4.0880)
     |> MilesYards.toDecimalMiles)

// Error
//printfn "%A" (MilesYards.fromMilesPointYards(-4.5))

// Error
//printfn "%A" (MilesYards.fromMilesPointYards(4.5))

module MilesChains =
    let private (~~) = float

    type MilesChains =  private MilesChains of wholeMiles : int * chains : int

    let fromMilesChains (wholeMiles : int, chains: int) : MilesChains = 
        if wholeMiles < 0 then
            raise <| ArgumentOutOfRangeException(nameof(wholeMiles),
                "Whole part must be >= 0")
        if chains < 0 || chains >= 80 then
            raise <| ArgumentOutOfRangeException(nameof(chains),
                "Chains must be >= 0 and < 80")
        MilesChains(wholeMiles, chains)

    [<Literal>]
    let ChainsPerMile = 80.

    let private convertChainsToMiles (chains : int) = 
        ~~chains / ChainsPerMile

    let toDecimalMiles (MilesChains(wholeMiles, chains)) : float = 
        ~~wholeMiles + (chains |> convertChainsToMiles)

printfn "%A" (MilesChains.fromMilesChains(51, 29) |> MilesChains.toDecimalMiles)
