namespace FruitShop

module Models =
    type Product = |Apples|Pommes|Mele|Bananas|Cherries

    let getProduct productName = 
        match productName with
        | "Apples"      -> Apples
        | "Mele"        -> Mele
        | "Pommes"      -> Pommes
        | "Bananas"     -> Bananas
        | "Cherries"    -> Cherries
        | _ -> failwith "Unhandled input"
        
    let priceOf product = 
        match product with
        | Apples    -> 100
        | Pommes    -> 100
        | Mele      -> 100
        | Bananas   -> 150
        | Cherries  -> 75

module Reductions =
    open Models

    type Reductions(count, countAll, priceOf) = 
        let cherries()  = -20 * (count Cherries / 2)
        let bananas()   = -priceOf Bananas * (count Bananas / 2)
        let pommes()    = -(3 * priceOf Pommes - 200) * (count Pommes / 3)
        let mele()      = -100 * (count Mele / 2)
        let allApples() = -100 * ((count Mele + count Apples + count Pommes) / 4)
        let all()       = -200 * (countAll() / 5)
        let reductions  = [| cherries; bananas; pommes; mele; allApples; all |]   

        member x.apply total = 
            [| cherries; bananas; pommes; mele; allApples; all |]
            |> Array.fold (fun newTotal reduction -> newTotal + reduction()) total

module Checkout = 
    open Models
    open Reductions

    type History() = 
        let mutable products = List.empty
        member x.count product = Seq.filter (fun p -> p = product) products |> Seq.length
        member x.countAll() = products.Length
        member x.append newProducts = 
            products <- Seq.append products newProducts |> Seq.toList
            products |> List.toSeq
    
    type Checkout() = 
        let history = History()
        let reductions = Reductions(history.count, history.countAll, priceOf)
        let getProductNames input = input.ToString().Split(',')
        
        let calculate products = 
            products |> Seq.map priceOf |> Seq.sum
        
        member x.send input = 
            input
            |> getProductNames 
            |> Seq.map getProduct
            |> history.append
            |> calculate
            |> reductions.apply