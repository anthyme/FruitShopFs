module Checkout

type Product = |Apples|Pommes|Mele|Bananas|Cherries

let priceOf = function Apples|Pommes|Mele -> 100 | Bananas -> 150 | Cherries -> 75

let parseProduct = function
    | "Apples"      -> Apples
    | "Mele"        -> Mele
    | "Pommes"      -> Pommes
    | "Bananas"     -> Bananas
    | "Cherries"    -> Cherries
    | _ -> failwith "Unhandled product name"

let splitProducts (input:string) = input.Split(',')

let applyReduction (products:Product list) total = 
    let count product = products |> Seq.filter ((=) product) |> Seq.length
    let cherries()  = -20 * (count Cherries / 2)
    let bananas()   = -priceOf Bananas * (count Bananas / 2)
    let pommes()    = -(3 * priceOf Pommes - 200) * (count Pommes / 3)
    let mele()      = -100 * (count Mele / 2)
    let allApples() = -100 * ((count Mele + count Apples + count Pommes) / 4)
    let all()       = -200 * (products.Length / 5)
    let applyReduction total reduction = total + reduction()
    [cherries; bananas; pommes; mele; allApples; all] |> List.fold applyReduction total

let computeResult products = products, products |> Seq.map priceOf |> Seq.sum |> applyReduction products

let checkout basket = splitProducts >> Seq.map parseProduct >> Seq.append basket >> Seq.toList >> computeResult