namespace Tests

open Xunit
open FsUnit.Xunit

module Checkout =
    type Product = 
        | Apples
        | Pommes
        | Mele
        | Bananas
        | Cherries

    let mutable total = 0
    let mutable nbrCherries = 0
    let mutable nbrBananas = 0
    let mutable nbrPommes = 0
    let mutable nbrMele = 0
    let mutable nbrApple = 0
    let mutable nbrFruits = 0

    let addProduct productName =
        
        let getProduct productName = 
            match productName with
            | "Apples" -> Apples
            | "Mele" -> Mele
            | "Pommes" -> Pommes
            | "Bananas" -> Bananas
            | "Cherries" -> Cherries

        let computeCherriesReduction product price  =
            nbrCherries <- nbrCherries + 1
            if nbrCherries % 2 = 0 then
                price - 20
            else
                price

        let computeBananasReduction product price =
            nbrBananas <- nbrBananas + 1
            if nbrBananas % 2 = 0 then  0 else price
            
        let computePommesReduction product price =
            nbrPommes <- nbrPommes + 1
            if nbrPommes % 3 = 0 then 200 - 2 * price else price
            
        let computeMeleReduction product price =
            nbrMele <- nbrMele + 1
            if nbrMele % 2 = 0 then 100 - price else price

        let computeAllApplesReduction price =
            nbrApple <- nbrApple + 1
            if nbrApple % 4 = 0 then price - 100 else price

        let multiFruitReduct price =
            nbrFruits <- nbrFruits + 1
            if nbrFruits % 5 = 0 then price - 200 else price

        let productPrice product = 
            match product with
            | Apples -> multiFruitReduct (computeAllApplesReduction 100)
            | Pommes -> multiFruitReduct (computeAllApplesReduction (computePommesReduction product 100))
            | Mele -> multiFruitReduct (computeAllApplesReduction (computeMeleReduction product 100))
            | Bananas -> multiFruitReduct (computeBananasReduction product 150)
            | Cherries -> multiFruitReduct (computeCherriesReduction product 75)
            | _ -> 0
        let ctotal = productName.ToString().Split(',') |> Seq.map getProduct |>  Seq.map productPrice |> Seq.sum
        
        total <- total + ctotal
        total
      

open Checkout
type ``Given checkout`` ()=
    let mytest _ =
//        Assert.Equal(100,(addProduct "Mele,Pommes,Apples,Pommes,Mele"))
//        Assert.Equal(250,(addProduct "Bananas")   ) 

        Assert.Equal(200,(addProduct "Mele,Pommes,Pommes,Mele"))
        Assert.Equal(150,(addProduct "Bananas")   ) 
        Assert.Equal(150,(addProduct "Mele,Pommes,Pommes,Apples,Mele"))


    [<Fact>] member test.
        ``when products price is good`` ()=
            mytest null        
        
            
            