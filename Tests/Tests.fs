namespace Tests
open Xunit
open FsUnit.Xunit
open Checkout

module Tests = 
    type ``Given checkout``() =
        let emptyBasket = []

        let check input expected basket = 
            let basket,total = checkout basket input
            expected |> should equal total
            basket

        [<Fact>]
        let ``when adding products price apply step 6 reductions``() =
            emptyBasket
            |> check "Mele,Pommes,Apples,Pommes,Mele" 100
            |> check "Bananas" 250

        [<Fact>]
        let ``when adding products price apply step 6 reductions for more fruits``() = 
            emptyBasket
            |> check "Mele,Pommes,Pommes,Mele" 200
            |> check "Bananas" 150
            |> check "Mele,Pommes,Pommes,Apples,Mele" 150
