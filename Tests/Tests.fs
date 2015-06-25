namespace Tests

open Xunit
open FsUnit.Xunit
open FruitShop.Checkout

module Tests = 
    type ``Given checkout``() = 
        let checkout = new Checkout()
        
        [<Fact>]
        let ``when adding products price apply step 6 reductions``() = 
            Assert.Equal(100, (checkout.send "Mele,Pommes,Apples,Pommes,Mele"))
            Assert.Equal(250, (checkout.send "Bananas"))
        
        [<Fact>]
        let ``when adding products price apply step 6 reductions for more fruits``() = 
            Assert.Equal(200, (checkout.send "Mele,Pommes,Pommes,Mele"))
            Assert.Equal(150, (checkout.send "Bananas"))
            Assert.Equal(150, (checkout.send "Mele,Pommes,Pommes,Apples,Mele"))
