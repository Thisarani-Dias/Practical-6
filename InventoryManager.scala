package q1

object InventoryManager {

    case class Product(id: Int, name: String, quantity: Int, price: Double)

    val inventory1: Map[Int, Product] = Map(
      101 -> Product(101, "invent1", 50, 2.99),
      102 -> Product(102, "invent2", 75, 3.49),
      103 -> Product(103, "invent3", 30, 1.99)
    )

    val inventory2: Map[Int, Product] = Map(
      102 -> Product(102, "invent2", 50, 3.79),
      104 -> Product(104, "invent4", 20, 4.99)
    )

    def getProductNames(inventory: Map[Int, Product]): Iterable[String] = {
      inventory.values.map(_.name)      //convert iterable[product] into iterable string
    }

    def calculateTotalValue(inventory: Map[Int, Product]): Double = {
      inventory.values.map(product => product.quantity * product.price).sum
    }

    def isInventoryEmpty(inventory: Map[Int, Product]): Boolean = {
      inventory.isEmpty
    }

    def mergeInventories(inventory1: Map[Int, Product], inventory2: Map[Int, Product]): Map[Int, Product] = {
      (inventory1.toSeq ++ inventory2.toSeq)      //convert to tuples and concat
      .groupBy(_._1)                  //grp tuples by product id
      .map { case (id, products) =>   //iterates over each id which is a tuple
        val combinedProduct = products.map(_._2).reduce { (p1, p2) =>       //combine products with same id
          p1.copy(            //new product
            quantity = p1.quantity + p2.quantity,     //add quantities
            price = p1.price max p2.price             //max price
          )
        }
        id -> combinedProduct           // new tuple
      }
  }

  def checkAndPrintProductDetails(inventory: Map[Int, Product], productId: Int): Unit = {
      inventory.get(productId) match {
        case Some(product) => println(s"Product found: $product")
        case None => println(s"No product found with ID $productId")
      }
    }

    def main(args: Array[String]): Unit = {
      println("I. Product names in inventory1:")
      getProductNames(inventory1).foreach(println)      //iterates over iterable[string] and prints names

      println("\nII. Total value of products in inventory1:")
      println(calculateTotalValue(inventory1))

      println("\nIII. Is inventory1 empty?")
      println(isInventoryEmpty(inventory1))

      println("\nIV. Merged inventories (inventory1 and inventory2):")
      val mergedInventory = mergeInventories(inventory1, inventory2)
      mergedInventory.values.foreach(println)

      println("\nV. Checking product with ID 102 in inventory1:")
      checkAndPrintProductDetails(inventory1, 102)
    }
}
