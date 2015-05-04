//O( 2^(n^2) )
def g(n:Int, display:Boolean = false):Long = {
    //this matrix is configured such that i points to j
    //in a bipartite gracefully labeled matrix
    val init:Array[Array[Option[Boolean]]] = Array.fill(n, n)(None)
    //general idea, take all diagonals until the sum of i and j is n-2        
    def sequence_from_diagonal(sum_of_diagonal:Int):List[(Int, Int)] = {
        //where sum_of_diagonal = i + j for every element in a diagonal
        //in the diagonal of a's reflection over the columns
        //00 01 02                  02 01 00
        //10 11 12      becomes     12 11 10
        //20 21 22                  22 21 20
        //this way, the first diagonal is the upper right corner
        //which must always be selected
        (0 to sum_of_diagonal).map{ i =>
            val j = sum_of_diagonal - i
            (i, n-1-j)
        }.toList
    }
    val sequence = (0 to n-1).map(sequence_from_diagonal).toArray
    def f(a:Array[Array[Option[Boolean]]]):Long = {
        def find_current:Int = {
            //+1 because we already filled the one we found
            for(i<-0 until n) if(a(0)(i) != None) return n-1-i +1 
            0
        }
        def emptyrow(i:Int):Boolean ={
            for(j<-0 until n) if(a(i)(j)==Some(true)) return false
            true
        }
        def emptycolumn(j:Int):Boolean ={
            for(i<-0 until n) if(a(i)(j)==Some(true)) return false
            true
        }
        def printA {
            //this just prints our matrix to the commandline
            val aprime = a.map{
                _.map{ v =>
                    if(v.getOrElse{false}) 1
                    else 0
                }
            }
            println(aprime.deep.mkString("\n"))
            println("-----------------------------------")
        }
        val current = find_current
        if(current == n-1) {
            //this is the base case, this diagonal should 
            //just be a row of zeros
            if(display) printA
            return 1
        }
        val totest = sequence(current)
        totest.foreach{ t:(Int, Int) =>
            //set this current diagonal to false to notion 
            //that we've looked at it
            val (i,j) = t
            a(i)(j) = Some(false)
        }
        totest.map{ t:(Int, Int) =>
            val (i, j) = t
            //if the column is empty corresponding to i
            //and the row is empty corresponding to j
            //then we can look at it
            //otherwise choosing that point breaks
            //the bipartite case
            if(emptyrow(j) && emptycolumn(i)){
                val aprime = a.map(_.clone)
                aprime(i)(j) = Some(true)
                f(aprime)
            }
            else 0
        }.sum
    }
    f(init)
}
println("example when n=4")
println(g(4, true))

/* EXAMPLE OUTPUT:
scala> :pa research.scala
Pasting file research.scala...
example when n=4
Array(0, 1, 1, 1)
Array(0, 0, 0, 0)                   This is a Star graph
Array(0, 0, 0, 0)
Array(0, 0, 0, 0)
-----------------------------------
Array(0, 0, 1, 1)
Array(0, 0, 1, 0)                   Line
Array(0, 0, 0, 0)
Array(0, 0, 0, 0)
-----------------------------------
Array(0, 0, 0, 1)
Array(0, 0, 1, 1)                   Line
Array(0, 0, 0, 0)
Array(0, 0, 0, 0)
-----------------------------------
Array(0, 0, 0, 1)
Array(0, 0, 0, 1)                   Star
Array(0, 0, 0, 1)
Array(0, 0, 0, 0)
-----------------------------------
4
g: (n: Int, display: Boolean)Int

scala> g(10,false)
res1: Int = 35494

scala> g(11,false)
res2: Int = 264178

scala> g(12,false)
res3: Int = 2124078

scala> g(13,false)
res4: Int = 18965372

*/