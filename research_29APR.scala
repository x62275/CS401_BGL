//O( 2^(n) )
def g(n:Int, display:Boolean = false):Long = {
    //this matrix is configured such that i points to j
    //in a bipartite gracefully labeled matrix
    val inita:Array[List[Int]] = Array.fill(n)(List())
    val initb:Array[List[Int]] = Array.fill(n)(List())
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
    def f(in:Array[List[Int]], out:Array[List[Int]], current:Int = 0):Long = {
        def emptyrow(i:Int):Boolean = out(i) == List.empty
        def emptycolumn(j:Int):Boolean = in(j) == List.empty
        def printA {
            //this just prints our matrix to the commandline
            ???
        }
        if(current == n-1) {
            //this is the base case, this diagonal should 
            //just be a row of zeros
            //if(display) printA
            return 1
        }
        val totest = sequence(current)
        totest.map{ t:(Int, Int) =>
            val (i, j) = t
            //if the column is empty corresponding to i
            //and the row is empty corresponding to j
            //then we can look at it
            //otherwise choosing that point breaks
            //the bipartite case
            if(emptyrow(j) && emptycolumn(i)){
                val inprime = in.clone
                val outprime = out.clone
                outprime(i) +:= j
                inprime(j) +:= i
                f(inprime, outprime, current + 1)
            }
            else 0
        }.sum
    }
    f(inita, initb)
}
println("example when n=4")
println(g(4, true))