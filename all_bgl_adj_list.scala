import runtime.ScalaRunTime.stringOf
import collection.immutable.Map
import Math.abs
def walk(tree1:String, tree2:String):Boolean ={
    // outputs: tree1==tree2
    val n = tree1.size
    if(n != tree2.size) false
    else if (tree1 == tree2) true
    else{
        var wtree = Array.fill[Int](n)(0)
        for(offset<-1 until n){
            //move first element to the end
            for(i<-offset until n+offset) wtree(i-offset) = tree1(i%n)
            //then renumber based on witness order
            var k:Map[Int, Int] = Map()
            var c = 1
            wtree.indices.foreach{ i =>
                wtree(i) = k.getOrElse( wtree(i), {
                    k += (wtree(i) -> c)
                    c += 1
                    c-1
                })
            }
            //compare to tree2
            if(wtree.mkString("") == tree2) return true
        }
        //figure out counterclockwise if necessary?
        false
    }
}
def walk(tree1:String):Set[String]={
    val n = tree1.size
    var result:Set[String] = Set(tree1)
    var wtree = Array.fill[Int](n)(0)
    for(offset<-1 until n){
        //move first element to the end
        for(i<-offset until n+offset) wtree(i-offset) = tree1(i%n)
        //then renumber based on witness order
        var k:Map[Int, Int] = Map()
        var c = 1
        wtree.indices.foreach{ i =>
            wtree(i) = k.getOrElse( wtree(i), {
                k += (wtree(i) -> c)
                c += 1
                c-1
            })
        }
        result += wtree.mkString("")
    }
    result
}
def adjList_toTree(in:Array[List[Int]], out:Array[List[Int]]):String = {
    // the unlabeled, unrooted tree:
    // o-o-o-o                  1-2-3-4
    //   |          consider      |
    //   o                        5
    // is denoted 1-2-3-4-3-2-5-2
    // this is found by traversing the tree clockwise
    // and labeling each edge in the order it was witnessed
    val n = in.size
    val merge:Array[List[Int]] = in.indices.map{ i =>
        in(i) ++ out(i)
    }.toArray
    var result:List[Int] = List()
    val edges = Array.fill[Int](n)(0)
    var position = 0
    for(i<- 1 to 2*(n-1)){
        result +:= position
        //where can I go?
        val posibilities = merge(position)
        //anything that has not been taken yet?
        //otherwise, take one that has been taken
        val weights:List[(Int, Int)] = posibilities.map(p=> (p, edges(abs(position-p))))
        val next = weights.minBy(_._2)._1
        //increment where i'm going in edges
        val e = abs(position-next)
        edges(e) = edges(e) + 1
        //update position
        position = next
    }
    val wtree = result.reverse.toArray
    //now fix up the labels
    var k:Map[Int, Int] = Map()
    var c = 1
    wtree.indices.foreach{ i =>
        wtree(i) = k.getOrElse( wtree(i), {
            k += (wtree(i) -> c)
            c += 1
            c-1
        })
    }
    wtree.mkString("")
}
//O( 2^(n) )
def g(n:Int, display:Boolean = false) {
    //this matrix is configured such that i points to j
    //in a bipartite gracefully labeled matrix
    val inita:Array[List[Int]] = Array.fill(n)(List())
    val initb:Array[List[Int]] = Array.fill(n)(List())
    var unique:List[String] = List()
    def inSet(tree:String):Boolean = {
        val s = walk(tree)
        for(i<-unique) 
            if(s(i)) return true
        false
    }
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
    def f(in:Array[List[Int]], out:Array[List[Int]], current:Int = 0) {
        def emptyrow(i:Int):Boolean = out(i) == List.empty
        def emptycolumn(j:Int):Boolean = in(j) == List.empty
        def printA {
            //this just prints our matrix to the commandline
            this.synchronized{
                println("IN:  " + stringOf(in))
                println("OUT: " + stringOf(out))
            }
        }
        if(current == n-1) {
            //this is the base case, this diagonal should 
            //just be a row of zeros
            if(display) printA
            val t = adjList_toTree(in, out)
            if(t=="12341234") printA
            if(!inSet(t)) unique +:= t
        }
        else{
            sequence(current).foreach{ t:(Int, Int) =>
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
            }
        }
    }
    f(inita, initb)
    println(unique.size)
    println(stringOf(unique)) //comment me
}
println("example when n=4")
g(4, true)

// IN:  Array(List(), List(), List(), List(2, 0), List(2, 0))
// OUT: Array(List(3, 4), List(), List(3, 4), List(), List())
// IN:  Array(List(), List(), List(1, 0), List(), List(1, 0))
// OUT: Array(List(2, 4), List(2, 4), List(), List(), List())
// these are not valid bgl - find the issue