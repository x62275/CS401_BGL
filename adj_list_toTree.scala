import collection.immutable.Map
import Math.abs
def walk(tree1:String, tree2:String):Boolean ={
    // outputs: tree1==tree2
    // added to this document for testing adjList_toTree()
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

//added for testing adjList_toTree()
def test(in:Array[List[Int]], out:Array[List[Int]],tree2:String, expected:Boolean){
    val tree1=adjList_toTree(in, out)
    if(walk(tree1,tree2)==expected) print(".")
    else println(tree1,tree2)
}

var in:Array[List[Int]] = Array(List(), List(0), List(0), List(0))
var out:Array[List[Int]] = Array(List(1, 2, 3), List(), List(), List())
test(in, out, "121314", true)

in = Array(List(), List(), List(1, 0), List(0))
out = Array(List(2, 3), List(2), List(), List())
test(in, out, "123432", true)
println("")