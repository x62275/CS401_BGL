import collection.immutable.Map
def walk(tree1:String, tree2:String):Boolean ={
    //outputs: tree1==tree2
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
def test(tree1:String,tree2:String, expected:Boolean){
    if(walk(tree1,tree2)==expected) print(".")
    else println(tree1,tree2)
}
test("12343532", "12324542", true)
test("12343532", "12343252", true)
test("12324542", "12343252", true)
test("1234543632", "1234565432", false)
test("1234546432", "1234565432", false)
test("1234353632", "1234565432", false)
test("1234353262", "1234565432", false)
test("1232425262", "1234565432", false)
println("")