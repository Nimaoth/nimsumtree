import nimsumtree/sumtree

export sumtree

when isMainModule:
  import "../tests/test_summary"

  # var tree = SumTree[int].new([0, 1, 1, 2, 3, 5, 8, 13, 21, 34])
  var tree = SumTree[int].new(@[8, 0, 5, 1, 21, 3, 34, 2, 1, 13, 2])
  var tree2 = SumTree[int].new(@[6, 4, 12, 9])
  # var tree = SumTree[int, treeChildrenMax].new([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  # var tree2 = SumTree[int, treeChildrenMax].new([15, 16, 17, 18, 19])
  echo tree.pretty
  echo tree2.pretty

  proc testCursor[T](steps: seq[(T, Bias)]) =
    var cursor = tree.initCursor (Count, Max)
    var cursor = tree.initCursor (Count, Max)
    var cursor = tree.initCursor (Count, Max)
    var cursor = tree.initCursor (Count, Max)

    for i, s in steps:
      let prefix = if i == 0: "--- " else: "    "
      # echo prefix, s, ", ", cursor.seekForward(s[0], s[1]), " -> ", cursor.item, " | ", cursor.itemSummary, " | ", cursor
      echo prefix, s, ", ", cursor.summary(Max, s[0], s[1]), ", ", cursor.first, ", ", cursor.last, " -> ", cursor.itemClone, " | ", cursor.itemSummary, " | ", cursor

  # testCursor @[(0.Count, Left)]
  # testCursor @[(1.Count, Left)]
  # testCursor @[(2.Count, Left)]
  # testCursor @[(3.Count, Left)]
  # testCursor @[(4.Count, Left)]
  # testCursor @[(5.Count, Left)]
  # testCursor @[(6.Count, Left)]
  # testCursor @[(7.Count, Left)]
  # testCursor @[(8.Count, Left)]

  testCursor @[(0.Count, Right)]
  testCursor @[(1.Count, Right)]
  testCursor @[(2.Count, Right)]
  testCursor @[(3.Count, Right)]
  testCursor @[(4.Count, Right)]
  testCursor @[(5.Count, Right)]
  testCursor @[(6.Count, Right)]
  testCursor @[(7.Count, Right)]
  testCursor @[(8.Count, Right)]

  # testCursor @[(2.Count, Left), (5.Count, Left), (9.Count, Left), (10.Count, Left), (11.Count, Left)]
  # testCursor @[(2.Count, Right), (5.Count, Right), (9.Count, Right), (10.Count, Right), (11.Count, Right)]

  # testCursor @[(0.Max, Left)]
  # testCursor @[(1.Max, Left)]
  # testCursor @[(2.Max, Left)]
  # testCursor @[(3.Max, Left)]
  # testCursor @[(4.Max, Left)]
  # testCursor @[(5.Max, Left)]


  var cursor = tree.initCursor (Count, Max)
  echo cursor.seekForward(3.Count, Right, ()), " -> ", cursor.itemClone, " | ", cursor.itemSummary, " | ", cursor


  echo "--- tree 1"
  echo tree.pretty
  # echo tree.leftmostLeaf.pretty
  # echo tree.rightmostLeaf.pretty

  echo "--- tree 2"
  echo tree2.pretty
  echo "--- append"
  # let tree3 = tree.clone()
  tree.append(tree2.move, ())
  # tree2.append tree.move
  echo "--- tree new"
  echo tree.pretty

  echo "--- old"
  # echo tree3.pretty

  var a = Arc[int].new()
  a.getMut.addr[] = 5
  echo $a
