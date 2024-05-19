import nimsumtree/sumtree
export sumtree

when isMainModule:
  type Count = distinct int
  type Max = distinct int
  type TestSummary = object
    count: Count
    max: Max
    # zeroes: int

  func clone*(a: TestSummary): TestSummary = a

  func `$`*(a: Count): string {.borrow.}
  func `+=`*(a: var Count, b: Count) {.borrow.}
  func addSummary*(a: var Count, b: Count) = a = (a.int + b.int).Count
  func clone*(a: Count): Count = a
  func cmp*(a: Count, b: Count): int = cmp(a.int, b.int)

  func addSummary*(a: var Count, b: TestSummary) = a += b.count
  func fromSummary*(_: typedesc[Count], a: TestSummary): Count = a.count

  func `$`*(a: Max): string {.borrow.}
  func `+=`*(a: var Max, b: Max) = a = max(a.int, b.int).Max
  func addSummary*(a: var Max, b: Max) = a = max(a.int, b.int).Max
  func clone*(a: Max): Max = a
  func cmp*(a: Max, b: Max): int = cmp(a.int, b.int)

  func addSummary*(a: var Max, b: TestSummary) = a += b.max
  func fromSummary*(_: typedesc[Max], a: TestSummary): Max = a.max

  func `+=`*(a: var (Count, Max), b: (Count, Max)) =
    a[0] += b[0]
    a[1] += b[1]
  func addSummary*(a: var (Count, Max), b: (Count, Max)) =
    a[0] += b[0]
    a[1] += b[1]
  func clone*(a: (Count, Max)): (Count, Max) = (a[0].clone(), a[1].clone())
  func cmp*(a: Count, b: (Count, Max)): int = cmp(a.int, b[0].int)

  func addSummary*(a: var (Count, Max), b: TestSummary) =
    a[0] += b.count
    a[1] = max(a[1].int, b.max.int).Max

  func fromSummary*(_: typedesc[(Count, Max)], a: TestSummary): (Count, Max) = (a.count, a.max)

  func `+=`*(a: var TestSummary, b: TestSummary) =
    a.count += b.count
    a.max = max(a.max.int, b.max.int).Max
    # a.zeroes += b.zeroes

  func addSummary*(a: var TestSummary, b: TestSummary) =
    a.count += b.count
    a.max = max(a.max.int, b.max.int).Max
    # a.zeroes += b.zeroes

  # func summary*(x: int): TestSummary = TestSummary(count: 1, max: x, zeroes: if x == 0: 1 else: 0)
  func summary*(x: int): TestSummary = TestSummary(count: 1.Count, max: x.Max)

  # var tree = SumTree[int].new([0, 1, 1, 2, 3, 5, 8, 13, 21, 34])
  # var tree = SumTree[int].new([8, 0, 5, 1, 21, 3, 34, 2, 1, 13, 2])
  # var tree2 = SumTree[int].new([6, 4, 12, 9])
  var tree = SumTree[int].new([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  var tree2 = SumTree[int].new([15, 16, 17, 18, 19])
  echo tree.pretty
  echo tree2.pretty

  proc testCursor[T: Dimension](steps: seq[(T, Bias)]) =
    var cursor = tree.initCursor (Count, Max)

    for i, s in steps:
      let prefix = if i == 0: "--- " else: "    "
      # echo prefix, s, ", ", cursor.seekForward(s[0], s[1]), " -> ", cursor.item, " | ", cursor.itemSummary, " | ", cursor
      echo prefix, s, ", ", cursor.summary(Max, s[0], s[1]), ", ", cursor.first, ", ", cursor.last, " -> ", cursor.item, " | ", cursor.itemSummary, " | ", cursor

  # testCursor @[(0.Count, Left)]
  # testCursor @[(1.Count, Left)]
  # testCursor @[(2.Count, Left)]
  # testCursor @[(3.Count, Left)]
  # testCursor @[(4.Count, Left)]
  # testCursor @[(5.Count, Left)]
  # testCursor @[(6.Count, Left)]
  # testCursor @[(7.Count, Left)]
  # testCursor @[(8.Count, Left)]

  # testCursor @[(0.Count, Right)]
  # testCursor @[(1.Count, Right)]
  # testCursor @[(2.Count, Right)]
  # testCursor @[(3.Count, Right)]
  # testCursor @[(4.Count, Right)]
  # testCursor @[(5.Count, Right)]
  # testCursor @[(6.Count, Right)]
  # testCursor @[(7.Count, Right)]
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
  echo cursor.seekForward(3.Count, Right), " -> ", cursor.item, " | ", cursor.itemSummary, " | ", cursor


  echo "--- tree 1"
  echo tree.pretty
  # echo tree.leftmostLeaf.pretty
  # echo tree.rightmostLeaf.pretty

  echo "--- tree 2"
  echo tree2.pretty
  echo "--- append"
  # let tree3 = tree.clone()
  tree.append tree2.move
  # tree2.append tree.move
  echo "--- tree new"
  echo tree.pretty

  echo "--- old"
  # echo tree3.pretty
