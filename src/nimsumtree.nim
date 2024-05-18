import std/[macros, strformat, sequtils, options, strutils]

const treeBase = 1
const treeChildrenMax = treeBase * 2

type
  Arc*[T] = object
    count: ref int
    value: ref T

  Array*[T; Cap: static int] = object
    data: array[Cap, T]
    len: int8

  Summary* {.explain.} = concept var a, b
    a.addSummary(b)

  Item* {.explain.} = concept a, type T
    a.summary is Summary

  Dimension*[S: Summary] {.explain.} = concept var a, b, type T
    a.addSummary(Summary)
    b.clone() is T
    T.fromSummary(Summary) is T

  # SeekTarget*[S: Summary, D: Dimension[S]] {.explain.} = concept a, type S, type D
  #   S is Summary
  #   D is Dimension[S]
  #   var b: D
  #   cmp(a, b) is int

  Bias* = enum Left, Right

template summaryType*(T: typedesc[Item]): untyped = typeof(T.default.summary)
template summaryArrayType*(T: typedesc[Item]): untyped = Array[typeof(T.default.summary), treeChildrenMax]

type
  StackEntry*[T: Item, D] = object
    tree {.cursor.}: SumTree[T]
    index: int
    position: D

  Cursor*[T: Item, D] = object
    tree {.cursor.}: SumTree[T]
    stack: seq[StackEntry[T, D]]
    position: D
    didSeek: bool
    atEnd: bool

  ItemArray*[T: Item] = Array[T, treeChildrenMax]
  ChildArray*[T: Item] = Array[SumTree[T], treeChildrenMax]
  SummaryArray*[T] = Array[T, treeChildrenMax]

  NodeKind* = enum Internal, Leaf
  Node*[T: Item] = object
    mSummary: T.summaryType
    mSummaries: T.summaryArrayType
    case kind: NodeKind
    of Internal:
      mHeight: uint8
      mChildren: ChildArray[T]
    of Leaf:
      mItems: ItemArray[T]

  SumTree*[T: Item] = distinct Arc[Node[T]]

  SeekAggregate* {.explain.} = concept var a
    type T = Item
    type S = Summary
    beginLeaf(a)
    endLeaf(a)
    pushItem(a, T, S)
    pushTree(a, SumTree[T], S)

proc `=copy`*[T](a: var Arc[T], b: Arc[T]) {.error.}
proc `=dup`*[T](a: Arc[T]): Arc[T] {.error.}

proc `=copy`*[T](a: var Node[T], b: Node[T]) {.error.}
proc `=dup`*[T](a: Node[T]): Node[T] {.error.}

proc `=copy`*[T](a: var SumTree[T], b: SumTree[T]) {.error.}
proc `=dup`*[T](a: SumTree[T]): SumTree[T] {.error.}

func initArray*(T: typedesc, capacity: static int): Array[T, capacity] =
  result = Array[T, capacity].default

template low*[T; C: static int](arr: Array[T, C]): int =
  0

template high*[T; C: static int](arr: Array[T, C]): int =
  int(arr.len - 1)

func `[]`*[T; C: static int](arr {.byref.}: Array[T, C], index: int): lent T =
  assert index >= 0
  assert index < C
  return arr.data[index]

func add*[T; C: static int](arr: var Array[T, C], val: sink T) =
  assert arr.len < C
  arr.data[arr.len.int] = val.move
  inc arr.len

macro evalOnceAs(expAlias, exp: untyped,
                 letAssigneable: static[bool]): untyped =
  ## Injects `expAlias` in caller scope, to avoid bugs involving multiple
  ## substitution in macro arguments such as
  ## https://github.com/nim-lang/Nim/issues/7187.
  ## `evalOnceAs(myAlias, myExp)` will behave as `let myAlias = myExp`
  ## except when `letAssigneable` is false (e.g. to handle openArray) where
  ## it just forwards `exp` unchanged.
  expectKind(expAlias, nnkIdent)
  var val = exp

  result = newStmtList()
  # If `exp` is not a symbol we evaluate it once here and then use the temporary
  # symbol as alias
  if exp.kind != nnkSym and letAssigneable:
    val = genSym()
    result.add(newLetStmt(val, exp))

  result.add(
    newProc(name = genSym(nskTemplate, $expAlias), params = [getType(untyped)],
      body = val, procType = nnkTemplateDef))

template mapIt*[T; C: static int](arr: Array[T, C], op: untyped): untyped =
  block:
    type OutType = typeof((
      block:
        var it {.inject.}: typeof(arr.data[0], typeOfProc);
        op), typeOfProc)

    evalOnceAs(arr2, arr, compiles((let _ = s)))

    var res: Array[OutType, C]
    res.len = arr2.len

    for i in 0..<arr2.len:
      var it {.inject, cursor.} = arr2.data[i]
      res.data[i] = op

    res

proc beginLeaf*(a: var tuple[]) = discard
proc endLeaf*(a: var tuple[]) = discard
proc pushItem*[T; S](a: var tuple[], item: T, summary: S) = discard
proc pushTree*[T; S](a: var tuple[], tree: SumTree[T], summary: S) = discard

type SummarySeekAggregate*[D] = object
  value: D

proc beginLeaf*[D](a: var SummarySeekAggregate[D]) = discard
proc endLeaf*[D](a: var SummarySeekAggregate[D]) = discard

proc pushItem*[D; T; S](a: var SummarySeekAggregate[D], item: T, summary: S) =
  a.value.addSummary(summary)

proc pushTree*[D; T; S: Summary](a: var SummarySeekAggregate[D], tree: SumTree[T], summary: S) =
  a.value.addSummary(summary)

proc `$`*[T](arc: Arc[T]): string = &"Arc({arc.value[]})"
proc `$`*[T: Item](node: Node[T]): string =
  case node.kind:
  of Internal:
    &"Internal({node.mSummary}, {node.mChildren})"
  of Leaf:
    &"Leaf({node.mSummary}, {node.mItems})"
proc `$`*[T: Item](tree: SumTree[T]): string = $Arc[Node[T]](tree)
proc `$`*(entry: StackEntry): string = &"(p: {entry.position}, i: {entry.index})"
proc `$`*(cursor: Cursor): string = &"Cursor(p: {cursor.position}, s: {cursor.didSeek}, e: {cursor.atEnd}, st: {cursor.stack})"

func pretty*[T: Item](node {.byref.}: Node[T]): string =
  case node.kind:
  of Internal:
    result = &"Internal({node.mSummary}, {node.mChildren.len}):\n"
    for i in 0..node.mChildren.high:
      if i > 0:
        result.add "\n"
      result.add node.mChildren[i].asNode.pretty.indent(2)

  of Leaf:
    result = &"Leaf({node.mSummary}, {node.mItems})"

func pretty*[T: Item](tree: SumTree[T]): string = Arc[Node[T]](tree).value[].pretty

func invert*(bias: Bias): Bias =
  case bias
  of Left: Right
  of Right: Left

func isLeaf*[T: Item](node: Node[T]): bool = node.kind == Leaf
func isInternal*[T: Item](node: Node[T]): bool = node.kind == Internal

func height*[T: Item](node: Node[T]): uint8 =
  case node.kind
  of Internal:
    node.mHeight
  of Leaf:
    0

func summary*[T: Item](node: Node[T]): T.summaryType = node.mSummary

template childSummaries*[T: Item](node: Node[T]): untyped =
  node.mSummaries.data.toOpenArray(0, node.mSummaries.high)

template childTrees*[T: Item](node: Node[T]): untyped =
  node.mChildren.data.toOpenArray(0, node.mChildren.high)

template childItems*[T: Item](node: Node[T]): untyped =
  node.mItems.data.toOpenArray(0, node.mItems.high)

func isUnderflowing*[T](node: Node[T]): bool =
  case node.kind
  of Internal:
    node.mChildren.len < treeBase
  of Leaf:
    node.mItems.len < treeBase

template asArc*[T: Item](tree: SumTree[T]): Arc[Node[T]] =
  Arc[Node[T]](tree)

func asNode*[T: Item](tree: SumTree[T]): lent Node[T] =
  Arc[Node[T]](tree).value[]

func isEmpty*[T: Item](tree: SumTree[T]): bool =
  case tree.asNode.kind
  of Internal:
    false
  of Leaf:
    tree.asNode.mItems.len == 0

func newLeaf*[T](): Node[T] =
  type Summary = T.summaryType
  Node[T](kind: Leaf, mSummary: Summary.default)

func new*[T](_: typedesc[Arc[T]]): Arc[T] =
  result.count = new int
  result.count[] = 1
  result.value = new T
  result.value[] = T.default

proc clone*[T](a {.byref.}: Arc[T]): Arc[T] =
  result.count = a.count
  result.count[] += 1
  result.value = a.value

func new*[T](_: typedesc[Arc[T]], default: sink T): Arc[T] =
  result.count = new int
  result.count[] = 1
  result.value = new T
  result.value[] = default.move

proc getUnique*[T](a: var Arc[T]): lent T =
  if a.count[] > 1:
    a = a.clone()
  return a.value[]

func new*[T: Item](_: typedesc[SumTree[T]]): SumTree[T] =
  SumTree[T](Arc[Node[T]].new(newLeaf[T]()))

proc clone*[T](a {.byref.}: SumTree[T]): SumTree[T] =
  SumTree[T](Arc[Node[T]](a).clone())

proc getUnique*[T](a: var SumTree[T]): lent Node[T] =
  if a.asArc.count[] > 1:
    a = a.clone()
  return a.asArc.value[]

func new*[T: Item](_: typedesc[SumTree[T]], items: openArray[T]): SumTree[T] =
  mixin summary
  mixin `+=`
  mixin addSummary

  var nodes: seq[Node[T]]
  var i = 0
  while i < items.len:
    let endIndex = min(i + treeChildrenMax, items.len)
    var subItems: ItemArray[T]
    subItems.len = int8(endIndex - i)
    for k in 0..<subItems.len.int:
      subItems.data[k] = items[i + k]

    i = endIndex

    var summaries: SummaryArray[T.summaryType] = subItems.mapIt(it.summary)
    var s: T.summaryType = summaries[0].clone()
    for k in 1..summaries.high:
      s += summaries[k]

    nodes.add Node[T](kind: Leaf, mSummary: s, mItems: subItems, mSummaries: summaries.move)

  var parentNodes: seq[Node[T]] = @[]
  var height: uint8 = 0
  while nodes.len > 1:
    inc height
    var currentParentNode = Node[T].none
    var tempNodes = nodes.move
    for childNode in tempNodes.mitems:
      if currentParentNode.isNone:
        currentParentNode = some Node[T](kind: Internal, mSummary: T.summaryType.default, mHeight: height)

      let childSummary = childNode.summary
      currentParentNode.get.mSummary += childSummary
      currentParentNode.get.mSummaries.add childSummary
      currentParentNode.get.mChildren.add SumTree[T](Arc[Node[T]].new(childNode.move))

      if currentParentNode.get.mChildren.len == treeChildrenMax:
        parentNodes.add currentParentNode.get.move
        currentParentNode = Node[T].none

    if currentParentNode.isSome:
      parentNodes.add currentParentNode.get.move
      currentParentNode = Node[T].none

    nodes = parentNodes.move

  if nodes.len == 0:
    SumTree[T](Arc[Node[T]].new(newLeaf[T]()))
  else:
    assert nodes.len == 1
    SumTree[T](Arc[Node[T]].new(nodes[0].move))

func leftmostLeaf*[T: Item](tree {.byref.}: SumTree[T]): lent SumTree[T] =
  case tree.asNode.kind
  of Leaf:
    result = tree
  else:
    result = tree.asNode.mChildren[0].leftmostLeaf

func rightmostLeaf*[T: Item](tree {.byref.}: SumTree[T]): lent SumTree[T] =
  case tree.asNode.kind
  of Leaf:
    result = tree
  else:
    result = tree.asNode.mChildren[tree.asNode.mChildren.high].rightmostLeaf

proc pushTreeRecursive*[T: Item](tree: var SumTree[T], other: sink SumTree[T]): Option[SumTree[T]] =
  var node {.cursor.} = tree.getUnique()
  case node.kind
  of Internal:
    let otherTree = other.clone
    let otherNode {.cursor.} = otherTree.asNode

    node.mSummary.addSummary(otherNode.mSummary)

    let heightDelta = node.mHeight - otherNode.mHeight
    var summariesToAppend: seq[T.summaryType] = @[]
    var treesToAppend: seq[SumTree[T]] = @[]


    discard
  of Leaf:
    echo "--- leaf"
    discard

func fromChildTrees*[T: Item](_: typedesc[SumTree[T]], left: sink SumTree[T], right: sink SumTree[T]): SumTree[T] =
  let height = left.asNode.height + 1

  var childSummaries: SummaryArray[T.summaryType]
  childSummaries.add(left.asNode.mSummary.clone())
  childSummaries.add(right.asNode.mSummary.clone())

  var sum = childSummaries[0].clone()
  for i in 1..childSummaries.high:
    sum.addSummary(childSummaries[i])

  var childTrees: ChildArray[T]
  childTrees.add left.move
  childTrees.add right.move

  SumTree[T](
    Arc[Node[T]].new(
      Node[T](
        kind: Internal,
        mHeight: height,
        mSummary: sum,
        mSummaries: childSummaries.move,
        mChildren: childTrees.move,
      )
    )
  )

proc append*[T: Item](tree: var SumTree[T], other: sink SumTree[T]) =
  if tree.isEmpty:
    tree = other.move
  elif other.asNode.isInternal or other.asNode.mItems.len > 0:
    if tree.asNode.height < other.asNode.height:
      assert other.asNode.isInternal
      for tree in other.asNode.childTrees:
        discard

    else:
      var splitTree = tree.pushTreeRecursive(other.move)
      if splitTree.isSome:
        tree = SumTree[T].fromChildTrees(tree.clone(), splitTree.get.move)

func initCursor*[T: Item](tree {.byref.}: SumTree[T], D: typedesc[Dimension]): Cursor[T, D] =
  result.tree = tree
  result.position = D.default
  result.atEnd = tree.isEmpty

func assertDidSeek*(self: Cursor) =
  assert self.didSeek

func reset*(self: var Cursor) =
  ## Reset the cursor to the beginning
  self.didSeek = false
  self.atEnd = self.tree.isEmpty
  self.stack.setLen 0
  self.position = typeof(self.position).default

proc seekInternal[T: Item; D: Dimension[T.summaryType]; Target; Aggregate](
    self: var Cursor[T, D], target: Target, bias: Bias, aggregate: var Aggregate): bool =

  template debugf(str: string): untyped =
    # echo "  ".repeat(self.stack.len) & &(str)
    discard

  if not self.didSeek:
    self.didSeek = true
    self.stack.add StackEntry[T, D](
      tree: self.tree,
      index: 0,
      position: D.default,
    )

  var iterations = 0

  var ascending = false
  block outer:
    while self.stack.len > 0:
      defer:
        inc iterations
      block inner:

        if iterations == 15:
          break outer

        debugf"loop {self.stack.len}, ascending: {ascending}, {self}"

        template entry: untyped = self.stack[self.stack.high]
        let node {.cursor.} = entry.tree.asNode

        case node.kind
        of Internal:
          if ascending:
            debugf"ascending 1: {entry.index}, {self.stack[self.stack.high].index}"
            entry.index += 1
            debugf"ascending 2: {entry.index}, {self.stack[self.stack.high].index}"
            entry.position = self.position.clone()

          for i in entry.index..node.mChildren.high:
            let childTree {.cursor.} = node.mChildren[i]
            let childSummary {.cursor.} = node.mSummaries[i]

            debugf"child {i}: {childSummary}"

            var childEnd = self.position.clone()
            debugf"childEnd: {childEnd}"
            childEnd.addSummary(childSummary)
            debugf"childEnd: {childEnd}"

            let comparison = target.cmp(childEnd)
            debugf"cmp: {target} <> {childEnd} -> {comparison}"
            if comparison > 0 or (comparison == 0 and bias == Right):
              debugf"ahead of target"
              self.position = childEnd
              aggregate.pushTree(childTree, childSummary)
              entry.index += 1
              debugf"index: {entry.index}, {self.stack[self.stack.high]}"
              entry.position = self.position.clone()

            else:
              debugf"behind target, enter child"
              self.stack.add StackEntry[T, D](
                tree: childTree,
                index: 0,
                position: self.position.clone(),
              )
              ascending = false
              break inner

        of Leaf:
          debugf"leaf: {node.mItems}"
          aggregate.beginLeaf()

          for i in entry.index..node.mItems.high:
            let item {.cursor.} = node.mItems[i]
            let itemSummary {.cursor.} = node.mSummaries[i]

            debugf"item: {item}, {itemSummary}"
            var childEnd = self.position.clone()
            childEnd.addSummary(itemSummary)

            let comparison = target.cmp(childEnd)
            debugf"cmp: {target} <> {childEnd} -> {comparison}"
            if comparison > 0 or (comparison == 0 and bias == Right):
              debugf"ahead of target"
              self.position = childEnd
              aggregate.pushItem(item, itemSummary)
              entry.index += 1
              debugf"index: {entry.index}, {self.stack[self.stack.high]}"

            else:
              debugf"found?"
              aggregate.endLeaf()
              break outer

          aggregate.endLeaf()

        discard self.stack.pop()
        ascending = true

  # After while
  self.atEnd = self.stack.len == 0
  assert self.stack.len == 0 or self.stack[self.stack.high].tree.asNode.isLeaf

  var endPosition = self.position.clone()
  if bias == Left:
    let sum = self.itemSummary()
    if sum.isSome:
      endPosition.addSummary(sum.get)

  # echo &"{target} <> {endPosition} -> {target.cmp(endPosition)}"
  return target.cmp(endPosition) == 0

proc seekForward*[T: Item; D: Dimension[T.summaryType]; Target](
    self: var Cursor[T, D], target: Target, bias: Bias): bool =
  ## Moves the cursor to the target

  var agg = ()
  self.seekInternal(target, bias, agg)

proc summary*[T: Item; D: Dimension[T.summaryType]; Target](
    self: var Cursor[T, D], Output: typedesc[Dimension], `end`: Target, bias: Bias): Output =
  ## Advances the cursor to `end` and returns the aggregated value of the `Output` dimension
  ## up until, but not including `end`

  var summary = SummarySeekAggregate[Output](value: Output.default)
  discard self.seekInternal(`end`, bias, summary)
  summary.value.move

func itemSummary*[T: Item, D: Dimension[T.summaryType]](self: Cursor[T, D]): Option[T.summaryType] =
  ## Returns the summary of the current item, or none if the cursor is past the end

  self.assertDidSeek
  if self.stack.len > 0:
    let entry {.cursor.} = self.stack[self.stack.high]
    let node {.cursor.} = entry.tree.asNode
    case node.kind
    of Leaf:
      if entry.index == node.mSummaries.len:
        return T.summaryType.none
      else:
        return node.mSummaries[entry.index].some
    of Internal:
      assert false, "Stack top should contain leaf node"

  return T.summaryType.none

proc first[T: Item; D: Dimension[T.summaryType]](self: Cursor[T, D]): lent D =
  ## Returns the aggregated value up until, but not including the current node
  self.position

proc last[T: Item; D: Dimension[T.summaryType]](self: Cursor[T, D]): D =
  ## Returns the aggregated value of the current node

  let summary = self.itemSummary
  if summary.isSome:
    result = self.position.clone()
    result.addSummary(summary.get)
  else:
    result = self.position.clone()

func item*[T: Item, D: Dimension[T.summaryType]](self: Cursor[T, D]): Option[T] =
  # Returns the current item, or none if path the end

  self.assertDidSeek
  if self.stack.len > 0:
    let entry {.cursor.} = self.stack[self.stack.high]
    let node {.cursor.} = entry.tree.asNode
    case node.kind
    of Leaf:
      if entry.index == node.mItems.len:
        return T.none
      else:
        return node.mItems[entry.index].some
    of Internal:
      assert false, "Stack top should contain leaf node"

  return T.none

when isMainModule:
  type Count = distinct int
  type Max = distinct int
  type TestSummary = object
    count: Count
    max: Max
    # zeroes: int

  func clone(a: TestSummary): TestSummary = a

  func `$`(a: Count): string {.borrow.}
  func `+=`(a: var Count, b: Count) {.borrow.}
  func addSummary(a: var Count, b: Count) = a = (a.int + b.int).Count
  func clone(a: Count): Count = a
  func cmp*(a: Count, b: Count): int = cmp(a.int, b.int)

  func addSummary(a: var Count, b: TestSummary) = a += b.count
  func fromSummary(_: typedesc[Count], a: TestSummary): Count = a.count

  func `$`(a: Max): string {.borrow.}
  func `+=`(a: var Max, b: Max) = a = max(a.int, b.int).Max
  func addSummary(a: var Max, b: Max) = a = max(a.int, b.int).Max
  func clone(a: Max): Max = a
  func cmp*(a: Max, b: Max): int = cmp(a.int, b.int)

  func addSummary(a: var Max, b: TestSummary) = a += b.max
  func fromSummary(_: typedesc[Max], a: TestSummary): Max = a.max

  func `+=`(a: var (Count, Max), b: (Count, Max)) =
    a[0] += b[0]
    a[1] += b[1]
  func addSummary(a: var (Count, Max), b: (Count, Max)) =
    a[0] += b[0]
    a[1] += b[1]
  func clone(a: (Count, Max)): (Count, Max) = (a[0].clone(), a[1].clone())
  func cmp*(a: Count, b: (Count, Max)): int = cmp(a.int, b[0].int)

  func addSummary(a: var (Count, Max), b: TestSummary) =
    a[0] += b.count
    a[1] = max(a[1].int, b.max.int).Max

  func fromSummary(_: typedesc[(Count, Max)], a: TestSummary): (Count, Max) = (a.count, a.max)

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
  var tree = SumTree[int].new([8, 0, 5, 1, 21, 3, 34, 2, 1, 13, 2])
  var tree2 = SumTree[int].new([6, 4, 12, 9])
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
  # testCursor @[(8.Count, Right)]

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
  tree.append tree2.clone()
  echo "--- tree new"
  echo tree.pretty
