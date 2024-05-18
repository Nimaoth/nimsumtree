import std/[macros, strformat, sequtils, options, strutils]

const treeBase = 1
const treeChildrenMax = treeBase * 2

type
  Arc*[T] = object
    value: ref T

  Summary* {.explain.} = concept var a, b
    a += b

  Item* {.explain.} = concept a, type T
    a.summary is Summary

  Dimension*[S: Summary] {.explain.} = concept var a, type T
    a.addSummary(Summary)
    a.clone() is T
    T.fromSummary(Summary) is T

  # SeekTarget*[S: Summary, D: Dimension[S]] {.explain.} = concept a, type S, type D
  #   S is Summary
  #   D is Dimension[S]
  #   var b: D
  #   cmp(a, b) is int

  Bias* = enum Left, Right

template summaryType*(T: typedesc[Item]): untyped = typeof(T.default.summary)

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

  NodeKind* = enum Internal, Leaf
  Node*[T: Item] = object
    mSummary: T.summaryType
    mSummaries: seq[T.summaryType]
    case kind: NodeKind
    of Internal:
      mHeight: uint8
      mChildren: seq[SumTree[T]]
    of Leaf:
      mItems: seq[T]

  SumTree*[T: Item] = distinct Arc[Node[T]]

proc `=copy`*[T](a: var Arc[T], b: Arc[T]) {.error.}
proc `=dup`*[T](a: Arc[T]): Arc[T] {.error.}

proc `=copy`*[T](a: var Node[T], b: Node[T]) {.error.}
proc `=dup`*[T](a: Node[T]): Node[T] {.error.}

proc `=copy`*[T](a: var SumTree[T], b: SumTree[T]) {.error.}
proc `=dup`*[T](a: SumTree[T]): SumTree[T] {.error.}

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

func height*[T: Item](node: Node[T]): int =
  case node.kind
  of Internal:
    node.mHeight
  of Leaf:
    0

func summary*[T: Item](node: Node[T]): T.summaryType = node.mSummary

template childSummaries*[T: Item](node: Node[T]): untyped =
  node.mSummaries.toOpenArray(0, node.mSummaries.high)

template childTrees*[T: Item](node: Node[T]): untyped =
  node.mChildren.toOpenArray(0, node.mChildren.high)

template childItems*[T: Item](node: Node[T]): untyped =
  node.mItems.toOpenArray(0, node.mItems.high)

func isUnderflowing*[T](node: Node[T]): bool =
  case node.kind
  of Internal:
    node.mChildren.len < treeBase
  of Leaf:
    node.mItems.len < treeBase

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
  result.value = new T
  result.value[] = T.default

func new*[T](_: typedesc[Arc[T]], default: sink T): Arc[T] =
  result.value = new T
  result.value[] = default.move

func new*[T: Item](_: typedesc[SumTree[T]]): SumTree[T] =
  SumTree[T](Arc[Node[T]].new(newLeaf[T]()))

func new*[T: Item](_: typedesc[SumTree[T]], items: openArray[T]): SumTree[T] =
  mixin summary
  mixin `+=`

  var nodes: seq[Node[T]]
  var i = 0
  while i < items.len:
    let endIndex = min(i + treeChildrenMax, items.len)
    let subItems: seq[T] = items[i..<endIndex]
    i = endIndex

    let summaries: seq[T.summaryType] = subItems.mapIt(it.summary)
    var s: T.summaryType = summaries[0]
    for k in 1..summaries.high:
      s += summaries[k]

    nodes.add Node[T](kind: Leaf, mSummary: s, mItems: subItems, mSummaries: summaries)

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

func initCursor*[T: Item](tree {.byref.}: SumTree[T], D: typedesc[Dimension]): Cursor[T, D] =
  result.tree = tree
  result.position = D.default
  result.atEnd = tree.isEmpty

func assertDidSeek*(self: Cursor) =
  assert self.didSeek

func reset*(self: var Cursor) =
  self.didSeek = false
  self.atEnd = self.tree.isEmpty
  self.stack.setLen 0
  self.position = typeof(self.position).default

proc seekInternal[T: Item, D: Dimension[T.summaryType], Target, Aggregate](
    self: var Cursor[T, D], target: Target, bias: Bias, aggregate: Aggregate): bool =

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
          break

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
              # aggregate.pushTree(childTree, childSummary) #todo
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
          # aggregate.beginLeaf() # todo

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
              # aggregate.pushItem(item, itemSummary) #todo
              entry.index += 1
              debugf"index: {entry.index}, {self.stack[self.stack.high]}"

            else:
              debugf"found?"
              # aggregate.endLeaf() # todo
              break outer

          # aggregate.endLeaf() # todo

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

  echo &"{target} <> {endPosition} -> {target.cmp(endPosition)}"
  return target.cmp(endPosition) == 0

proc seekForward*[T: Item, D: Dimension[T.summaryType], Target](
    self: var Cursor[T, D], target: Target, bias: Bias): bool =
  self.seekInternal(target, bias, ())

func itemSummary*[T: Item, D: Dimension[T.summaryType]](self: Cursor[T, D]): Option[T.summaryType] =
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

func item*[T: Item, D: Dimension[T.summaryType]](self: Cursor[T, D]): Option[T] =
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

  func `$`(a: Count): string {.borrow.}
  func `+=`(a: var Count, b: Count) {.borrow.}
  func clone(a: Count): Count = a
  func cmp*(a: Count, b: Count): int =
    cmp(a.int, b.int)

  func `$`(a: Max): string {.borrow.}
  func `+=`(a: var Max, b: Max) {.borrow.}
  func clone(a: Max): Max = a
  func cmp*(a: Max, b: Max): int =
    cmp(a.int, b.int)

  func `+=`*(a: var TestSummary, b: TestSummary) =
    a.count += b.count
    a.max = max(a.max.int, b.max.int).Max
    # a.zeroes += b.zeroes

  func addSummary(a: var Count, b: TestSummary) = a += b.count
  func fromSummary(_: typedesc[Count], a: TestSummary): Count = a.count

  func addSummary(a: var Max, b: TestSummary) = a += b.max
  func fromSummary(_: typedesc[Max], a: TestSummary): Max = a.max

  # func summary*(x: int): TestSummary = TestSummary(count: 1, max: x, zeroes: if x == 0: 1 else: 0)
  func summary*(x: int): TestSummary = TestSummary(count: 1.Count, max: x.Max)

  var tree = SumTree[int].new([0, 1, 1, 2, 3, 5, 8, 13, 21, 34])
  echo tree.pretty

  proc testCursor[T: Dimension](steps: seq[(T, Bias)]) =
    var cursor = tree.initCursor T

    for i, s in steps:
      let prefix = if i == 0: "--- " else: "    "
      echo prefix, s, ", ", cursor.seekForward(s[0], s[1]), " -> ", cursor.item, " | ", cursor.itemSummary, " | ", cursor

  testCursor @[(0.Count, Left)]
  testCursor @[(1.Count, Left)]
  testCursor @[(2.Count, Left)]
  testCursor @[(3.Count, Left)]
  testCursor @[(4.Count, Left)]
  testCursor @[(5.Count, Left)]

  testCursor @[(0.Count, Right)]
  testCursor @[(1.Count, Right)]
  testCursor @[(2.Count, Right)]
  testCursor @[(3.Count, Right)]
  testCursor @[(4.Count, Right)]
  testCursor @[(5.Count, Right)]

  testCursor @[(2.Count, Left), (5.Count, Left), (9.Count, Left), (10.Count, Left), (11.Count, Left)]
  testCursor @[(2.Count, Right), (5.Count, Right), (9.Count, Right), (10.Count, Right), (11.Count, Right)]