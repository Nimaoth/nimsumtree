import std/[macros, strformat, sequtils, options, strutils]

type
  Arc*[T] = object
    value: ref T

  Summary* {.explain.} = concept var a, b
    a += b

  Item* {.explain.} = concept a, type T
    a.summary is Summary

  Dimension*[S: Summary] {.explain.} = concept var a, type T
    a.addSummary(Summary)
    T.fromSummary(Summary) is T

  Bias* = enum Left, Right

proc `=copy`*[T](a: var Arc[T], b: Arc[T]) {.error.}
proc `=dup`*[T](a: Arc[T]): Arc[T] {.error.}

template summaryType*(T: typedesc[Item]): untyped = typeof(T.default.summary)

const treeBase = 1
const treeChildrenMax = treeBase * 2

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
      mChildren: seq[Arc[Node[T]]]
    of Leaf:
      mItems: seq[T]

  SumTree*[T: Item] = distinct Arc[Node[T]]

func `$`*[T](arc: Arc[T]): string = &"Arc({arc.value[]})"
func `$`*[T: Item](node: Node[T]): string =
  case node.kind:
  of Internal:
    &"Internal({node.mSummary}, {node.mChildren})"
  of Leaf:
    &"Leaf({node.mSummary}, {node.mItems})"
func `$`*[T: Item](tree: SumTree[T]): string = $Arc[Node[T]](tree)

func pretty*[T: Item](node {.byref.}: Node[T]): string =
  case node.kind:
  of Internal:
    result = &"Internal({node.mSummary}, {node.mChildren.len}):\n"
    for i in 0..node.mChildren.high:
      if i > 0:
        result.add "\n"
      result.add node.mChildren[i].value[].pretty.indent(2)

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
      currentParentNode.get.mChildren.add Arc[Node[T]].new(childNode.move)

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

when isMainModule:
  type Count = distinct int
  type TestSummary = object
    count: Count
    # max: int
    # zeroes: int

  func `+=`(a: var Count, b: Count) {.borrow.}

  func `+=`*(a: var TestSummary, b: TestSummary) =
    a.count += b.count
    # a.max = max(a.max, b.max)
    # a.zeroes += b.zeroes

  func addSummary(a: var Count, b: TestSummary) =
    a += b.count

  func fromSummary(_: typedesc[Count], a: TestSummary): Count = a.count

  # func summary*(x: int): TestSummary = TestSummary(count: 1, max: x, zeroes: if x == 0: 1 else: 0)
  func summary*(x: int): TestSummary = TestSummary(count: 1.Count)

  var tree = SumTree[int].new([0, 1, 1, 2, 3, 5, 8, 13, 21, 34])
  echo tree.pretty

  var cursor = tree.initCursor Count
  echo cursor
