import std/[macros, strformat, sequtils, options, strutils, sugar]
import arc, static_array

export arc, static_array, options, strutils

const debugNodeLifecycle = false
const debugAppend = false

var recursion = 0

type
  Summary* = concept var a, b
    a += b
    a.addSummary(b)

  Item* = concept a, type T of Clone
    a.summary is Summary

  Dimension* = concept var a, b, type T
    a.addSummary(Summary)
    b.clone() is T
    T.fromSummary(Summary) is T

  # SeekTarget*[S: Summary, D: Dimension[S]] {.explain.} = concept a, type S, type D
  #   S is Summary
  #   D is Dimension[S]
  #   var b: D
  #   cmp(a, b) is int

  Bias* = enum Left, Right

template summaryType*(T: typedesc): untyped = typeof(T.default.summary)
template summaryArrayType*(T: typedesc, C: static int): untyped = Array[typeof(T.default.summary), C]

type
  ItemArray*[T; C: static int] = Array[T, C]

  SummaryArray*[T; C: static int] = Array[T, C]

  HeightType = uint16

  NodeKind* = enum Internal, Leaf
  Node*[T; C: static int] = object
    mSummary: T.summaryType
    mSummaries: summaryArrayType(T, C)
    case kind: NodeKind
    of Internal:
      mHeight: HeightType
      mChildren: Array[Arc[Node[T, C]], C]
    of Leaf:
      mItemArray: Array[T, C]

  ChildArray*[T; C: static int] = Array[Arc[Node[T, C]], C]

  ArcNode*[T; C: static int] = Arc[Node[T, C]]
  SumTree*[T; C: static int] = object
    root: Arc[Node[T, C]]

  SeekAggregate* = concept var a
    type T = Item
    type S = Summary
    beginLeaf(a)
    endLeaf(a)
    pushItem(a, T, S)
    # pushTree(a, SumTree[T, 5], S)

  StackEntry*[T: Item; D; C: static int] = object
    node {.cursor.}: Arc[Node[T, C]]
    index: int
    position: D

  Cursor*[T: Item; D; C: static int] = object
    node {.cursor.}: Arc[Node[T, C]]
    stack: seq[StackEntry[T, D, C]]
    position: D
    didSeek: bool
    atEnd: bool

  Stats* = object
    height: int
    internal: int
    leaf: int

    itemBytes: int
    summariesBytes: int
    totalBytes: int

# static:
#   type Data = array[128, int8]
#   func summary(d: Data): int = discard
#   const size = 12
#   echo sizeof(Data)
#   echo sizeof(Node[Data, size])
#   echo sizeof(ArcNode[Data, size])
#   echo sizeof(Array[ArcNode[Data, size], size])

proc `=copy`*[T; C: static int](a: var Node[T, C], b: Node[T, C]) {.error.}
proc `=dup`*[T; C: static int](a: Node[T, C]): Node[T, C] {.error.}

proc `=copy`*[T; C: static int](a: var SumTree[T, C], b: SumTree[T, C]) {.error.}
proc `=dup`*[T; C: static int](a: SumTree[T, C]): SumTree[T, C] {.error.}

template mapIt*[T](self: Option[T], op: untyped): untyped =
  type OutType = typeof((
    block:
      var it{.inject.}: typeof(self.get, typeOfProc);
      op), typeOfProc)
  if self.isSome:
    let it {.cursor, inject.} = self.get
    some(op)
  else:
    OutType.none

type End*[D] = object
func cmp*[D: Dimension](a: End[D], b: D): int = 1

# impl SeekAggregate for tuple[]
proc beginLeaf*(a: var tuple[]) = discard
proc endLeaf*(a: var tuple[]) = discard
proc pushItem*[T; S](a: var tuple[], item: T, summary: S) = discard
proc pushTree*[T; S; C: static int](a: var tuple[], self: ArcNode[T, C], summary: S) = discard

type DebugSeekAggregate* = object

# impl SeekAggregate for DebugSeekAggregate
proc beginLeaf*(a: var DebugSeekAggregate) =
  echo &"beginLeaf"

proc endLeaf*(a: var DebugSeekAggregate) =
  echo &"endLeaf"

proc pushItem*[T; S](a: var DebugSeekAggregate, item: T, summary: S) =
  echo &"pushItem {item}, {summary}"

proc pushTree*[T; S: Summary; C: static int](
    a: var DebugSeekAggregate, self: ArcNode[T, C], summary: S) =
  echo &"pushTree {self}, {summary}"

type SummarySeekAggregate*[D] = object
  value: D

# impl SeekAggregate for SummarySeekAggregate
proc beginLeaf*[D](a: var SummarySeekAggregate[D]) = discard
proc endLeaf*[D](a: var SummarySeekAggregate[D]) = discard

proc pushItem*[D; T; S](a: var SummarySeekAggregate[D], item: T, summary: S) =
  mixin addSummary
  a.value.addSummary(summary)

proc pushTree*[D; T; S: Summary; C: static int](
    a: var SummarySeekAggregate[D], self: ArcNode[T, C], summary: S) =
  mixin addSummary
  a.value.addSummary(summary)

type SliceSeekAggregate*[T; C: static int] = object
  node: ArcNode[T, C]
  leafItems: Array[T, C]
  leafItemSummaries: summaryArrayType(T, C)
  leafSummary: T.summaryType

# impl SeekAggregate for SliceSeekAggregate
proc beginLeaf*[T; C: static int](self: var SliceSeekAggregate[T, C]) = discard
proc endLeaf*[T; C: static int](self: var SliceSeekAggregate[T, C]) =
  self.node.append Arc.new(Node[T, C](
    kind: Leaf,
    mSummary: self.leafSummary.move,
    mSummaries: self.leafItemSummaries.move,
    mItemArray: self.leafItems.move,
  ))

  assert self.leafSummary == T.summaryType.default
  assert self.leafItemSummaries == T.summaryArrayType(C).default

proc pushItem*[T; S; C: static int](self: var SliceSeekAggregate[T, C], item: T, summary: S) =
  mixin addSummary
  self.leafItems.add item.clone()
  self.leafItemSummaries.add summary.clone()
  self.leafSummary += summary

proc pushTree*[T; S: Summary; C: static int](
    self: var SliceSeekAggregate[T, C], node: ArcNode[T, C], summary: S) =
  self.node.append(node.clone())

proc `$`*[T: Item; C: static int](node {.byref.}: Node[T, C]): string =
  case node.kind:
  of Internal:
    &"Internal(h: {node.mHeight}, {node.mSummary}, children: {node.mChildren.len})"
  of Leaf:
    &"Leaf({node.mSummary}, items: {node.mItemArray.len})"

proc `$`*[T: Item; C: static int](tree {.byref.}: SumTree[T, C]): string = $tree.root

proc `$`*(entry: StackEntry): string = &"(p: {entry.position}, i: {entry.index})"

proc `$`*(cursor: Cursor): string =
  &"Cursor(p: {cursor.position}, s: {cursor.didSeek}, e: {cursor.atEnd}, st: {cursor.stack})"

func pretty*[T: Item; C: static int](node {.byref.}: Node[T, C], id: int = 0, count: int = 0): string =
  case node.kind:
  of Internal:
    result = &"Internal(_{id}, #{count}, {node.mSummary}, {node.mChildren.high}):\n"
    # result = &"Internal(_{id}, #{count}, {node.mSummary}, {node.mChildren.high}, {node.mSummaries}):\n"
    for i in 0..node.mChildren.high:
      if i > 0:
        result.add "\n"
      result.add node.mChildren[i].get.pretty(
        node.mChildren[i].id, node.mChildren[i].count).indent(2)

  of Leaf:
    result = &"Leaf(_{id}, #{count}, {node.mSummary}, {node.mItemArray})"
    # result = &"Leaf(_{id}, #{count}, {node.mSummary}, {node.mItemArray}, {node.mSummaries})"

func pretty*[T: Item; C: static int](node{.byref.}: ArcNode[T, C]): string =
  let id = node.id
  let count = node.count
  node.get.pretty(id, count)

func pretty*[T: Item; C: static int](tree {.byref.}: SumTree[T, C]): string =
  let id = tree.root.id
  let count = tree.root.count
  tree.root.get.pretty(id, count)

func invert*(bias: Bias): Bias =
  case bias
  of Left: Right
  of Right: Left

func `==`*[T: Item; C: static int](a {.byref.}: Node[T, C], b {.byref.}: Node[T, C]): bool =
  if a.kind != b.kind:
    return false
  if a.summary != b.summary:
    return false
  if a.mSummaries.len != b.mSummaries.len:
    return false
  if a.mSummaries != b.mSummaries:
    return false

  case a.kind
  of Leaf:
    return a.mItemArray == b.mItemArray
  of Internal:
    if a.mHeight != b.mHeight:
      return false
    return a.mChildren == b.mChildren

func isLeaf*[T: Item; C: static int](node: Node[T, C]): bool = node.kind == Leaf
func isInternal*[T: Item; C: static int](node: Node[T, C]): bool = node.kind == Internal

func height*[T: Item; C: static int](node: Node[T, C]): int =
  case node.kind
  of Internal:
    node.mHeight.int
  of Leaf:
    0

func isLeaf*[T: Item; C: static int](node: ArcNode[T, C]): bool = node.get.isLeaf
func isInternal*[T: Item; C: static int](node: ArcNode[T, C]): bool = node.get.isInternal

func isLeaf*[T: Item; C: static int](tree: SumTree[T, C]): bool = tree.root.get.isLeaf
func isInternal*[T: Item; C: static int](tree: SumTree[T, C]): bool = tree.root.get.isInternal

func height*[T: Item; C: static int](node: ArcNode[T, C]): int = node.get.height
func height*[T: Item; C: static int](tree: SumTree[T, C]): int = tree.root.height

func sum*[T: Summary; C: static int](arr {.byref.}: Array[T, C]): T =
  mixin addSummary
  if arr.len == 0:
    result = T.default
  else:
    result = arr[0].clone()
    for i in 1..arr.high:
      result.addSummary(arr[i])

func summary*[T: Item; C: static int](node: Node[T, C]): T.summaryType = node.mSummary
func summary*[T: Item; C: static int](node: ArcNode[T, C]): T.summaryType = node.get.summary
func summary*[T: Item; C: static int](tree: SumTree[T, C]): T.summaryType = tree.root.summary
func extent*[T: Item; C: static int](tree: SumTree[T, C], D: typedesc[Dimension]): D =
  result = D.default
  result.addSummary(tree.root.get.mSummary)

template childSummaries*[T: Item; C: static int](node: Node[T, C]): untyped =
  node.mSummaries.toOpenArray

template childTrees*[T: Item; C: static int](node: Node[T, C]): untyped =
  node.mChildren.toOpenArray

template childItems*[T: Item; C: static int](node: Node[T, C]): untyped =
  node.mItemArray.toOpenArray

func isUnderflowing*[T; C: static int](node: Node[T, C]): bool =
  case node.kind
  of Internal:
    node.mChildren.len < C div 2
  of Leaf:
    node.mItemArray.len < C div 2

func toTree*[T: Item; C: static int](node: sink ArcNode[T, C]): SumTree[T, C] =
  SumTree[T, C](root: node)

func isEmpty*[T: Item; C: static int](node: ArcNode[T, C]): bool =
  case node.get.kind
  of Internal:
    false
  of Leaf:
    node.get.mItemArray.len == 0

func newLeaf*[T; C: static int](): Node[T, C] =
  type Summary = T.summaryType
  Node[T, C](kind: Leaf, mSummary: Summary.default)

proc clone*[T; C: static int](a {.byref.}: Node[T, C]): Node[T, C] =
  when debugNodeLifecycle:
    echo indent(&"Node.clone {a}", recursion)

  result = Node[T, C](kind: a.kind)
  result.mSummary = a.mSummary.clone()
  result.mSummaries = a.mSummaries.clone()
  case a.kind
  of Internal:
    # todo: right now we're cloning the child arc, but should we deep deep copy?
    result.mHeight = a.mHeight
    result.mChildren = a.mChildren.clone()
  of Leaf:
    result.mItemArray = a.mItemArray.clone()

proc new*[T: Item; C: static int](_: typedesc[SumTree[T, C]]): SumTree[T, C] =
  assert C mod 2 == 0
  Arc.new(newLeaf[T, C]()).toTree

proc clone*[T; C: static int](a {.byref.}: SumTree[T, C]): SumTree[T, C] =
  a.root.clone().toTree

proc new*[T: Item; C: static int](_: typedesc[SumTree[T, C]], items: sink seq[T]): SumTree[T, C] =
  mixin summary
  mixin `+=`
  mixin addSummary

  var nodes: seq[Node[T, C]]
  var i = 0
  while i < items.len:
    let endIndex = min(i + C, items.len)
    var subItems: ItemArray[T, C]
    subItems.len = endIndex - i
    for k in 0..<subItems.len:
      subItems[k] = items[i + k].move

    i = endIndex

    var summaries: SummaryArray[T.summaryType, C] = subItems.mapIt(it.summary)
    var s: T.summaryType = summaries[0].clone()
    for k in 1..summaries.high:
      s += summaries[k]

    nodes.add Node[T, C](kind: Leaf, mSummary: s, mItemArray: subItems, mSummaries: summaries.move)

  var parentNodes: seq[Node[T, C]] = @[]
  var height: HeightType = 0
  while nodes.len > 1:
    inc height
    var currentParentNode = Node[T, C].none
    var tempNodes = nodes.move
    for childNode in tempNodes.mitems:
      if currentParentNode.isNone:
        currentParentNode = some Node[T, C](
          kind: Internal, mSummary: T.summaryType.default, mHeight: height
        )

      let childSummary = childNode.summary
      currentParentNode.get.mSummary += childSummary
      currentParentNode.get.mSummaries.add childSummary
      currentParentNode.get.mChildren.add Arc.new(childNode.move)

      if currentParentNode.get.mChildren.len == C:
        parentNodes.add currentParentNode.get.move
        currentParentNode = Node[T, C].none

    if currentParentNode.isSome:
      parentNodes.add currentParentNode.get.move
      currentParentNode = Node[T, C].none

    nodes = parentNodes.move

  if nodes.len == 0:
    result = Arc.new(newLeaf[T, C]()).toTree
  else:
    assert nodes.len == 1
    result = Arc.new(nodes[0].move).toTree

  # echo indent(result.pretty, recursion)
  # echo "-----------------"

func leftmostLeaf*[T: Item; C: static int](self {.byref.}: ArcNode[T, C]): lent ArcNode[T, C] =
  case self.get.kind
  of Leaf:
    result = self
  else:
    result = self.get.mChildren[0].leftmostLeaf

func rightmostLeaf*[T: Item; C: static int](self {.byref.}: ArcNode[T, C]): lent ArcNode[T, C] =
  case self.get.kind
  of Leaf:
    result = self
  else:
    result = self.get.mChildren[self.get.mChildren.high].rightmostLeaf

func first*[T: Item; C: static int](self {.byref.}: ArcNode[T, C]): Option[ptr T] =
  let leaf {.cursor.} = self.leftmostLeaf
  if leaf.get.mItemArray.len > 0:
    result = leaf.get.mItemArray[0].addr.some

func last*[T: Item; C: static int](self {.byref.}: ArcNode[T, C]): Option[ptr T] =
  let leaf {.cursor.} = self.rightmostLeaf
  if leaf.get.mItemArray.len > 0:
    result = leaf.get.mItemArray[leaf.get.mItemArray.high].addr.some

func leftmostLeaf*[T: Item; C: static int](self {.byref.}: SumTree[T, C]): lent ArcNode[T, C] =
  return self.root.leftmostLeaf

func rightmostLeaf*[T: Item; C: static int](self {.byref.}: SumTree[T, C]): lent ArcNode[T, C] =
  return self.root.rightmostLeaf

func first*[T: Item; C: static int](self {.byref.}: SumTree[T, C]): Option[ptr T] =
  self.root.first

func last*[T: Item; C: static int](self {.byref.}: SumTree[T, C]): Option[ptr T] =
  self.root.last

proc updateLastRecursive[T: Item; C: static int](
    self: var ArcNode[T, C], f: proc(node: var T)): Option[T.summaryType] =
  self.makeUnique()

  case self.get.kind
  of Internal:
    var lastSummary = self.getMut.mSummaries[self.get.mSummaries.high].addr
    var lastChild = self.get.mChildren[self.get.mChildren.high].addr
    lastSummary[] = lastChild[].updateLastRecursive(f).get
    self.getMut.mSummary = self.get.mSummaries.sum
    self.get.mSummary.clone().some

  of Leaf:
    if self.get.mItemArray.len > 0:
      let item = self.get.mItemArray[self.get.mItemArray.high].addr
      f(item[])
      self.getMut.mSummaries[self.get.mSummaries.high] = item[].summary()
      self.getMut.mSummary = self.get.mSummaries.sum
      self.get.mSummary.clone().some

    else:
      T.summaryType.none

proc updateLast*[T: Item; C: static int](
    self: var ArcNode[T, C], f: proc(node: var T)) =
  self.updateLastRecursive(f)

proc updateLast*[T: Item; C: static int](
    self: var SumTree[T, C], f: proc(node: var T)) =
  discard self.root.updateLastRecursive(f)

proc pushTreeRecursive[T: Item; C: static int](
    self: var ArcNode[T, C], other: sink ArcNode[T, C]): Option[ArcNode[T, C]] =
  mixin addSummary

  template debugf(str: static string): untyped =
    when debugAppend:
      echo indent(&str, recursion)

  when debugAppend:
    recursion += 2
    defer:
      recursion -= 2

  # debugf"pushTreeRecursive:\n- tree: {self.pretty.indent(1)}\n- other: {other.pretty.indent(1)}"
  debugf"pushTreeRecursive:- tree: {self}, other: {other}"
  debugf"{self.pretty}"

  template node: Node[T, C] = self.getMut
  self.makeUnique()

  case node.kind
  of Internal:
    debugf"--- internal"
    let otherNode {.cursor.} = other.get
    node.mSummary.addSummary(otherNode.mSummary)

    let heightDelta = node.height - otherNode.height
    var summariesToAppend: SummaryArray[T.summaryType, C]
    var treesToAppend: ChildArray[T, C]

    debugf"height: {node.mHeight}, {otherNode.height}, d: {heightDelta}, {otherNode.isUnderflowing}"
    if heightDelta == 0:
      summariesToAppend = otherNode.mSummaries.clone()
      treesToAppend =  otherNode.mChildren.clone()
    elif heightDelta == 1 and not otherNode.isUnderflowing():
      debugf"not underflowing"
      summariesToAppend.add otherNode.summary.clone()
      treesToAppend.add other.clone()
    else:
      debugf"big height delta or undeflowing"
      var treeToAppend = node.mChildren[node.mChildren.high].pushTreeRecursive(other.clone())
      node.mSummaries[node.mSummaries.high] = node.mChildren[node.mChildren.high].get.summary.clone()

      if treeToAppend.isSome:
        debugf"-> {treeToAppend.get.pretty}"
        summariesToAppend.add treeToAppend.get.get.summary.clone()
        treesToAppend.add treeToAppend.get.move
      else:
        debugf"-> none"

    debugf"toAppend: {summariesToAppend}, {treesToAppend}"
    assert summariesToAppend.len == treesToAppend.len

    let childCount = node.mChildren.len + treesToAppend.len
    debugf"childCount: {node.mChildren.len}, {treesToAppend.len}, {childCount}"
    if childCount > C:
      debugf"over max, split"
      var leftSummaries: SummaryArray[T.summaryType, C]
      var rightSummaries: SummaryArray[T.summaryType, C]
      var leftTrees: ChildArray[T, C]
      var rightTrees: ChildArray[T, C]

      let midpoint = (childCount + childCount mod 2) div 2

      # todo: make this work to save clones?
      # if midpoint == node.mSummaries.len and heightDelta == 0:
      #   # No need to modify, the nodes already have the same height and can be directly appended
      #   return other.some

      block:
        debugf"midpoint: {midpoint}"
        for i in 0..<min(midpoint, node.mSummaries.len):
          leftSummaries.add node.mSummaries[i]
          leftTrees.add node.mChildren[i].clone()

        if node.mSummaries.len > midpoint:
          for i in 0..<(node.mSummaries.len - midpoint):
            rightSummaries.add node.mSummaries[midpoint + i].clone()
            rightTrees.add node.mChildren[midpoint + i].clone()
          for i in 0..<summariesToAppend.len:
            rightSummaries.add summariesToAppend[i].clone()
            rightTrees.add treesToAppend[i].clone()

        elif node.mSummaries.len < midpoint:
          for i in 0..<(midpoint - node.mSummaries.len):
            leftSummaries.add summariesToAppend[i].clone()
            leftTrees.add treesToAppend[i].clone()
          for i in (midpoint - node.mSummaries.len)..<summariesToAppend.len:
            rightSummaries.add summariesToAppend[i].clone()
            rightTrees.add treesToAppend[i].clone()

        else:
          for i in 0..<summariesToAppend.len:
            rightSummaries.add summariesToAppend[i].clone()
            rightTrees.add treesToAppend[i].clone()

      assert leftSummaries.len == leftTrees.len
      assert rightSummaries.len == rightTrees.len

      debugf"left: {leftSummaries}, {leftTrees}"
      debugf"right: {rightSummaries}, {rightTrees}"

      node.mSummaries = leftSummaries
      node.mSummary = node.mSummaries.sum()
      node.mChildren = leftTrees.move

      return some(Arc.new(Node[T, C](
        kind: Internal,
        mHeight: node.mHeight,
        mSummary: rightSummaries.sum(),
        mSummaries: rightSummaries,
        mChildren: rightTrees,
      )))

    else:
      node.mSummaries.add summariesToAppend
      node.mChildren.add treesToAppend
      debugf"extend internal {self.pretty}"
      return ArcNode[T, C].none

  of Leaf:
    debugf"--- leaf"

    let otherNode {.cursor.} = other.get
    let childCount = node.mItemArray.len + otherNode.mItemArray.len
    if childCount > C:
      var leftSummaries: SummaryArray[T.summaryType, C]
      var rightSummaries: SummaryArray[T.summaryType, C]
      var leftItems: ItemArray[T, C]
      var rightItems: ItemArray[T, C]

      let midpoint = (childCount + childCount mod 2) div 2
      if midpoint == node.mSummaries.len:
        return other.some

      self.makeUnique()

      block:
        for i in 0..<min(midpoint, node.mSummaries.len):
          leftSummaries.add node.mSummaries[i]
          leftItems.add node.mItemArray[i].clone()

        if node.mSummaries.len > midpoint:
          for i in 0..<(node.mSummaries.len - midpoint):
            rightSummaries.add node.mSummaries[midpoint + i].clone()
            rightItems.add node.mItemArray[midpoint + i].clone()
          for i in 0..<otherNode.mSummaries.len:
            rightSummaries.add otherNode.mSummaries[i].clone()
            rightItems.add otherNode.mItemArray[i].clone()

        elif node.mSummaries.len < midpoint:
          for i in 0..<(midpoint - node.mSummaries.len):
            leftSummaries.add otherNode.mSummaries[i].clone()
            leftItems.add otherNode.mItemArray[i].clone()
          for i in (midpoint - node.mSummaries.len)..<otherNode.mSummaries.len:
            rightSummaries.add otherNode.mSummaries[i].clone()
            rightItems.add otherNode.mItemArray[i].clone()

        else:
          for i in 0..<otherNode.mSummaries.len:
            rightSummaries.add otherNode.mSummaries[i].clone()
            rightItems.add otherNode.mItemArray[i].clone()

      assert leftSummaries.len == leftItems.len
      assert rightSummaries.len == rightItems.len

      node.mSummary = leftSummaries.sum()
      node.mSummaries = leftSummaries
      node.mItemArray = leftItems

      return some(Arc.new(Node[T, C](
        kind: Leaf,
        mSummary: rightSummaries.sum(),
        mSummaries: rightSummaries,
        mItemArray: rightItems,
      )))

    else:
      self.makeUnique()
      node.mSummary.addSummary(otherNode.mSummary)
      node.mItemArray.add(otherNode.mItemArray.clone())
      node.mSummaries.add(otherNode.mSummaries.clone())

proc fromChildTrees*[T: Item; C: static int](
    _: typedesc[ArcNode[T, C]], left: sink ArcNode[T, C], right: sink ArcNode[T, C]): ArcNode[T, C] =
  mixin addSummary

  when debugAppend:
    echo indent(&"--- fromChildTrees: {left}, {right}", recursion)
    # echo indent(left.pretty, recursion)
    # echo "---"
    # echo indent(right.pretty, recursion)

  let height = left.get.height + 1

  var childSummaries: SummaryArray[T.summaryType, C]
  childSummaries.add(left.get.mSummary.clone())
  childSummaries.add(right.get.mSummary.clone())

  var sum = childSummaries[0].clone()
  for i in 1..childSummaries.high:
    sum.addSummary(childSummaries[i])

  var childTrees: ChildArray[T, C]
  childTrees.add left.move
  childTrees.add right.move

  result = Arc.new(
    Node[T, C](
      kind: Internal,
      mHeight: height.HeightType,
      mSummary: sum,
      mSummaries: childSummaries.move,
      mChildren: childTrees.move,
    )
  )

  when debugAppend:
    # echo "---"
    echo indent(result.pretty, recursion)
    echo indent("--> fromChildTrees", recursion)

proc append*[T: Item; C: static int](self: var ArcNode[T, C], other: sink ArcNode[T, C]) =
  when debugAppend:
    echo indent(&"append {self}, {other}", recursion)

    recursion += 2
    defer:
      recursion -= 2

  if self.isEmpty:
    self = other.move
  elif other.get.isInternal or other.get.mItemArray.len > 0:
    if self.get.height < other.get.height:
      assert other.get.isInternal
      for tree in other.get.childTrees:
        self.append tree.clone()

    else:
      var splitTree = self.pushTreeRecursive(other.move)
      if splitTree.isSome:
        self = ArcNode[T, C].fromChildTrees(self.clone(), splitTree.get.move)

proc append*[T: Item; C: static int](self: var SumTree[T, C], other: sink SumTree[T, C]) =
  self.root.append(other.root)

proc extend*[T: Item; C: static int](self: var SumTree[T, C], values: sink seq[T]) =
  self.append SumTree[T, C].new(values)

proc add*[T: Item; C: static int](self: var SumTree[T, C], item: sink T) =
  let summary = item.summary
  self.root.append Arc.new(Node[T, C](
    kind: Leaf,
    mSummary: summary.clone(),
    mSummaries: [summary].toArray(C),
    mItemArray: [item.move].toArray(C),
  ))

func initCursor*[T: Item; C: static int](
    self {.byref.}: SumTree[T, C], D: typedesc): Cursor[T, D, C] =

  result.node = self.root
  result.position = D.default
  result.atEnd = self.root.isEmpty

func initCursor*[T: Item; C: static int](
  self {.byref.}: SumTree[T, C]): Cursor[T, tuple[], C] =

  result.node = self.root
  result.position = ()
  result.atEnd = self.root.isEmpty

func assertDidSeek*(self: Cursor) =
  assert self.didSeek

func reset*(self: var Cursor) =
  ## Reset the cursor to the beginning
  self.didSeek = false
  self.atEnd = self.node.isEmpty
  self.stack.setLen 0
  self.position = typeof(self.position).default

proc seekInternal[T: Item; D; Target; Aggregate; C: static int](
    self: var Cursor[T, D, C], target: Target, bias: Bias, aggregate: var Aggregate): bool =

  mixin addSummary

  template debugf(str: string): untyped =
    # echo "  ".repeat(self.stack.len) & &(str)
    discard

  if not self.didSeek:
    self.didSeek = true
    self.stack.add StackEntry[T, D, C](
      node: self.node,
      index: 0,
      position: D.default,
    )

  var ascending = false
  block outer:
    while self.stack.len > 0:
      block inner:

        debugf"loop {self.stack.len}, ascending: {ascending}, {self}"

        template entry: untyped = self.stack[self.stack.high]
        let node {.cursor.} = entry.node.get

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
              self.stack.add StackEntry[T, D, C](
                node: childTree,
                index: 0,
                position: self.position.clone(),
              )
              ascending = false
              break inner

        of Leaf:
          debugf"leaf: {node.mItemArray}"
          aggregate.beginLeaf()

          for i in entry.index..node.mItemArray.high:
            let item {.cursor.} = node.mItemArray[i]
            let itemSummary {.cursor.} = node.mSummaries[i]

            debugf"item: {item}, {itemSummary}"
            var childEnd = self.position.clone()
            childEnd.addSummary(itemSummary)

            let comparison = target.cmp(childEnd)
            debugf"cmp: {target} <> {childEnd} -> {comparison}"
            if comparison > 0 or (comparison == 0 and bias == Right):
              debugf"before of target"
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
  assert self.stack.len == 0 or self.stack[self.stack.high].node.get.isLeaf

  var endPosition = self.position.clone()
  if bias == Left:
    let sum = self.itemSummary()
    if sum.isSome:
      endPosition.addSummary(sum.get)

  # echo &"{target} <> {endPosition} -> {target.cmp(endPosition)}"
  return target.cmp(endPosition) == 0

proc seek*[T: Item; D; Target; C: static int](
    self: var Cursor[T, D, C], target: Target, bias: Bias): bool =
  ## Resets and moves the cursor to the target. Returns true if the target position was found

  self.reset()
  var agg = ()
  self.seekInternal(target, bias, agg)

proc seekForward*[T: Item; D; Target; C: static int](
    self: var Cursor[T, D, C], target: Target, bias: Bias): bool =
  ## Moves the cursor to the target. Returns true if the target position was found

  var agg = ()
  self.seekInternal(target, bias, agg)

proc summary*[T: Item; D: Dimension; Target; C: static int](
    self: var Cursor[T, D, C], Output: typedesc[Dimension], `end`: Target, bias: Bias): Output =
  ## Advances the cursor to `end` and returns the aggregated value of the `Output` dimension
  ## up until, but not including `end`

  var summary = SummarySeekAggregate[Output](value: Output.default)
  discard self.seekInternal(`end`, bias, summary)
  summary.value.move

proc slice*[T: Item; D: Dimension; Target; C: static int](
    self: var Cursor[T, D, C], `end`: Target, bias: Bias): SumTree[T, C] =
  ## Returns a new sum tree representing covering the items from the current position
  ## to the given end location. #todo: current node included?

  var slice = SliceSeekAggregate[T, C](
    node: Arc.new(Node[T, C](kind: Leaf)),
    leafSummary: T.summaryType.default,
  )
  discard self.seekInternal(`end`, bias, slice)
  SumTree[T, C](root: slice.node.move)

proc suffix*[T: Item; D: Dimension; C: static int](
    self: var Cursor[T, D, C]): SumTree[T, C] =
  ## Returns a new sum tree representing the remainder of the cursors tree from the current position
  ## to the end. #todo: current node included?
  self.slice(End[D](), Right)

proc nextInternal[T: Item; D: Dimension; C: static int](
    self: var Cursor[T, D, C], filterNode: proc(s: T.summaryType): bool) =
  ## Moves the cursor to the next leaf
  mixin addSummary

  template debugf(str: string): untyped =
    # echo "  ".repeat(self.stack.len) & &(str)
    discard

  debugf"nextInternal {self}"

  var descend = false

  if self.stack.len == 0:
    if not self.atEnd:
      self.stack.add StackEntry[T, D, C](
        node: self.node,
        index: 0,
        position: D.default,
      )
      descend = true
    self.didSeek = true

  block outer:
    while self.stack.len > 0:
      debugf"loop {self.stack.len}, descend: {descend}, {self}"

      template entry: untyped = self.stack[self.stack.high]
      let node {.cursor.} = entry.node.get

      case node.kind
      of Internal:
        if not descend:
          debugf"ascending 1: {entry.index}, {self.stack[self.stack.high].index}"
          entry.index += 1
          entry.position = self.position.clone()

        while entry.index < node.mSummaries.len:
          let nextSummary {.cursor.} = node.mSummaries[entry.index]

          if filterNode(nextSummary):
            break
          else:
            entry.index += 1
            entry.position.addSummary(nextSummary)
            self.position.addSummary(nextSummary)


        if entry.index < node.mChildren.len:
          descend = true
          self.stack.add StackEntry[T, D, C](
            node: node.mChildren[entry.index],
            index: 0,
            position: self.position.clone(),
          )

        else:
          descend = false
          discard self.stack.pop()

      of Leaf:
        debugf"leaf: {node.mItemArray}"
        if not descend:
          debugf"{entry}"
          let itemSummary {.cursor.} = node.mSummaries[entry.index]
          entry.index += 1
          entry.position.addSummary(itemSummary)
          self.position.addSummary(itemSummary)

        while entry.index < node.mItemArray.len:
          let nextItemSummary {.cursor.} = node.mSummaries[entry.index]
          if filterNode(nextItemSummary):
            return
          else:
            entry.index += 1
            entry.position.addSummary(nextItemSummary)
            self.position.addSummary(nextItemSummary)

        descend = false
        discard self.stack.pop()

    self.atEnd = self.stack.len == 0

proc next*[T: Item; D: Dimension; C: static int](self: var Cursor[T, D, C]) =
  ## Moves the cursor to the next leaf
  self.nextInternal((_: T.summaryType) => true)

proc prevInternal[T: Item; D: Dimension; C: static int](
    self: var Cursor[T, D, C], filterNode: proc(s: T.summaryType): bool) =
  ## Moves the cursor to the prev leaf
  mixin addSummary

  template debugf(str: string): untyped =
    # echo "  ".repeat(self.stack.len) & &(str)
    discard

  debugf"prevInternal {self}"

  if not self.didSeek:
    # Wrap around to end
    self.didSeek = true
    self.atEnd = true

  if self.atEnd:
    self.position = D.default
    self.atEnd = self.node.isEmpty
    if not self.node.isEmpty:
      self.stack.add StackEntry[T, D, C](
        node: self.node,
        index: self.node.get.mSummaries.len,
        position: D.fromSummary(self.node.summary()),
      )

  var descend = false
  while self.stack.len > 0:
    debugf"loop {self.stack.len}, descend: {descend}, {self}"

    if self.stack.len >= 2:
      self.position = self.stack[^2].position.clone()
    else:
      self.position = D.default

    template entry: untyped = self.stack[self.stack.high]
    let node {.cursor.} = entry.node.get

    if not descend:
      if entry.index == 0:
        discard self.stack.pop()
        continue
      else:
        entry.index -= 1

    for summary in entry.node.get.mSummaries.toOpenArray(0, entry.index - 1):
      self.position.addSummary(summary)

    entry.position = self.position.clone()

    descend = filterNode(entry.node.get.mSummaries[entry.index])
    case node.kind
    of Internal:
      debugf"internal: {node}"
      if descend:
        let tree {.cursor.} = node.mChildren[entry.index]
        self.stack.add StackEntry[T, D, C](
          node: tree,
          index: tree.get.mSummaries.len - 1,
          position: D.default,
        )

    of Leaf:
      debugf"leaf: {node}"
      if descend:
        break

proc prev*[T: Item; D: Dimension; C: static int](self: var Cursor[T, D, C]) =
  ## Moves the cursor to the prev leaf
  self.prevInternal((_: T.summaryType) => true)

proc itemSummary*[T: Item, D; C: static int](self: Cursor[T, D, C]): Option[T.summaryType] =
  ## Returns the summary of the current item, or none if the cursor is past the end

  self.assertDidSeek
  if self.stack.len > 0:
    let entry {.cursor.} = self.stack[self.stack.high]
    let node {.cursor.} = entry.node.get
    case node.kind
    of Leaf:
      if entry.index >= node.mSummaries.len:
        return T.summaryType.none
      else:
        return node.mSummaries[entry.index].some
    of Internal:
      assert false, "Stack top should contain leaf node"

  return T.summaryType.none

proc startPos*[T: Item; D; C: static int](self: Cursor[T, D, C]): lent D =
  ## Returns the aggregated value up until, but not including the current node
  self.position

proc endPos*[T: Item; D; C: static int](self: Cursor[T, D, C]): D =
  ## Returns the aggregated value of the current node
  mixin addSummary

  self.assertDidSeek
  let summary = self.itemSummary
  if summary.isSome:
    result = self.position.clone()
    result.addSummary(summary.get)
  else:
    result = self.position.clone()

proc nextLeaf*[T: Item, D; C: static int](self: Cursor[T, D, C]): Option[ptr ArcNode[T, C]] =
  if self.stack.len > 0:
    assert self.stack[self.stack.high].node.isLeaf
  for i in countdown(self.stack.high - 1, 0):
    let entry {.cursor.} = self.stack[i]
    if entry.index < entry.node.get.mChildren.len - 1:
      return entry.node.get.mChildren[entry.index + 1].leftmostLeaf.addr.some

proc prevLeaf*[T: Item, D; C: static int](self: Cursor[T, D, C]): Option[ptr ArcNode[T, C]] =
  if self.stack.len > 0:
    assert self.stack[self.stack.high].node.isLeaf
  for i in countdown(self.stack.high - 1, 0):
    let entry {.cursor.} = self.stack[i]
    if entry.index > 0:
      return entry.node.get.mChildren[entry.index - 1].rightmostLeaf.addr.some

proc nextItem*[T: Item, D; C: static int](self: Cursor[T, D, C]): Option[ptr T] =
  if not self.didSeek:
    result = self.node.first
  elif self.stack.len > 0:
    let entry {.cursor.} = self.stack[self.stack.high]
    if entry.index == entry.node.get.mItemArray.high:
      result = self.nextLeaf.mapIt(it[].get.mItemArray.first).flatten
    else:
      result = entry.node.get.mItemArray[entry.index + 1].addr.some

  elif not self.atEnd:
    result = self.node.first

proc prevItem*[T: Item, D; C: static int](self: Cursor[T, D, C]): Option[ptr T] =
  self.assertDidSeek
  if self.stack.len > 0:
    let entry {.cursor.} = self.stack[self.stack.high]
    if entry.index == 0:
      result = self.prevLeaf.mapIt(it[].get.mItemArray.last).flatten
    else:
      result = entry.node.get.mItemArray[entry.index - 1].addr.some

  elif self.atEnd:
    result = self.node.last

proc item*[T: Item, D; C: static int](self: Cursor[T, D, C]): Option[ptr T] =
  # Returns the current item, or none if path the end

  self.assertDidSeek
  if self.stack.len > 0:
    let entry {.cursor.} = self.stack[self.stack.high]
    let node {.cursor.} = entry.node.get
    case node.kind
    of Leaf:
      if entry.index >= node.mItemArray.len:
        return (ptr T).none
      else:
        return entry.node.get.mItemArray[entry.index].addr.some
    of Internal:
      assert false, "Stack top should contain leaf node"

  return (ptr T).none

proc itemClone*[T: Item, D; C: static int](self: Cursor[T, D, C]): Option[T] =
  # Returns the current item, or none if path the end

  self.assertDidSeek
  if self.stack.len > 0:
    let entry {.cursor.} = self.stack[self.stack.high]
    let node {.cursor.} = entry.node.get
    case node.kind
    of Leaf:
      if entry.index >= node.mItemArray.len:
        return T.none
      else:
        return entry.node.get.mItemArray[entry.index].clone.some
    of Internal:
      assert false, "Stack top should contain leaf node"

  return T.none

# impl Dimension for tuple[]
proc addSummary*[S](a: var tuple[], summary: S) = discard
proc fromSummary*[S](_: typedesc[tuple[]], summary: S): tuple[] = ()

proc toSeq*[T: Item; C: static int](self: SumTree[T, C]): seq[T] =
  # echo self.pretty
  var cursor = self.initCursor tuple[]
  cursor.next()
  var i = cursor.item
  while i.isSome:
    result.add i.get[].clone()
    cursor.next()
    i = cursor.item

iterator items*[T: Item; C: static int](self: SumTree[T, C]): lent T =
  var cursor = self.initCursor tuple[]
  cursor.next()
  while cursor.stack.len > 0:
    let entry {.cursor.} = cursor.stack[cursor.stack.high]
    if entry.index >= entry.node.get.mItemArray.len:
      break

    let p = addr cursor.stack[cursor.stack.high].node.get.mItemArray[entry.index]
    yield p[]
    cursor.next()

proc didSeek*(self: Cursor): bool =
  ## Returns true if the cursor moved to an item
  self.didSeek

proc atEnd*(self: Cursor): bool =
  ## Returns true if the cursor at the end (no more items left)
  self.atEnd

proc countStats*(node {.byref.}: Node, stats: var Stats) =
  stats.totalBytes += sizeof(node)
  stats.summariesBytes += sizeof(node.summary) + sizeof(node.mSummaries)

  case node.kind
  of Internal:
    stats.internal += 1
    stats.summariesBytes += sizeof(node.summary) + sizeof(node.mSummaries)
    for i in 0..node.mChildren.high:
      node.mChildren[i].get.countStats(stats)

  of Leaf:
    stats.leaf += 1
    stats.itemBytes += sizeof(node.mItemArray)

proc stats*(tree: SumTree): Stats =
  # echo "countStats ", typeof(tree), ", ", sizeof(tree.root.get)
  result.height = tree.height
  tree.root.get.countStats(result)
