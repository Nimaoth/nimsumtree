import std/[macros, strformat, sequtils, options, strutils, sugar]
import arc, static_array

export arc, static_array, options, strutils

const debugNodeLifecycle = false
const debugAppend = false

var recursion = 0

# todo: at some point use concepts when they work
# type
#   Summary* = concept var a, b
#     a += b
#     a.addSummary(b)

#   Item* = concept a, type T of Clone
#     a.summary is Summary

#   Dimension* = concept var a, b, type T
#     # a.addSummary(c) # Causes issues with ambigous symbols
#     b.clone() is T
#     T.fromSummary(Summary) is T

#   SeekTarget*[S: Summary, D: Dimension[S]] {.explain.} = concept a, type S, type D
#     S is Summary
#     D is Dimension[S]
#     var b: D
#     cmp(a, b) is int

const sumTreeBase {.intdefine.} = 0
when sumTreeBase > 0:
  const treeBase* = sumTreeBase

elif defined(testing):
  import std/strutils

  const treeBase* = 2
  static:
    echo &"Sumtree: test environment, use treeBase = {treeBase}"

else:
  const treeBase* = 12

static:
  assert treeBase mod 2 == 0

type
  Bias* = enum Left, Right

template summaryType*(I: typedesc): untyped = typeof(I.default.summary)
template summaryArrayType*(I: typedesc): untyped = Array[typeof(I.default.summary), treeBase]

type
  ItemArray*[I] = Array[I, treeBase]

  SummaryArray*[T] = Array[T, treeBase]

  HeightType = uint16

  NodeKind* = enum Internal, Leaf
  Node*[I] = object
    mSummary: I.summaryType
    mSummaries: I.summaryArrayType
    case kind: NodeKind
    of Internal:
      mHeight: HeightType
      mChildren: Array[Arc[Node[I]], treeBase]
    of Leaf:
      mItemArray: Array[I, treeBase]

  ChildArray*[I] = Array[Arc[Node[I]], treeBase]

  ArcNode*[I] = Arc[Node[I]]
  SumTree*[I] = object
    root: Arc[Node[I]]

  # SeekAggregate* = concept var a
  #   type I = Item
  #   type S = Summary
  #   beginLeaf(a)
  #   endLeaf(a)
  #   pushItem(a, I, S)
  #   # pushTree(a, SumTree[I, 5], S)

  StackEntry*[I; D] = object
    node {.cursor.}: Arc[Node[I]]
    index: int
    position: D

  Cursor*[I; D] = object
    node {.cursor.}: Arc[Node[I]]
    stack: seq[StackEntry[I, D]]
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

proc `=copy`*[I](a: var Node[I], b: Node[I]) {.error.}
proc `=dup`*[I](a: Node[I]): Node[I] {.error.}

proc `=copy`*[I](a: var SumTree[I], b: SumTree[I]) {.error.}
proc `=dup`*[I](a: SumTree[I]): SumTree[I] {.error.}

func `$`*[I](node {.byref.}: Node[I]): string =
  case node.kind:
  of Internal:
    &"Internal(h: {node.mHeight}, {node.mSummary}, children: {node.mChildren.len})"
  of Leaf:
    &"Leaf({node.mSummary}, items: {node.mItemArray.len})"

func `$`*[I](tree {.byref.}: SumTree[I]): string = $tree.root

func `$`*(entry: StackEntry): string = &"(p: {entry.position}, i: {entry.index})"

func `$`*(cursor: Cursor): string =
  &"Cursor(p: {cursor.position}, s: {cursor.didSeek}, e: {cursor.atEnd}, st: {cursor.stack})"

func pretty*[I](node {.byref.}: Node[I], id: int = 0, count: int = 0): string =
  case node.kind:
  of Internal:
    result = &"Internal(_{id}, #{count}, {node.mSummary}, {node.mChildren.len - 1}):\n"
    # result = &"Internal(_{id}, #{count}, {node.mSummary}, {node.mChildren.high}, {node.mSummaries}):\n"
    for i in 0..node.mChildren.high:
      if i > 0:
        result.add "\n"
      result.add node.mChildren[i].get.pretty(
        node.mChildren[i].id, node.mChildren[i].count).indent(2)

  of Leaf:
    # result = &"Leaf(_{id}, #{count}, {node.mSummary}):\n{node.mItemArray.toOpenArray.join($'\\n').indent(2)}"
    result = &"Leaf(_{id}, #{count}, {node.mSummary}, {node.mItemArray})"
    # result = &"Leaf(_{id}, #{count}, {node.mSummary}, {node.mItemArray}, {node.mSummaries})"

func checkInvariants[I](self: ArcNode[I], allowUnderflow: bool = true) =
  when defined(testing):
    template node: untyped = self.get

    assert node.mSummaries.sum == node.mSummary, &"Summary doesn't match summaries: {self.pretty}"

    case node.kind
    of Internal:
      # todo: when are they allowed to underflow?
      # if not allowUnderflow:
      #   assert node.mChildren.len >= C div 2, &"Node is underflowing: {self.pretty}"
      assert node.mChildren.len == node.mSummaries.len, &"Child count doesn't match summary count ({node.mChildren.len} != {node.mSummaries.len}): {self.pretty}"

      for i in 0..<node.mChildren.len:
        assert node.mSummaries[i] == node.mChildren[i].get.mSummary, &"Child summary doesn't match cached ({i}): {self.pretty}"

        let allowUnderflow = allowUnderflow and i == node.mChildren.high
        node.mChildren[i].checkInvariants(allowUnderflow)

    of Leaf:
      # todo: when are they allowed to underflow?
      # if not allowUnderflow:
      #   assert node.mItemArray.len >= C div 2, &"Leaf is underflowing ({node.mItemArray.len} < {C div 2}): {self.pretty}"
      assert node.mItemArray.len == node.mSummaries.len, &"Item count doesn't match summary count ({node.mItemArray.len} != {node.mSummaries.len}): {self.pretty}"
      for i in 0..<node.mItemArray.len:
        assert node.mSummaries[i] == node.mItemArray[i].summary, &"Item summary doesn't match cached ({i}): {self.pretty}"

func checkInvariants*[I](self: SumTree[I]) =
  when defined(testing):
    self.root.checkInvariants(true)

template mapIt*[I](self: Option[I], op: untyped): untyped =
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
func cmp*[D; C](a: End[D], b: D, cx: C): int = 1

# impl SeekAggregate for tuple[]
func beginLeaf*[C](a: var tuple[], cx: C) = discard
func endLeaf*[C](a: var tuple[], cx: C) = discard
func pushItem*[I; S; C](a: var tuple[], item: I, summary: S, cx: C) = discard
func pushTree*[I; S; C](a: var tuple[], self: ArcNode[I], summary: S, cx: C) = discard

type DebugSeekAggregate* = object

# impl SeekAggregate for DebugSeekAggregate
func beginLeaf*[C](a: var DebugSeekAggregate, cx: C) =
  echo &"beginLeaf"

func endLeaf*[C](a: var DebugSeekAggregate, cx: C) =
  echo &"endLeaf"

func pushItem*[I; S; C](a: var DebugSeekAggregate, item: I, summary: S, cx: C) =
  echo &"pushItem {item}, {summary}"

func pushTree*[I; S; C](a: var DebugSeekAggregate, self: ArcNode[I], summary: S, cx: C) =
  echo &"pushTree {self}, {summary}"

type SummarySeekAggregate*[D] = object
  value: D

# impl SeekAggregate for SummarySeekAggregate
func beginLeaf*[D, C](a: var SummarySeekAggregate[D], cx: C) = discard
func endLeaf*[D, C](a: var SummarySeekAggregate[D], cx: C) = discard

func pushItem*[D; I; S, C](a: var SummarySeekAggregate[D], item: I, summary: S, cx: C) =
  mixin addSummary
  a.value.addSummary(summary, cx)

func pushTree*[D; I; S, C](a: var SummarySeekAggregate[D], self: ArcNode[I], summary: S, cx: C) =
  mixin addSummary
  a.value.addSummary(summary, cx)

type SliceSeekAggregate*[I] = object
  node: ArcNode[I]
  leafItems: Array[I, treeBase]
  leafItemSummaries: I.summaryArrayType
  leafSummary: I.summaryType

# impl SeekAggregate for SliceSeekAggregate
func beginLeaf*[I, C](self: var SliceSeekAggregate[I], cx: C) = discard
func endLeaf*[I, C](self: var SliceSeekAggregate[I], cx: C) =
  self.node.append(Arc.new(Node[I](
    kind: Leaf,
    mSummary: self.leafSummary.move,
    mSummaries: self.leafItemSummaries.move,
    mItemArray: self.leafItems.move,
  )), cx)

func pushItem*[I; S, C](self: var SliceSeekAggregate[I], item: I, summary: S, cx: C) =
  mixin addSummary
  self.leafItems.add item.clone()
  self.leafItemSummaries.add summary.clone()
  self.leafSummary.addSummary(summary, cx)

func pushTree*[I; S, C](self: var SliceSeekAggregate[I], node: ArcNode[I], summary: S, cx: C) =
  self.node.append(node.clone(), cx)

func pretty*[I](node{.byref.}: ArcNode[I]): string =
  let id = node.id
  let count = node.count
  node.get.pretty(id, count)

func pretty*[I](tree {.byref.}: SumTree[I]): string =
  let id = tree.root.id
  let count = tree.root.count
  tree.root.get.pretty(id, count)

func invert*(bias: Bias): Bias =
  case bias
  of Left: Right
  of Right: Left

func `==`*[I](a {.byref.}: Node[I], b {.byref.}: Node[I]): bool =
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

func isLeaf*[I](node: Node[I]): bool = node.kind == Leaf
func isInternal*[I](node: Node[I]): bool = node.kind == Internal

func height*[I](node: Node[I]): int =
  case node.kind
  of Internal:
    node.mHeight.int
  of Leaf:
    0

func isLeaf*[I](node: ArcNode[I]): bool = node.get.isLeaf
func isInternal*[I](node: ArcNode[I]): bool = node.get.isInternal

func isLeaf*[I](tree: SumTree[I]): bool = tree.root.get.isLeaf
func isInternal*[I](tree: SumTree[I]): bool = tree.root.get.isInternal

func height*[I](node: ArcNode[I]): int = node.get.height
func height*[I](tree: SumTree[I]): int = tree.root.height

func sum*[I, C](arr {.byref.}: Array[I, treeBase], cx: C): I =
  mixin addSummary
  if arr.len == 0:
    result = I.default
  else:
    result = arr[0].clone()
    for i in 1..arr.high:
      result.addSummary(arr[i], cx)

func summary*[I](node: Node[I]): I.summaryType = node.mSummary
func summary*[I](node: ArcNode[I]): I.summaryType = node.get.summary
func summary*[I](tree: SumTree[I]): I.summaryType = tree.root.summary
func extent*[I, C](tree: SumTree[I], D: typedesc, cx: C): D =
  result = D.default
  result.addSummary(tree.root.get.mSummary, cx)

template childSummaries*[I](node: Node[I]): untyped =
  node.mSummaries.toOpenArray

template childTrees*[I](node: Node[I]): untyped =
  node.mChildren.toOpenArray

template childItems*[I](node: Node[I]): untyped =
  node.mItemArray.toOpenArray

func isUnderflowing*[I](node: Node[I]): bool =
  case node.kind
  of Internal:
    node.mChildren.len < treeBase div 2
  of Leaf:
    node.mItemArray.len < treeBase div 2

func toTree*[I](node: sink ArcNode[I]): SumTree[I] =
  SumTree[I](root: node)

func isEmpty*[I](node: ArcNode[I]): bool =
  case node.get.kind
  of Internal:
    false
  of Leaf:
    node.get.mItemArray.len == 0

func newLeaf*[I](): Node[I] =
  type Summary = I.summaryType
  Node[I](kind: Leaf, mSummary: Summary.default)

func clone*[I](a {.byref.}: Node[I]): Node[I] =
  # when debugNodeLifecycle:
  #   echo indent(&"Node.clone {a}", recursion)

  result = Node[I](kind: a.kind)
  result.mSummary = a.mSummary.clone()
  result.mSummaries = a.mSummaries.clone()
  case a.kind
  of Internal:
    # todo: right now we're cloning the child arc, but should we deep deep copy?
    result.mHeight = a.mHeight
    result.mChildren = a.mChildren.clone()
  of Leaf:
    result.mItemArray = a.mItemArray.clone()

func new*[I](_: typedesc[SumTree[I]]): SumTree[I] =
  Arc.new(newLeaf[I]()).toTree

func clone*[I](a {.byref.}: SumTree[I]): SumTree[I] =
  a.root.clone().toTree

func new*[I](_: typedesc[SumTree[I]], items: sink seq[I]): SumTree[I] =
  mixin summary
  mixin `+=`
  mixin addSummary

  var nodes: seq[Node[I]]
  var i = 0
  while i < items.len:
    let endIndex = min(i + treeBase, items.len)
    var subItems: ItemArray[I]
    subItems.len = endIndex - i
    for k in 0..<subItems.len:
      subItems[k] = items[i + k].move

    i = endIndex

    var summaries: SummaryArray[I.summaryType] = subItems.mapIt(it.summary)
    var s: I.summaryType = summaries[0].clone()
    for k in 1..summaries.high:
      s += summaries[k]

    nodes.add Node[I](kind: Leaf, mSummary: s, mItemArray: subItems, mSummaries: summaries.move)

  var parentNodes: seq[Node[I]] = @[]
  var height: HeightType = 0
  while nodes.len > 1:
    inc height
    var currentParentNode = Node[I].none
    var tempNodes = nodes.move
    for childNode in tempNodes.mitems:
      if currentParentNode.isNone:
        currentParentNode = some Node[I](
          kind: Internal, mSummary: I.summaryType.default, mHeight: height
        )

      let childSummary = childNode.summary
      currentParentNode.get.mSummary += childSummary
      currentParentNode.get.mSummaries.add childSummary
      currentParentNode.get.mChildren.add Arc.new(childNode.move)

      if currentParentNode.get.mChildren.len == treeBase:
        parentNodes.add currentParentNode.get.move
        currentParentNode = Node[I].none

    if currentParentNode.isSome:
      parentNodes.add currentParentNode.get.move
      currentParentNode = Node[I].none

    nodes = parentNodes.move

  if nodes.len == 0:
    result = Arc.new(newLeaf[I]()).toTree
  else:
    assert nodes.len == 1
    result = Arc.new(nodes[0].move).toTree

  # echo indent(result.pretty, recursion)
  # echo "-----------------"

func leftmostLeaf*[I](self {.byref.}: ArcNode[I]): lent ArcNode[I] =
  case self.get.kind
  of Leaf:
    result = self
  else:
    result = self.get.mChildren[0].leftmostLeaf

func rightmostLeaf*[I](self {.byref.}: ArcNode[I]): lent ArcNode[I] =
  case self.get.kind
  of Leaf:
    result = self
  else:
    result = self.get.mChildren[self.get.mChildren.high].rightmostLeaf

func first*[I](self {.byref.}: ArcNode[I]): Option[ptr I] =
  let leaf {.cursor.} = self.leftmostLeaf
  if leaf.get.mItemArray.len > 0:
    result = leaf.get.mItemArray[0].addr.some

func last*[I](self {.byref.}: ArcNode[I]): Option[ptr I] =
  let leaf {.cursor.} = self.rightmostLeaf
  if leaf.get.mItemArray.len > 0:
    result = leaf.get.mItemArray[leaf.get.mItemArray.high].addr.some

func leftmostLeaf*[I](self {.byref.}: SumTree[I]): lent ArcNode[I] =
  return self.root.leftmostLeaf

func rightmostLeaf*[I](self {.byref.}: SumTree[I]): lent ArcNode[I] =
  return self.root.rightmostLeaf

func first*[I](self {.byref.}: SumTree[I]): Option[ptr I] =
  self.root.first

func last*[I](self {.byref.}: SumTree[I]): Option[ptr I] =
  self.root.last

func updateLastRecursive[I; C](self: var ArcNode[I], f: proc(node: var I) {.noSideEffect.}, cx: C): Option[I.summaryType] =
  self.makeUnique()

  case self.get.kind
  of Internal:
    var lastSummary = self.getMut.mSummaries[self.get.mSummaries.high].addr
    var lastChild = self.get.mChildren[self.get.mChildren.high].addr
    lastSummary[] = lastChild[].updateLastRecursive(f, cx).get
    self.getMut.mSummary = self.get.mSummaries.sum(cx)
    self.get.mSummary.clone().some

  of Leaf:
    if self.get.mItemArray.len > 0:
      let item = self.get.mItemArray[self.get.mItemArray.high].addr
      f(item[])
      self.getMut.mSummaries[self.get.mSummaries.high] = item[].summary()
      self.getMut.mSummary = self.get.mSummaries.sum(cx)
      self.get.mSummary.clone().some

    else:
      I.summaryType.none

func updateLast[I; C](self: var ArcNode[I], f: proc(node: var I) {.noSideEffect.}, cx: C) =
  discard self.updateLastRecursive(f, cx)

func updateLast*[I; C](self: var SumTree[I], f: proc(node: var I) {.noSideEffect.}, cx: C) =
  self.root.updateLast(f, cx)
  self.checkInvariants()

func pushTreeRecursive[I; C](self: var ArcNode[I], other: sink ArcNode[I], cx: C): Option[ArcNode[I]] =
  mixin addSummary

  # template debugf(str: static string): untyped =
  #   when debugAppend:
  #     echo indent(&str, recursion)

  when debugAppend:
    recursion += 2
    defer:
      recursion -= 2

  # debugf"pushTreeRecursive:\n- tree: {self.pretty.indent(1)}\n- other: {other.pretty.indent(1)}"
  # debugf"pushTreeRecursive:- tree: {self}, other: {other}"
  # debugf"{self.pretty}"

  template node: Node[I] = self.getMut
  self.makeUnique()

  case node.kind
  of Internal:
    # debugf"--- internal"
    let otherNode {.cursor.} = other.get
    node.mSummary.addSummary(otherNode.mSummary, cx)

    let heightDelta = node.height - otherNode.height
    var summariesToAppend: SummaryArray[I.summaryType]
    var treesToAppend: ChildArray[I]

    # debugf"height: {node.mHeight}, {otherNode.height}, d: {heightDelta}, {otherNode.isUnderflowing}"
    if heightDelta == 0:
      summariesToAppend = otherNode.mSummaries.clone()
      treesToAppend =  otherNode.mChildren.clone()
    elif heightDelta == 1 and not otherNode.isUnderflowing():
      # debugf"not underflowing"
      summariesToAppend.add otherNode.summary.clone()
      treesToAppend.add other.clone()
    else:
      # debugf"big height delta or undeflowing"
      var treeToAppend = node.mChildren[node.mChildren.high].pushTreeRecursive(other.clone(), cx)
      node.mSummaries[node.mSummaries.high] = node.mChildren[node.mChildren.high].get.summary.clone()

      if treeToAppend.isSome:
        # debugf"-> {treeToAppend.get.pretty}"
        summariesToAppend.add treeToAppend.get.get.summary.clone()
        treesToAppend.add treeToAppend.get.move
      # else:
        # debugf"-> none"

    # debugf"toAppend: {summariesToAppend}, {treesToAppend}"
    assert summariesToAppend.len == treesToAppend.len

    let childCount = node.mChildren.len + treesToAppend.len
    # debugf"childCount: {node.mChildren.len}, {treesToAppend.len}, {childCount}"
    if childCount > treeBase:
      # debugf"over max, split"
      var leftSummaries: SummaryArray[I.summaryType]
      var rightSummaries: SummaryArray[I.summaryType]
      var leftTrees: ChildArray[I]
      var rightTrees: ChildArray[I]

      let midpoint = (childCount + childCount mod 2) div 2

      # todo: make this work to save clones?
      # if midpoint == node.mSummaries.len and heightDelta == 0:
      #   # No need to modify, the nodes already have the same height and can be directly appended
      #   return other.some

      block:
        # debugf"midpoint: {midpoint}"
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

      # debugf"left: {leftSummaries}, {leftTrees}"
      # debugf"right: {rightSummaries}, {rightTrees}"

      node.mSummaries = leftSummaries
      node.mSummary = node.mSummaries.sum(cx)
      node.mChildren = leftTrees.move

      return some(Arc.new(Node[I](
        kind: Internal,
        mHeight: node.mHeight,
        mSummary: rightSummaries.sum(cx),
        mSummaries: rightSummaries,
        mChildren: rightTrees,
      )))

    else:
      node.mSummaries.add summariesToAppend
      node.mChildren.add treesToAppend
      # debugf"extend internal {self.pretty}"
      return ArcNode[I].none

  of Leaf:
    # debugf"--- leaf"

    let otherNode {.cursor.} = other.get
    let childCount = node.mItemArray.len + otherNode.mItemArray.len
    if childCount > treeBase:
      var leftSummaries: SummaryArray[I.summaryType]
      var rightSummaries: SummaryArray[I.summaryType]
      var leftItems: ItemArray[I]
      var rightItems: ItemArray[I]

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

      node.mSummary = leftSummaries.sum(cx)
      node.mSummaries = leftSummaries
      node.mItemArray = leftItems

      return some(Arc.new(Node[I](
        kind: Leaf,
        mSummary: rightSummaries.sum(cx),
        mSummaries: rightSummaries,
        mItemArray: rightItems,
      )))

    else:
      self.makeUnique()
      node.mSummary.addSummary(otherNode.mSummary, cx)
      node.mItemArray.add(otherNode.mItemArray.clone())
      node.mSummaries.add(otherNode.mSummaries.clone())

func fromChildTrees[I; C](_: typedesc[ArcNode[I]], left: sink ArcNode[I], right: sink ArcNode[I], cx: C): ArcNode[I] =
  mixin addSummary

  # when debugAppend:
  #   echo indent(&"--- fromChildTrees: {left}, {right}", recursion)
    # echo indent(left.pretty, recursion)
    # echo "---"
    # echo indent(right.pretty, recursion)

  let height = left.get.height + 1

  var childSummaries: SummaryArray[I.summaryType]
  childSummaries.add(left.get.mSummary.clone())
  childSummaries.add(right.get.mSummary.clone())

  var sum = childSummaries[0].clone()
  for i in 1..childSummaries.high:
    sum.addSummary(childSummaries[i], cx)

  var childTrees: ChildArray[I]
  childTrees.add left.move
  childTrees.add right.move

  result = Arc.new(
    Node[I](
      kind: Internal,
      mHeight: height.HeightType,
      mSummary: sum,
      mSummaries: childSummaries.move,
      mChildren: childTrees.move,
    )
  )

  # when debugAppend:
  #   # echo "---"
  #   echo indent(result.pretty, recursion)
  #   echo indent("--> fromChildTrees", recursion)

func append*[I; C](self: var ArcNode[I], other: sink ArcNode[I], cx: C) =
  # when debugAppend:
  #   echo indent(&"append {self}, {other}", recursion)

  #   recursion += 2
  #   defer:
  #     recursion -= 2

  if self.isEmpty:
    self = other.move
  elif other.get.isInternal or other.get.mItemArray.len > 0:
    if self.get.height < other.get.height:
      assert other.get.isInternal
      for tree in other.get.childTrees:
        self.append tree.clone(), cx

    else:
      var splitTree = self.pushTreeRecursive(other.move, cx)
      if splitTree.isSome:
        self = ArcNode[I].fromChildTrees(self.clone(), splitTree.get.move, cx)

func append*[I; C](self: var SumTree[I], other: sink SumTree[I], cx: C) =
  self.root.append(other.root, cx)
  self.checkInvariants()

func extend*[I; C](self: var SumTree[I], values: sink seq[I], cx: C) =
  self.append(SumTree[I].new(values), cx)

func add*[I; C](self: var SumTree[I], item: sink I, cx: C) =
  let summary = item.summary
  self.root.append(Arc.new(Node[I](
    kind: Leaf,
    mSummary: summary.clone(),
    mSummaries: [summary].toArray(treeBase),
    mItemArray: [item.move].toArray(treeBase),
  )), cx)
  self.checkInvariants()

func initCursor*[I](self {.byref.}: SumTree[I], D: typedesc): Cursor[I, D] =
  result.node = self.root
  result.position = D.default
  result.atEnd = self.root.isEmpty

func initCursor*[I, D](self {.byref.}: SumTree[I], init: D): Cursor[I, D] =
  result.node = self.root
  result.position = init
  result.atEnd = self.root.isEmpty

func initCursor*[I](self {.byref.}: SumTree[I]): Cursor[I, tuple[]] =
  result.node = self.root
  result.position = ()
  result.atEnd = self.root.isEmpty

func clone*[I, D](a {.byref.}: Cursor[I, D]): Cursor[I, D] =
  result.node = a.node.clone()
  result.stack = a.stack
  result.position = a.position
  result.didSeek = a.didSeek
  result.atEnd = a.atEnd

func assertDidSeek*(self: Cursor) =
  assert self.didSeek

func reset*(self: var Cursor) =
  ## Reset the cursor to the beginning
  self.didSeek = false
  self.atEnd = self.node.isEmpty
  self.stack.setLen 0
  self.position = typeof(self.position).default

func seekInternal[I; D; T; A; C](self: var Cursor[I, D], target: T, bias: Bias, aggregate: var A, cx: C): bool =
  mixin addSummary

  # template debugf(str: string): untyped =
  #   # echo "  ".repeat(self.stack.len) & &(str)
  #   discard

  if not self.didSeek:
    self.didSeek = true
    self.stack.add StackEntry[I, D](
      node: self.node,
      index: 0,
      position: D.default,
    )

  var ascending = false
  block outer:
    while self.stack.len > 0:
      block inner:

        # debugf"loop {self.stack.len}, ascending: {ascending}, {self}"

        template entry: untyped = self.stack[self.stack.high]
        let node {.cursor.} = entry.node.get

        case node.kind
        of Internal:
          if ascending:
            # debugf"ascending 1: {entry.index}, {self.stack[self.stack.high].index}"
            entry.index += 1
            # debugf"ascending 2: {entry.index}, {self.stack[self.stack.high].index}"
            entry.position = self.position.clone()

          for i in entry.index..node.mChildren.high:
            let childTree {.cursor.} = node.mChildren[i]
            let childSummary {.cursor.} = node.mSummaries[i]

            # debugf"child {i}: {childSummary}"

            var childEnd = self.position.clone()
            # debugf"childEnd: {childEnd}"
            childEnd.addSummary(childSummary, cx)
            # debugf"childEnd: {childEnd}"

            let comparison = target.cmp(childEnd, cx)
            # debugf"cmp: {target} <> {childEnd} -> {comparison}"
            if comparison > 0 or (comparison == 0 and bias == Right):
              # debugf"ahead of target"
              self.position = childEnd
              aggregate.pushTree(childTree, childSummary, cx)
              entry.index += 1
              # debugf"index: {entry.index}, {self.stack[self.stack.high]}"
              entry.position = self.position.clone()

            else:
              # debugf"behind target, enter child"
              self.stack.add StackEntry[I, D](
                node: childTree,
                index: 0,
                position: self.position.clone(),
              )
              ascending = false
              break inner

        of Leaf:
          # debugf"leaf: {node.mItemArray}"
          aggregate.beginLeaf(cx)

          for i in entry.index..node.mItemArray.high:
            let item {.cursor.} = node.mItemArray[i]
            let itemSummary {.cursor.} = node.mSummaries[i]

            # debugf"item: {item}, {itemSummary}"
            var childEnd = self.position.clone()
            childEnd.addSummary(itemSummary, cx)

            let comparison = target.cmp(childEnd, cx)
            # debugf"cmp: {target} <> {childEnd} -> {comparison}"
            if comparison > 0 or (comparison == 0 and bias == Right):
              # debugf"before of target"
              self.position = childEnd
              aggregate.pushItem(item, itemSummary, cx)
              entry.index += 1
              # debugf"index: {entry.index}, {self.stack[self.stack.high]}"

            else:
              # debugf"found?"
              aggregate.endLeaf(cx)
              break outer

          aggregate.endLeaf(cx)

        discard self.stack.pop()
        ascending = true

  # After while
  self.atEnd = self.stack.len == 0
  assert self.stack.len == 0 or self.stack[self.stack.high].node.get.isLeaf

  var endPosition = self.position.clone()
  if bias == Left:
    let sum = self.itemSummary()
    if sum.isSome:
      endPosition.addSummary(sum.get, cx)

  # echo &"{target} <> {endPosition} -> {target.cmp(endPosition)}"
  return target.cmp(endPosition, cx) == 0

func seek*[I; D; T; C](self: var Cursor[I, D], target: T, bias: Bias, cx: C): bool =
  ## Resets and moves the cursor to the target. Returns true if the target position was found

  self.reset()
  var agg = ()
  self.seekInternal(target, bias, agg, cx)

func seekForward*[I; D; T; C](self: var Cursor[I, D], target: T, bias: Bias, cx: C): bool =
  ## Moves the cursor to the target. Returns true if the target position was found

  var agg = ()
  self.seekInternal(target, bias, agg, cx)

func summary*[I; D; T; C](self: var Cursor[I, D], Output: typedesc, `end`: T, bias: Bias, cx: C): Output =
  ## Advances the cursor to `end` and returns the aggregated value of the `Output` dimension
  ## up until, but not including `end`

  var summary = SummarySeekAggregate[Output](value: Output.default)
  discard self.seekInternal(`end`, bias, summary, cx)
  summary.value.move

func slice*[I; D; T; C](self: var Cursor[I, D], `end`: T, bias: Bias, cx: C): SumTree[I] =
  ## Returns a new sum tree representing covering the items from the current position
  ## to the given end location. #todo: current node included?

  var slice = SliceSeekAggregate[I](
    node: Arc.new(newLeaf[I]()),
    leafSummary: I.summaryType.default,
    leafItemSummaries: I.summaryArrayType.default,
  )
  discard self.seekInternal(`end`, bias, slice, cx)
  result = SumTree[I](root: slice.node.move)
  result.checkInvariants()

func suffix*[I; D; C](self: var Cursor[I, D], cx: C): SumTree[I] =
  ## Returns a new sum tree representing the remainder of the cursors tree from the current position
  ## to the end. #todo: current node included?
  self.slice(End[D](), Right, cx)

func nextInternal[I; D; C](self: var Cursor[I, D], filterNode: proc(s: I.summaryType): bool {.noSideEffect.}, cx: C) =
  ## Moves the cursor to the next leaf
  mixin addSummary

  # template debugf(str: string): untyped =
  #   # echo "  ".repeat(self.stack.len) & &(str)
  #   discard

  # debugf"nextInternal {self}"

  var descend = false

  if self.stack.len == 0:
    if not self.atEnd:
      self.stack.add StackEntry[I, D](
        node: self.node,
        index: 0,
        position: D.default,
      )
      descend = true
    self.didSeek = true

  block outer:
    while self.stack.len > 0:
      # debugf"loop {self.stack.len}, descend: {descend}, {self}"

      template entry: untyped = self.stack[self.stack.high]
      let node {.cursor.} = entry.node.get

      case node.kind
      of Internal:
        if not descend:
          # debugf"ascending 1: {entry.index}, {self.stack[self.stack.high].index}"
          entry.index += 1
          entry.position = self.position.clone()

        while entry.index < node.mSummaries.len:
          let nextSummary {.cursor.} = node.mSummaries[entry.index]

          if filterNode(nextSummary):
            break
          else:
            entry.index += 1
            entry.position.addSummary(nextSummary, cx)
            self.position.addSummary(nextSummary, cx)


        if entry.index < node.mChildren.len:
          descend = true
          self.stack.add StackEntry[I, D](
            node: node.mChildren[entry.index],
            index: 0,
            position: self.position.clone(),
          )

        else:
          descend = false
          discard self.stack.pop()

      of Leaf:
        # debugf"leaf: {node.mItemArray}"
        if not descend:
          # debugf"{entry}"
          let itemSummary {.cursor.} = node.mSummaries[entry.index]
          entry.index += 1
          entry.position.addSummary(itemSummary, cx)
          self.position.addSummary(itemSummary, cx)

        while entry.index < node.mItemArray.len:
          let nextItemSummary {.cursor.} = node.mSummaries[entry.index]
          if filterNode(nextItemSummary):
            return
          else:
            entry.index += 1
            entry.position.addSummary(nextItemSummary, cx)
            self.position.addSummary(nextItemSummary, cx)

        descend = false
        discard self.stack.pop()

    self.atEnd = self.stack.len == 0

func next*[I; D; C](self: var Cursor[I, D], cx: C) =
  ## Moves the cursor to the next leaf
  self.nextInternal((_: I.summaryType) => true, cx)

func prevInternal[I; D; C](self: var Cursor[I, D], filterNode: proc(s: I.summaryType): bool {.noSideEffect.}, cx: C) =
  ## Moves the cursor to the prev leaf
  mixin addSummary

  # template debugf(str: string): untyped =
  #   # echo "  ".repeat(self.stack.len) & &(str)
  #   discard

  # debugf"prevInternal {self}"

  if not self.didSeek:
    # Wrap around to end
    self.didSeek = true
    self.atEnd = true

  if self.atEnd:
    self.position = D.default
    self.atEnd = self.node.isEmpty
    if not self.node.isEmpty:
      self.stack.add StackEntry[I, D](
        node: self.node,
        index: self.node.get.mSummaries.len,
        position: D.fromSummary(self.node.summary(), cx),
      )

  var descend = false
  while self.stack.len > 0:
    # debugf"loop {self.stack.len}, descend: {descend}, {self}"

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
      self.position.addSummary(summary, cx)

    entry.position = self.position.clone()

    descend = filterNode(entry.node.get.mSummaries[entry.index])
    case node.kind
    of Internal:
      # debugf"internal: {node}"
      if descend:
        let tree {.cursor.} = node.mChildren[entry.index]
        self.stack.add StackEntry[I, D](
          node: tree,
          index: tree.get.mSummaries.len - 1,
          position: D.default,
        )

    of Leaf:
      # debugf"leaf: {node}"
      if descend:
        break

func prev*[I; D; C](self: var Cursor[I, D], cx: C) =
  ## Moves the cursor to the prev leaf
  self.prevInternal((_: I.summaryType) => true, cx)

func itemSummary*[I, D](self: Cursor[I, D]): Option[I.summaryType] =
  ## Returns the summary of the current item, or none if the cursor is past the end

  self.assertDidSeek
  if self.stack.len > 0:
    let entry {.cursor.} = self.stack[self.stack.high]
    let node {.cursor.} = entry.node.get
    case node.kind
    of Leaf:
      if entry.index >= node.mSummaries.len:
        return I.summaryType.none
      else:
        return node.mSummaries[entry.index].some
    of Internal:
      assert false, "Stack top should contain leaf node"

  return I.summaryType.none

func startPos*[I; D](self: Cursor[I, D]): lent D =
  ## Returns the aggregated value up until, but not including the current node
  self.position

func endPos*[I; D; C](self: Cursor[I, D], cx: C): D =
  ## Returns the aggregated value of the current node
  mixin addSummary

  self.assertDidSeek
  let summary = self.itemSummary
  if summary.isSome:
    result = self.position.clone()
    result.addSummary(summary.get, cx)
  else:
    result = self.position.clone()

func nextLeaf*[I, D](self: Cursor[I, D]): Option[ptr ArcNode[I]] =
  if self.stack.len > 0:
    assert self.stack[self.stack.high].node.isLeaf
  for i in countdown(self.stack.high - 1, 0):
    let entry {.cursor.} = self.stack[i]
    if entry.index < entry.node.get.mChildren.len - 1:
      return entry.node.get.mChildren[entry.index + 1].leftmostLeaf.addr.some

func prevLeaf*[I, D](self: Cursor[I, D]): Option[ptr ArcNode[I]] =
  if self.stack.len > 0:
    assert self.stack[self.stack.high].node.isLeaf
  for i in countdown(self.stack.high - 1, 0):
    let entry {.cursor.} = self.stack[i]
    if entry.index > 0:
      return entry.node.get.mChildren[entry.index - 1].rightmostLeaf.addr.some

func nextItem*[I, D](self: Cursor[I, D]): Option[ptr I] =
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

func prevItem*[I, D](self: Cursor[I, D]): Option[ptr I] =
  self.assertDidSeek
  if self.stack.len > 0:
    let entry {.cursor.} = self.stack[self.stack.high]
    if entry.index == 0:
      result = self.prevLeaf.mapIt(it[].get.mItemArray.last).flatten
    else:
      result = entry.node.get.mItemArray[entry.index - 1].addr.some

  elif self.atEnd:
    result = self.node.last

func item*[I, D](self: Cursor[I, D]): Option[ptr I] =
  # Returns the current item, or none if path the end

  self.assertDidSeek
  if self.stack.len > 0:
    let entry {.cursor.} = self.stack[self.stack.high]
    let node {.cursor.} = entry.node.get
    case node.kind
    of Leaf:
      if entry.index >= node.mItemArray.len:
        return (ptr I).none
      else:
        return entry.node.get.mItemArray[entry.index].addr.some
    of Internal:
      assert false, "Stack top should contain leaf node"

  return (ptr I).none

func itemClone*[I, D](self: Cursor[I, D]): Option[I] =
  # Returns the current item, or none if path the end

  self.assertDidSeek
  if self.stack.len > 0:
    let entry {.cursor.} = self.stack[self.stack.high]
    let node {.cursor.} = entry.node.get
    case node.kind
    of Leaf:
      if entry.index >= node.mItemArray.len:
        return I.none
      else:
        return entry.node.get.mItemArray[entry.index].clone.some
    of Internal:
      assert false, "Stack top should contain leaf node"

  return I.none

# impl Dimension for tuple[]
func addSummary*[S; C](a: var tuple[], summary: S, cx: C) = discard
func fromSummary*[S; C](_: typedesc[tuple[]], summary: S, cx: C): tuple[] = ()

func toSeq*[I; C](self: SumTree[I], cx: C): seq[I] =
  # echo self.pretty
  var cursor = self.initCursor tuple[]
  cursor.next(cx)
  var i = cursor.item
  while i.isSome:
    result.add i.get[].clone()
    cursor.next(cx)
    i = cursor.item

iterator items*[I; C](self: SumTree[I], cx: C): lent I =
  var cursor = self.initCursor tuple[]
  cursor.next(cx)
  while cursor.stack.len > 0:
    let entry {.cursor.} = cursor.stack[cursor.stack.high]
    if entry.index >= entry.node.get.mItemArray.len:
      break

    let p = addr cursor.stack[cursor.stack.high].node.get.mItemArray[entry.index]
    yield p[]
    cursor.next(cx)

func didSeek*(self: Cursor): bool =
  ## Returns true if the cursor moved to an item
  self.didSeek

func atEnd*(self: Cursor): bool =
  ## Returns true if the cursor at the end (no more items left)
  self.atEnd

func countStats*(node {.byref.}: Node, stats: var Stats) =
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

func stats*(tree: SumTree): Stats =
  # echo "countStats ", typeof(tree), ", ", sizeof(tree.root.get)
  result.height = tree.height
  tree.root.get.countStats(result)
