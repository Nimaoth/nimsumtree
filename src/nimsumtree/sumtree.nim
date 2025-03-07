import std/[macros, strformat, sequtils, options, strutils, sugar, algorithm]
import arc, static_array

export arc, static_array, options, strutils

{.push gcsafe.}
{.push raises: [].}

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

else:
  const treeBase* = 12

static:
  echo &"Sumtree treeBase = {treeBase}"

static:
  assert treeBase mod 2 == 0

type
  Bias* = enum Left, Right

template summaryType*(I: typedesc): untyped = typeof(I.default.summary)
template summaryArrayType*(I: typedesc): untyped = Array[typeof(I.default.summary), treeBase]
template keyType*(I: typedesc): untyped = typeof(I.default.key())

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
    node* {.cursor.}: Arc[Node[I]]
    index*: int
    position*: D

  Cursor*[I; D] = object
    node* {.cursor.}: Arc[Node[I]]
    stack*: seq[StackEntry[I, D]]
    position: D
    didSeek: bool
    atEnd: bool

  FilterCursor*[I; D] = object
    cursor: Cursor[I, D]
    filterNode: proc(summary: I.summaryType): bool {.noSideEffect, raises: [].}

  Stats* = object
    height: int
    internal: int
    leaf: int
    items: int

    itemBytes: int
    summariesBytes: int
    totalBytes: int

  EditKind* = enum Insert, Remove
  Edit*[I] = object
    case kind*: EditKind
    of Insert:
      item*: I
    of Remove:
      key*: I.keyType

# static:
#   type Data = array[128, int8]
#   proc summary(d: Data): int = discard
#   const size = 12
#   echo sizeof(Data)
#   echo sizeof(Node[Data, size])
#   echo sizeof(ArcNode[Data, size])
#   echo sizeof(Array[ArcNode[Data, size], size])

# proc `=copy`*[I](a: var Node[I], b: Node[I]) {.error.}
# proc `=dup`*[I](a: Node[I]): Node[I] {.error.}

# proc `=copy`*[I](a: var SumTree[I], b: SumTree[I]) {.error.}
# proc `=dup`*[I](a: SumTree[I]): SumTree[I] {.error.}

# proc `=copy`*[I, D](a: var StackEntry[I, D], b: StackEntry[I, D]) {.error.}
# proc `=dup`*[I, D](a: StackEntry[I, D]): StackEntry[I, D] {.error.}

# proc `=copy`*[I, D](a: var Cursor[I, D], b: Cursor[I, D]) {.error.}
# proc `=dup`*[I, D](a: Cursor[I, D]): Cursor[I, D] {.error.}

proc isNil*[I](a: SumTree[I]): bool {.inline.} = a.root.isNil

proc `$`*[I](node {.byref.}: Node[I]): string =
  case node.kind:
  of Internal:
    &"Internal(h: {node.mHeight}, {node.mSummary}, children: {node.mChildren.len})"
  of Leaf:
    &"Leaf({node.mSummary}, items: {node.mItemArray.len})"

proc `$`*[I](tree {.byref.}: SumTree[I]): string = $tree.root

proc `$`*(entry: StackEntry): string = &"(p: {entry.position}, i: {entry.index})"

proc `$`*(cursor: Cursor): string =
  &"Cursor(p: {cursor.position}, s: {cursor.didSeek}, e: {cursor.atEnd}, st: {cursor.stack})"

proc pretty*[I](node {.byref.}: Node[I], id: int = 0, count: int = 0): string =
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

proc checkInvariants[I](self: ArcNode[I], allowUnderflow: bool = true) =
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

proc checkInvariants*[I](self: SumTree[I]) =
  when defined(testing):
    self.root.checkInvariants(true)

template mapOpt*[I](self: Option[I], op: untyped): untyped =
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
proc cmp*[D; C](a: End[D], b: D, cx: C): int = 1

# impl SeekAggregate for tuple[]
proc beginLeaf*[C](a: var tuple[], cx: C) = discard
proc endLeaf*[C](a: var tuple[], cx: C) = discard
proc pushItem*[I; S; C](a: var tuple[], item: I, summary: S, cx: C) = discard
proc pushTree*[I; S; C](a: var tuple[], self: ArcNode[I], summary: S, cx: C) = discard

type DebugSeekAggregate* = object

# impl SeekAggregate for DebugSeekAggregate
proc beginLeaf*[C](a: var DebugSeekAggregate, cx: C) =
  echo &"beginLeaf"

proc endLeaf*[C](a: var DebugSeekAggregate, cx: C) =
  echo &"endLeaf"

proc pushItem*[I; S; C](a: var DebugSeekAggregate, item: I, summary: S, cx: C) =
  echo &"pushItem {item}, {summary}"

proc pushTree*[I; S; C](a: var DebugSeekAggregate, self: ArcNode[I], summary: S, cx: C) =
  echo &"pushTree {self}, {summary}"

type SummarySeekAggregate*[D] = object
  value: D

# impl SeekAggregate for SummarySeekAggregate
proc beginLeaf*[D, C](a: var SummarySeekAggregate[D], cx: C) = discard
proc endLeaf*[D, C](a: var SummarySeekAggregate[D], cx: C) = discard

proc pushItem*[D; I; S, C](a: var SummarySeekAggregate[D], item: I, summary: S, cx: C) =
  mixin addSummary
  a.value.addSummary(summary, cx)

proc pushTree*[D; I; S, C](a: var SummarySeekAggregate[D], self: ArcNode[I], summary: S, cx: C) =
  mixin addSummary
  a.value.addSummary(summary, cx)

type SliceSeekAggregate*[I] = object
  node: ArcNode[I]
  leafItems: Array[I, treeBase]
  leafItemSummaries: I.summaryArrayType
  leafSummary: I.summaryType

# impl SeekAggregate for SliceSeekAggregate
proc beginLeaf*[I, C](self: var SliceSeekAggregate[I], cx: C) = discard
proc endLeaf*[I, C](self: var SliceSeekAggregate[I], cx: C) =
  self.node.append(Arc.new(Node[I](
    kind: Leaf,
    mSummary: self.leafSummary.move,
    mSummaries: self.leafItemSummaries.move,
    mItemArray: self.leafItems.move,
  )), cx)

proc pushItem*[I; S, C](self: var SliceSeekAggregate[I], item: I, summary: S, cx: C) =
  mixin addSummary
  self.leafItems.add item.clone()
  self.leafItemSummaries.add summary
  self.leafSummary.addSummary(summary, cx)

proc pushTree*[I; S, C](self: var SliceSeekAggregate[I], node: ArcNode[I], summary: S, cx: C) =
  self.node.append(node.clone(), cx)

proc pretty*[I](node{.byref.}: ArcNode[I]): string =
  let id = node.id
  let count = node.count
  node.get.pretty(id, count)

proc pretty*[I](tree {.byref.}: SumTree[I]): string =
  let id = tree.root.id
  let count = tree.root.count
  tree.root.get.pretty(id, count)

proc invert*(bias: Bias): Bias =
  case bias
  of Left: Right
  of Right: Left

proc `==`*[I](a {.byref.}: Node[I], b {.byref.}: Node[I]): bool =
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

proc isLeaf*[I](node: Node[I]): bool = node.kind == Leaf
proc isInternal*[I](node: Node[I]): bool = node.kind == Internal

proc height*[I](node: Node[I]): int =
  case node.kind
  of Internal:
    node.mHeight.int
  of Leaf:
    0

proc isLeaf*[I](node: ArcNode[I]): bool = node.get.isLeaf
proc isInternal*[I](node: ArcNode[I]): bool = node.get.isInternal

proc isLeaf*[I](tree: SumTree[I]): bool = tree.root.get.isLeaf
proc isInternal*[I](tree: SumTree[I]): bool = tree.root.get.isInternal

proc height*[I](node: ArcNode[I]): int = node.get.height
proc height*[I](tree: SumTree[I]): int = tree.root.height

proc sum*[I, C](arr {.byref.}: Array[I, treeBase], cx: C): I =
  mixin addSummary
  if arr.len == 0:
    result = I.default
  else:
    result = arr[0]
    for i in 1..arr.high:
      result.addSummary(arr[i], cx)

proc summary*[I](node: Node[I]): auto = node.mSummary
proc summary*[I](node: ArcNode[I]): auto = node.get.summary
proc summary*[I](tree: SumTree[I]): auto = tree.root.summary
proc extent*[I, C](tree: SumTree[I], D: typedesc, cx: C): D =
  result = D.default
  result.addSummary(tree.root.get.mSummary, cx)

template childSummaries*[I](node: Node[I]): untyped =
  node.mSummaries.toOpenArray

template childTrees*[I](node: Node[I]): untyped =
  node.mChildren.toOpenArray

template childItems*[I](node: Node[I]): untyped =
  node.mItemArray.toOpenArray

proc isUnderflowing*[I](node: Node[I]): bool =
  case node.kind
  of Internal:
    node.mChildren.len < treeBase div 2
  of Leaf:
    node.mItemArray.len < treeBase div 2

proc toTree*[I](node: sink ArcNode[I]): SumTree[I] =
  SumTree[I](root: node)

proc isEmpty*[I](node: ArcNode[I]): bool =
  case node.get.kind
  of Internal:
    false
  of Leaf:
    node.get.mItemArray.len == 0

proc isEmpty*[I](self: SumTree[I]): bool = self.root.isEmpty

proc newLeaf*[I](): Node[I] =
  type Summary = I.summaryType
  Node[I](kind: Leaf, mSummary: Summary.default)

proc clone*[I](a {.byref.}: Node[I]): Node[I] =
  result = Node[I](kind: a.kind)
  result.mSummary = a.mSummary
  result.mSummaries = a.mSummaries
  case a.kind
  of Internal:
    # todo: right now we're cloning the child arc, but should we deep deep copy?
    result.mHeight = a.mHeight
    result.mChildren = a.mChildren.clone()
  of Leaf:
    result.mItemArray = a.mItemArray.clone()

proc new*[I](_: typedesc[SumTree[I]]): SumTree[I] =
  Arc.new(newLeaf[I]()).toTree

proc clone*[I](a {.byref.}: SumTree[I]): SumTree[I] =
  a.root.clone().toTree

proc new*[I, C](_: typedesc[SumTree[I]], fillItems: proc(items: var ItemArray[I]) {.gcsafe, raises: [].}, cx: C): SumTree[I] =
  mixin summary
  mixin `+=`
  mixin addSummary

  var nodes: seq[Arc[Node[I]]]
  var subItems: Array[I, treeBase]
  while true:
    fillItems(subItems)
    if subItems.len == 0:
      break

    var summaries: SummaryArray[I.summaryType] = subItems.mapIt(it.summary)
    var s: I.summaryType = summaries[0]
    for k in 1..summaries.high:
      s.addSummary(summaries[k], cx)

    nodes.add Arc.new(Node[I](kind: Leaf, mSummary: s, mItemArray: subItems.move, mSummaries: summaries.move))

  var parentNodes: seq[Arc[Node[I]]] = @[]
  var height: HeightType = 0
  while nodes.len > 1:
    inc height
    var currentParentNode: Arc[Node[I]]
    var tempNodes = nodes.move
    for childNode in tempNodes.mitems:
      if currentParentNode.isNil:
        currentParentNode = Arc.new(Node[I](
          kind: Internal, mSummary: I.summaryType.default, mHeight: height
        ))

      let childSummary = childNode.get.summary
      currentParentNode.getMut.mSummary.addSummary(childSummary, cx)
      currentParentNode.getMut.mSummaries.add childSummary
      currentParentNode.getMut.mChildren.add childNode.move

      if currentParentNode.get.mChildren.len == treeBase:
        parentNodes.add currentParentNode.move
        currentParentNode = Arc[Node[I]]()

    if not currentParentNode.isNil:
      parentNodes.add currentParentNode.move
      currentParentNode = Arc[Node[I]]()

    nodes = parentNodes.move

  if nodes.len == 0:
    result = Arc.new(newLeaf[I]()).toTree
  else:
    assert nodes.len == 1
    result = nodes[0].toTree

proc new*[I, C](_: typedesc[SumTree[I]], items: openArray[I], cx: C): SumTree[I] =
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
      subItems[k] = items[i + k].clone()

    i = endIndex

    var summaries: SummaryArray[I.summaryType] = subItems.mapIt(it.summary)
    var s: I.summaryType = summaries[0]
    for k in 1..summaries.high:
      s.addSummary(summaries[k], cx)

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
      currentParentNode.get.mSummary.addSummary(childSummary, cx)
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

proc new*[I](_: typedesc[SumTree[I]], items: openArray[I]): SumTree[I] = SumTree[I].new(items, ())

proc new*[I, C](_: typedesc[SumTree[I]], items: sink seq[I], cx: C): SumTree[I] =
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
    var s: I.summaryType = summaries[0]
    for k in 1..summaries.high:
      s.addSummary(summaries[k], cx)

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
      currentParentNode.get.mSummary.addSummary(childSummary, cx)
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

proc new*[I](_: typedesc[SumTree[I]], items: sink seq[I]): SumTree[I] = SumTree[I].new(items.ensureMove, ())

proc leftmostLeaf*[I](self {.byref.}: ArcNode[I]): lent ArcNode[I] =
  case self.get.kind
  of Leaf:
    result = self
  else:
    result = self.get.mChildren[0].leftmostLeaf()

proc rightmostLeaf*[I](self {.byref.}: ArcNode[I]): lent ArcNode[I] =
  case self.get.kind
  of Leaf:
    result = self
  else:
    result = self.get.mChildren[self.get.mChildren.high].rightmostLeaf()

proc first*[I](self {.byref.}: ArcNode[I]): Option[ptr I] =
  let leaf {.cursor.} = self.leftmostLeaf
  if leaf.get.mItemArray.len > 0:
    result = leaf.get.mItemArray[0].addr.some

proc last*[I](self {.byref.}: ArcNode[I]): Option[ptr I] =
  let leaf {.cursor.} = self.rightmostLeaf
  if leaf.get.mItemArray.len > 0:
    result = leaf.get.mItemArray[leaf.get.mItemArray.high].addr.some

proc leftmostLeaf*[I](self {.byref.}: SumTree[I]): lent ArcNode[I] =
  return self.root.leftmostLeaf

proc rightmostLeaf*[I](self {.byref.}: SumTree[I]): lent ArcNode[I] =
  return self.root.rightmostLeaf

proc first*[I](self {.byref.}: SumTree[I]): Option[ptr I] =
  self.root.first

proc last*[I](self {.byref.}: SumTree[I]): Option[ptr I] =
  self.root.last

proc updateSummary*[I; C](self: var ArcNode[I], index: int, cx: C) =
  case self.get.kind
  of Internal:
    self.getMut.mSummaries[index] = self.get.mChildren[index].summary
  of Leaf:
    self.getMut.mSummaries[index] = self.get.mItemArray[index].summary()
  self.getMut.mSummary = self.get.mSummaries.sum(cx)

proc updateLastRecursive[I; C](self: var ArcNode[I], f: proc(node: var I) {.noSideEffect, raises: [].}, cx: C): Option[I.summaryType] =
  self.makeUnique()

  case self.get.kind
  of Internal:
    var lastSummary = self.getMut.mSummaries[self.get.mSummaries.high].addr
    var lastChild = self.get.mChildren[self.get.mChildren.high].addr
    lastSummary[] = lastChild[].updateLastRecursive(f, cx).get
    self.getMut.mSummary = self.get.mSummaries.sum(cx)
    self.get.mSummary.some

  of Leaf:
    if self.get.mItemArray.len > 0:
      let item = self.get.mItemArray[self.get.mItemArray.high].addr
      f(item[])
      self.getMut.mSummaries[self.get.mSummaries.high] = item[].summary()
      self.getMut.mSummary = self.get.mSummaries.sum(cx)
      self.get.mSummary.some

    else:
      I.summaryType.none

proc updateLast[I; C](self: var ArcNode[I], f: proc(node: var I) {.noSideEffect, raises: [].}, cx: C) =
  discard self.updateLastRecursive(f, cx)

proc updateLast*[I; C](self: var SumTree[I], f: proc(node: var I) {.noSideEffect, raises: [].}, cx: C) =
  self.root.updateLast(f, cx)
  self.checkInvariants()

proc updateLast*[I](self: var SumTree[I], f: proc(node: var I) {.noSideEffect, raises: [].}) =
  self.root.updateLast(f, ())

func pushTreeRecursive[I; C](self: var ArcNode[I], other: sink ArcNode[I], cx: C): Option[ArcNode[I]] =
  mixin addSummary

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
      summariesToAppend = otherNode.mSummaries
      treesToAppend =  otherNode.mChildren.clone()
    elif heightDelta == 1 and not otherNode.isUnderflowing():
      # debugf"not underflowing"
      summariesToAppend.add otherNode.mSummary
      treesToAppend.add other.clone()
    else:
      # debugf"big height delta or undeflowing"
      var treeToAppend = node.mChildren[node.mChildren.high].pushTreeRecursive(other.clone(), cx)
      node.mSummaries[node.mSummaries.high] = node.mChildren[node.mChildren.high].get.mSummary

      if treeToAppend.isSome:
        # debugf"-> {treeToAppend.get.pretty}"
        summariesToAppend.add treeToAppend.get.get.mSummary
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
            rightSummaries.add node.mSummaries[midpoint + i]
            rightTrees.add node.mChildren[midpoint + i].clone()
          for i in 0..<summariesToAppend.len:
            rightSummaries.add summariesToAppend[i]
            rightTrees.add treesToAppend[i].clone()

        elif node.mSummaries.len < midpoint:
          for i in 0..<(midpoint - node.mSummaries.len):
            leftSummaries.add summariesToAppend[i]
            leftTrees.add treesToAppend[i].clone()
          for i in (midpoint - node.mSummaries.len)..<summariesToAppend.len:
            rightSummaries.add summariesToAppend[i]
            rightTrees.add treesToAppend[i].clone()

        else:
          for i in 0..<summariesToAppend.len:
            rightSummaries.add summariesToAppend[i]
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
            rightSummaries.add node.mSummaries[midpoint + i]
            rightItems.add node.mItemArray[midpoint + i].clone()
          for i in 0..<otherNode.mSummaries.len:
            rightSummaries.add otherNode.mSummaries[i]
            rightItems.add otherNode.mItemArray[i].clone()

        elif node.mSummaries.len < midpoint:
          for i in 0..<(midpoint - node.mSummaries.len):
            leftSummaries.add otherNode.mSummaries[i]
            leftItems.add otherNode.mItemArray[i].clone()
          for i in (midpoint - node.mSummaries.len)..<otherNode.mSummaries.len:
            rightSummaries.add otherNode.mSummaries[i]
            rightItems.add otherNode.mItemArray[i].clone()

        else:
          for i in 0..<otherNode.mSummaries.len:
            rightSummaries.add otherNode.mSummaries[i]
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
      node.mSummaries.add(otherNode.mSummaries)

  return ArcNode[I].none

proc fromChildTrees[I; C](_: typedesc[ArcNode[I]], left: sink ArcNode[I], right: sink ArcNode[I], cx: C): ArcNode[I] =
  mixin addSummary

  let height = left.get.height + 1

  var childSummaries: SummaryArray[I.summaryType]
  childSummaries.add(left.get.mSummary)
  childSummaries.add(right.get.mSummary)

  var sum = childSummaries[0]
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

proc append*[I; C](self: var ArcNode[I], other: sink ArcNode[I], cx: C) =
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
        assert not splitTree.get.isNil
        self = ArcNode[I].fromChildTrees(self.clone(), splitTree.get.move, cx)

proc append*[I; C](self: var SumTree[I], other: sink SumTree[I], cx: C) =
  self.root.append(other.root, cx)
  self.checkInvariants()

proc append*[I](self: var SumTree[I], other: sink SumTree[I]) = self.append(other, ())

proc extend*[I; C](self: var SumTree[I], values: sink seq[I], cx: C) =
  self.append(SumTree[I].new(values, cx), cx)

proc extend*[I; C](self: var SumTree[I], values: sink seq[I]) = self.extend(values.ensureMove, ())

proc add*[I; C](self: var SumTree[I], item: sink I, cx: C) =
  let summary = item.summary
  self.root.append(Arc.new(Node[I](
    kind: Leaf,
    mSummary: summary,
    mSummaries: [summary].toArray(treeBase),
    mItemArray: [item.move].toArray(treeBase),
  )), cx)
  self.checkInvariants()

proc add*[I](self: var SumTree[I], item: sink I) = self.add(item.ensureMove, ())

proc cmp*[I](a, b: Edit[I]): int = cmp(a.key, b.key)
proc key*[I](a: Edit[I]): I.keyType =
  mixin key
  case a.kind
  of Insert:
    a.item.key()
  of Remove:
    a.key

proc get*[I; K; C](self: SumTree[I], key: K, cx: C): Option[ptr I] =
  var cursor = self.initCursor(K)
  if cursor.seek(key, Left, cx):
    return cursor.item
  return none(ptr I)

proc edit*[I; C](self: var SumTree[I], edits: sink seq[Edit[I]], cx: C): seq[I] =
  mixin key
  type Key = I.keyType()

  if edits.len == 0:
    return @[]

  var removed = newSeq[I]()

  proc cmpEdit(a, b: Edit[I]): int = cmp(a.key(), b.key())
  edits.sort(cmpEdit)

  self = block:
    var cursor = self.initCursor(Key)
    var newTree = SumTree[I].new()
    var bufferedItems = newSeq[I]()

    discard cursor.seek(Key.default, Left, cx)
    for edit in edits:
      let newKey = edit.key()
      var oldItem = cursor.item
      if oldItem.mapOpt(it[].key() < newKey).get(false):
        newTree.extend(bufferedItems.move, cx)
        bufferedItems = newSeq[I]()
        let slice = cursor.slice(newKey, Left, cx)
        newTree.append(slice, cx)
        oldItem = cursor.item

      if oldItem.isSome:
        if oldItem.get[].key == newKey:
          removed.add(oldItem.get[].clone())
          cursor.next(cx)

      case edit.kind
      of Insert:
        bufferedItems.add(edit.item)
      of Remove:
        discard

    newTree.extend(bufferedItems, cx)
    newTree.append(cursor.suffix(cx), cx)
    newTree

  return removed

proc initCursor*[I](self {.byref.}: SumTree[I], D: typedesc): Cursor[I, D] {.noinit.} =
  result.node = self.root
  result.stack = newSeqOfCap[StackEntry[I, D]](self.height)
  result.position = D.default
  result.didSeek = false
  result.atEnd = self.root.isEmpty

proc initCursor*[I, D](self {.byref.}: SumTree[I], init: D): Cursor[I, D] {.noinit.} =
  result.node = self.root
  result.stack = newSeqOfCap[StackEntry[I, D]](self.height)
  result.position = init
  result.didSeek = false
  result.atEnd = self.root.isEmpty

proc initCursor*[I](self {.byref.}: SumTree[I]): Cursor[I, tuple[]] {.noinit.} =
  result.node = self.root
  result.stack = newSeqOfCap[StackEntry[I, tuple[]]](self.height)
  result.position = ()
  result.didSeek = false
  result.atEnd = self.root.isEmpty

proc filter*[I, D](self: sink Cursor[I, D], filter: proc(summary: I.summaryType): bool {.noSideEffect, raises: [].}): FilterCursor[I, D] =
  result.cursor = self
  result.filterNode = filter

proc clone*[I, D](a {.byref.}: Cursor[I, D]): Cursor[I, D] =
  result.node = a.node.clone()
  result.stack = a.stack
  result.position = a.position
  result.didSeek = a.didSeek
  result.atEnd = a.atEnd

proc assertDidSeek*(self: Cursor) =
  assert self.didSeek

proc resetCursor*(self: var Cursor) =
  ## Reset the cursor to the beginning
  self.didSeek = false
  self.atEnd = self.node.isEmpty
  self.stack.setLen 0
  self.position = typeof(self.position).default

proc itemSummary*[I, D](self: Cursor[I, D]): auto =
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

proc itemSummary*[I, D](self: FilterCursor[I, D]): auto = self.cursor.itemSummary

proc seekInternal[I; D; T; A; C](self: var Cursor[I, D], target: T, bias: Bias, aggregate: var A, cx: C): bool =
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
            entry.position = self.position

          for i in entry.index..node.mChildren.high:
            let childTree {.cursor.} = node.mChildren[i]
            let childSummary {.cursor.} = node.mSummaries[i]

            # debugf"child {i}: {childSummary}"

            var childEnd = self.position
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
              entry.position = self.position

            else:
              # debugf"behind target, enter child"
              self.stack.add StackEntry[I, D](
                node: childTree,
                index: 0,
                position: self.position,
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
            var childEnd = self.position
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

  var endPosition = self.position
  if bias == Left:
    let sum = self.itemSummary()
    if sum.isSome:
      endPosition.addSummary(sum.get, cx)

  # echo &"{target} <> {endPosition} -> {target.cmp(endPosition)}"
  return target.cmp(endPosition, cx) == 0

proc seek*[I; D; T; C](self: var Cursor[I, D], target: T, bias: Bias, cx: C): bool =
  ## Resets and moves the cursor to the target. Returns true if the target position was found

  self.resetCursor()
  var agg = ()
  self.seekInternal(target, bias, agg, cx)

proc seekForward*[I; D; T; C](self: var Cursor[I, D], target: T, bias: Bias, cx: C): bool =
  ## Moves the cursor to the target. Returns true if the target position was found
  assert not self.node.isNil

  var agg = ()
  self.seekInternal(target, bias, agg, cx)

proc summary*[I; D; T; C](self: var Cursor[I, D], Output: typedesc, `end`: T, bias: Bias, cx: C): Output =
  ## Advances the cursor to `end` and returns the aggregated value of the `Output` dimension
  ## up until, but not including `end`

  var summary = SummarySeekAggregate[Output](value: Output.default)
  discard self.seekInternal(`end`, bias, summary, cx)
  summary.value.move

proc slice*[I; D; T; C](self: var Cursor[I, D], `end`: T, bias: Bias, cx: C): SumTree[I] =
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

proc slice*[I; D; T](self: var Cursor[I, D], `end`: T, bias: Bias): SumTree[I] = self.slice(`end`, bias, ())

proc suffix*[I; D; C](self: var Cursor[I, D], cx: C): SumTree[I] =
  ## Returns a new sum tree representing the remainder of the cursors tree from the current position
  ## to the end. #todo: current node included?
  self.slice(End[D](), Right, cx)

proc suffix*[I; D](self: var Cursor[I, D]): SumTree[I] = self.suffix(())

proc nextInternal[I; D; C; S](self: var Cursor[I, D], filterNode: proc(s: S): bool {.noSideEffect, raises: [].}, cx: C) =
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
          entry.position = self.position

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
            position: self.position,
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

proc next*[I; D; C](self: var Cursor[I, D], cx: C) =
  ## Moves the cursor to the next leaf
  self.nextInternal((_: I.summaryType) => true, cx)

proc next*[I; D](self: var Cursor[I, D]) =
  ## Moves the cursor to the next leaf
  self.nextInternal((_: I.summaryType) => true, ())

proc prevInternal[I; D; C](self: var Cursor[I, D], filterNode: proc(s: I.summaryType): bool {.noSideEffect, raises: [].}, cx: C) =
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
      self.position = self.stack[^2].position
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

    entry.position = self.position

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

proc prev*[I; D; C](self: var Cursor[I, D], cx: C) =
  ## Moves the cursor to the prev leaf
  self.prevInternal((_: I.summaryType) => true, cx)

proc prev*[I; D](self: var Cursor[I, D]) =
  self.prev(())

proc startPos*[I; D](self: Cursor[I, D]): lent D =
  ## Returns the aggregated value up until, but not including the current node
  self.position

proc startPos*[I; D](self: FilterCursor[I, D]): lent D = self.cursor.startPos

proc endPos*[I; D; C](self: Cursor[I, D], cx: C): D =
  ## Returns the aggregated value of the current node
  mixin addSummary

  self.assertDidSeek
  let summary = self.itemSummary
  if summary.isSome:
    result = self.position
    result.addSummary(summary.get, cx)
  else:
    result = self.position

proc endPos*[I; D](self: Cursor[I, D]): D =
  self.endPos(())

proc endPos*[I; D; C](self: FilterCursor[I, D], cx: C): D = self.cursor.endPos(cx)

proc endPos*[I; D](self: FilterCursor[I, D]): D = self.cursor.endPos(())

proc nextLeaf*[I, D](self: Cursor[I, D]): Option[ptr ArcNode[I]] =
  if self.stack.len > 0:
    assert self.stack[self.stack.high].node.isLeaf
  for i in countdown(self.stack.high - 1, 0):
    let entry {.cursor.} = self.stack[i]
    if entry.index < entry.node.get.mChildren.len - 1:
      return entry.node.get.mChildren[entry.index + 1].leftmostLeaf().addr.some

proc prevLeaf*[I, D](self: Cursor[I, D]): Option[ptr ArcNode[I]] =
  if self.stack.len > 0:
    assert self.stack[self.stack.high].node.isLeaf
  for i in countdown(self.stack.high - 1, 0):
    let entry {.cursor.} = self.stack[i]
    if entry.index > 0:
      return entry.node.get.mChildren[entry.index - 1].rightmostLeaf().addr.some

proc nextItem*[I, D](self: Cursor[I, D]): Option[ptr I] =
  if not self.didSeek:
    result = self.node.first
  elif self.stack.len > 0:
    let entry {.cursor.} = self.stack[self.stack.high]
    if entry.index == entry.node.get.mItemArray.high:
      result = self.nextLeaf().mapOpt(it[].get.mItemArray.first).flatten
    else:
      result = entry.node.get.mItemArray[entry.index + 1].addr.some

  elif not self.atEnd:
    result = self.node.first

proc prevItem*[I, D](self: Cursor[I, D]): Option[ptr I] =
  self.assertDidSeek
  if self.stack.len > 0:
    let entry {.cursor.} = self.stack[self.stack.high]
    if entry.index == 0:
      result = self.prevLeaf().mapOpt(it[].get.mItemArray.last).flatten
    else:
      result = entry.node.get.mItemArray[entry.index - 1].addr.some

  elif self.atEnd:
    result = self.node.last

proc item*[I, D](self: Cursor[I, D]): Option[ptr I] =
  # Returns the current item, or none if path the end

  self.assertDidSeek
  if self.stack.len > 0:
    let entry = self.stack[self.stack.high].addr
    let node = entry.node.get.addr
    case node.kind
    of Leaf:
      if entry.index >= node.mItemArray.len:
        return (ptr I).none
      else:
        return entry.node.get.mItemArray[entry.index].addr.some
    of Internal:
      assert false, "Stack top should contain leaf node"

  return (ptr I).none

proc item*[I, D](self: FilterCursor[I, D]): Option[ptr I] = self.cursor.item

proc itemClone*[I, D](self: Cursor[I, D]): Option[I] =
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

proc next*[I, D, C](self: var FilterCursor[I, D], cx: C) =
  self.cursor.nextInternal(self.filterNode, cx)

proc prev*[I, D, C](self: var FilterCursor[I, D], cx: C) =
  self.cursor.prevInternal(self.filterNode, cx)

proc next*[I, D](self: var FilterCursor[I, D]) = self.next(())

proc prev*[I, D](self: var FilterCursor[I, D]) = self.prev(())

proc fromSummary*[S; C](D: typedesc, summary: S, cx: C): D =
  result = D.default
  result.addSummary(summary, cx)

# impl Dimension for tuple[]
proc addSummary*[S; C](a: var tuple[], summary: S, cx: C) = discard
proc fromSummary*[S; C](_: typedesc[tuple[]], summary: S, cx: C): tuple[] = ()

proc toSeq*[I; C](self: SumTree[I], cx: C): seq[I] =
  # echo self.pretty
  var cursor = self.initCursor tuple[]
  cursor.next(cx)
  var i = cursor.item
  while i.isSome:
    result.add i.get[].clone()
    cursor.next(cx)
    i = cursor.item

proc toSeq*[I](self: SumTree[I]): seq[I] = self.toSeq(())

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
    stats.items += node.mItemArray.len
    stats.itemBytes += sizeof(node.mItemArray)

proc stats*(tree: SumTree): Stats =
  # echo "countStats ", typeof(tree), ", ", sizeof(tree.root.get)
  result.height = tree.height
  tree.root.get.countStats(result)
