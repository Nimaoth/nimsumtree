import std/[macros, strformat, sequtils, options, strutils]
import arc, static_array

export arc, static_array

const treeBase {.intdefine.} = 1
const treeChildrenMax = treeBase * 2

type
  Summary* = concept var a, b
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

  ItemArray*[T] = Array[T, treeChildrenMax]
  ChildArray*[T] = Array[SumTree[T], treeChildrenMax]
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

  SeekAggregate* = concept var a
    type T = Item
    type S = Summary
    beginLeaf(a)
    endLeaf(a)
    pushItem(a, T, S)
    pushTree(a, SumTree[T], S)

proc `=copy`*[T](a: var Node[T], b: Node[T]) {.error.}
proc `=dup`*[T](a: Node[T]): Node[T] {.error.}

proc `=copy`*[T](a: var SumTree[T], b: SumTree[T]) {.error.}
proc `=dup`*[T](a: SumTree[T]): SumTree[T] {.error.}

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

proc `$`*[T: Item](node {.byref.}: Node[T]): string =
  case node.kind:
  of Internal:
    &"Internal(h: {node.mHeight}, {node.mSummary}, children: {node.mChildren.len})"
  of Leaf:
    &"Leaf({node.mSummary}, items: {node.mItems.len})"
proc `$`*[T: Item](tree {.byref.}: SumTree[T]): string = $Arc[Node[T]](tree)
proc `$`*(entry: StackEntry): string = &"(p: {entry.position}, i: {entry.index})"
proc `$`*(cursor: Cursor): string = &"Cursor(p: {cursor.position}, s: {cursor.didSeek}, e: {cursor.atEnd}, st: {cursor.stack})"

func pretty*[T: Item](node {.byref.}: Node[T], id: int = 0, count: int = 0): string =
  case node.kind:
  of Internal:
    result = &"Internal(_{id}, #{count}, {node.mSummary}, {node.mChildren.high}):\n"
    for i in 0..node.mChildren.high:
      if i > 0:
        result.add "\n"
      result.add node.mChildren[i].asNode.pretty(
        node.mChildren[i].asArc.id, node.mChildren[i].asArc.count).indent(2)

  of Leaf:
    result = &"Leaf(_{id}, #{count}, {node.mSummary}, {node.mItems})"

func pretty*[T: Item](tree {.byref.}: SumTree[T]): string =
  let id = Arc[Node[T]](tree).id
  let count = Arc[Node[T]](tree).count
  tree.asNode.pretty(id, count)

func invert*(bias: Bias): Bias =
  case bias
  of Left: Right
  of Right: Left

func isLeaf*[T: Item](node: Node[T]): bool = node.kind == Leaf
func isInternal*[T: Item](node: Node[T]): bool = node.kind == Internal

func height*[T: Item](node: Node[T]): int =
  case node.kind
  of Internal:
    node.mHeight.int
  of Leaf:
    0

func height*[T: Item](tree: SumTree[T]): int = tree.asNode.height

func sum*[T: Summary; C: static int](arr {.byref.}: Array[T, C]): T =
  assert arr.len > 0
  result = arr[0].clone()
  for i in 1..arr.high:
    result.addSummary(arr[i])

func summary*[T: Item](node: Node[T]): T.summaryType = node.mSummary
func summary*[T: Item](tree: SumTree[T]): T.summaryType = tree.asNode.summary

template childSummaries*[T: Item](node: Node[T]): untyped =
  node.mSummaries.toOpenArray

template childTrees*[T: Item](node: Node[T]): untyped =
  node.mChildren.toOpenArray

template childItems*[T: Item](node: Node[T]): untyped =
  node.mItems.toOpenArray

func isUnderflowing*[T](node: Node[T]): bool =
  case node.kind
  of Internal:
    node.mChildren.len < treeBase
  of Leaf:
    node.mItems.len < treeBase

template asArc*[T: Item](tree: SumTree[T]): Arc[Node[T]] =
  Arc[Node[T]](tree)

func asNode*[T: Item](tree: var SumTree[T]): var Node[T] =
  Arc[Node[T]](tree).get

func asNode*[T: Item](tree: SumTree[T]): lent Node[T] =
  Arc[Node[T]](tree).get

func isEmpty*[T: Item](tree: SumTree[T]): bool =
  case tree.asNode.kind
  of Internal:
    false
  of Leaf:
    tree.asNode.mItems.len == 0

func newLeaf*[T](): Node[T] =
  type Summary = T.summaryType
  Node[T](kind: Leaf, mSummary: Summary.default)

proc clone*[T](a {.byref.}: Node[T]): Node[T] =
  result = Node[T](kind: a.kind)
  result.mSummary = a.mSummary.clone()
  result.mSummaries = a.mSummaries.clone()
  case a.kind
  of Internal:
    # todo: right now we're cloning the child arc, but should we deep deep copy?
    result.mHeight = a.mHeight
    result.mChildren = a.mChildren.clone()
  of Leaf:
    result.mItems = a.mItems.clone()

proc clone*[T](a {.byref.}: SumTree[T]): SumTree[T] =
  SumTree[T](Arc[Node[T]](a).clone())

proc getUnique*[T: Item](a: var SumTree[T]): var Node[T] =
  # echo &"getUnique _{a.asArc.id[]}, {a.asArc.count[]}"
  if a.asArc.count[] > 1:
    a = SumTree[T](Arc[Node[T]].new(a.asArc.value[].clone()))
  return a.asArc.value[]

proc makeUnique*[T: Item](a: var SumTree[T]) =
  # echo &"makeUnique _{a.asArc.id[]}, {a.asArc.count[]}"
  if a.asArc.count > 1:
    # Note: clone the node, not the arc
    a = SumTree[T](Arc[Node[T]].new(a.asNode.clone()))

proc new*[T: Item](_: typedesc[SumTree[T]], items: openArray[T]): SumTree[T] =
  mixin summary
  mixin `+=`
  mixin addSummary

  var nodes: seq[Node[T]]
  var i = 0
  while i < items.len:
    let endIndex = min(i + treeChildrenMax, items.len)
    var subItems: ItemArray[T]
    subItems.len = endIndex - i
    for k in 0..<subItems.len:
      subItems[k] = items[i + k]

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
    result = SumTree[T](Arc[Node[T]].new(newLeaf[T]()))
  else:
    assert nodes.len == 1
    result = SumTree[T](Arc[Node[T]].new(nodes[0].move))

  # echo result.pretty
  # echo "-----------------"

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
  template debugf(str: static string): untyped =
    # echo indent(&str, 1)
    discard

  debugf "pushTreeRecursive:\n- tree: {tree.pretty.indent(1)}\n- other: {other.pretty.indent(1)}"

  tree.makeUnique()
  template node: Node[T] = tree.asNode

  case node.kind
  of Internal:
    debugf"--- internal"
    let otherNode {.cursor.} = other.asNode

    node.mSummary.addSummary(otherNode.mSummary)

    let heightDelta = node.height - otherNode.height
    var summariesToAppend: SummaryArray[T.summaryType]
    var treesToAppend: ChildArray[T]

    debugf"height: {node.mHeight}, {otherNode.height}, d: {heightDelta}, other underflow: {otherNode.isUnderflowing}"
    if heightDelta == 0:
      summariesToAppend = otherNode.mSummaries.clone()
      treesToAppend =  otherNode.mChildren.clone()
    elif heightDelta == 1 and not otherNode.isUnderflowing():
      debugf"not underflowing"
      summariesToAppend.add otherNode.summary.clone()
      treesToAppend.add other
    else:
      debugf"big height delta or undeflowing"
      var treeToAppend = node.mChildren[node.mChildren.high].pushTreeRecursive(other)
      node.mSummaries[node.mSummaries.high] = node.mChildren[node.mChildren.high].asNode.summary.clone()

      if treeToAppend.isSome:
        debugf"-> {treeToAppend.get.pretty}"
        summariesToAppend.add treeToAppend.get.asNode.summary.clone()
        treesToAppend.add treeToAppend.get.move
      else:
        debugf"-> none"

    debugf"toAppend: {summariesToAppend}, {treesToAppend}"
    assert summariesToAppend.len == treesToAppend.len

    let childCount = node.mChildren.len + treesToAppend.len
    debugf"childCount: {node.mChildren.len}, {treesToAppend.len}, {childCount}"
    if childCount > treeChildrenMax:
      debugf"over max, split"
      var leftSummaries: SummaryArray[T.summaryType]
      var rightSummaries: SummaryArray[T.summaryType]
      var leftTrees: ChildArray[T]
      var rightTrees: ChildArray[T]

      let midpoint = (childCount + childCount mod 2) div 2
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
            rightTrees.add treesToAppend[midpoint + i].clone()

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

      node.mSummary = leftSummaries.sum()
      node.mSummaries = leftSummaries
      node.mChildren = leftTrees


      return some(SumTree[T](Arc[Node[T]].new(Node[T](
        kind: Internal,
        mHeight: node.mHeight,
        mSummary: rightSummaries.sum(),
        mSummaries: rightSummaries,
        mChildren: rightTrees,
      ))))

    else:
      node.mSummaries.add summariesToAppend
      node.mChildren.add treesToAppend
      debugf"extend internal {tree.pretty}"
      return SumTree[T].none

  of Leaf:
    debugf"--- leaf"

    let otherNode {.cursor.} = other.asNode
    let childCount = node.mItems.len + otherNode.mItems.len
    if childCount > treeChildrenMax:
      var leftSummaries: SummaryArray[T.summaryType]
      var rightSummaries: SummaryArray[T.summaryType]
      var leftItems: ItemArray[T]
      var rightItems: ItemArray[T]

      let midpoint = (childCount + childCount mod 2) div 2
      block:
        for i in 0..<min(midpoint, node.mSummaries.len):
          leftSummaries.add node.mSummaries[i]
          leftItems.add node.mItems[i]

        if node.mSummaries.len > midpoint:
          for i in 0..<(node.mSummaries.len - midpoint):
            rightSummaries.add node.mSummaries[midpoint + i].clone()
            rightItems.add node.mItems[midpoint + i]
          for i in 0..<otherNode.mSummaries.len:
            rightSummaries.add otherNode.mSummaries[i].clone()
            rightItems.add otherNode.mItems[midpoint + i]

        elif node.mSummaries.len < midpoint:
          for i in 0..<(midpoint - node.mSummaries.len):
            leftSummaries.add otherNode.mSummaries[i].clone()
            leftItems.add otherNode.mItems[i]
          for i in (midpoint - node.mSummaries.len)..<otherNode.mSummaries.len:
            rightSummaries.add otherNode.mSummaries[i].clone()
            rightItems.add otherNode.mItems[i]

        else:
          for i in 0..<otherNode.mSummaries.len:
            rightSummaries.add otherNode.mSummaries[i].clone()
            rightItems.add otherNode.mItems[i]

      assert leftSummaries.len == leftItems.len
      assert rightSummaries.len == rightItems.len

      node.mSummary = leftSummaries.sum()
      node.mSummaries = leftSummaries
      node.mItems = leftItems

      return some(SumTree[T](Arc[Node[T]].new(Node[T](
        kind: Leaf,
        mSummary: rightSummaries.sum(),
        mSummaries: rightSummaries,
        mItems: rightItems,
      ))))

    else:
      node.mSummary.addSummary(otherNode.mSummary)
      node.mItems.add(otherNode.mItems)
      node.mSummaries.add(otherNode.mSummaries.clone())

proc fromChildTrees*[T: Item](_: typedesc[SumTree[T]], left: sink SumTree[T], right: sink SumTree[T]): SumTree[T] =
  # echo &"--- fromChildTrees: {left}, {right}"
  # echo left.pretty
  # echo "---"
  # echo right.pretty

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

  result = SumTree[T](
    Arc[Node[T]].new(
      Node[T](
        kind: Internal,
        mHeight: height.uint8,
        mSummary: sum,
        mSummaries: childSummaries.move,
        mChildren: childTrees.move,
      )
    )
  )

  # echo "---"
  # echo result.pretty
  # echo "--> fromChildTrees"

proc append*[T: Item](self: var SumTree[T], other: sink SumTree[T]) =
  # echo &"append {self}, {other}"
  if self.isEmpty:
    self = other.move
  elif other.asNode.isInternal or other.asNode.mItems.len > 0:
    if self.asNode.height < other.asNode.height:
      assert other.asNode.isInternal
      for tree in other.asNode.childTrees:
        self.append tree.clone()

    else:
      var splitTree = self.pushTreeRecursive(other.move)
      if splitTree.isSome:
        self = SumTree[T].fromChildTrees(self.clone(), splitTree.get.move)

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

proc seekInternal[T: Item; D: Dimension; Target; Aggregate](
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

proc seekForward*[T: Item; D: Dimension; Target](
    self: var Cursor[T, D], target: Target, bias: Bias): bool =
  ## Moves the cursor to the target

  var agg = ()
  self.seekInternal(target, bias, agg)

proc summary*[T: Item; D: Dimension; Target](
    self: var Cursor[T, D], Output: typedesc[Dimension], `end`: Target, bias: Bias): Output =
  ## Advances the cursor to `end` and returns the aggregated value of the `Output` dimension
  ## up until, but not including `end`

  var summary = SummarySeekAggregate[Output](value: Output.default)
  discard self.seekInternal(`end`, bias, summary)
  summary.value.move

func itemSummary*[T: Item, D: Dimension](self: Cursor[T, D]): Option[T.summaryType] =
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

proc first*[T: Item; D: Dimension](self: Cursor[T, D]): lent D =
  ## Returns the aggregated value up until, but not including the current node
  self.position

proc last*[T: Item; D: Dimension](self: Cursor[T, D]): D =
  ## Returns the aggregated value of the current node

  let summary = self.itemSummary
  if summary.isSome:
    result = self.position.clone()
    result.addSummary(summary.get)
  else:
    result = self.position.clone()

func item*[T: Item, D: Dimension](self: Cursor[T, D]): Option[T] =
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
