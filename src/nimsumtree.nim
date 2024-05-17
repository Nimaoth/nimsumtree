import std/[macros, strformat]


type
  Arc*[T] = object
    value: ref T

  Summary* {.explain.} = concept var a, b
    a += b

  Item* {.explain.} = concept a, type T
    a.summary is Summary

template summaryType*(T: typedesc[Item]): untyped = typeof(T.default.summary)

const treeBase = 6

type
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
func `$`*[T: Item](node: Node[T]): string = &"Node({node.kind}, {node.mSummary})"
func `$`*[T: Item](tree: SumTree[T]): string = $Arc[Node[T]](tree)

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

when isMainModule:
  type TestSummary = object
    count: int
    max: int
    zeroes: int

  type TestSummary2 = object
    count: int
    max: int
    ones: int

  func `+=`(a: var TestSummary, b: TestSummary) =
    a.count += b.count
    a.max = max(a.max, b.max)
    a.zeroes += b.zeroes

  func `+=`(a: var TestSummary2, b: TestSummary2) =
    a.count += b.count
    a.max = max(a.max, b.max)
    a.ones += b.ones

  func summary(x: int): TestSummary = TestSummary(count: 1, max: x, zeroes: if x == 0: 1 else: 0)
  # func summary(x: int): TestSummary2 = TestSummary2(count: 1, max: x, ones: if x == 1: 1 else: 0)

  var node = newLeaf[int]()
  echo node
  echo node.isLeaf
  echo node.isInternal
  echo node.summary
  echo "---"

  for s in node.childSummaries:
    echo s

  echo "---"
  var tree = SumTree[int].new()
  echo $tree
