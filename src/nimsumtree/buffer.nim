import std/[strutils, sequtils, strformat, tables, sets, sugar, algorithm]
import clock

import nimsumtree/sumtree
import rope

type
  Fragment* = object
    loc*: Locator
    timestamp*: Lamport
    insertionOffset*: int
    len*: int
    visible*: bool
    deletions*: HashSet[Lamport]
    maxUndos: Global

  FragmentTextSummary = object
    visible: int
    deleted: int

  FragmentSummary = object
    text: FragmentTextSummary
    maxId: Locator
    maxVersion: Global
    minInsertionVersion: Global
    maxInsertionVersion: Global

  FullOffset* = distinct int

  VersionedFullOffset* = distinct Option[FullOffset]

  UndoMapKey = object
    editId: Lamport
    undoId: Lamport

  UndoMapEntry = object
    key: UndoMapKey
    undoCount: uint32

  TransactionId* = Lamport
  Transaction* = object
    id*: TransactionId
    editIds*: seq[Lamport]
    start: Global

  InsertionSlice* = object
    insertionId: Lamport
    range: Range[int]

  InsertionFragment = object
    timestamp: Lamport
    splitOffset: int
    fragmentId: Locator

  InsertionFragmentKey = object
    timestamp: Lamport
    splitOffset: int

  Edit*[D] = object
    old*: Range[D]
    new*: Range[D]

  Patch*[D] = object
    edits*: seq[Edit[D]]

  Anchor* = object
    timestamp*: Lamport
    offset*: int
    bias: Bias

func fullOffset*(offset: VersionedFullOffset): FullOffset =
  let self = (Option[FullOffset])(offset)
  assert self.isSome
  self.get

# impl Item for Fragment
func clone*(a: Fragment): Fragment = a

func summary*(self: Fragment): FragmentSummary =
  var maxVersion = initGlobal()
  maxVersion.observe(self.timestamp)
  for d in self.deletions:
    maxVersion.observe(d)

  # todo: undo

  let textSummary = if self.visible:
    FragmentTextSummary(visible: self.len)
  else:
    FragmentTextSummary(deleted: self.len)

  var minInsertionVersion = initGlobal()
  minInsertionVersion.observe(self.timestamp)
  let maxInsertionVersion = minInsertionVersion

  FragmentSummary(
    maxId: self.loc,
    text: textSummary,
    maxVersion: maxVersion,
    minInsertionVersion: minInsertionVersion,
    maxInsertionVersion: maxInsertionVersion,
  )

func insertionSlice(self: Fragment): InsertionSlice =
  InsertionSlice(
    insertionId: self.timestamp,
    range: self.insertionOffset...(self.insertionOffset + self.len),
  )

func `$`*(self: FullOffset): string {.borrow.}
func `<`*(a, b: FullOffset): bool {.borrow.}
func `<=`*(a, b: FullOffset): bool {.borrow.}
func `==`*(a, b: FullOffset): bool {.borrow.}
func `+`*(a, b: FullOffset): FullOffset {.borrow.}
func `-`*(a, b: FullOffset): FullOffset {.borrow.}
func cmp*(a, b: FullOffset): int {.borrow.}
func `$`*(self: VersionedFullOffset): string {.borrow.}

func addSummary*[A, B, C](a: var (A, B), b: FragmentSummary, cx: C) =
  a[0].addSummary(b, cx)
  a[1].addSummary(b, cx)

# impl Item for UndoMapEntry
func clone*(self: UndoMapEntry): UndoMapEntry = self
func summary*(self: UndoMapEntry): UndoMapKey = self.key

# impl KeyedItem for UndoMapEntry
func key*(self: UndoMapEntry): UndoMapKey = self.key

# impl Summary for UndoMapKey
func `<`*(a, b: UndoMapKey): bool =
  if a.editId == b.editId:
    return a.undoId < b.undoId
  return a.editId < b.editId

func `<=`*(a, b: UndoMapKey): bool =
  if a.editId == b.editId:
    return a.undoId <= b.undoId
  return a.editId <= b.editId

func cmp*[C](a, b: UndoMapKey, cx: C): int =
  if a.editId == b.editId:
    return cmp(a.undoId, b.undoId)
  return cmp(a.editId, b.editId)

func addSummary*[C](a: var UndoMapKey, b: UndoMapKey, cx: C) =
  a = max(a, b)

func clone*(a: UndoMapKey): UndoMapKey = a

# impl Item for InsertionFragment
func clone*(self: InsertionFragment): InsertionFragment = self
func summary*(self: InsertionFragment): InsertionFragmentKey =
  InsertionFragmentKey(timestamp: self.timestamp, splitOffset: self.splitOffset)

# impl KeyedItem for InsertionFragment
func key*(self: InsertionFragment): InsertionFragmentKey = self.summary

# impl Summary for InsertionFragmentKey
func `<`*(a, b: InsertionFragmentKey): bool =
  if a.timestamp == b.timestamp:
    return a.splitOffset < b.splitOffset
  return a.timestamp < b.timestamp

func `<=`*(a, b: InsertionFragmentKey): bool =
  if a.timestamp == b.timestamp:
    return a.splitOffset <= b.splitOffset
  return a.timestamp <= b.timestamp

func cmp*[C](a, b: InsertionFragmentKey, cx: C): int =
  if a.timestamp == b.timestamp:
    return cmp(a.splitOffset, b.splitOffset)
  return cmp(a.timestamp, b.timestamp)

func addSummary*[C](a: var InsertionFragmentKey, b: InsertionFragmentKey, cx: C) =
  a = b

func clone*(a: InsertionFragmentKey): InsertionFragmentKey = a

func new*(_: typedesc[InsertionFragment], fragment: Fragment): InsertionFragment =
  InsertionFragment(
    timestamp: fragment.timestamp,
    splitOffset: fragment.insertionOffset,
    fragmentId: fragment.loc
  )

func insertNew*(_: typedesc[InsertionFragment], fragment: Fragment): sumtree.Edit[InsertionFragment] =
  sumtree.Edit[InsertionFragment](kind: Insert, item: InsertionFragment.new(fragment))

# impl Summary for FragmentSummary
func addSummary*[C](a: var FragmentSummary, b: FragmentSummary, cx: C) =
  a.maxId = b.maxId
  a.text.visible += b.text.visible
  a.text.deleted += b.text.deleted
  a.maxVersion.join(b.maxVersion)
  a.minInsertionVersion.meet(b.minInsertionVersion)
  a.maxInsertionVersion.join(b.maxInsertionVersion)

func clone*(a: FragmentTextSummary): FragmentTextSummary = a
func clone*(a: FragmentSummary): FragmentSummary = a

# impl Dimension for int
func addSummary*[C](a: var int, b: FragmentSummary, cx: C) = a += b.text.visible
func fromSummary*[C](_: typedesc[int], a: FragmentSummary, cx: C): int = a.text.visible
# impl SeekTarget for int
func cmp*[C](a: int, b: FragmentSummary, cx: C): int = cmp(a, b.text.visible)
func cmp*[C](a: int, b: FragmentTextSummary, cx: C): int = cmp(a, b.visible)

# impl Dimension for FullOffset
func addSummary*[C](a: var FullOffset, b: FragmentSummary, cx: C) = a = a + FullOffset(b.text.visible + b.text.deleted)
func fromSummary*[C](_: typedesc[FullOffset], a: FragmentSummary, cx: C): FullOffset = FullOffset(a.text.visible + a.text.deleted)

# impl Dimension for FragmentTextSummary
func addSummary*[C](a: var FragmentTextSummary, b: FragmentSummary, cx: C) =
  a.visible += b.text.visible
  a.deleted += b.text.deleted

func fromSummary*[C](_: typedesc[FragmentTextSummary], a: FragmentSummary, cx: C): FragmentTextSummary = a.text


# impl Dimension for VersionedFullOffset
func default*(_: typedesc[VersionedFullOffset]): VersionedFullOffset = 0.FullOffset.some.VersionedFullOffset
func clone*(a: VersionedFullOffset): VersionedFullOffset = a

func addSummary*(a: var VersionedFullOffset, summary: FragmentSummary, cx: Option[Global]) =
  let self = (Option[FullOffset])(a)
  if self.isSome:
    let version {.cursor.} = cx.get
    if version.observedAll(summary.maxInsertionVersion):
      # debugEcho &"addSummary: {a}, {summary}, {cx}: observed"
      a = (self.get + FullOffset(summary.text.visible + summary.text.deleted)).some.VersionedFullOffset
    elif version.observedAny(summary.minInsertionVersion):
      # debugEcho &"addSummary: {a}, {summary}, {cx}: not observed"
      a = FullOffset.none.VersionedFullOffset

func fromSummary*[C](_: typedesc[VersionedFullOffset], a: FragmentSummary, cx: C): VersionedFullOffset =
  result = 0.FullOffset.some.VersionedFullOffset
  result.addSummary(a, cx)

# impl SeekTarget for VersionedFullOffset
func cmp*[C](a: VersionedFullOffset, b: VersionedFullOffset, cx: C): int =
  let a = (Option[FullOffset])(a)
  let b = (Option[FullOffset])(b)
  if a.isSome and b.isSome:
    cmp(a.get, b.get)
  elif a.isSome:
    -1
  else:
    assert false
    0

# impl Dimension for Option[Locator]
func clone*(a: Option[Locator]): Option[Locator] = a
func addSummary*[C](a: var Option[Locator], b: FragmentSummary, cx: C) = a = b.maxId.some
func fromSummary*[C](_: typedesc[Option[Locator]], a: FragmentSummary, cx: C): Option[Locator] = a.maxId.some
func cmp*[C](a: Option[Locator], b: Option[Locator], cx: C): int =
  if a.isSome and b.isSome:
    cmp(a.get, b.get)
  elif a.isSome:
    1
  elif b.isSome:
    -1
  else:
    0

type
  EditOperation* = object
    timestamp*: Lamport
    version*: Global
    ranges*: seq[Slice[FullOffset]]
    newTexts*: seq[string]

  UndoOperation* = object
    timestamp*: Lamport
    version*: Global
    counts*: Table[Lamport, uint32]

  OperationKind* {.pure.} = enum Edit, Undo
  Operation* = object
    case kind*: OperationKind
    of Edit:
      edit*: EditOperation
    of Undo:
      undo*: UndoOperation

  UndoMap* = object
    tree*: SumTree[UndoMapEntry]

  HistoryEntry = object
    transaction: Transaction

  History* = object
    baseText*: Rope
    operations*: Table[Lamport, Operation]
    insertionSlices: Table[Lamport, seq[InsertionSlice]]
    undoStack*: seq[HistoryEntry]
    redoStack*: seq[HistoryEntry]

  BufferSnapshot* = object
    replicaId*: ReplicaId
    visibleText*: Rope
    deletedText*: Rope
    undoMap*: UndoMap
    fragments*: SumTree[Fragment]
    insertions: SumTree[InsertionFragment]
    version*: Global

  Buffer* = object
    snapshot: BufferSnapshot
    history*: History
    timestamp*: Lamport
    deferredOps: seq[Operation]
    deferredReplicas: HashSet[ReplicaId]
    patches*: seq[tuple[editId: Lamport, patch: Patch[uint32]]]

proc edit*(self: var Buffer, edits: openArray[tuple[range: Slice[int], text: string]]): Operation

func `$`*(self: Fragment): string = &"I({self.timestamp}:{self.loc}, {self.insertionOffset}, visible: {self.visible}, {self.len}, deletions: {self.deletions})"
func `$`*(self: seq[Fragment]): string = "Is(\n" & self.mapIt($it).join("\n").indent(1, "  ") & "\n)"
func contentString*(self: Buffer): string = $self.snapshot.visibleText
func contentLength*(self: Buffer): int = self.snapshot.visibleText.bytes
proc applyUndo*(self: var Buffer, undo: UndoOperation): bool
func summaryForAnchor(self: BufferSnapshot, D: typedesc, anchor: Anchor): Option[D]

func len*(self: BufferSnapshot): int = self.visibleText.bytes
func ownVersion*(self: BufferSnapshot): SeqNumber = self.version[self.replicaId]

func toOffset*(offset: int, snapshot: BufferSnapshot): int = offset
func toOffset*(point: Point, snapshot: BufferSnapshot): int = snapshot.visibleText.pointToOffset(point)
func toOffset*(anchor: Anchor, snapshot: BufferSnapshot): int = snapshot.summaryForAnchor(int, anchor).get

func version*(op: Operation): Global =
  case op.kind
  of Edit:
    op.edit.version
  of Undo:
    op.undo.version

func timestamp*(op: Operation): Lamport =
  case op.kind
  of Edit:
    op.edit.timestamp
  of Undo:
    op.undo.timestamp

func `$`*(self: Buffer): string =
  result = &"Buffer({self.snapshot.replicaId}, {self.snapshot.version}, {self.timestamp}\n"
  result.add &"  visibleText: {self.snapshot.visibleText}\n"
  result.add &"  deletedText: {self.snapshot.deletedText}\n"
  result.add &"  fragmentsNew: {($self.snapshot.fragments.toSeq(())).indent(2).strip}\n"

func low*(_: typedesc[Anchor]): Anchor = Anchor(timestamp: Lamport.low, offset: 0, bias: Left)
func high*(_: typedesc[Anchor]): Anchor = Anchor(timestamp: Lamport.high, offset: int.high, bias: Right)

func fragmentIdForAnchor(self: BufferSnapshot, anchor: Anchor): Locator =
  if anchor == Anchor.low:
    return Locator.low
  if anchor == Anchor.high:
    return Locator.high

  let anchorKey = InsertionFragmentKey(timestamp: anchor.timestamp, splitOffset: anchor.offset)
  var cursor = self.insertions.initCursor(InsertionFragmentKey)
  discard cursor.seek(anchorKey, anchor.bias, ())
  if cursor.item.isSome:
    let insertion {.cursor.} = cursor.item.get[]
    let comparison = cmp(insertion.key(), anchorKey)
    if comparison > 0 or (comparison == 0 and anchor.bias == Left and anchor.offset > 0):
      cursor.prev(())
  else:
    cursor.prev(())

  if cursor.item.isSome:
    let insertion {.cursor.} = cursor.item.get[]
    assert insertion.timestamp == anchor.timestamp
    return insertion.fragmentId

  assert false, &"Anchor {anchor}, not found in {self}"

func textSummaryForRange[O](self: BufferSnapshot, D: typedesc, range: Range[O]): D =
  var cursor = self.visibleText.cursor(range.a.toOffset(self))
  cursor.summary(D, range.b.toOffset(self))

func summaryForAnchor(self: BufferSnapshot, D: typedesc, anchor: Anchor): Option[D] =
  if anchor == Anchor.low:
    return D.default.some
  if anchor == Anchor.high:
    return D.fromSummary(self.visibleText.summary, ()).some

  let anchorKey = InsertionFragmentKey(timestamp: anchor.timestamp, splitOffset: anchor.offset)
  var cursor = self.insertions.initCursor(InsertionFragmentKey)
  discard cursor.seek(anchorKey, anchor.bias, ())
  if cursor.item.isSome:
    let insertion {.cursor.} = cursor.item.get[]
    let comparison = cmp(insertion.key(), anchorKey)
    if comparison > 0 or (comparison == 0 and anchor.bias == Left and anchor.offset > 0):
      cursor.prev(())
  else:
    cursor.prev(())

  if cursor.item.isSome:
    let insertion {.cursor.} = cursor.item.get[]
    assert insertion.timestamp == anchor.timestamp
    var fragmentCursor = self.fragments.initCursor (Option[Locator], int)
    discard fragmentCursor.seek(insertion.fragmentId.some, Left, ())
    let fragment = fragmentCursor.item.get
    var fragmentOffset = fragmentCursor.startPos[1]
    if fragment[].visible:
      fragmentOffset += anchor.offset - insertion.splitOffset
    return self.textSummaryForRange(D, 0...fragmentOffset).some

  return D.none

func summary*(self: Anchor, D: typedesc, snapshot: BufferSnapshot): D =
  return snapshot.summaryForAnchor(D, self).get

func anchorAtOffset(self: BufferSnapshot, offset: int, bias: Bias): Anchor =
  if bias == Left and offset == 0:
    Anchor.low
  elif bias == Right and offset == self.len:
    Anchor.high
  else:
    var cursor = self.fragments.initCursor(int)
    discard cursor.seek(offset, bias, ())
    let fragment {.cursor.} = cursor.item.get[]
    let overshoot = offset - cursor.startPos
    Anchor(
      timestamp: fragment.timestamp,
      offset: fragment.insertionOffset + overshoot,
      bias: bias,
    )

func anchorAt*[D](self: BufferSnapshot, position: D, bias: Bias): Anchor =
  self.anchorAtOffset(position.toOffset(self), bias)

func anchorBefore*[D](self: BufferSnapshot, position: D): Anchor =
  self.anchorAt(position, Left)

func anchorAfter*[D](self: BufferSnapshot, position: D): Anchor =
  self.anchorAt(position, Right)

func canResolve*(self: BufferSnapshot, anchor: Anchor): bool =
  anchor == Anchor.low or anchor == Anchor.high or self.version.observed(anchor.timestamp)

func cmp*(a, b: Anchor, buffer: BufferSnapshot): int =
  let fragmentIdComparison = if a.timestamp == b.timestamp:
    0
  else:
    cmp(buffer.fragmentIdForAnchor(a), buffer.fragmentIdForAnchor(b))
  if fragmentIdComparison != 0:
    return fragmentIdComparison
  if a.offset != b.offset:
    return cmp(a.offset, b.offset)
  return cmp(a.bias, b.bias)

func initEdit*[D](old, new: Range[D]): Edit[D] = Edit[D](old: old, new: new)

func isEmpty*[D](self: Edit[D]): bool =
  self.old.len == 0 and self.new.len == 0

func flatten*[D1, D2](self: Edit[(D1, D2)]): (Edit[D1], Edit[D2]) =
  (
    Edit[D1](
      old: self.old.a[0]...self.old.b[0],
      new: self.new.a[0]...self.new.b[0],
    ),
    Edit[D2](
      old: self.old.a[1]...self.old.b[1],
      new: self.new.a[1]...self.new.b[1],
    )
  )

func invert*[D](self: var Patch[D]): var Patch[D] =
  for edit in self.edits.mitems:
    swap(edit.old, edit.new)
  self

func clear*[D](self: var Patch[D]) =
  self.edits.setLen(0)

func isEmpty*[D](self: Patch[D]): bool =
  self.edits.len == 0

func add*[D](self: var Patch[D], edit: Edit[D]) =
  if edit.isEmpty:
    return

  if self.edits.len > 0:
    var last = self.edits[^1].addr
    if last[].old.b >= edit.old.a:
      last[].old.b = edit.old.b
      last[].new.b = edit.new.b
      return

  self.edits.add edit

proc binarySearchBy[T, K](a: openArray[T], key: K,
                         cmp: proc (x: T, y: K): int {.closure.}): (bool, int) {.effectsOf: cmp.} =
  ## Binary search for `key` in `a`. Return the index of `key` and whether is was found
  ## Assumes that `a` is sorted according to `cmp`.
  ##
  ## `cmp` is the comparator function to use, the expected return values are
  ## the same as those of system.cmp.
  runnableExamples:
    assert binarySearch(["a", "b", "c", "d"], "d", system.cmp[string]) == 3
    assert binarySearch(["a", "b", "c", "d"], "c", system.cmp[string]) == 2
  let len = a.len

  if len == 0:
    return (false, 0)

  if len == 1:
    if cmp(a[0], key) == 0:
      return (true, 0)
    else:
      return (false, 0)

  result = (true, 0)

  var idx = 0
  if (len and (len - 1)) == 0:
    # when `len` is a power of 2, a faster shr can be used.
    var step = len shr 1
    var cmpRes: int
    while step > 0:
      let i = idx or step
      cmpRes = cmp(a[i], key)
      if cmpRes == 0:
        return (true, i)

      if cmpRes < 0:
        idx = i
      step = step shr 1
    if cmp(a[idx], key) != 0: result[0] = false
  else:
    var b = len
    var cmpRes: int
    while idx < b:
      var mid = (idx + b) shr 1
      cmpRes = cmp(a[mid], key)
      if cmpRes == 0:
        return (true, mid)

      if cmpRes < 0:
        idx = mid + 1
      else:
        b = mid
    if idx >= len or cmp(a[idx], key) != 0: result[0] = false

  result[1] = idx

func oldToNew*[D](self: Patch[D], old: D): D =
  func cmpFn(a: Edit[D], b: D): int = cmp(a.old.a, old)
  let (found, idx) = self.patches.binarySearchBy(old, cmpFn)
  let index = if found:
    idx
  elif idx == 0:
    return old
  else:
    idx - 1

  if idx in 0..self.edits.high:
    if old >= self.old.b:
      return edit.new.b + (old - edit.old.b)
    else:
      return edit.new.a
  else:
    return old

func initFragment*(timestamp: Lamport, len: int, insertionOffset: int, loc: Locator): Fragment =
  result = Fragment(
    timestamp: timestamp,
    insertionOffset: insertionOffset,
    loc: loc,
    len: len,
    visible: true,
    deletions: initHashSet[Lamport](),
  )

func new*(_: typedesc[UndoMap]): UndoMap = UndoMap(tree: SumTree[UndoMapEntry].new())
func clone*(self: UndoMap): UndoMap = UndoMap(tree: self.tree.clone())

proc initHistory*(baseText: sink Rope): History =
  History(
    baseText: baseText
  )

proc initBuffer*(replicaId: ReplicaId = 0.ReplicaId, content: string = ""): Buffer =
  let history = initHistory(Rope.new(content))

  var fragments = SumTree[Fragment].new()
  var insertions = SumTree[InsertionFragment].new()

  var timestamp = initLamport(replicaId, 1)
  var version = initGlobal()

  let visibleText = history.baseText.clone()
  if visibleText.bytes > 0:
    var insertionTimestamp = initLamport(0.ReplicaId, 1)
    timestamp.observe(insertionTimestamp)
    version.observe(insertionTimestamp)
    let fragment = Fragment(
      timestamp: insertionTimestamp,
      insertionOffset: 0,
      loc: between(Locator.low, Locator.high),
      len: content.len,
      visible: true,
      deletions: initHashSet[Lamport](),
      maxUndos: initGlobal(),
    )
    insertions.add(InsertionFragment.new(fragment), ())
    fragments.add(fragment, ())
    discard timestamp.tick()

  result = Buffer(
    snapshot: BufferSnapshot(
      replicaId: replicaId,
      fragments: fragments,
      visibleText: visibleText,
      deletedText: Rope.new(),
      undoMap: UndoMap.new(),
      insertions: SumTree[InsertionFragment].new(),
      version: version,
    ),
    history: history,
    deferredOps: @[],
    deferredReplicas: initHashSet[ReplicaId](),
    timestamp: timestamp,
  )

proc clone*(self: History): History =
  result.baseText = self.baseText.clone()
  result.operations = self.operations
  result.insertionSlices = self.insertionSlices

proc clone*(self: BufferSnapshot): BufferSnapshot =
  result.replicaId = self.replicaId
  result.fragments = self.fragments.clone()
  result.visibleText = self.visibleText.clone()
  result.deletedText = self.deletedText.clone()
  result.undoMap = self.undoMap.clone()
  result.insertions = self.insertions.clone()
  result.version = self.version

proc clone*(self: Buffer, replicaId: ReplicaId): Buffer =
  result.snapshot = self.snapshot.clone()
  result.history = self.history.clone()
  result.deferredOps = self.deferredOps
  result.deferredReplicas = self.deferredReplicas
  result.snapshot.replicaId = replicaId
  result.timestamp = initLamport(replicaId)

func insert*(self: var UndoMap, undo: UndoOperation) =
  # var edits: seq[sumtree.Edit[UndoMapEntry]]
  let edits = collect:
    for editId, count in undo.counts.pairs:
      sumtree.Edit[UndoMapEntry](
        kind: Insert,
        item: UndoMapEntry(
          key: UndoMapKey(editId: editId, undoId: undo.timestamp),
          undoCount: count
        )
      )

  discard self.tree.edit(edits, ())

func undoCount*(self: UndoMap, editId: Lamport): uint32 =
  var cursor = self.tree.initCursor(UndoMapKey)
  discard cursor.seek(UndoMapKey(editId: editId, undoId: Lamport.default), Left, ())
  while cursor.item.isSome:
    let entry = cursor.item.get
    if entry.key.editId != editId:
      break
    result = max(result, entry.undoCount)
    cursor.next(())

func undoCount*(self: UndoMap, editId: Lamport, version: Global): uint32 =
  var cursor = self.tree.initCursor(UndoMapKey)
  discard cursor.seek(UndoMapKey(editId: editId, undoId: Lamport.default), Left, ())
  while cursor.item.isSome:
    let entry = cursor.item.get
    if entry.key.editId != editId:
      break
    if version.observed(entry.key.undoId):
      result = max(result, entry.undoCount)
    cursor.next(())

func isUndone*(self: UndoMap, editId: Lamport): bool = self.undoCount(editId) mod 2 == 1
func wasUndone*(self: UndoMap, editId: Lamport, version: Global): bool = self.undoCount(editId, version) mod 2 == 1

type RopeBuilder = object
  oldVisibleCursor: RopeCursor
  oldDeletedCursor: RopeCursor
  newVisible: Rope
  newDeleted: Rope

proc initRopeBuilder(oldVisibleCursor, oldDeletedCursor: RopeCursor): RopeBuilder =
  result.oldVisibleCursor = oldVisibleCursor
  result.oldDeletedCursor = oldDeletedCursor
  result.newVisible = Rope.new()
  result.newDeleted = Rope.new()

func wasVisible(self: Fragment, version: Global, undoMap: UndoMap): bool =
  if not version.observed(self.timestamp):
    return false
  if undoMap.wasUndone(self.timestamp, version):
    return false

  for d in self.deletions:
    if version.observed(d) and not undoMap.wasUndone(d, version):
      return false

  return true

func isVisible(self: Fragment, undoMap: UndoMap): bool =
  if undoMap.isUndone(self.timestamp):
    return false

  for d in self.deletions:
    if not undoMap.isUndone(d):
      return false

  return true

proc add(self: var RopeBuilder, len: int, wasVisible: bool, isVisible: bool) =
  let text: Rope = if wasVisible:
    self.oldVisibleCursor.slice(self.oldVisibleCursor.offset + len)
  else:
    self.oldDeletedCursor.slice(self.oldDeletedCursor.offset + len)

  if isVisible:
    self.newVisible.add text
  else:
    self.newDeleted.add text

proc add(self: var RopeBuilder, fragment: Fragment, wasVisible: bool) =
  self.add(fragment.len, wasVisible, fragment.visible)

proc add(self: var RopeBuilder, content: string) =
  self.newVisible.add content

proc add(self: var RopeBuilder, len: FragmentTextSummary) =
  self.add(len.visible, true, true)
  self.add(len.deleted, false, false)

proc finish(self: var RopeBuilder): (Rope, Rope) =
  self.newVisible.add(self.oldVisibleCursor.suffix())
  self.newDeleted.add(self.oldDeletedCursor.suffix())
  (self.newVisible.move, self.newDeleted.move)

proc applyRemoteEdit(self: var Buffer, edit: EditOperation) =
  var editIndex = 0
  while editIndex < edit.ranges.len:
    var ropeBuilder = initRopeBuilder(self.snapshot.visibleText.cursor(0), self.snapshot.deletedText.cursor(0))

    let cx = edit.version.some
    var oldFragments = self.snapshot.fragments.initCursor[:Fragment, tuple[versionedFullOffset: VersionedFullOffset, offset: int]]((VersionedFullOffset.default, 0))
    var newFragments = oldFragments.slice(edit.ranges[editIndex].a.some.VersionedFullOffset, Bias.Left, cx)
    ropeBuilder.add(newFragments.summary().text)

    var fragmentStart = oldFragments.startPos().versionedFullOffset.fullOffset()
    var insertionOffset = 0
    var insertionSlices = newSeqOfCap[InsertionSlice](edit.ranges.len)
    var newInsertions = newSeqOfCap[sumtree.Edit[InsertionFragment]](edit.ranges.len)
    var patch = Patch[uint32]()

    defer:
      if fragmentStart > oldFragments.startPos().versionedFullOffset.fullOffset:
        let fragmentEnd = oldFragments.endPos(cx).versionedFullOffset.fullOffset
        if fragmentEnd > fragmentStart:
          var suffix = oldFragments.item.get[].clone()
          suffix.len = fragmentEnd.int - fragmentStart.int
          suffix.insertionOffset += fragmentStart.int - oldFragments.startPos().versionedFullOffset.fullOffset.int
          newInsertions.add(InsertionFragment.insertNew(suffix))
          ropeBuilder.add(suffix, suffix.visible)
          newFragments.add(suffix, ())

        oldFragments.next(cx)

      let suffix = oldFragments.suffix(cx)
      ropeBuilder.add(suffix.summary().text)
      newFragments.append(suffix, ())

      (self.snapshot.visibleText, self.snapshot.deletedText) = ropeBuilder.finish()
      self.snapshot.fragments = newFragments
      self.history.insertionSlices[edit.timestamp] = insertionSlices
      discard self.snapshot.insertions.edit(newInsertions, ())
      self.patches.add((edit.timestamp, patch))

    block innerLoop:
      while editIndex < edit.ranges.len:
        let range = edit.ranges[editIndex]
        let newText = edit.newTexts[editIndex]

        var fragmentEnd = oldFragments.endPos(cx).versionedFullOffset.fullOffset

        if range.a < fragmentStart:
          # Trying to insert out of order, commit current edits by breaking inner loop
          break innerLoop

        if fragmentEnd < range.a:
          if fragmentStart > oldFragments.startPos().versionedFullOffset.fullOffset:
            if fragmentEnd > fragmentStart:
              var suffix = oldFragments.item.get[].clone()
              suffix.len = fragmentEnd.int - fragmentStart.int
              suffix.insertionOffset += fragmentStart.int - oldFragments.startPos().versionedFullOffset.fullOffset.int
              newInsertions.add(InsertionFragment.insertNew(suffix))
              ropeBuilder.add(suffix, suffix.visible)
              newFragments.add(suffix, ())

            oldFragments.next(cx)

          let slice = oldFragments.slice(range.a.some.VersionedFullOffset, Bias.Left, cx)
          ropeBuilder.add(slice.summary().text)
          newFragments.append(slice, ())
          fragmentStart = oldFragments.startPos().versionedFullOffset.fullOffset

        fragmentEnd = oldFragments.endPos(cx).versionedFullOffset.fullOffset
        if fragmentEnd == range.a and fragmentEnd > fragmentStart:
          var prefix = oldFragments.item.get[].clone()
          prefix.len = fragmentEnd.int - fragmentStart.int
          prefix.insertionOffset += fragmentStart.int - oldFragments.startPos().versionedFullOffset.fullOffset.int
          newInsertions.add(InsertionFragment.insertNew(prefix))
          ropeBuilder.add(prefix, prefix.visible)
          newFragments.add(prefix, ())
          oldFragments.next(cx)
          fragmentStart = oldFragments.startPos().versionedFullOffset.fullOffset

        while (let fragment = oldFragments.item; fragment.isSome):
          if fragmentStart == range.a and fragment.get.timestamp > edit.timestamp:
            ropeBuilder.add(fragment.get[], fragment.get[].visible)
            newFragments.add(fragment.get[].clone(), ())
            oldFragments.next(cx)
          else:
            break

        if fragmentStart < range.a:
          var prefix = oldFragments.item.get[].clone()
          prefix.len = range.a.int - fragmentStart.int
          prefix.insertionOffset += fragmentStart.int - oldFragments.startPos().versionedFullOffset.fullOffset.int
          prefix.loc = between(newFragments.summary.maxId, prefix.loc)
          newInsertions.add(InsertionFragment.insertNew(prefix))
          ropeBuilder.add(prefix, prefix.visible)
          newFragments.add(prefix, ())
          fragmentStart = range.a

        # Insert new text
        if newText.len > 0:
          var oldStart = oldFragments.startPos().offset.uint32
          if oldFragments.item.mapIt(it[].visible).get(false):
            oldStart = (oldStart.int + fragmentStart.int - oldFragments.startPos().versionedFullOffset.fullOffset.int).uint32
          let newStart = newFragments.summary().text.visible.uint32
          let id = between(newFragments.summary.maxId, oldFragments.item.mapIt(it[].loc).get(Locator.high))
          let fragment = initFragment(edit.timestamp, newText.len, insertionOffset, id)
          patch.add initEdit(oldStart...oldStart, newStart...(newStart + newText.len.uint32))
          insertionSlices.add fragment.insertionSlice
          newInsertions.add(InsertionFragment.insertNew(fragment))
          newFragments.add(fragment, ())
          ropeBuilder.add(newText)
          insertionOffset += newText.len

        while fragmentStart < range.b + 1.FullOffset:
          let fragment = oldFragments.item().get
          let fragmentEnd = oldFragments.endPos(cx).versionedFullOffset.fullOffset
          var intersection = fragment[].clone()
          let intersectionEnd = min(range.b + 1.FullOffset, fragmentEnd)

          if fragment[].wasVisible(edit.version, self.snapshot.undoMap):
            intersection.len = intersectionEnd.int - fragmentStart.int
            intersection.insertionOffset += fragmentStart.int - oldFragments.startPos().versionedFullOffset.fullOffset.int
            intersection.loc = between(newFragments.summary.maxId, intersection.loc)
            intersection.deletions.incl(edit.timestamp)
            intersection.visible = false
            insertionSlices.add intersection.insertionSlice

          if intersection.len > 0:
            if fragment[].visible and not intersection.visible:
              var oldStart = (oldFragments.startPos().offset.int + fragmentStart.int - oldFragments.startPos().versionedFullOffset.fullOffset.int).uint32
              let newStart = newFragments.summary().text.visible.uint32
              patch.add initEdit(oldStart...(oldStart + intersection.len.uint32), newStart...newStart)
            newInsertions.add(InsertionFragment.insertNew(intersection))
            ropeBuilder.add(intersection, fragment.visible)
            newFragments.add(intersection, ())
            fragmentStart = intersectionEnd

          if fragmentEnd <= range.b + 1.FullOffset:
            oldFragments.next(cx)

        inc editIndex

proc applyOp(self: var Buffer, op: Operation): bool =
  case op.kind
  of Edit:
    if not self.snapshot.version.observed(op.edit.timestamp):
      self.applyRemoteEdit(op.edit)
      self.snapshot.version.observe(op.edit.timestamp)
      self.timestamp.observe(op.edit.timestamp)
      discard self.timestamp.tick() # todo: necessary?

  of Undo:
    if not self.snapshot.version.observed(op.undo.timestamp):
      if not self.applyUndo(op.undo):
        return false
      self.snapshot.version.observe(op.undo.timestamp)
      self.timestamp.observe(op.undo.timestamp)

  return true

func canApplyOp(self: var Buffer, op: Operation): bool =
  if op.timestamp.replicaId in self.deferredReplicas:
    return false
  return self.snapshot.version.observedAll(op.version)

proc flushDeferredOps(self: var Buffer): bool =
  self.deferredReplicas.clear()
  let currentDeferredOps = self.deferredOps
  self.deferredOps.setLen(0)

  var deferredOps = newSeq[Operation]()
  for op in currentDeferredOps:
    if self.canApplyOp(op):
      if not self.applyOp(op):
        return false
    else:
      self.deferredReplicas.incl(op.timestamp.replicaId)
      deferredOps.add(op)

  self.deferredOps.add(deferredOps)
  return true

proc applyRemote*(self: var Buffer, ops: openArray[Operation]): bool =
  var deferredOps = newSeq[Operation]()
  for op in ops:
    if self.canApplyOp(op):
      if not self.applyOp(op):
        return false
    else:
      self.deferredReplicas.incl(op.timestamp.replicaId)
      deferredOps.add(op)

  self.deferredOps.add(deferredOps)
  return self.flushDeferredOps()

proc applyLocal*(self: var Buffer, edits: openArray[tuple[range: Slice[int], text: string]], timestamp: Lamport): EditOperation =
  result.timestamp = timestamp
  result.version = self.snapshot.version
  result.ranges = newSeqOfCap[Slice[FullOffset]](edits.len)
  result.newTexts = newSeqOfCap[string](edits.len)
  self.snapshot.version.observe(timestamp)

  var opIndex = 0
  while opIndex < edits.len:
    var ropeBuilder = initRopeBuilder(self.snapshot.visibleText.cursor(0), self.snapshot.deletedText.cursor(0))
    var insertionOffset = 0

    var oldFragments = self.snapshot.fragments.initCursor(FragmentTextSummary)
    var newFragments = oldFragments.slice(edits[opIndex].range.a, Bias.Right, ())
    ropeBuilder.add(newFragments.summary().text)

    var fragmentStart = oldFragments.startPos().visible

    var insertionSlices = newSeqOfCap[InsertionSlice](edits.len)
    var newInsertions = newSeqOfCap[sumtree.Edit[InsertionFragment]](edits.len)
    var patch = Patch[uint32]()

    defer:
      if fragmentStart > oldFragments.startPos().visible:
        let fragmentEnd = oldFragments.endPos(()).visible
        if fragmentEnd > fragmentStart:
          var suffix = oldFragments.item.get[].clone()
          suffix.len = fragmentEnd - fragmentStart
          suffix.insertionOffset += fragmentStart - oldFragments.startPos().visible
          newInsertions.add(InsertionFragment.insertNew(suffix))
          ropeBuilder.add(suffix, suffix.visible)
          newFragments.add(suffix, ())

        oldFragments.next(())

      let suffix = oldFragments.suffix(())
      ropeBuilder.add(suffix.summary().text)
      newFragments.append(suffix, ())

      (self.snapshot.visibleText, self.snapshot.deletedText) = ropeBuilder.finish()
      self.snapshot.fragments = newFragments
      self.history.insertionSlices[timestamp] = insertionSlices
      discard self.snapshot.insertions.edit(newInsertions, ())
      self.patches.add((timestamp, patch))

    block innerLoop:
      while opIndex < edits.len:
        let edit {.cursor.} = edits[opIndex]

        let fragmentEnd = oldFragments.endPos(()).visible

        if edit.range.a < fragmentStart:
          # Trying to insert out of order, commit current edits by breaking inner loop
          break innerLoop

        if fragmentEnd < edit.range.a:
          if fragmentStart > oldFragments.startPos().visible:
            if fragmentEnd > fragmentStart:
              var suffix = oldFragments.item.get[].clone()
              suffix.len = fragmentEnd - fragmentStart
              suffix.insertionOffset += fragmentStart - oldFragments.startPos().visible
              newInsertions.add(InsertionFragment.insertNew(suffix))
              ropeBuilder.add(suffix, suffix.visible)
              newFragments.add(suffix, ())

            oldFragments.next(())

          let slice = oldFragments.slice(edit.range.a, Bias.Right, ())
          ropeBuilder.add(slice.summary().text)
          newFragments.append(slice, ())

          fragmentStart = oldFragments.startPos().visible

        let fullRangeStart = FullOffset(edit.range.a + oldFragments.startPos().deleted)

        # Take prefix of overlapping fragment
        if fragmentStart < edit.range.a:
          var prefix = oldFragments.item.get[].clone()
          prefix.len = edit.range.a - fragmentStart
          prefix.insertionOffset += fragmentStart - oldFragments.startPos().visible
          prefix.loc = between(newFragments.summary.maxId, prefix.loc)
          newInsertions.add(InsertionFragment.insertNew(prefix))
          ropeBuilder.add(prefix, prefix.visible)
          newFragments.add(prefix, ())
          fragmentStart = edit.range.a

        # Insert new text
        if edit.text.len > 0:
          let newStart = newFragments.summary().text.visible.uint32
          let id = between(newFragments.summary.maxId, oldFragments.item.mapIt(it[].loc).get(Locator.high))
          let fragment = initFragment(timestamp, edit.text.len, insertionOffset, id)
          patch.add initEdit(fragmentStart.uint32...fragmentStart.uint32, newStart...(newStart + edit.text.len.uint32))
          insertionSlices.add fragment.insertionSlice
          newInsertions.add(InsertionFragment.insertNew(fragment))
          newFragments.add(fragment, ())
          ropeBuilder.add(edit.text)
          insertionOffset += edit.text.len

        # Delete overlapping fragments
        while fragmentStart < edit.range.b + 1:
          let fragment = oldFragments.item().get
          let fragmentEnd = oldFragments.endPos(()).visible
          var intersection = fragment[].clone()
          let intersectionEnd = min(edit.range.b + 1, fragmentEnd)
          if fragment[].visible:
            intersection.len = intersectionEnd - fragmentStart
            intersection.insertionOffset += fragmentStart - oldFragments.startPos().visible
            intersection.loc = between(newFragments.summary.maxId, intersection.loc)
            intersection.deletions.incl(timestamp)
            intersection.visible = false

          if intersection.len > 0:
            if fragment[].visible and not intersection.visible:
              let newStart = newFragments.summary().text.visible.uint32
              patch.add initEdit(fragmentStart.uint32...intersectionEnd.uint32, newStart...newStart)
              insertionSlices.add intersection.insertionSlice

            newInsertions.add(InsertionFragment.insertNew(intersection))
            ropeBuilder.add(intersection, fragment.visible)
            newFragments.add(intersection, ())
            fragmentStart = intersectionEnd

          if fragmentEnd <= edit.range.b + 1:
            oldFragments.next(())

        let fullRangeEnd = FullOffset(edit.range.b + 1 + oldFragments.startPos().deleted)
        result.ranges.add fullRangeStart..<fullRangeEnd
        result.newTexts.add edit.text

        inc opIndex

proc fragmentIdsForEdits(self: var Buffer, edits: openArray[Lamport]): seq[Locator] =
  var insertionSlices = newSeqOfCap[InsertionSlice](edits.len)
  for editId in edits:
    if self.history.insertionSlices.contains(editId):
      insertionSlices.add(self.history.insertionSlices[editId])

  func cmpFn(a, b: InsertionSlice): int =
    if a.insertionId == b.insertionId:
      if a.range.a == b.range.a:
        return cmp(b.range.a, a.range.a)
      return cmp(a.range.b, b.range.b)
    return cmp(a.insertionId, b.insertionId)

  insertionSlices.sort(cmpFn)

  var fragmentIds = newSeq[Locator]()

  var cursor = self.snapshot.insertions.initCursor(InsertionFragmentKey)
  for slice in insertionSlices:
    if slice.insertionId != cursor.startPos.timestamp or slice.range.a > cursor.startPos.splitOffset:
      discard cursor.seekForward(InsertionFragmentKey(timestamp: slice.insertionId, splitOffset: slice.range.a), Left, ())

    while cursor.item.isSome:
      let item = cursor.item.get
      if item[].timestamp != slice.insertionId or item[].splitOffset >= slice.range.b:
        break

      fragmentIds.add item[].fragmentId
      cursor.next(())

  fragmentIds.sort()

  return fragmentIds

proc applyUndo*(self: var Buffer, undo: UndoOperation): bool =
  self.snapshot.undoMap.insert(undo)

  var patch = Patch[uint32]()
  var oldFragments = self.snapshot.fragments.initCursor((Option[Locator], int))
  var newFragments = SumTree[Fragment].new()
  var newRopes = initRopeBuilder(self.snapshot.visibleText.cursor(0), self.snapshot.deletedText.cursor(0))

  let editIds = collect:
    for id in undo.counts.keys:
      id

  for fragmentId in self.fragmentIdsForEdits(editIds):
    let precedingFragments = oldFragments.slice(fragmentId.some, Left, ())
    newRopes.add(precedingFragments.summary.text)
    newFragments.append(precedingFragments, ())

    if oldFragments.item.isSome:
      var fragment = oldFragments.item.get[].clone()
      let fragmentWasVisible = fragment.visible

      fragment.visible = fragment.isVisible(self.snapshot.undoMap)
      fragment.maxUndos.observe(undo.timestamp)

      let oldStart = oldFragments.startPos[1]
      let newStart = newFragments.summary.text.visible
      if fragmentWasVisible and not fragment.visible:
        patch.add initEdit(oldStart.uint32...(oldStart.uint32 + fragment.len.uint32), newStart.uint32...newStart.uint32)
      elif not fragmentWasVisible and fragment.visible:
        patch.add initEdit(oldStart.uint32...oldStart.uint32, newStart.uint32...(newStart.uint32 + fragment.len.uint32))

      newRopes.add(fragment, fragmentWasVisible)
      newFragments.add(fragment, ())
      oldFragments.next(())

  let suffix = oldFragments.suffix(())
  newRopes.add(suffix.summary.text)
  newFragments.append(suffix, ())

  let (visibleText, deletedText) = newRopes.finish()
  self.snapshot.fragments = newFragments
  self.snapshot.visibleText = visibleText
  self.snapshot.deletedText = deletedText
  self.patches.add((undo.timestamp, patch))

  return true

proc undoOrRedo*(self: var Buffer, transaction: Transaction): Option[Operation] =
  var counts = initTable[Lamport, uint32]()
  for editId in transaction.editIds:
    counts[editId] = self.snapshot.undoMap.undoCount(editId) + 1

  let undo = UndoOperation(
    timestamp: self.timestamp.tick(),
    version: self.snapshot.version,
    counts: counts
  )
  if not self.applyUndo(undo):
    return Operation.none

  self.snapshot.version.observe(undo.timestamp)

  Operation(kind: Undo, undo: undo).some

proc push(self: var History, op: Operation) =
  self.operations[op.timestamp] = op

proc pushUndo(self: var History, edit: EditOperation) =
  assert self.undoStack.len > 0
  self.undoStack[^1].transaction.editIds.add(edit.timestamp)

proc startTransaction(self: var History, start: Global, clock: var Lamport): Option[TransactionId] =
  let id = clock.tick()
  self.undoStack.add HistoryEntry(
    transaction: Transaction(
      id: id,
      start: start,
      editIds: @[],
    )
  )

proc endTransaction(self: var History): Option[ptr HistoryEntry] =
  assert self.undoStack.len > 0
  if self.undoStack[^1].transaction.editIds.len == 0:
    discard self.undoStack.pop()
    return none(ptr HistoryEntry)
  else:
    self.redoStack.setLen(0)
    return self.undoStack[^1].addr.some

proc startTransaction(self: var Buffer): Option[TransactionId] =
  self.history.startTransaction(self.snapshot.version, self.timestamp)

proc endTransaction(self: var Buffer): Option[ptr HistoryEntry] =
  self.history.endTransaction()

proc edit*(self: var Buffer, edits: openArray[tuple[range: Slice[int], text: string]]): Operation =
  discard self.startTransaction()
  if self.timestamp.value == 0:
    discard self.timestamp.tick()
  let timestamp = self.timestamp.tick()
  let operation = Operation(kind: Edit, edit: self.applyLocal(edits, timestamp))
  self.history.push(operation)
  self.history.pushUndo(operation.edit)
  self.snapshot.version.observe(timestamp)
  discard self.endTransaction()
  operation

proc undo*(self: var Buffer, op: Lamport): Operation =
  var counts = initTable[Lamport, uint32]()
  counts[op] = self.snapshot.undoMap.undoCount(op) + 1

  let undo = UndoOperation(
    timestamp: self.timestamp.tick(),
    version: self.snapshot.version,
    counts: counts
  )
  discard self.applyUndo(undo)
  self.snapshot.version.observe(undo.timestamp)

  Operation(kind: Undo, undo: undo)

proc redo*(self: var Buffer): Option[tuple[transactionId: TransactionId, op: Operation]] =
  if self.history.redoStack.len > 0:
    let entry = self.history.redoStack.pop
    self.history.undoStack.add(entry)
    let op = self.undoOrRedo(entry.transaction)
    if op.isSome:
      return (entry.transaction.id, op.get).some

proc undo*(self: var Buffer): Option[tuple[transactionId: TransactionId, op: Operation]] =
  if self.history.undoStack.len > 0:
    let entry = self.history.undoStack.pop
    self.history.redoStack.add(entry)
    let op = self.undoOrRedo(entry.transaction)
    if op.isSome:
      return (entry.transaction.id, op.get).some

proc delete*(self: var Buffer, pos: Slice[int]): seq[Operation] =
  assert pos.a >= 0
  assert pos.a <= pos.b + 1
  assert pos.b < self.snapshot.visibleText.bytes

  result.add self.edit([(pos, "")])

proc deleteRange*(self: var Buffer, pos: Slice[int]): seq[Operation] =
  assert pos.a >= 0
  assert pos.a <= pos.b
  assert pos.b < self.snapshot.visibleText.bytes

  result.add self.edit([(pos, "")])

proc insertText*(self: var Buffer, pos: int, text: string): seq[Operation] =
  if text.len == 0:
    return

  result.add self.edit([(pos..<pos, text)])

proc dump*(self: Buffer): string =
  var startFullOffset = 0
  var visibleOffset = 0
  var lastTimestamp = initLamport(0.ReplicaId)
  var lastVisible = false

  var startVisibleOffset = 0

  var ropeBuilder = initRopeBuilder(self.snapshot.visibleText.cursor(0), self.snapshot.deletedText.cursor(0))
  result.add &"'{self.snapshot.visibleText}' | '{self.snapshot.deletedText}'\n"
  let fragments = self.snapshot.fragments.toSeq(())
  for i, item in fragments:
    ropeBuilder.add(item, item.visible)

    if true or i == fragments.high or fragments[i + 1].timestamp != item.timestamp or fragments[i + 1].visible != item.visible:
      result.add &"[{startFullOffset}|{startVisibleOffset}] {item.timestamp}, visible: {item.visible}, len: {item.len}, {item.insertionOffset}..<{item.insertionOffset+item.len}, '{ropeBuilder.newVisible}-{ropeBuilder.newDeleted}'   | {item}\n"
      startFullOffset = i + 1
      startVisibleOffset = visibleOffset
      ropeBuilder.newVisible = Rope.new()
      ropeBuilder.newDeleted = Rope.new()
      if item.visible:
        inc startVisibleOffset

    lastTimestamp = item.timestamp
    lastVisible = item.visible

    if item.visible:
      inc visibleOffset

func items*(self: Buffer): seq[Fragment] = self.snapshot.fragments.toSeq(()).mapIt(block:
  var x = it
  x.loc = Locator.low
  x)

func snapshot*(self: Buffer): BufferSnapshot = self.snapshot.clone()
func visibleText*(self: Buffer): lent Rope = self.snapshot.visibleText
