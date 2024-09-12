import std/[strutils, sequtils, strformat, tables, sets]
import clock

import nimsumtree/sumtree
import rope

var debugLog* = false
var debugLog2* = false

type
  Fragment* = object
    loc*: Locator
    timestamp*: Lamport
    visible*: bool
    insertionOffset*: int
    len*: int
    deletions*: HashSet[Lamport]

  FragmentTextSummary = object
    visible: int
    deleted: int

  FragmentSummary = object
    text: FragmentTextSummary
    maxId: Locator = Locator(@[uint64.low])
    maxVersion: Global
    minInsertionVersion: Global
    maxInsertionVersion: Global

  FullOffset* = distinct int

  VersionedFullOffset* = distinct Option[FullOffset]

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

func `$`*(self: FullOffset): string {.borrow.}
func `<`*(a, b: FullOffset): bool {.borrow.}
func `<=`*(a, b: FullOffset): bool {.borrow.}
func `==`*(a, b: FullOffset): bool {.borrow.}
func `+`*(a, b: FullOffset): FullOffset {.borrow.}
func `-`*(a, b: FullOffset): FullOffset {.borrow.}
func cmp*(a, b: FullOffset): int {.borrow.}
func `$`*(self: VersionedFullOffset): string {.borrow.}

func addSummary*[A, B](a: var (A, B), b: FragmentSummary, cx: Option[Global]) =
  a[0].addSummary(b, cx)
  a[1].addSummary(b, cx)

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
func addSummary*[C](a: var Option[Locator], b: FragmentSummary, cx: C) = a = b.maxId.some
func fromSummary*[C](_: typedesc[Option[Locator]], a: FragmentSummary, cx: C): Option[Locator] = a.maxId.some

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

  OperationKind* = enum Edit, Undo
  Operation* = object
    case kind*: OperationKind
    of Edit:
      edit*: EditOperation
    of Undo:
      undo*: UndoOperation

  Buffer* = object
    replicaId*: ReplicaId
    version*: Global
    timestamp*: Lamport
    fragments*: SumTree[Fragment]
    visibleText*: Rope
    deletedText*: Rope

proc edit*(self: var Buffer, edits: openArray[tuple[range: Slice[int], text: string]]): Operation

func `$`*(self: Fragment): string = &"I({self.timestamp}:{self.loc}, {self.insertionOffset}, visible: {self.visible}, {self.len}, deletions: {self.deletions})"
func `$`*(self: seq[Fragment]): string = "Is(\n" & self.mapIt($it).join("\n").indent(1, "  ") & "\n)"
func contentString*(self: Buffer): string = $self.visibleText
func contentLength*(self: Buffer): int = self.visibleText.bytes

func `$`*(self: Buffer): string =
  result = &"Buffer({self.replicaId}, {self.version}, {self.timestamp}\n"
  result.add &"  visibleText: {self.visibleText}\n"
  result.add &"  deletedText: {self.deletedText}\n"
  result.add &"  fragmentsNew: {($self.fragments.toSeq(())).indent(2).strip}\n"

func initFragment*(timestamp: Lamport, len: int, insertionOffset: int, loc = Locator.low): Fragment =
  result = Fragment(
    timestamp: timestamp,
    insertionOffset: insertionOffset,
    loc: loc,
    len: len,
    visible: true,
    deletions: initHashSet[Lamport](),
  )

proc initBuffer*(replicaId: ReplicaId = 0.ReplicaId, content: string = ""): Buffer =
  result = Buffer(timestamp: initLamport(replicaId))
  result.fragments = SumTree[Fragment].new()
  result.visibleText = Rope.new()
  result.deletedText = Rope.new()
  if content.len > 0:
    let timestamp = initLamport(0.ReplicaId, 1)
    result.timestamp.observe(timestamp)
    result.version.observe(timestamp)
    result.fragments.add(Fragment(
      timestamp: timestamp,
      insertionOffset: 0,
      loc: Locator.low,
      len: content.len,
      visible: true,
      deletions: initHashSet[Lamport](),
    ), ())
    result.visibleText.add(content)
    discard result.timestamp.tick()

proc clone*(doc: Buffer): Buffer =
  result.replicaId = doc.replicaId
  result.version = doc.version
  result.timestamp = doc.timestamp
  result.fragments = doc.fragments.clone()
  result.visibleText = doc.visibleText.clone()
  result.deletedText = doc.deletedText.clone()

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

func wasVisible(self: Fragment, version: Global): bool =
  if not version.observed(self.timestamp):
    return false
  for d in self.deletions:
    if version.observed(d):
      return false
  return true

proc add(self: var RopeBuilder, doc: Buffer, len: int, wasVisible: bool, isVisible: bool) =
  if debugLog: echo &"    builder.add ('{self.newVisible}', '{self.newDeleted}', add {len}, {wasVisible}, {isVisible})"
  let text: Rope = if wasVisible:
    self.oldVisibleCursor.slice(self.oldVisibleCursor.offset + len)
  else:
    self.oldDeletedCursor.slice(self.oldDeletedCursor.offset + len)

  if debugLog: echo &"      '{doc.visibleText}, '{doc.deletedText}', text: '{text}'"
  defer:
    if debugLog: echo &"      '{self.newVisible}', '{self.newDeleted}'"

  if isVisible:
    self.newVisible.add text
  else:
    self.newDeleted.add text

proc add(self: var RopeBuilder, doc: Buffer, fragment: Fragment, wasVisible: bool) =
  self.add(doc, fragment.len, wasVisible, fragment.visible)

proc add(self: var RopeBuilder, content: string) =
  if debugLog: echo &"    builder.add '{content}'"
  defer:
    if debugLog: echo &"      {self.newVisible}"
  self.newVisible.add content

proc add(self: var RopeBuilder, doc: Buffer, len: FragmentTextSummary) =
  self.add(doc, len.visible, true, true)
  self.add(doc, len.deleted, false, false)

proc finish(self: var RopeBuilder): (Rope, Rope) =
  self.newVisible.add(self.oldVisibleCursor.suffix())
  self.newDeleted.add(self.oldDeletedCursor.suffix())
  (self.newVisible.move, self.newDeleted.move)

proc applyRemoteEdit(self: var Buffer, edit: EditOperation) =
  if debugLog: echo &"applyRemoteEdit {edit} to {self.replicaId}"
  var editIndex = 0
  while editIndex < edit.ranges.len:
    var ropeBuilder = initRopeBuilder(self.visibleText.cursor(0), self.deletedText.cursor(0))

    let cx = edit.version.some
    var oldFragments = self.fragments.initCursor[:Fragment, tuple[versionedFullOffset: VersionedFullOffset, offset: int]]((VersionedFullOffset.default, 0))
    var newFragments = oldFragments.slice(edit.ranges[editIndex].a.some.VersionedFullOffset, Bias.Left, cx)
    ropeBuilder.add(self, newFragments.summary().text)

    var fragmentStart = oldFragments.startPos().versionedFullOffset.fullOffset()
    var insertionOffset = 0

    defer:
      if fragmentStart > oldFragments.startPos().versionedFullOffset.fullOffset:
        if debugLog2: echo &"    Part of current fragment left: {fragmentStart} > {oldFragments.startPos().versionedFullOffset.fullOffset}"
        let fragmentEnd = oldFragments.endPos(cx).versionedFullOffset.fullOffset
        if fragmentEnd > fragmentStart:
          if debugLog2: echo &"      Consume rest of fragment: {fragmentEnd} > {fragmentStart}"
          var suffix = oldFragments.item.get[].clone()
          suffix.len = fragmentEnd.int - fragmentStart.int
          suffix.insertionOffset += fragmentStart.int - oldFragments.startPos().versionedFullOffset.fullOffset.int
          ropeBuilder.add(self, suffix, suffix.visible)
          newFragments.add(suffix, ())

        oldFragments.next(cx)

      let suffix = oldFragments.suffix(cx)
      ropeBuilder.add(self, suffix.summary().text)
      if debugLog2: echo suffix.toSeq(())
      newFragments.append(suffix, ())
      if debugLog2: echo newFragments.toSeq(())

      (self.visibleText, self.deletedText) = ropeBuilder.finish()
      self.fragments = newFragments

    block innerLoop:
      while editIndex < edit.ranges.len:
        let range = edit.ranges[editIndex]
        let newText = edit.newTexts[editIndex]

        var fragmentEnd = oldFragments.endPos(cx).versionedFullOffset.fullOffset
        if debugLog: echo &"  apply full range: {range}, '{newText}', {fragmentStart}..<{fragmentEnd}"

        if range.a < fragmentStart:
          # Trying to insert out of order, commit current edits by breaking inner loop
          if debugLog2: echo "    ============================= Trying to insert out of order, commit current edits by breaking inner loop"
          break innerLoop

        if fragmentEnd < range.a:
          if debugLog2: echo &"    Take fragment starting before current edit: {fragmentEnd} < {range.a}"
          if fragmentStart > oldFragments.startPos().versionedFullOffset.fullOffset:
            if debugLog2: echo &"      Take remaining part of fragment starting before current edit: {fragmentStart} > {oldFragments.startPos().versionedFullOffset.fullOffset}"
            if fragmentEnd > fragmentStart:
              if debugLog2: echo &"        Take suffix of fragment starting before current edit: {fragmentEnd} > {fragmentStart}"
              var suffix = oldFragments.item.get[].clone()
              suffix.len = fragmentEnd.int - fragmentStart.int
              suffix.insertionOffset += fragmentStart.int - oldFragments.startPos().versionedFullOffset.fullOffset.int
              ropeBuilder.add(self, suffix, suffix.visible)
              newFragments.add(suffix, ())

            oldFragments.next(cx)

          let slice = oldFragments.slice(range.a.some.VersionedFullOffset, Bias.Left, cx)
          ropeBuilder.add(self, slice.summary().text)
          newFragments.append(slice, ())
          fragmentStart = oldFragments.startPos().versionedFullOffset.fullOffset

        fragmentEnd = oldFragments.endPos(cx).versionedFullOffset.fullOffset
        if debugLog2: echo &"    End of non concurrent fragment: {fragmentEnd} == {range.a} and {fragmentEnd} > {fragmentStart}"
        if fragmentEnd == range.a and fragmentEnd > fragmentStart:
          if debugLog2: echo &"    End of non concurrent fragment: {fragmentEnd} == {range.a} and {fragmentEnd} > {fragmentStart}"
          var prefix = oldFragments.item.get[].clone()
          prefix.len = fragmentEnd.int - fragmentStart.int
          prefix.insertionOffset += fragmentStart.int - oldFragments.startPos().versionedFullOffset.fullOffset.int
          ropeBuilder.add(self, prefix, prefix.visible)
          newFragments.add(prefix, ())
          oldFragments.next(cx)
          fragmentStart = oldFragments.startPos().versionedFullOffset.fullOffset

        while (let fragment = oldFragments.item; fragment.isSome):
          if fragmentStart == range.a and fragment.get.timestamp > edit.timestamp:
            ropeBuilder.add(self, fragment.get[], fragment.get[].visible)
            newFragments.add(fragment.get[].clone(), ())
            oldFragments.next(cx)
          else:
            break

        if fragmentStart < range.a:
          if debugLog2: echo &"    Take prefix of overlapping fragment: {fragmentStart} < {range.a}"
          var prefix = oldFragments.item.get[].clone()
          prefix.len = range.a.int - fragmentStart.int
          prefix.insertionOffset += fragmentStart.int - oldFragments.startPos().versionedFullOffset.fullOffset.int
          ropeBuilder.add(self, prefix, prefix.visible)
          newFragments.add(prefix, ())
          fragmentStart = range.a

        # Insert new text
        if newText.len > 0:
          if debugLog2: echo &"    Add new fragment for insert text: '{newText}'"
          # todo
          # var oldStart = oldFragments.startPos().offset
          # if oldFragments.item.mapIt(it[].visible).get(false):
          #   if debugLog2: echo &"      Old fragment was visible, offset oldStart: {oldStart} -> {oldStart + fragmentStart.int - oldFragments.startPos().versionedFullOffset.fullOffset.int}"
          #   oldStart += fragmentStart.int - oldFragments.startPos().versionedFullOffset.fullOffset.int
          # let newStart = newFragments.summary().text.visible

          let fragment = initFragment(edit.timestamp, newText.len, insertionOffset)
          newFragments.add(fragment, ())
          ropeBuilder.add(newText)
          insertionOffset += newText.len

        while fragmentStart < range.b + 1.FullOffset:
          if debugLog2: echo &"    Delete fragments: {fragmentStart} < {range.b + 1.FullOffset}, delete: {range}"
          let fragment = oldFragments.item().get
          let fragmentEnd = oldFragments.endPos(cx).versionedFullOffset.fullOffset
          var intersection = fragment[].clone()
          let intersectionEnd = min(range.b + 1.FullOffset, fragmentEnd)

          if fragment[].wasVisible(edit.version):
            intersection.len = intersectionEnd.int - fragmentStart.int
            intersection.insertionOffset += fragmentStart.int - oldFragments.startPos().versionedFullOffset.fullOffset.int
            # todo
            # intersection.loc = between()
            intersection.deletions.incl(edit.timestamp)
            intersection.visible = false

          if intersection.len > 0:
            # todo
            # if fragment[].visible and not intersection.visible:
            #   let newStart = newFragments.summary().text.visible
            ropeBuilder.add(self, intersection, fragment.visible)
            newFragments.add(intersection, ())
            fragmentStart = intersectionEnd

          if fragmentEnd <= range.b + 1.FullOffset:
            oldFragments.next(cx)

        inc editIndex

proc applyRemote*(self: var Buffer, ops: openArray[Operation]) =
  if debugLog: echo &"-------------------------------------"
  if debugLog: echo &"applyRemote: {ops}"
  for op in ops:
    case op.kind
    of Edit:
      if not self.version.observed(op.edit.timestamp):
        self.applyRemoteEdit(op.edit)
        self.version.observe(op.edit.timestamp)
        self.timestamp.observe(op.edit.timestamp)
        discard self.timestamp.tick() # todo: necessary?
      else:
        if debugLog2: echo "already observed"

    of Undo:
      discard

proc applyLocal*(self: var Buffer, edits: openArray[tuple[range: Slice[int], text: string]], timestamp: Lamport): EditOperation =
  if debugLog: echo &"applyLocal {edits}, {timestamp} to {self.replicaId}"
  result.timestamp = timestamp
  result.version = self.version
  result.ranges = newSeqOfCap[Slice[FullOffset]](edits.len)
  result.newTexts = newSeqOfCap[string](edits.len)
  self.version.observe(timestamp)

  var opIndex = 0
  while opIndex < edits.len:
    var ropeBuilder = initRopeBuilder(self.visibleText.cursor(0), self.deletedText.cursor(0))
    var insertionOffset = 0

    var oldFragments = self.fragments.initCursor(FragmentTextSummary)
    var newFragments = oldFragments.slice(edits[opIndex].range.a, Bias.Right, ())
    ropeBuilder.add(self, newFragments.summary().text)

    var fragmentStart = oldFragments.startPos().visible
    if debugLog2: echo &"{newFragments.toSeq(())}"

    defer:
      if fragmentStart > oldFragments.startPos().visible:
        if debugLog2: echo &"    Part of current fragment left: {fragmentStart} > {oldFragments.startPos().visible}"
        let fragmentEnd = oldFragments.endPos(()).visible
        if fragmentEnd > fragmentStart:
          if debugLog2: echo &"      Consume rest of fragment: {fragmentEnd} > {fragmentStart}"
          var suffix = oldFragments.item.get[].clone()
          suffix.len = fragmentEnd - fragmentStart
          suffix.insertionOffset += fragmentStart - oldFragments.startPos().visible
          ropeBuilder.add(self, suffix, suffix.visible)
          newFragments.add(suffix, ())

        oldFragments.next(())

      let suffix = oldFragments.suffix(())
      ropeBuilder.add(self, suffix.summary().text)
      newFragments.append(suffix, ())

      (self.visibleText, self.deletedText) = ropeBuilder.finish()
      self.fragments = newFragments

    block innerLoop:
      while opIndex < edits.len:
        let edit {.cursor.} = edits[opIndex]
        if debugLog: echo &"  apply {edit}"

        let fragmentEnd = oldFragments.endPos(()).visible

        if edit.range.a < ropeBuilder.newVisible.bytes:
          # Trying to insert out of order, commit current edits by breaking inner loop
          break innerLoop

        if fragmentEnd < edit.range.a:
          if debugLog2: echo &"    Take fragment starting before current edit: {fragmentEnd} < {edit.range.a}"
          if fragmentStart > oldFragments.startPos().visible:
            if debugLog2: echo &"      Take remaining part of fragment starting before current edit: {fragmentStart} > {oldFragments.startPos().visible}"
            if fragmentEnd > fragmentStart:
              if debugLog2: echo &"        Take suffix of fragment starting before current edit: {fragmentEnd} > {fragmentStart}"
              var suffix = oldFragments.item.get[].clone()
              suffix.len = fragmentEnd - fragmentStart
              suffix.insertionOffset += fragmentStart - oldFragments.startPos().visible
              ropeBuilder.add(self, suffix, suffix.visible)
              newFragments.add(suffix, ())

            oldFragments.next(())

          let slice = oldFragments.slice(edit.range.a, Bias.Right, ())
          ropeBuilder.add(self, slice.summary().text)
          newFragments.append(slice, ())

          fragmentStart = oldFragments.startPos().visible

        let fullRangeStart = FullOffset(edit.range.a + oldFragments.startPos().deleted)

        if fragmentStart < edit.range.a:
          if debugLog2: echo &"    Take prefix of overlapping fragment: {fragmentStart} < {edit.range.a}"
          var prefix = oldFragments.item.get[].clone()
          prefix.len = edit.range.a - fragmentStart
          prefix.insertionOffset += fragmentStart - oldFragments.startPos().visible
          # todo
          # prefix.loc = between(newFragment.summary().maxId, prefix.loc)
          ropeBuilder.add(self, prefix, prefix.visible)
          newFragments.add(prefix, ())
          fragmentStart = edit.range.a

        if debugLog2: echo &"{opIndex}: fragmentPos: {fragmentStart}..<{fragmentEnd}"

        # Insert new text
        if edit.text.len > 0:
          if debugLog2: echo &"    Add new fragment for insert text: '{edit.text}'"
          # todo
          # let newStart = newFragments.summary().text.visible
          let fragment = initFragment(timestamp, edit.text.len, insertionOffset)
          newFragments.add(fragment, ())
          ropeBuilder.add(edit.text)
          insertionOffset += edit.text.len

        while fragmentStart < edit.range.b + 1:
          if debugLog2: echo &"    Delete fragments: {fragmentStart} < {edit.range.b + 1}, delete: {edit.range}"
          let fragment = oldFragments.item().get
          let fragmentEnd = oldFragments.endPos(()).visible
          var intersection = fragment[].clone()
          let intersectionEnd = min(edit.range.b + 1, fragmentEnd)
          if fragment[].visible:
            intersection.len = intersectionEnd - fragmentStart
            intersection.insertionOffset += fragmentStart - oldFragments.startPos().visible
            # todo
            # intersection.loc = between()
            intersection.deletions.incl(timestamp)
            intersection.visible = false

          if intersection.len > 0:
            # todo
            # if fragment[].visible:
            #   let newStart = newFragments.summary().text.visible
            ropeBuilder.add(self, intersection, fragment.visible)
            newFragments.add(intersection, ())
            fragmentStart = intersectionEnd

          if fragmentEnd <= edit.range.b + 1:
            oldFragments.next(())

        let fullRangeEnd = FullOffset(edit.range.b + 1 + oldFragments.startPos().deleted)
        result.ranges.add fullRangeStart..<fullRangeEnd
        result.newTexts.add edit.text

        inc opIndex

proc edit*(self: var Buffer, edits: openArray[tuple[range: Slice[int], text: string]]): Operation =
  if debugLog: echo &"edit {self.timestamp.replicaId}: {edits}"
  # self.startTransaction()
  if self.timestamp.value == 0:
    discard self.timestamp.tick()
  let timestamp = self.timestamp.tick()
  let operation = Operation(kind: Edit, edit: self.applyLocal(edits, timestamp))
  # self.endTransaction()
  operation

proc delete*(self: var Buffer, pos: Slice[int]): seq[Operation] =
  assert pos.a >= 0
  assert pos.a <= pos.b + 1
  assert pos.b < self.visibleText.bytes

  if debugLog: echo &"-------------------------------------"
  if debugLog: echo &"delete({pos}) in\n{($self).indent(4)}"
  result.add self.edit([(pos, "")])
  if debugLog: echo &"delete -> ({pos}) in\n{($self).indent(4)}"

proc deleteRange*(self: var Buffer, pos: Slice[int]): seq[Operation] =
  assert pos.a >= 0
  assert pos.a <= pos.b
  assert pos.b < self.visibleText.bytes

  if debugLog: echo &"-------------------------------------"
  if debugLog: echo &"deleteRange({pos}) in\n{($self).indent(4)}"
  result.add self.edit([(pos, "")])
  if debugLog: echo &"deleteRange -> ({pos}) in\n{($self).indent(4)}"

proc insertText*(self: var Buffer, pos: int, text: string): seq[Operation] =
  if text.len == 0:
    return

  if debugLog: echo &"-------------------------------------"
  if debugLog: echo &"insertText({pos}, '{text}')\n{($self).indent(4)}"
  result.add self.edit([(pos..<pos, text)])
  if debugLog: echo &"insertText -> ({pos}, '{text}') in\n{($self).indent(4)}"

proc dump*(self: Buffer): string =
  var startFullOffset = 0
  var visibleOffset = 0
  var lastTimestamp = initLamport(0.ReplicaId)
  var lastVisible = false

  var startVisibleOffset = 0

  var ropeBuilder = initRopeBuilder(self.visibleText.cursor(0), self.deletedText.cursor(0))
  result.add &"'{self.visibleText}' | '{self.deletedText}'\n"
  let fragments = self.fragments.toSeq(())
  for i, item in fragments:
    ropeBuilder.add(self, item, item.visible)

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

func items*(self: Buffer): seq[Fragment] = self.fragments.toSeq(())
