import std/[enumerate]

type
  ReplicaId* = distinct uint16
  SeqNumber* = uint32
  Lamport* = object
    replicaId*: ReplicaId
    value*: SeqNumber

  Global* = distinct seq[uint32]
  Locator* = distinct seq[uint64]

func `$`*(_: ReplicaId): string {.borrow.}
func `==`*(a, b: ReplicaId): bool {.borrow.}
func `<`*(a, b: ReplicaId): bool {.borrow.}
func `<=`*(a, b: ReplicaId): bool {.borrow.}
func low*(_: typedesc[Lamport]): Lamport = Lamport(replicaId: ReplicaId.low, value: SeqNumber.low)
func high*(_: typedesc[Lamport]): Lamport = Lamport(replicaId: ReplicaId.high, value: SeqNumber.high)

func `==`*(a, b: Global): bool {.borrow.}
proc setLen(a: var Global, len: Natural) {.borrow.}
proc len(a: Global): int {.borrow.}

func `$`*(self: Lamport): string = "L(" & $self.replicaId & ":" & $self.value & ")"
func initLamport*(replicaId: ReplicaId, value: SeqNumber = 0): Lamport = Lamport(replicaId: replicaId, value: value)
func asNumber*(self: Lamport): uint64 = (self.value.uint64 shl 32) or self.replicaId.uint64
func cmp*(a, b: Lamport): int =
  result = cmp(a.value, b.value)
  if result == 0:
    result = cmp(a.replicaId, b.replicaId)

func `<`*(a, b: Lamport): bool = cmp(a, b) < 0
func `<=`*(a, b: Lamport): bool = cmp(a, b) <= 0

func tick*(self: var Lamport): Lamport =
  result = self
  inc self.value

func observe*(self: var Lamport, timestamp: Lamport) =
  self.value = max(self.value, timestamp.value)

func initGlobal*(): Global = Global(newSeq[uint32]())

template asBase(self: Global): seq[uint32] = (seq[uint32])(self)
func `[]`*(self: Global, replicaId: ReplicaId): SeqNumber =
  if replicaId.int in 0..self.asBase.high:
    self.asBase[replicaId.int]
  else:
    0

iterator timestamps*(self: Global): Lamport =
  type Seq = seq[uint32]
  for replicaId, value in self.Seq:
    yield initLamport(replicaId.ReplicaId, value)

func `$`*(self: Global): string =
  result = "G("
  for i, timestamp in enumerate(self.timestamps):
    if i > 0:
      result.add ", "
    result.add $timestamp
  result.add ")"

func observe*(self: var Global, timestamp: Lamport) =
  if timestamp.value > 0:
    let newLen = timestamp.replicaId.int + 1
    if newLen > self.len:
      self.setLen(newLen)
    self.asBase[timestamp.replicaId.int] = max(self.asBase[timestamp.replicaId.int], timestamp.value)

func toGlobal*(timestamps: openArray[Lamport]): Global =
  for t in timestamps:
    result.observe(t)

func join*(self: var Global, other: Global) =
  ## Basically self = max(self, other)
  if other.len > self.len:
    self.setLen(other.len)
  for i in 0..<min(self.len, other.len):
    self.asBase[i] = max(self.asBase[i], other.asBase[i])

func meet*(self: var Global, other: Global) =
  ## Basically self = min(self, other), except if one of them is 0 it `i`, then max(self, other)
  if other.len > self.len:
    self.setLen(other.len)

  var newLen = 0
  for i in 0..<max(self.len, other.len):
    var left = self.asBase[i].addr
    var right = other[i.ReplicaId]

    if left[] == 0:
      left[] = right
    elif right > 0:
      left[] = min(left[], right)

    if left[] != 0:
      newLen = i + 1

  self.setLen(newLen)

func observed*(self: Global, timestamp: Lamport): bool =
  self[timestamp.replicaId] >= timestamp.value

func observedAny*(self: Global, other: Global): bool =
  for i in 0..<min(self.len, other.len):
    if other.asBase[i] > 0 and self.asBase[i] >= other.asBase[i]:
      return true
  return false

func observedAll*(self: Global, other: Global): bool =
  ## Returns true if all versions `self[i]` >= `other[i]`
  if other.len > self.len:
    return false
  for i in 0..<min(self.len, other.len):
    if self.asBase[i] < other.asBase[i]:
      return false
  return true

func changedSince*(self: Global, other: Global): bool =
  if self.len > other.len:
    return true
  for i in 0..<min(self.len, other.len):
    if self.asBase[i] > other.asBase[i]:
      return true
  return true

func default*(_: typedesc[Locator]): Locator = Locator(@[uint64.low])
template asBase(self: Locator): seq[uint64] = (seq[uint64])(self)

func `=copy`*(self: var Locator, other: Locator) =
  self.asBase.setLen(other.asBase.len)
  for i in 0..<self.asBase.len:
    self.asBase[i] = other.asBase[i]

func len*(self: Locator): int {.borrow.}
func cmp*(a, b: Locator): int =
  if a.len > 0 and b.len > 0:
    let ax = cast[ptr UncheckedArray[uint64]](a.asBase[0].addr)
    let bx = cast[ptr UncheckedArray[uint64]](b.asBase[0].addr)
    for i in 0..<min(a.len, b.len):
      let c = cmp(ax[i], bx[i])
      if c != 0:
        return c
  return cmp(a.len, b.len)

func `<`*(a, b: Locator): bool = cmp(a, b) < 0
func `<=`*(a, b: Locator): bool = cmp(a, b) <= 0
func `==`*(a, b: Locator): bool = cmp(a, b) == 0
func low*(_: typedesc[Locator]): Locator = Locator(@[uint64.low])
func high*(_: typedesc[Locator]): Locator = Locator(@[uint64.high])

func `$`*(self: Locator): string =
  result = "Loc("
  for i, val in self.asBase.pairs:
    if i > 0:
      result.add ", "
    result.add $val
  result.add ")"

proc between*(a, b: Locator): Locator =
  assert a < b
  for i in 0..int.high:
    let lhs: uint64 = if i < a.len: a.asBase[i] else: uint64.low
    let rhs: uint64 = if i < b.len: b.asBase[i] else: uint64.high
    if rhs < lhs:
      echo "todo: use saturating sub: ", lhs, " >= ", rhs
    assert rhs >= lhs, "todo: use saturating sub"
    let mid = lhs + ((rhs - lhs) shr 16) # Zed uses 48. Try different numbers once there are benchmarks
    result.asBase.add mid
    if mid > lhs:
      break

  assert result > a
  assert result < b

assert Locator(@[0.uint64]) < Locator(@[1.uint64])
assert not (Locator(@[1.uint64]) < Locator(@[0.uint64]))
assert Locator(@[0.uint64]) <= Locator(@[0.uint64])
assert Locator(@[0.uint64]) <= Locator(@[0.uint64, 1.uint64])
assert Locator(@[0.uint64]) < Locator(@[0.uint64, 1.uint64])
assert Locator(@[0.uint64]) < Locator(@[0.uint64, 1.uint64])
assert not (Locator(@[1.uint64]) < Locator(@[0.uint64, 1.uint64]))
assert Locator(@[1.uint64]) < Locator(@[1.uint64, 1.uint64])


