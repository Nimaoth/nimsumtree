import "../nimsumtree/sumtree"

type Count* = distinct int
type Max* = distinct int
type TestSummary* = object
  count*: Count
  max*: Max

func clone*(a: TestSummary): TestSummary = a

func `$`*(a: Count): string {.borrow.}
func `+=`*(a: var Count, b: Count) {.borrow.}
func `==`*(a: Count, b: Count): bool {.borrow.}
func addSummary*(a: var Count, b: Count) = a = (a.int + b.int).Count
func clone*(a: Count): Count = a
func cmp*(a: Count, b: Count): int = cmp(a.int, b.int)

func addSummary*(a: var Count, b: TestSummary) = a += b.count
func fromSummary*(_: typedesc[Count], a: TestSummary): Count = a.count

func `$`*(a: Max): string {.borrow.}
func `+=`*(a: var Max, b: Max) = a = max(a.int, b.int).Max
func `==`*(a: Max, b: Max): bool {.borrow.}
func addSummary*(a: var Max, b: Max) = a = max(a.int, b.int).Max
func clone*(a: Max): Max = a
func cmp*(a: Max, b: Max): int = cmp(a.int, b.int)

func addSummary*(a: var Max, b: TestSummary) = a += b.max
func fromSummary*(_: typedesc[Max], a: TestSummary): Max = a.max

func `+=`*(a: var (Count, Max), b: (Count, Max)) =
  a[0] += b[0]
  a[1] += b[1]
func addSummary*(a: var (Count, Max), b: (Count, Max)) =
  a[0] += b[0]
  a[1] += b[1]
func clone*(a: (Count, Max)): (Count, Max) = (a[0].clone(), a[1].clone())
func cmp*(a: Count, b: (Count, Max)): int = cmp(a.int, b[0].int)
func cmp*(a: Count, b: TestSummary): int = cmp(a.int, b.count.int)

func addSummary*(a: var (Count, Max), b: TestSummary) =
  a[0] += b.count
  a[1] = max(a[1].int, b.max.int).Max

func fromSummary*(_: typedesc[(Count, Max)], a: TestSummary): (Count, Max) = (a.count, a.max)

func summary*(x: int): TestSummary = TestSummary(count: 1.Count, max: x.Max)

func `+=`*(a: var TestSummary, b: TestSummary) =
  a.count += b.count
  a.max = max(a.max.int, b.max.int).Max

func addSummary*(a: var TestSummary, b: TestSummary) =
  a.count += b.count
  a.max = max(a.max.int, b.max.int).Max

func fromSummary*(_: typedesc[TestSummary], a: TestSummary): TestSummary = a