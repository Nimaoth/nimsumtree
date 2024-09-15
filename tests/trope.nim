import std/[unittest, unicode]
import nimsumtree/[sumtree, rope]

test "Empty":
  check $Rope.new() == ""
  check $Rope.new("") == ""

test "One line":
  var a = Rope.new("abcdefghijklmnopqrstuvwxyz")
  check $a == "abcdefghijklmnopqrstuvwxyz"
  check a.bytes == 26
  check a.chars == 26
  check a.endPoint == Point(row: 0, column: 26)

proc testStringSummary(text: string) =
  var point = Point()
  for c in text:
    if c == '\n':
      point.row += 1
      point.column = 0
    else:
      point.column += 1

  var a = Rope.new(text)
  check $a == text
  check a.bytes == text.len
  check a.chars == text.runeLen
  check a.endPoint == point

  var c = a.cursor()
  let s = c.summary(TextSummary, text.len)
  check s.bytes == text.len
  check s.len == text.runeLen.Count
  check s.lines == point

  # echo &"{text.len} bytes, {text.runeLen} runes"
  # echo a.tree.stats
  # echo a.tree.pretty

test "Various string summaries":
  testStringSummary("abcdefghijklmnopqrstuvwxyz")

  testStringSummary("""import os
let a = 5
echo a""")

  const a = staticRead("text_example.txt")
  testStringSummary(a)

test "Rope.replace":
  var a = Rope.new("abcdefghijklmnopqrstuvwxyz")
  check $a == "abcdefghijklmnopqrstuvwxyz"
  a.replace(0..<5, "12345")
  check $a == "12345fghijklmnopqrstuvwxyz"
  a.replace(5..<10, "6789")
  check $a == "123456789klmnopqrstuvwxyz"
  a.replace(10..<15, "")
  check $a == "123456789kqrstuvwxyz"
  a.replace(15..<15, "*/+-")
  check $a == "123456789kqrstu*/+-vwxyz"

test "Rope.slice":
  var a = Rope.new("abcdefghijklmnopqrstuvwxyz")
  check $a.slice(0, 0) == ""
  check $a.slice(0, 5) == "abcde"
  check $a.slice(5, 10) == "fghij"
  check $a.slice(10, 15) == "klmno"
  check $a.slice(15, 20) == "pqrst"
  check $a.slice(20, 25) == "uvwxy"
  check $a.slice(25, 26) == "z"
  check $a.slice(26, 26) == ""

  var b = toRope """import os
let a = 5
echo a"""

  check $b.slice(0, 0) == ""
  check $b.slice(0, 1) == "i"
  check $b.slice(0, 6) == "import"
  check $b.slice(7, 9) == "os"
  check $b.slice(8, 11) == "s\nl"
  check $b.slice(10, 13) == "let"
  check $b.slice(14, 20) == "a = 5\n"
  check $b.slice(20, 26) == "echo a"

test "Rope.sliceRows":
  var a = Rope.new("abcdefghijklmnopqrstuvwxyz")
  check $a.sliceRows(0, 0) == ""
  check $a.sliceRows(0, 1) == "abcdefghijklmnopqrstuvwxyz"

  var b = toRope """import os
let a = 5
echo a"""

  check $b.sliceRows(0, 0) == ""
  check $b.sliceRows(0, 1) == "import os\n"
  check $b.sliceRows(1, 2) == "let a = 5\n"
  check $b.sliceRows(2, 3) == "echo a"

  check $b.sliceRows(0, 2) == "import os\nlet a = 5\n"
  check $b.sliceRows(1, 3) == "let a = 5\necho a"

  check $b.sliceRows(0, 3) == """import os
let a = 5
echo a"""

test "Rope.addFront":
  var a = Rope.new("")
  a.addFront("abc")
  check $a == "abc"
  a.addFront("defgh")
  check $a == "defghabc"
  a.addFront("ijklmnopq")
  check $a == "ijklmnopqdefghabc"
  a.addFront("rstuvwxyz")
  check $a == "rstuvwxyzijklmnopqdefghabc"

test "Basic replace and slice":
  var a = Rope.new("Hello world!")
  check $a == "Hello world!"
  a.replace(6..<11, "you")
  check $a == "Hello you!"

  let b: Rope = a.slice(6, 10)
  check $b == "you!"
