import std/[unittest, enumerate, strformat, sugar, unicode, os]
import nimsumtree/[sumtree]
import rope

test "Empty":
  check $Rope.new() == ""
  check $Rope.new("") == ""

test "One line":
  var a = Rope.new("abcdefghijklmnopqrstuvwxyz")
  check $a == "abcdefghijklmnopqrstuvwxyz"
  check a.bytes == 26
  check a.chars == 26
  check a.lines == Point(row: 0, column: 26)

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
  check a.lines == point

  # echo &"{text.len} bytes, {text.runeLen} runes"
  # echo a.tree.stats

test "Various string summaries":
  testStringSummary("abcdefghijklmnopqrstuvwxyz")

  testStringSummary("""import os
let a = 5
echo a""")

  const a = staticRead("text_example.txt")
  testStringSummary(a)
