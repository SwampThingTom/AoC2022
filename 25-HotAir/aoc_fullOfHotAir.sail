rule!aoc_fullOfHotAir

a!localVariables(
  local!snafuValues: rule!aoc_day25InputSample(),
  local!decimalValues: a!forEach(
    items: local!snafuValues,
    expression: rule!aoc_snafuToDec(fv!item)
  ),
  local!sum: fn!reduce(fn!sum, 0, local!decimalValues),
  local!sumAsSnafu: rule!aoc_decToSnafu(local!sum, ""),
  a!textField(
    value: concat("Part 1: ", local!sumAsSnafu),
    readOnly: true()
  )
)

-----

rule!aoc_snafuToDec
    ri!snafuValue: Text

a!localVariables(
  local!digits: a!forEach(
    items: charat(ri!snafuValue, enumerate(len(ri!snafuValue)) + 1),
    expression: choose(
      wherecontains(fv!item, { "=", "-", "0", "1", "2" }),
      -2, -1, 0, 1, 2
    )
  ),
  fn!reduce(rule!aoc_snafuDigitsToDec, 0, local!digits)
)

-----

rule!aoc_snafuDigitsToDec
    ri!acc: Number(Integer)
    ri!digit: Number(Integer)

ri!acc * 5 + ri!digit

-----

rule!aoc_decToSnafu
    ri!decimalValue: Number(Integer)
    ri!acc: Text

a!localVariables(
  local!digit: tointeger(mod(ri!decimalValue + 2, 5) - 2),
  if(
    ri!decimalValue = 0,
    ri!acc,
    rule!aoc_decToSnafu(
      (ri!decimalValue - local!digit) / 5,
      concat(
        choose(
          wherecontains(
            local!digit,
            { -2, -1, 0, 1, 2 }
          ),
          "=", "-", "0", "1", "2"
        ),
        ri!acc
      )
    )
  )
)

-----

rule!aoc_day25InputSample

{
  "1=-0-2",
  "12111",
  "2=0=",
  "21",
  "2=01",
  "111",
  "20012",
  "112",
  "1=-1=",
  "1-12",
  "12",
  "1=",
  "122",
}
