#!/usr/bin/env gst -q

" Tuning Trouble
  https://adventofcode.com/2022/day/6
"

Object subclass: Detector [
	" Scans text looking for a 'marker' where all characters are unique. "

	findMarkerInText: text length: markerLength  [
		" Returns the index of the start-of-packet marker in text. "
		| index endOfText |

		index := 1.
		endOfText := (text size) - markerLength.

		[ index < endOfText ] whileTrue: [
			(self isMarkerInText: text at: index length: markerLength)
				ifTrue: [ ^ index + markerLength - 1 ].
			index := index + 1
		].

		^ -1
	]

	isMarkerInText: text at: index length: markerLength [
		" Returns true if the marker length number of characters of text
		  starting at index are all unique. "
		
		(index + markerLength - 1) to: (index + 1) by: -1 do: [ :i |
			(i - 1) to: index by: -1 do: [ :j |
				((text at: i) = (text at: j)) ifTrue: [ ^ false ]
			].
		].

		^ true
	]
]

file := FileStream open: 'input.txt' mode: FileStream read.
text := file nextLine.
file close.

'Part 1: ' display.
detector := Detector new.
markerPosition := detector findMarkerInText: text length: 4.
markerPosition displayNl.

'Part 2: ' display.
markerPosition := detector findMarkerInText: text length: 14.
markerPosition displayNl.
