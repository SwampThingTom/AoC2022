; Rock Paper Scissors
; https://adventofcode.com/2022/day/2

; Boot loader
!source "loader.asm"
+start_at $1000

; C-64 BASIC ROM subroutines
print_word=$bdcd	; Print 16-bit integer (A=msb, X=lsb)
print_str=$ab1e		; Print string (Y=#>string, A=#<string)

; PETSCII (yes, really) characters
petscii_cr=13		; Same as ascii (shrug)

; Initialize the zeropage pointers for each part.
!macro init_game .scores {
	; store address of first rps game data in next_game
	lda #<strategy_data
	sta next_game
	lda #>strategy_data
	sta next_game+1

	; store address of scores table in scores
	lda #<.scores
	sta scores
	lda #>.scores
	sta scores+1

	; clear result
	lda #0
	sta result
	sta result+1
}

; Add accumulator to 16-bit result.
!macro add_word .result {	
	clc
	adc .result
	sta .result
	bcc .done
	inc .result+1
.done
}

; Print null-terminated text in .string.
!macro print_s .string {
	lda #<.string
	ldy #>.string
	jsr print_str
}

; Print 16-bit value in .word.
!macro print_i .word {
	lda .word
	tax
	lda .word+1
	jsr print_word
}

; Zeropage addresses
next_game=$fb		; pointer to next game data
scores=$fd              ; pointer to scores data

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Solve both parts of today's puzzle and print results.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
entry
	; part 1
	+init_game scores1
	jsr play_rps
	jsr print_result
	; part 2
	+init_game scores2
	lda #'2'		; update message text for "PART 2"
	sta res_msg+5
	jsr play_rps
	jsr print_result
	rts

print_result
	+print_s res_msg
	+print_i result
	+print_s newline
	rts

; Scores for each of the 9 combinations of rps using the part 1 rules.
; Each score uses one byte. A byte of padding is added between each
; row to make indexing simpler.
;             AX, AY, AZ, 0
;             BX, BY, BZ, 0
;             CX, CY, CZ, 0
scores1	!byte 4, 8, 3, 0	; opponent chooses rock ('A')
	!byte 1, 5, 9, 0	; opponent chooses paper ('B')
	!byte 7, 2, 6, 0	; opponent chooses scissors ('C')
; Scores for each of the 9 combinations of rps using the part 2 rules.
scores2	!byte 3, 4, 8, 0	; opponent chooses rock ('A')
	!byte 1, 5, 9, 0	; opponent chooses paper ('B')
	!byte 2, 6, 7, 0	; opponent chooses scissors ('C')
index	!byte 0			; index into scores table
result	!word 0			; final result for each part
res_msg	!text "PART 1: ", 0
newline	!text petscii_cr, 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 1
;
; Sums the scores for each game.
; strategy_data contains 2 characters for each game.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
play_rps
	; load opponent move
	ldy #0
	lda (next_game),y
	beq .done		; are we done?

	; calculate index into scores table for opponent move
	sec
	sbc #'A'
	asl
	asl
	sta index

	; load "my" move
	ldy #1
	lda (next_game),y

	; update index into scores table for "my" move
	sec
	sbc #'X'
	clc
	adc index

	; update score
	tay
	lda (scores),y		; load score for this game
	+add_word result	; add to total score

	; update next_game pointer and continue
	lda #2
	+add_word next_game
	jmp play_rps
.done
	rts

; Data
!source "input.asm"
