INCLUDE "hardware.inc"
SECTION "Header", ROM0[$100]
	jp EntryPoint
	ds $150 - @, 0
EntryPoint:
	ld a, 0
	ld [rNR52], a
	ld hl, GivingStack
	ld a, l
	ld [GivingSP], a
	ld a, h
	ld [GivingSP+1], a
Main:
	ld hl, main+2 ; skip the first word of main (it's ENTER, since we define main in the usual way)
	ld a, l
	ld [GivingIP], a
	ld a, h
	ld [GivingIP+1], a
	jp NEXT
SECTION "Giving Variables", WRAM0
GivingBase: ds 1024
GivingStack:
GivingIP: dw
GivingW: dw
GivingSP: dw
SECTION "Giving Forth", ROM0

;; load \1 into \2 \3. clobbers a
MACRO GLoad
	ld a, [\1]
	ld \3, a
	ld a, [\1+1]
	ld \2, a
ENDM

;; store \2 \3 into \1. clobbers a
MACRO GStore
	ld a, \3
	ld [\1], a
	ld a, \2
	ld [\1+1], a
ENDM

;; replace r16 param with [param]. sets hl to hl+2. clobbers a
MACRO GDeref
	ld l, \2
	ld h, \1
	ld a, [hl+]
	ld \2, a
	ld a, [hl+]
	ld \1, a
ENDM

;; push \1 \2 to the stack. clobbers hl and a
MACRO GPush
	GLoad GivingSP,h,l
	dec hl
	dec hl
	ld a, \2
	ld [hl+],a
	ld a, \1
	ld [hl],a
	dec hl
	GStore GivingSP,h,l
ENDM

;; pop from the stack to \1 \2. clobbers hl and a
MACRO GPop
	GLoad GivingSP,h,l
	ld a, [hl+]
	ld \2, a
	ld a, [hl+]
	ld \1, a
	GStore GivingSP,h,l
ENDM

;; negate \1 \2. clobbers a, hl
MACRO GNegate
	ld a, \2
	cpl
	ld \2, a
	ld a, \1
	cpl
	ld \1, a
	ld hl, $0001
	add hl, \1\2
	ld \2, l
	ld \1, h
ENDM
