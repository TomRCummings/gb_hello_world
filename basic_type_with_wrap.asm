; Jump table for rst and interrupts
SECTION "rst0", ROM0[$0000]
	jp begin
SECTION "rst1", ROM0[$0008]
	jp begin
SECTION "rst2", ROM0[$0010]
	jp begin
SECTION "rst3", ROM0[$0018]
	jp begin
SECTION "rst4", ROM0[$0020]
	jp begin
SECTION "rst5", ROM0[$0028]
	jp begin
SECTION "rst6", ROM0[$0030]
	jp begin
SECTION "rst7", ROM0[$0038]
	jp begin
SECTION "intVBlank", ROM0[$0040]
	jp begin
SECTION "intLCDStat", ROM0[$0048]
	jp begin
SECTION "intTimer", ROM0[$0050]
	jp begin
SECTION "intSerial", ROM0[$0058]
	jp begin
SECTION "intJoypad", ROM0[$0060]
	jp begin

; Cartridge header
SECTION "header",ROM0[$0100]
	nop ; Boot rom jumps here when finished executing
	jp begin

; Nintendo logo
DB $CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
DB $00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
DB $BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E

DB "HELLO WORLD",0,0,0,0,0   ; Cart name - padded to 16 bytes
DB 0, 0                      ; New licensee code
DB 0                         ; SGB flag
DB 0                         ; Cart type
DB 0                         ; ROM Size
DB 0                         ; RAM Size
DB 1                         ; Destination code
DB $33                       ; Old licensee code
DB 0                         ; Mask ROM version
DB 0                         ; Header checksum
DW 0                         ; Global checksum

; Contains alphabetical font data and some useful related constants
INCLUDE "font.inc"

message: ; The string we'll print, length-prefixed
	db 23
	db "Hello World this is Tom"

begin:
	di ; disable interrupts
	ld sp, $fffe ; setup the stack pointer to the top of RAM (it "grows" down)

	ld A, %11100100 ; Set "normal" palette (darkest to lightest)
	ld [$FF47], A ; Load palette into register
	ld A, 0
	ld [$FF42], A ; Set scroll X to 0
	ld [$FF43], A ; Set scroll Y to 0

	call OffScreen ; Turn the screen off before writing some data to VRAM

	call LoadFont ; Put the font data into VRAM
	call ClearScreen ; Clear any data in the tile map area of VRAM

	ld HL, message
	call PrintText
	
	; Turn the screen back on (easier to do than turning it off)
	ld A, [$FF40] ; Get the current LCD Control register
	set 7, A ; Set the LCD/PPU control bit
	ld [$FF40], A
	
	;ld A, %00010000 ; Set VBlank source on in the LCD stat register
	;ld [$FF41], A

	;ld A, %00000001 ; Set VBlank interrupt enabled in the IE register
	;ld [$FFFF], A
	;ei
	
wait:
	nop
	jr wait

; Function: OffScreen
; Safely turns off the LCD/PPU during the next VBlank
; Parameters: none
; Returns: none
OffScreen:
	push AF ; Save AF to the stack, since we'll use A in this function
	ld A, [$FF40]
	bit 7, A ; Test if LCD/PPU is already off
	jp z, OffScreenReturn;
.wait:
	ld A, [$FF0F] ; Get the Interrupt Flag register
	bit 0, A ; Test the VBlank bit of the Interrupt Flag register (we can only off the LCD when in VBlank)
	jr z, .wait ; Wait for VBlank if the Z flag is set
	
	ld A, [$FF40] ; Get the current LCD Control register
	res 7, A ; Reset the LCD/PPU control bit
	ld [$FF40], A
OffScreenReturn:
	pop AF ; Restore saved registers
	ret

; Function: LoadFont
; Puts the font data into VRAM memory starting at $8000. 
; ASSUMES that the screen is off.
; Parameters: none
; Returns: none
LoadFont:
	push AF
	push HL
	push DE
	push BC
	ld BC, 848 ; counter, equals the size of UPPER_FONT and LOWER_FONT
	ld DE, $8000 ; destination for font data
	ld HL, UPPER_FONT ; source of font data
.loop:
	ldi A, [HL] ; get the current byte and increment source pointer
	ld [DE], A ; store the current byte in destination
	inc DE ; increment the destination pointer
	dec BC ; decrement LSB of counter
	ld A, 0
	cp C ; check if the LSB of the counter is zero
	jp nz, .loop ; if not, loop
	cp B ; check if the MSB of the counter is zero
	jp nz, .loop ; if not, loop
.finish:
	pop BC
	pop DE
	pop HL
	pop AF
	ret
	

; Function: ClearScreen
; "Zeros" all of the tile indexes in the tile map starting at $9800.
; Parameters: none
; Returns: none
ClearScreen:
	push HL
	push AF
	ld HL, $9800
.loop:
	ld A, 0
	ldi [HL], A
	ld A, $FF
	cp L
	jp nz, .loop
	ld A, $9B
	cp H
	jp nz, .loop

	pop AF
	pop HL
	ret

; Function: PrintText
; Displays text on the screen as background. 
; ASSUMES that font data is already in VRAM at $8000. Also assumes the text is UTF-8 encoded.
; Parameters: 	HL - Pointer to text to print
; Returns: none
PrintText:
	push AF
	push DE
	push BC
	ldi A, [HL]
	ld C, A
	ld DE, $9800 ; Base address for tile map in VRAM

.loop:
	ldi A, [HL] ; Get char code and increment string pointer
	cp 97 ; Check if upper- or lower-case
	jr c, .upper ; Maps utf character codes to their index in font
	sub 6
.upper:
	sub 64
	ld [DE], A ; Save the tile index in tile map
	inc DE ; Increment tile map pointer
	; Check if needs to be wrapped around
	push DE
	push BC
	ld BC, 32
	call fastMod
	pop BC
	ld A, E
	cp 20
	pop DE
	jr nz, .skipNewLine
	ld A, E
	add 12
	ld E, A
	ld A, D
	adc 0
	ld D, A	

.skipNewLine:
	dec C
	jr nz, .loop

.finish:
	pop BC
	pop DE
	pop AF
	ret

; Function: modulus
; Computes the remainder of the division between dividend and divisor. Note: this function computes the
; modulus the long way (subtraction in a loop).
; Parameters: DE - dividend
;	      BC - divisor
; Returns: DE - remainder
modulus:
	push AF
.check:
	ld A, D
	cp B
	jr c, .finish
	jr z, .zero

.start:
	ld A, E
	sub C
	ld E, A
	ld A, D
	sbc B
	ld D, A
	jr .check
	
.finish:
	pop AF
	ret
.zero:
	ld A, E
	cp C
	jr c, .finish
	jr .start

; Function: fastMod
; Computes the remainder of the divison between dividend and divisor. Note: ASSUMES that divisor is a power of 2.
; Parameters: DE - dividend
;	      BC - divisor
; Returns: DE - remainder
fastMod:
	push AF
	dec BC
	ld A, D
	and B
	ld D, A
	ld A, E
	and C
	ld E, A
	pop AF
	ret
	
; ISR: VBlank
ISRVBlank:
	reti

; ISR: LCDStat
ISRLCDStat:
	reti
