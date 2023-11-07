TITLE KAKASHKA
include cs240.inc
	
DOSEXIT = 4C00h
DOS = 21h
TIMER_HANDLER = 1ch
SPEED = 3

SPEAKER_PORT = 61h
READY_TIMER		= 0B6h
TIMER_DATA_PORT		= 42h
TIMER_CONTROL_PORT	= 43h


.8086

.data

;; MUSIC
NOTE_TICKS = 2
NOTE_GAP_TICKS = 2


Life BYTE 3

Alarms	LABEL	WORD
	WORD	20 DUP(0)
HandlerCount = ($ - Alarms) / 4

GameOver BYTE 0

PlayScore WORD 0

GameOn BYTE 0 ; 0 -> pause, 1 -> resume

CursorPos WORD 0000h, 0000h

ballCurrentX BYTE 40
ballCurrentY BYTE 21

ballOnBrick BYTE 0

velocityX BYTE 0
velocityY BYTE 0

paddleMovement BYTE 0 ; 0 - no move; 1 -> right; 2 -> left

ballNextX BYTE 40
ballNextY BYTE 18

paddleX BYTE 37
paddleY BYTE 22

brickY BYTE 4
brickX BYTE 2

paddleChar BYTE 0DFh

brickChar BYTE 0DCh

bricksScores LABEL BYTE 
BYTE 12 Dup(1) ; y = 5 start at 4 to 8, 10 to 14
BYTE 11 Dup(1) ; y = 6 start at 7 to 11, 13 to 17
BYTE 12 Dup(1) ; y = 7
BYTE 11 Dup(1) ; y = 8
BYTE 12 Dup(1) ; y = 9
BYTE 11 Dup(1) ; y = 10
BYTE 12 Dup(1) ; y = 11



gameLayout LABEL BYTE
BYTE "+------------------------------------------------------------------------------+"
BYTE "|  Lives:                                                                      |" 
BYTE "|  Score:                                                                      |" 
BYTE "+------------------------------------------------------------------------------+"
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "+------------------------------------------------------------------------------+"
BYTE "                                                                                " 
BYTE 0

loserLayout LABEL BYTE
BYTE "+------------------------------------------------------------------------------+"
BYTE "|  Lives:                                                                      |" 
BYTE "|  Score:                                                                      |" 
BYTE "+------------------------------------------------------------------------------+"
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|    Y88b    /                         888       ,88~-_   ,d88~~\ 888~~        |" 
BYTE "|     Y88b  /   e88~-_  888  888       888      d888   \  8888    888___       |" 
BYTE "|      Y88b/   d888   i 888  888       888     88888    | `Y88b   888          |" 
BYTE "|       Y8Y    8888   | 888  888       888     88888    |  `Y88b, 888          |" 
BYTE "|        Y     Y888   ' 888  888       888      Y888   /     8888 888          |" 
BYTE "|       /       88_-~    88_-888       888____   `88_-~   \__88P' 888___       |" 

BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "+------------------------------------------------------------------------------+"
BYTE "                                                                                " 
BYTE 0

WinnerLayout LABEL BYTE
BYTE "+------------------------------------------------------------------------------+"
BYTE "|  Lives:                                                                      |" 
BYTE "|  Score:                                                                      |" 
BYTE "+------------------------------------------------------------------------------+"
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|      Y88b    /                         Y88b         / 888 888b    |          |" 
BYTE "|       Y88b  /   e88~-_  888  888        Y88b       /  888 |Y88b   |          |" 
BYTE "|        Y88b/   d888   i 888  888         Y88b  e  /   888 | Y88b  |          |" 
BYTE "|         Y8Y    8888   | 888  888          Y88bd8b/    888 |  Y88b |          |" 
BYTE "|          Y     Y888   ' 888  888           Y88Y8Y     888 |   Y88b|          |" 
BYTE "|         /       88_-~    88_-888            Y  Y      888 |    Y888          |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "+------------------------------------------------------------------------------+"
BYTE "                                                                                " 
BYTE 0

; MusicScore LABEL WORD 
; WORD 1614, 2153 ; F#  C#
; WORD 1614, 2153, 1614, 2153
; WORD 1614, 2153, 1614, 2153, 1614, 2153
; WORD 2420, 2711, 2153, 2153 ; B A C#
; WORD 2711, 2420, 1810 ; A B E
; WORD 1918, 0 ; D#
; MusicScore WORD 1614, 2153, 1614, 2153, 1614, 2153, 2420, 2711, 2153, 2153, 2711, 2420, 1810, 1918, 0h
; ; MusicScore WORD 1614
; ; WORD 2153
; ; WORD 0
; MusicIndex WORD 0 ; pointer to current note in MusicScore
;;
MusicScore WORD 1614, 2153, 1614, 2153, 1614, 2153, 2420, 2711, 2153, 2153, 2711, 2420, 1810, 1918, 1810, 1614, 1918, 2420
WORD 1614, 2420, 1614, 2420, 1614, 2420, 1614, 2560, 1614, 2560, 1521, 2560
WORD 1614, 2032, 1614, 2032, 1810, 1614
WORD 1810, 3233, 2032, 3233, 2153, 3233, 0
MusicIndex WORD 0 ; pointer to current note in MusicScore

.code

OldTimerHandler DWORD	00000000h

Tick WORD 0


; MUSIC 

SpeakerOn PROC
	pushf
	push	ax

	in	al, SPEAKER_PORT		; Read the speaker register
	or	al, 03h				; Set the two low bits high
	out	SPEAKER_PORT, al		; Write the speaker register

	pop	ax
	popf
	ret
SpeakerOn ENDP

SpeakerOff PROC
	pushf
	push	ax

	in	al, SPEAKER_PORT		; Read the speaker register
	and	al, 0FCh			; Clear the two low bits high
	out	SPEAKER_PORT, al		; Write the speaker register

	pop	ax
	popf
	ret
SpeakerOff ENDP

SetupMusic PROC
	;; DX = OFFSET of location of score
	pushf
	push dx
	mov dx, OFFSET MusicScore
	mov	MusicIndex, dx
	pop dx
	popf
	ret 
SetupMusic ENDP


PlayFrequency PROC
	;; Frequency is found in DX
	pushf
	push ax
	push bx
	push cx
	push dx 
	push si 
	push di 
	push bp 
	push es 
	
	cmp	dx, 0
	je	rest

	; call	NoteFrequencyToTimerCount

	mov	al, READY_TIMER			; Get the timer ready
	out	TIMER_CONTROL_PORT, al

	mov	al, dl
	out	TIMER_DATA_PORT, al		; Send the count low byte
	
	mov	al, dh
	out	TIMER_DATA_PORT, al		; Send the count high byte
	
	call	SpeakerOn

done:	
	pop es 
	pop bp 
	pop di 
	pop si 
	pop dx 
	pop cx 
	pop bx 
	pop ax
	popf
	ret
rest:
	call	SpeakerOff
	jmp	done
PlayFrequency ENDP

PlayNextNote PROC
	pushf
	push ax
	push bx
	push cx
	push dx 
	push si 
	push di 
	push bp 
	push es 

	mov	si, MusicIndex
	cmp	WORD PTR [si], 0
	jne	cont

	;; Repeat tune
	
	mov	si, OFFSET MusicScore
	mov	MusicIndex, si

cont:	
	; where should frequency be ??? in DX 
	mov dx, [si]
	call	PlayFrequency
 	mov	ax, NOTE_TICKS
 	mov	dx, OFFSET StopNote
	call	RegisterAlarm
done:	
	add si, 2
	mov MusicIndex, si
	pop es 
	pop bp 
	pop di 
	pop si 
	pop dx 
	pop cx 
	pop bx 
	pop ax
	popf
	ret
PlayNextNote ENDP

StopNote PROC
	pushf
	push ax
	push bx
	push cx
	push dx 
	push si 
	push di 
	push bp 
	push es 

	call	SpeakerOff
	
 	mov	ax, NOTE_GAP_TICKS
 	mov	dx, OFFSET PlayNextNote
	call	RegisterAlarm

done:	
	pop es 
	pop bp 
	pop di 
	pop si 
	pop dx 
	pop cx 
	pop bx 
	pop ax
	popf
	ret
StopNote ENDP

;;;;;;

DisplayScore PROC 
	pushf
	push ax
	push bx
	push cx
	push dx 
	push si 
	push di 
	push bp 
	push es 

	mov ax, 0B800h 
	mov es, ax 

	; lives 
	mov di, 180
	mov bl, Life
	cmp bx, 3
	and bx, 0Fh
	je lifeThree
	cmp bx, 2
	je lifeTwo
	cmp bx, 1
	je lifeOne
lifeThree:
	mov dl, '3'
	mov es:[di], dl
	jmp scoring
lifeTwo:
	mov dl, '2'
	mov es:[di], dl
	jmp scoring
lifeOne:
	mov dl, '1'
	mov es:[di], dl
	jmp scoring
	
scoring:
	add di, 160
	mov dh, ' '

	mov dx, PlayScore
; if score is less than 10
	cmp dx, 10
	jae twodigitScore
	add dx, 48
	mov es:[di], dl
	jmp scoringDone

twodigitScore:
	; need to get the lower digit and the upper digit...
	mov dx, PlayScore
	call SplitIntoDigits
	add dh, 48
	add dl, 48
	mov es:[di], dh 
	mov es:[di + 2], dl
scoringDone:
	pop es 
	pop bp 
	pop di 
	pop si 
	pop dx 
	pop cx 
	pop bx 
	pop ax
	popf
	ret 
DisplayScore ENDP

SplitIntoDigits PROC 
	; Number to split in dx 
	pushf
	push cx 
	mov cx, 0
countTens:
	cmp dx, 10
	jb lessThanTen
	inc cx
	sub dx, 10
	jmp countTens
lessThanTen:
	mov dh, cl 
	

	pop cx
	popf
	ret
SplitIntoDigits ENDP


CursorOff PROC ;turns the cursor off
  pushf
  push ax
  push bx
  push cx
  mov bh, 0
  mov ah, 03h
  int 10h
  mov bx, offset CursorPos
  mov [bx], dx
  mov ah, 01h
  mov cx, 2607h
  int 10h
  pop cx
  pop bx
  pop ax
  popf
  ret
CursorOff ENDP

CursorOn PROC ;turns the cursor on
  pushf
  push ax
  push bx
  push cx
  push dx

  mov bx, offset CursorPos
  mov dx, [bx]
  mov bh, 0
  mov ah, 02h
  int 10h

  mov ah, 01h
  mov cx, 0607h
  int 10h

  pop dx
  pop cx
  pop bx
  pop ax
  popf
  ret
CursorOn ENDP

DOS_GetInterruptVector PROC
	;; AL = interrupt number
	;; Returns:
	;; ES:BX = current interrupt handler
	
	push	ax
	
	mov	ah, 35h		; DOS function to get vector
	int	DOS		; Call DOS
	;; Returns:
	;; ES:BX = current interrupt handler
	
	pop	ax
	ret
DOS_GetInterruptVector ENDP

DOS_SetInterruptVector PROC
	;; AL = interrupt number
	;; ES:DX = new interrupt handler
	push	ax
	push	bx
	push	ds
	
	mov	bx, es		; DS = ES
	mov	ds, bx
	mov	ah, 25h		; DOS function to set vector
	int	DOS		; Call DOS
	
	pop	ds
	pop	bx
	pop	ax
	ret
DOS_SetInterruptVector ENDP

HookTimerInterrupt PROC
    pushf
    push es
    push bx
    push ax
    push dx

    ; gets old interrupt 
    mov al, TIMER_HANDLER
    call DOS_GetInterruptVector ; ES:BX = old interrupt
    mov dx, bx ; DX = BX = offset
    mov ax, es ; AX = ES = segment
    mov bx, offset OldTimerHandler ; BX = offset OldTimeHandler

    ; saves old interrupt in OldTimerHandler
    mov cs:[bx], dx
    mov cs:[bx + 2], ax

    mov	ax, cs				; ES = CS
	mov	es, ax				; ES = segment of handler
	mov	dx, NewTimerHandler	; DX = offset of handler
	mov	al, TIMER_HANDLER      		; Timer Handler Interrrupt
	call DOS_SetInterruptVector

    pop dx
    pop ax
    pop bx
    pop es
    popf
    ret
HookTimerInterrupt ENDP

UnHookInterrupt PROC
    pushf
    push es
    push bx
    push ax
    push dx

    
    mov bx, offset OldTimerHandler
    mov al, TIMER_HANDLER
    mov es, cs:[bx + 2]
    mov dx, cs:[bx]

	call DOS_SetInterruptVector

    pop dx
    pop ax
    pop bx
    pop es
    popf
    ret
UnHookInterrupt ENDP

NewTimerHandler PROC
	inc	cs:Tick
	jmp	cs:OldTimerHandler
NewTimerHandler ENDP

RegisterAlarm PROC
	;; AX=tick count
	;; DX=Handler offset
	
	pushf
	push	bx
	push	cx

	mov	cx, 0
	mov	bx, OFFSET Alarms
	jmp	cond
top:
	cmp	WORD PTR [bx], 0
	jne	used
	mov	WORD PTR [bx], ax
	mov	WORD PTR [bx + 2], dx
	jmp	done
used:	
	inc	cx
	add	bx, 4
cond:	
	cmp	cx, HandlerCount
	jl	top
done:	
	pop	cx
	pop	bx
	popf
	ret
RegisterAlarm ENDP

CheckAlarms PROC
	
	pushf
	push	bx
	push	cx
	
	cmp	cs:Tick, 0
	je	notick
	mov	cs:Tick, 0

	;; for (cx = 0; cx < HandlerCount; cx += 4)...
	mov	cx, 0
	mov	bx, OFFSET Alarms ;
	jmp	cond
top:
	cmp	WORD PTR [bx], 0 ; See if this alarm is in use
	je	unused
	dec	WORD PTR [bx]	; Yup. Take a tick away
	cmp	WORD PTR [bx], 0 ; Did it go off?
	ja	running
	call	WORD PTR [bx + 2] ; Yup. Call the alarm code
	
running:	
unused:	
	add	bx, 4		; Skip to the next handler
	inc	cx
cond:	
	cmp	cx, HandlerCount
	jl	top
	
notick:	
	pop	cx
	pop	bx
	popf
	ret
CheckAlarms ENDP

RegBallAlarm PROC
	; call dumpregs
	pushf 
	push ax
	push dx
	call BallMovement
	mov ax, SPEED
	mov dx, offset RegBallAlarm
	call RegisterAlarm
	pop dx
	pop ax
	popf
	ret
RegBallAlarm ENDP

BallMovement PROC
	pushf
	push ax
	push dx ; paddleX

	call EraseBall
	cmp GameOn, 0
	je done

	; call BallMovementBrick 

	mov dl, paddleX; dl = paddleX

	mov ah, velocityX ; ah = velocity X
	mov al, velocityY ; al = velocity Y

	add ballCurrentX, ah
	add ballCurrentY, al

	cmp ballCurrentY, 4
	jg checklow
	mov ballCurrentY, 4
	neg al	
	mov velocityY, al
checklow:
	cmp ballCurrentY, 21
	jl checkxleft
	mov ballCurrentY, 21
	cmp ballCurrentX, dl
	jb over
	add dl, 10
	cmp ballCurrentX, dl
	ja over
	neg al	
	mov velocityY, al
checkxleft:
	cmp ballCurrentX, 1
	jg checkxright
	mov ballCurrentX, 1
	neg ah	
	mov velocityX, ah
checkxright:
	cmp ballCurrentX, 78
	jl done
	mov ballCurrentX, 78
	neg ah
	mov velocityX, ah
	jmp done
over:
	call EraseBall
	dec Life
	cmp Life, 0
	je gamelose
	mov GameOn, 0
	mov BallCurrentX, 40
	mov ballCurrentY, 21
	jmp done
gamelose:
	mov GameOver, 1
done:
	call BallMovementBrick
	cmp ballOnBrick, 0 ; no collision
	je skipCollision

	; Temporarily play game sound here
	push dx  ; ;; ; ; 
	mov dx, 1500 ; ;; ; ; 
	call PlayFrequency; ;; ; ; 
	pop dx; ;; ; ; 
	; sound ^^^ when pop

	mov ax, PlayScore
	add ax, 1
	mov PlayScore, ax
	mov al, velocityY
	neg al
	mov velocityY, al
	mov ballOnBrick, 0
	cmp PlayScore, 500
	jae win
	jmp skipCollision
win:
	mov GameOver, 0
skipCollision:
	call SpawnBall
	; mov dx, OFFSET ballCurrentX
	; mov cx, 2 
	; call DumpMem 
	pop dx
	pop ax
	popf
	ret
BallMovement ENDP


BallMovementBrick1 PROC
	pushf
	push ax
	push bx
	push cx
	push dx 
	push si 
	push di 
	push bp 
	push es 

	
	mov ah, ballCurrentY ; ah = Y
	mov al, ballCurrentX ; al = X
	mov si, 0 ; SI = index of array
	
	mov	ax, 0B800h
	mov es, ax

	mov cl, ah
	mov ch, 0
	mov bx, 0

	addbx:
		add bx, 160
		loop addbx

	mov cl, al
	addbx1:
		inc bx
		inc bx
		loop addbx1

	mov dl, ' '
	cmp es:[bx], dl
	je nobrick
	mov BallonBrick, 1
	mov bx, offset bricksScores

	sub ah, 5 ; ah = Y
	cmp ah, 0
	je bricks12
	cmp ah, 2
	je bricks12
	cmp ah, 4
	je bricks12

	subtr:
		inc si
		sub al, 6
		cmp al, 7
		jg subtr
		dec si

		mov cl, ah
		mov ch, 0
		mov di, 12
	here:
		add bx, di
		call switchDI
		loop here
		jmp outofhere

	bricks12:
		inc si
		sub al, 6
		cmp al, 4
		jg bricks12
		dec si

		mov cl, ah
		mov ch, 0
		cmp cl, 0
		je outofhere
		mov di, 11
	here1:
		add bx, di
		call switchDI
		loop here1


	outofhere:
		add bx, si
		mov dl, [bx]
		dec dl
		mov [bx], dl
		jmp done
	nobrick:
		mov ballOnBrick, 0
	done:
	pop es 
	pop bp 
	pop di 
	pop si 
	pop dx 
	pop cx 
	pop bx 
	pop ax
	popf
ret
BallMovementBrick1 ENDP


SwitchDI PROC 
	pushf
	cmp di, 12
	je switchTo11
switchTo12:
	mov di, 12 
	jmp Switched 
switchTo11:
	mov di, 11
	jmp Switched
Switched:
	popf 
	ret 
SwitchDI ENDP


BallMovementBrick PROC 
	pushf
	push ax
	push bx
	push cx
	push dx 
	push si 
	push di 
	push bp 
	push es 

	mov ballOnBrick, 0
	mov ah, ballCurrentY ; 0B
	mov al, ballCurrentX ; 1E
	mov si, 0 

	; range of brick levels
	cmp ah, 5 
	jb brickNoTouch 
	cmp ah, 11
	ja brickNoTouch

	; check here which kind of level it interacted with: 
	cmp al, 4
	jb brickNoTouch
	cmp al, 74
	ja brickNoTouch
	sub al, 4 ; for easier calculations, now it's 0 - 70
	
	cmp ah, 5
	pushf 
	mov bp, 0
	popf
	je brickWith12
	cmp ah, 6
	pushf 
	add bp, 12
	popf
	je brickWith11 
	cmp ah, 7
	pushf 
	add bp, 11
	popf
	je brickWith12
	cmp ah, 8
	pushf 
	add bp, 12
	popf
	je brickWith11 
	cmp ah, 9
	pushf 
	add bp, 11
	popf
	je brickWith12
	cmp ah, 10
	pushf 
	add bp, 12
	popf
	je brickWith11 
	cmp ah, 11
	pushf 
	add bp, 11
	popf
	je brickWith12
	jmp brickNoTouch

brickWith12: 
	cmp al, 5
	jb brickDecrease
	je brickNoTouch
	cmp si, 12 ; 11 to 12
	je brickNoTouch
	sub al, 6 ;; CHANGED HERE
	inc si
	jmp brickWith12

brickWith11:
	sub al, 3
brickWith11Loop:
	cmp al, 5
	jb brickDecrease
	je brickNoTouch
	cmp si, 11 ;  10 to 11
	je brickNoTouch
	sub al, 6 ; CHANGED HERE
	inc si
	jmp brickWith11Loop

brickDecrease:
	mov bx, OFFSET bricksScores
	mov cl, 0
	add bx, bp
	add bx, si
	cmp [bx], cl ; if in table it's 0 or less than 0 
	je brickNoTouch
	mov cl, 1
	sub [bx], cl
	mov ballOnBrick, 1
brickNoTouch:
	pop es 
	pop bp 
	pop di 
	pop si 
	pop dx 
	pop cx 
	pop bx 
	pop ax
	popf
	ret 
BallMovementBrick ENDP


ErasePaddle PROC 
	pushf
	push dx
	push ax
    push di
    push cx

	mov dh, paddleY
	mov dl, paddleX

	mov di, ax
	mov ax, 0
	mov al, dh 
	mov bx, 160
	push dx
	mul bx
	pop dx
	and dx, 11111111b
	add ax, dx
	add ax, dx
	mov di, ax ; setup location 

	mov dl, ' ' ; setup char
	mov cx, 10 ; counter
	mov	ax, 0B800h ; screen loc
	mov	es, ax ; screen seg
	mov bp, 0

onePaddleCharDelete:
	mov es:[di + bp], dl
	add bp, 2
	loop onePaddleCharDelete

    pop cx
    pop di
	pop ax
	pop dx
	popf
	ret
ErasePaddle ENDP

MovePaddle PROC 
	pushf
    push dx
	push ax
    push di
    push cx

	mov dh, paddleY
	mov dl, paddleX

	mov di, ax
	mov ax, 0
	mov al, dh 
	mov bx, 160
	push dx
	mul bx
	pop dx
	and dx, 11111111b
	add ax, dx
	add ax, dx
	mov di, ax ; setup location 

	mov dl, paddleChar ; setup char
	mov dh, ' '
	mov cx, 10 ; counter
	mov	ax, 0B800h ; screen loc
	mov	es, ax ; screen seg
	mov bp, 0

onePaddleChar:
	mov es:[di + bp], dl 
	add bp, 2
	loop onePaddleChar

    pop cx
    pop di
	pop ax
	pop dx
	popf
	ret
MovePaddle ENDP


RefreshBricksGrid PROC 
	pushf
	push ax
	push bx
	push cx
	push dx 
	push si 
	push di 
	push bp 
	push es 

	push cx 
	push dx 
	mov dx, OFFSET bricksScores
	mov cx, 100
	; call DumpMem
	pop dx 
	pop cx

	mov bh, 0100b ; second level color
	mov bl, 0111b ; white color (first level)
	
	mov dl, brickChar
	mov ax, 0B800h ; screen start
	mov es, ax
	mov ax, 0
	; level by level.. check if they are not 0, display them..
	; 12 11 12 11 12 11 12
	mov si, 0
	mov bx, OFFSET bricksScores
	mov di, 800
	
bricksLevelSeven:
	add di, 8
	mov cx, 12
drawBrickLevelSeven:
	mov ah, [bx + si]
	cmp ah, 0
	jle skipBrickLevelSeven
	mov es:[di], dl
	mov es:[di + 2], dl
	mov es:[di + 4], dl
	mov es:[di + 6], dl
	mov es:[di + 8], dl
	jmp continueBrickLevelSeven
skipBrickLevelSeven:
	push dx 
	mov dh, ' '
	mov es:[di], dh
	mov es:[di + 2], dh
	mov es:[di + 4], dh
	mov es:[di + 6], dh
	mov es:[di + 8], dh
	pop dx
continueBrickLevelSeven:
	add di, 12
	add si, 1
	loop drawBrickLevelSeven

	mov di, 960
bricksLevelSix:
	add di, 14
	mov cx, 11
drawBrickLevelSix:
	mov ah, [bx + si]
	cmp ah, 0
	jle skipBrickLevelSix
	mov es:[di], dl
	mov es:[di + 2], dl
	mov es:[di + 4], dl
	mov es:[di + 6], dl
	mov es:[di + 8], dl
	jmp continueBrickLevelSix
skipBrickLevelSix:
	push dx 
	mov dh, ' '
	mov es:[di], dh
	mov es:[di + 2], dh
	mov es:[di + 4], dh
	mov es:[di + 6], dh
	mov es:[di + 8], dh
	pop dx
continueBrickLevelSix:
	add si, 1
	add di, 12 
	loop drawBrickLevelSix


	mov di, 1120
bricksLevelFive:
	add di, 8
	mov cx, 12
drawBrickLevelFive:
	mov ah, [bx + si]
	cmp ah, 0
	jle skipBricksLevelFive
	mov es:[di], dl
	mov es:[di + 2], dl
	mov es:[di + 4], dl
	mov es:[di + 6], dl
	mov es:[di + 8], dl
	jmp continueBrickLevelFive
skipBricksLevelFive:
	push dx 
	mov dh, ' '
	mov es:[di], dh
	mov es:[di + 2], dh
	mov es:[di + 4], dh
	mov es:[di + 6], dh
	mov es:[di + 8], dh
	pop dx
continueBrickLevelFive:
	add si, 1
	add di, 12 
	loop drawBrickLevelFive


	mov di, 1280
bricksLevelFour:
	add di, 14
	mov cx, 11
drawBrickLevelFour:
	mov ah, [bx + si]
	cmp ah, 0
	jle skipBrickLevelFour
	mov es:[di], dl
	mov es:[di + 2], dl
	mov es:[di + 4], dl
	mov es:[di + 6], dl
	mov es:[di + 8], dl
	jmp continueBrickLevelFour
skipBrickLevelFour:
	push dx 
	mov dh, ' '
	mov es:[di], dh
	mov es:[di + 2], dh
	mov es:[di + 4], dh
	mov es:[di + 6], dh
	mov es:[di + 8], dh
	pop dx
continueBrickLevelFour:
	add si, 1
	add di, 12 
	loop drawBrickLevelFour
	


	mov di, 1440
bricksLevelThree:
	add di, 8
	mov cx, 12
drawBrickLevelThree:
	mov ah, [bx + si]
	cmp ah, 0
	jle skipBrickLevelThree
	mov es:[di], dl
	mov es:[di + 2], dl
	mov es:[di + 4], dl
	mov es:[di + 6], dl
	mov es:[di + 8], dl
	jmp continueBrickLevelThree
skipBrickLevelThree:
	push dx 
	mov dh, ' '
	mov es:[di], dh
	mov es:[di + 2], dh
	mov es:[di + 4], dh
	mov es:[di + 6], dh
	mov es:[di + 8], dh
	pop dx
continueBrickLevelThree:
	add si, 1
	add di, 12 
	loop drawBrickLevelThree 


	mov di, 1600
bricksLevelTwo:
	add di, 14
	mov cx, 11
drawBrickLevelTwo:
	mov ah, [bx + si]
	cmp ah, 0
	jle skipBrickLevelTwo
	mov es:[di], dl
	mov es:[di + 2], dl
	mov es:[di + 4], dl
	mov es:[di + 6], dl
	mov es:[di + 8], dl
	jmp continueBrickLevelTwo
skipBrickLevelTwo:
	push dx 
	mov dh, ' '
	mov es:[di], dh
	mov es:[di + 2], dh
	mov es:[di + 4], dh
	mov es:[di + 6], dh
	mov es:[di + 8], dh
	pop dx
continueBrickLevelTwo:
	add si, 1
	add di, 12 
	loop drawBrickLevelTwo




	mov di, 1760
bricksLevelOne:
	add di, 8
	mov cx, 12
drawBrickLevelOne:
	mov ah, [bx + si]
	cmp ah, 0
	jle skipBrickLevelOne
	mov es:[di], dl
	mov es:[di + 2], dl
	mov es:[di + 4], dl
	mov es:[di + 6], dl
	mov es:[di + 8], dl
	jmp continueBrickLevelOne
skipBrickLevelOne:
	push dx 
	mov dh, ' '
	mov es:[di], dh
	mov es:[di + 2], dh
	mov es:[di + 4], dh
	mov es:[di + 6], dh
	mov es:[di + 8], dh
	pop dx
continueBrickLevelOne:
	add si, 1
	add di, 12 
	loop drawBrickLevelOne


	pop es 
	pop bp 
	pop di 
	pop si 
	pop dx 
	pop cx 
	pop bx 
	pop ax
	popf 
	ret
RefreshBricksGrid ENDP


SpawnBall PROC 
	pushf
    push dx
	push ax
    push di
    push cx

	mov dh, ballCurrentY
	mov dl, ballCurrentX
	mov di, ax
	mov ax, 0
	mov al, dh 
	mov bx, 160
	push dx
	mul bx
	pop dx
	and dx, 11111111b
	add ax, dx
	add ax, dx
	mov di, ax ; setup location 

	mov ax, 0B800h
	mov es, ax 
	mov dl, 'o'
	mov bp, 0
	
	mov es:[di], dl

    pop cx
    pop di
	pop ax
	pop dx
	popf 
	ret
SpawnBall ENDP


EraseBall PROC 
	pushf
    push dx
	push ax
    push di
    push cx
	mov dh, ballCurrentY
	mov dl, ballCurrentX
	mov di, ax
	mov ax, 0
	mov al, dh 
	mov bx, 160
	push dx
	mul bx
	pop dx
	and dx, 11111111b
	add ax, dx
	add ax, dx
	mov di, ax ; setup location 
	mov ax, 0B800h
	mov es, ax 
	mov dl, ' '
	mov bp, 0
	mov es:[di], dl
    pop cx
    pop di
	pop ax
	pop dx
	popf 
	ret
EraseBall ENDP

SetupScreen PROC 
	; dx - picture offset
	pushf
	push di

	mov di, 0
	mov bp, dx
	mov cx, 1920
	mov ax, 0
; screen is 80 * 24
pixel:
	mov	ax, 0B800h
	mov	es, ax

	mov bl, 0111b ; color everything to white
	mov es:[di + 1], bl 

	mov dl, ds:[bp]
	cmp dl, ' '
	je empty 
	mov bl, 0101b
	mov es:[di + 1], bl
empty:
	mov es:[di], dl
	add di, 2
	inc bp
loop pixel
	pop di
	popf
	ret
SetupScreen ENDP


LoserScreen PROC 
	; dx - picture offset
	pushf
	push di
	mov dx, OFFSET loserLayout

	mov di, 0
	mov bp, dx
	mov cx, 1920
	mov ax, 0
; screen is 80 * 24
pixel:
	mov	ax, 0B800h
	mov	es, ax

	mov dl, ds:[bp]
	cmp dl, ' '
	je empty 
	mov bl, 0111b
	mov es:[di + 1], bl
empty:
	mov es:[di], dl
	add di, 2
	inc bp
loop pixel
	pop di
	popf
	ret
LoserScreen ENDP


WinnerScreen PROC 
	; dx - picture offset
	pushf
	push di
	mov dx, OFFSET WinnerLayout

	mov di, 0
	mov bp, dx
	mov cx, 1920
	mov ax, 0
; screen is 80 * 24
pixel:
	mov	ax, 0B800h
	mov	es, ax

	mov dl, ds:[bp]
	cmp dl, ' '
	je empty 
	mov bl, 0111b
	mov es:[di + 1], bl
empty:
	mov es:[di], dl
	add di, 2
	inc bp
loop pixel
	pop di
	popf
	ret
WinnerScreen ENDP

EraseScreen PROC 
	; dx - picture offset
	pushf
	push ax
	push bx
	push cx
	push dx 
	push si 
	push di 
	push bp 
	push es 
	mov dx, OFFSET gameLayout

	mov di, 0
	mov bp, dx
	mov cx, 1920
	mov ax, 0
; screen is 80 * 24
pixel:
	mov	ax, 0B800h
	mov	es, ax

	mov dl, ds:[bp]
	mov es:[di], dl
	add di, 2
	inc bp
	loop pixel
	pop es 
	pop bp 
	pop di 
	pop si 
	pop dx 
	pop cx 
	pop bx 
	pop ax
	popf
	ret
EraseScreen ENDP


UserAction PROC 
	pushf

	call MovePaddle
	mov paddleMovement, 0
input:
	mov ax, 0
	mov ah, 0Bh
	int DOS
	cmp al, 0
	je finishUserAction
	mov ah, 07h 
	int 21h
	call ErasePaddle
    cmp al, 'q'
    je gameOverUA
	cmp al, 'a'
	je paddleLeft 
	cmp al, 'd' 
	je paddleRight
	cmp al, ' '
	je space
	jmp finishUserAction
paddleRight:
	cmp paddleX, 69
	je skipright
	inc paddleX
skipright:
	call MovePaddle
	mov paddleMovement, 1
	; jmp input 
	jmp finishUserAction
paddleLeft:
	cmp paddleX, 1
	je skipleft
	dec paddleX
skipleft:
	call MovePaddle
	mov paddleMovement, 2
	; jmp input 
	jmp finishUserAction
gameOverUA:
    mov GameOver, 1
	jmp finishUserAction

space:
	
	cmp GameOn, 0
	je change
	dec GameOn
	jmp finishUserAction
change:
	inc GameOn
	mov velocityX, -1
	mov velocityY, -1
finishUserAction:
	popf
	ret
UserAction ENDP


GameLoop PROC
	pushf
	push ax
	jmp	cond
  top:
	call DisplayScore
    call CheckAlarms
    call UserAction
	call RefreshBricksGrid
	call SpawnBall
  cond:
    cmp	GameOver, 0
    je	top
  done:
	cmp GameOver, 1 ; you lose
	je lose

	cmp GameOver, 2 ; you win
	je win

lose:
	call LoserScreen
	jmp finish
win:
	call WinnerScreen
	jmp finish
finish:

    pop ax
    popf
    ret
GameLoop ENDP



main PROC
	mov	ax, @data	; Setup the data segment
	mov	ds, ax
	call CursorOff

    call HookTimerInterrupt

	call RegBallAlarm

	call SetupMusic
	mov	ax, 3
 	mov	dx, OFFSET PlayNextNote
	call	RegisterAlarm

	mov dx, OFFSET gameLayout
	call SetupScreen
	call SpawnBall
	call GameLoop
    
    call UnHookInterrupt
	call SpeakerOff

	mov	ax, DOSEXIT	; Signal DOS that we are done
	int	DOS
main ENDP
END main