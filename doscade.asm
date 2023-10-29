include cs240.inc
	
DOSEXIT = 4C00h
DOS = 21h
TIMER_HANDLER = 1ch
SPEED = 5

.8086

.data


Alarms	LABEL	WORD
	WORD	20 DUP(0)
HandlerCount = ($ - Alarms) / 4

GameOver BYTE 0

CursorPos WORD 0000h, 0000h

ballCurrentX BYTE 40
ballCurrentY BYTE 19

velocityX BYTE 1
velocityY BYTE 1

ballNextX BYTE 40
ballNextY BYTE 18

paddleX BYTE 37
paddleY BYTE 20

brickY BYTE 4
brickX BYTE 2

paddleChar BYTE 0DFh

brickChar BYTE 0DCh

bricksScores LABEL BYTE 
BYTE 12 Dup(2) ; y = 5 start at 4 to 8, 10 to 14
BYTE 11 Dup(2) ; y = 6 
BYTE 12 Dup(2) ; y = 7
BYTE 11 Dup(1) ; y = 8
BYTE 12 Dup(1) ; y = 9
BYTE 11 Dup(1) ; y = 10
BYTE 12 Dup(1) ; y = 11


gameLayoutBricks LABEL BYTE
BYTE "+------------------------------------------------------------------------------+"
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "+------------------------------------------------------------------------------+"
BYTE "|                                                                              |" 
BYTE "|   ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####    |" ; 12
BYTE "|      ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####       |" ; 11
BYTE "|   ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####    |"
BYTE "|      ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####       |" 
BYTE "|   ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####    |"
BYTE "|      ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####       |" 
BYTE "|   ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####    |"
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

gameLayout LABEL BYTE
BYTE "+------------------------------------------------------------------------------+"
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
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

.code

OldTimerHandler DWORD	00000000h

Tick WORD 0

CursorOff PROC ;turns the cursor off
  pushf
  push ax
  push bx
  push cx
  mov bh, 0
  mov ah, 03h
  int 10h
  mov bx, offset CursorPos
  ;mov [bx], cx
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
	mov ax, SPEED
	mov dx, offset BallMovement
	call RegisterAlarm
	pop dx
	pop ax
	popf
	ret
RegBallAlarm ENDP

BallMovement PROC
	pushf
	push ax

	; call EraseBall
	
	mov ah, velocityX
	mov al, velocityY

	add ballCurrentX, ah
	add ballCurrentY, al

	cmp ballCurrentY, 4
	jg checklow
	mov ballCurrentY, 4
	neg al	
	mov velocityY, al
checklow:
	cmp ballCurrentY, 22
	jl checkxleft
	mov ballCurrentY, 22
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
done:
	call SpawnBall
	pop ax
	popf
	ret
BallMovement ENDP
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
	je skipBrickLevelSeven
	mov es:[di], dl
	mov es:[di + 2], dl
	mov es:[di + 4], dl
	mov es:[di + 6], dl
	mov es:[di + 8], dl
skipBrickLevelSeven:
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
	je skipBrickLevelSix
	mov es:[di], dl
	mov es:[di + 2], dl
	mov es:[di + 4], dl
	mov es:[di + 6], dl
	mov es:[di + 8], dl
skipBrickLevelSix:
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
	je skipBricksLevelFive
	mov es:[di], dl
	mov es:[di + 2], dl
	mov es:[di + 4], dl
	mov es:[di + 6], dl
	mov es:[di + 8], dl
skipBricksLevelFive:
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
	je skipBrickLevelFour
	mov es:[di], dl
	mov es:[di + 2], dl
	mov es:[di + 4], dl
	mov es:[di + 6], dl
	mov es:[di + 8], dl
skipBrickLevelFour:
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
	je skipBrickLevelThree
	mov es:[di], dl
	mov es:[di + 2], dl
	mov es:[di + 4], dl
	mov es:[di + 6], dl
	mov es:[di + 8], dl
skipBrickLevelThree:
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
	je skipBrickLevelTwo
	mov es:[di], dl
	mov es:[di + 2], dl
	mov es:[di + 4], dl
	mov es:[di + 6], dl
	mov es:[di + 8], dl
skipBrickLevelTwo:
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
	je skipBrickLevelOne
	mov es:[di], dl
	mov es:[di + 2], dl
	mov es:[di + 4], dl
	mov es:[di + 6], dl
	mov es:[di + 8], dl
skipBrickLevelOne:
	add si, 1
	add di, 12 
	loop drawBrickLevelOne

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


UserAction PROC 
	pushf

	call MovePaddle
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
	jmp finishUserAction
paddleRight:
	inc paddleX
	call MovePaddle
	; jmp input 
	jmp finishUserAction
paddleLeft:
	dec paddleX
	call MovePaddle
	; jmp input 
	jmp finishUserAction
gameOverUA:
    mov GameOver, 1
finishUserAction:
	popf
	ret
UserAction ENDP


GameLoop PROC
	pushf
	push ax


    ; call SpawnBricks
	jmp	cond
  top:
	
    call CheckAlarms
    call UserAction
	call RefreshBricksGrid
	call BallMovement
  cond:
    cmp	GameOver, 0
    je	top
  done:

    pop ax
    popf
    ret
GameLoop ENDP



main PROC
	mov	ax, @data	; Setup the data segment
	mov	ds, ax
	call CursorOff

    call HookTimerInterrupt


	mov dx, OFFSET gameLayout
	call RegBallAlarm
	call SetupScreen
	call SpawnBall
	call GameLoop
    
    call UnHookInterrupt

	mov	ax, DOSEXIT	; Signal DOS that we are done
	int	DOS
main ENDP
END main