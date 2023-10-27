include cs240.inc
	
DOSEXIT = 4C00h
DOS = 21h
TIMER_HANDLER = 1ch

.8086

.data

Alarms	LABEL	WORD
	WORD	20 DUP(0)
HandlerCount = ($ - Alarms) / 4

GameOver BYTE 0

CursorPos WORD 0000h, 0000h

BallX BYTE 20
BallY Byte 38

paddleX BYTE 37
paddleY BYTE 20

bricky BYTE 4
brickx BYTE 2

paddleChar BYTE 0DCh

gameLayout LABEL BYTE
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
	mov cx, 6 ; counter
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

	mov dl, 0DCh ; setup char
	mov cx, 6 ; counter
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

SpawnBricks PROC
pushf
    push dx
	push ax
    push di
    push cx
    push si


	mov dh, bricky
	mov dl, brickx
    mov si, 0

    jmp cond

top:
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

	mov dl, 0DCh ; setup char
	mov cx, 6 ; counter
	mov	ax, 0B800h ; screen loc
	mov	es, ax ; screen seg
	mov bp, 0
    inc si
onePaddleChar:
	mov es:[di + bp], dl 
	add bp, 2
	loop onePaddleChar

    mov dh, bricky
    mov dl, brickx
    mov ax, 6
    mul si
    add dl, al
cond:
    cmp si, 5
    jb top

    pop si
    pop cx
    pop di
	pop ax
	pop dx
	popf
	ret
SpawnBricks ENDP



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
	mov ah, 07h 
	int 21h
	call ErasePaddle
    cmp al, 'q'
    je over
	cmp al, 'a'
	je paddleLeft 
	cmp al, 'd' 
	je paddleRight
	jmp finishUserAction
paddleRight:
	inc paddleX
	call MovePaddle
	jmp input 
paddleLeft:
	dec paddleX
	call MovePaddle
	jmp input 
over:
    mov GameOver, 1
finishUserAction:
	popf
	ret
UserAction ENDP


GameLoop PROC
	pushf
	push ax


    call SpawnBricks
	jmp	cond
  top:
    call CheckAlarms
    call UserAction
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
	call SetupScreen
	call GameLoop
    
    call UnHookInterrupt

	mov	ax, DOSEXIT	; Signal DOS that we are done
	int	DOS
main ENDP
END main