include cs240.inc
	
DOSEXIT = 4C00h
DOS = 21h

.8086

.data

paddleX BYTE 37
paddleY BYTE 20

paddleChar BYTE 0DCh

gameLayout LABEL BYTE
BYTE "+------------------------------------------------------------------------------+"
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                                                                              |" 
BYTE "|                        "
BYTE 0DFh
BYTE 0DFh
BYTE 0DFh
BYTE 0DFh
BYTE "                                                  |" 
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


ErasePaddle PROC 
	pushf
	push dx
	push ax
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

	pop ax
	pop dx
	popf
	ret
ErasePaddle ENDP

MovePaddle PROC 
	pushf
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

	popf
	ret
MovePaddle ENDP


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

	mov dh, paddleY
	mov dl, paddleX
	call MovePaddle

input:
	mov ah, 07h 
	int 21h
	call ErasePaddle
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

finishUserAction:
	popf
	ret
UserAction ENDP


main PROC
	mov	ax, @data	; Setup the data segment
	mov	ds, ax
	
	mov dx, OFFSET gameLayout
	call SetupScreen
	call UserAction


	mov	ax, DOSEXIT	; Signal DOS that we are done
	int	DOS
main ENDP
END main