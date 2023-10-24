include cs240.inc
	
DOSEXIT = 4C00h
DOS = 21h

.8086

.data

paddleX BYTE 37

paddle LABEL BYTE 
BYTE 0DCh
BYTE 0DCh
BYTE 0DCh
BYTE 0DCh
BYTE 0DCh
BYTE 0DCh

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


MovePaddle PROC 
	pushf
	; dh - pixels down
	; dl - pixels right

	mov di, ax
	mov ax, 0
	mov al, dh 
	mov bx, 160
	push dx
	mul bx
	pop dx
	and dx, 00001111b
	add ax, dx
	add ax, dx
	mov di, ax

	mov dx, OFFSET paddle 
	mov bp, dx
	mov dl, ds:[bp]

	mov	ax, 0B800h
	mov	es, ax
	mov es:[di], dl
	add di, 2
	mov es:[di], dl
	add di, 2
	mov es:[di], dl
	add di, 2
	mov es:[di], dl
	add di, 2
	mov es:[di], dl

	mov dl, 'K'
	mov es:[200], dl

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

; col:
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
	; inc ax
	; cmp ax, 24
	; jb col
; row2:
; 	mov dl, ds:[bp]
; 	mov	ax, 0B800h
; 	mov	es, ax
; 	mov es:[di], dl
; 	add di, 2
; 	inc bp
; 	call ReadChar
; loop row2

	pop di
	popf
	ret
SetupScreen ENDP

InteractionLoop PROC 
	mov dh, 20
	mov dl, 37
	call MovePaddle
input:
	mov ah, 07h 
	int 21h
	cmp al, 'a'
	je paddleRight 
	jmp paddleLeft
paddleRight:
	mov dh, 20
	mov dl, 36
	call MovePaddle
	jmp input 
paddleLeft:
	mov dh, 20
	mov dl, 38
	call MovePaddle
	jmp input 
	ret
InteractionLoop ENDP

main PROC
	mov	ax, @data	; Setup the data segment
	mov	ds, ax
	
	mov dx, OFFSET gameLayout
	call SetupScreen
	call InteractionLoop
	mov dh, 10
	mov dl, 1
	call MovePaddle
	call ReadChar


	mov	ax, DOSEXIT	; Signal DOS that we are done
	int	DOS
main ENDP
END main