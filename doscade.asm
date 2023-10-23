include cs240.inc
	
DOSEXIT = 4C00h
DOS = 21h

.8086

.data

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
BYTE "|                                     "
BYTE 0DCh
BYTE 0DCh
BYTE 0DCh
BYTE 0DCh
BYTE "                                     |" 
BYTE "|                                                                              |" 
BYTE "+------------------------------------------------------------------------------+"
BYTE "                                                                                " 
BYTE 0

.code

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
	mov dl, ds:[bp]
	mov	ax, 0B800h
	mov	es, ax
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

main PROC
	mov	ax, @data	; Setup the data segment
	mov	ds, ax
	

	mov dx, OFFSET gameLayout
	call SetupScreen ; setup screen just set ups the screen in variable stored in dx
	call ReadChar


	mov	ax, DOSEXIT	; Signal DOS that we are done
	int	DOS
main ENDP
END main