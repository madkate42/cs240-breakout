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
;; MUSIC
NOTE_TICKS = 4
NOTE_GAP_TICKS = 2


.8086
.code


main PROC
	mov	ax, @data	; Setup the data segment
	mov	ds, ax
	MOV AH, 00h  ; interrupts to get system time        
   INT 1AH      ; CX:DX now hold number of clock ticks since midnight      

   mov  ax, dx
   xor  dx, dx
   mov  cx, 3    
   div  cx       ; here dx contains the remainder of the division - from 0 to 9

   add  dl, '0'  ; to ascii from '0' to '9'
   mov ah, 2h   ; call interrupt to display a value in DL
   int 21h  

	mov	ax, DOSEXIT	; Signal DOS that we are done
	int	DOS
main ENDP
END main