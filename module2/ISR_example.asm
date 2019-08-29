; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 0 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP52
$LIST

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BOOT_BUTTON   equ P4.5
SOUND_OUT     equ P3.7
UPDOWN        equ P0.0

; Reset vector
org 0000H
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0003H
	reti

; Timer/Counter 0 overflow interrupt vector
org 000BH
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0013H
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 001BH
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0023H 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 002BH
	ljmp Timer2_ISR

dseg at 30h
Count1ms:     ds 2 ; Used to determine when half second has passed
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop

second: ds 1 ;used to display clock time
min: ds 1
hours:ds 1

position:ds 1
setsecond:ds 1
setmin:ds 1
sethours:ds 1

alarmsecond:ds 1
alarmmin:ds 1
alarmhours:ds 1



bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
day: dbit 1
setday:dbit 1
alarmday:dbit 1
cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.4
LCD_RW equ P1.5
LCD_E  equ P1.6
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the position of the counter
Initial_Message:  db '                ', 0
DISPLAYTIME: db 'xx:xx:xxxx',0
AMMESSAGE: db 'am',0
PMMESSAGE: db 'pm',0
uparrow: db '0xc5',0
ready: db 'ready!',0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	; One millisecond interrupt
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
    setb EA   ; Enable Global interrupts
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	; Define a latency correction for the timer reload
	CORRECTION EQU (4+4+2+2+4+4) ; lcall+ljmp+clr+mov+mov+setb
	; In mode 1 we need to reload the timer.
	clr TR0
	mov TH0, #high(TIMER0_RELOAD+CORRECTION)
	mov TL0, #low(TIMER0_RELOAD+CORRECTION)
	setb TR0
	cpl SOUND_OUT ; Connect speaker to P3.7!
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer.  Autoreload mode.
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 0 interrupt
    setb TR2  ; Only enabled when sound is on.  To turn sound of clear TR1
    setb EA   ; Enable Global interrupts
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically in ISR
	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	
	; 1000 milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know half second had passed
	cpl TR1 ; This line makes a beep-silence-beep-silence sound
	; Reset the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	
	
	; Increment the BCD counter
	mov a, second
	jnb UPDOWN, Timer2_ISR_decrement
	add a, #0x1
	sjmp Timer2_ISR_da
Timer2_ISR_decrement:
	add a, #0x99
	CJNE a, #0x99, Timer2_ISR_da
	mov a, #0x59
Timer2_ISR_da:
	da a
	mov second, a
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #7FH
    mov PMOD, #0 ; Configure all ports in bidirectional mode
    lcall Timer0_Init
    lcall Timer2_Init
    lcall LCD_4BIT
    lcall setarrow


    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
    setb half_seconds_flag
	mov second, #0x00
	mov min, #0x59
	mov hours, #0x12
	mov day, #0x0
	mov alarmsecond, #0x05
	mov alarmmin, #0x59
	mov alarmhours, #0x12
	mov alarmday, #0x0
	Set_Cursor(2, 1)
    Send_Constant_String(#DISPLAYTIME)
    
	; After initialization the program stays in this 'forever' loop
loop:
	Set_Cursor(1, 7)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(alarmsecond)
	Set_Cursor(1, 1)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(alarmhours)
	Set_Cursor(1, 4)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(alarmmin)
	Set_Cursor(1, 9)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(alarmday)
	jnb p2.4, settime1 ;check set time button
	jnb p2.5, setalarm1 ;check set alarm button

	mov a, day
	subb a, alarmday

	jnz KEEPGOING
	mov a, hours
	subb a, alarmhours

	jnz KEEPGOING
	mov a, min
	subb a, alarmmin

	jnz KEEPGOING
	mov a, second
	subb a, alarmsecond
	jnz KEEPGOING
ALARMTIME1:	LJMP ALARMTIME
	loop1: LJMP loop
KEEPGOING:
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; wait for button release
	; A clean press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop the timer and reset the milli-seconds counter, to resync everything.
	clr TR0
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Now clear the BCD counter
	mov second, #0x00
	setb TR0                ; Re-enable the timer
	sjmp loop_b             ; Display the new value
loop_a:
	jnb half_seconds_flag, loop1
loop_b:
    clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 0
lcall SHOWTIME
	sjmp SETTIMED ;display time
settime1:ljmp settime ;mid jump due to jnz/cjnz 
setalarm1:ljmp setalarm;mid jump due to jnz/cjnz 

SETTIMED:	mov day, a 

	
Go:	
lcall SHOWTIME
	ljmp loop

setright1:ljmp setright;mid jump due to jnz/cjnz 
setleft1:ljmp setleft;mid jump due to jnz/cjnz 
setdown1:ljmp setdown;mid jump due to jnz/cjnz 
setup1:ljmp setup;mid jump due to jnz/cjnz 

setalarm:jnb p2.5, $ ;stay until button is no longer pressed
	
	sjmp setit
settime:
	jnb p2.4, $;stay until button is no longer pressed
setit:
	mov position, #0x0 ;temp value for setting
	mov setday, day
	mov sethours, hours
	mov setmin, min
	mov setsecond, second
checkset:
	jnb p2.3,next1 ;checks which button is pressed
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jnb p2.3,setright1 ;checks which button is pressed
next1:
	jb p2.2, next2
		Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jnb p2.2,setleft1 ;checks which button is pressed
next2:
	jb p2.1,next3
		Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jnb p2.1,setdown1 ;checks which button is pressed
next3:
	jb p2.0,next4
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jnb p2.0,setup1 ;checks which button is pressed
next4:
	Set_Cursor(2, 4)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(setmin) ; display setmin at 2,4
	Set_Cursor(2, 1)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(sethours) ; display setmin at 2,1
	Set_Cursor(2, 7)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(setsecond) ; display setmin at 2,7
	Set_Cursor(1, 1) 
    Send_Constant_String(#Initial_Message);clear top row
	mov a, position
	cjnE a, #0x00, innothours
	mov a, #0x80 ;put arrow above hour position
	sjmp pointtoposition
innothours:
	cjnE a, #0x01, innotmin
	mov a, #0x83 ;put arrow above min position
	sjmp pointtoposition
innotmin:
	cjnE a, #0x02, innotsecond
	mov a, #0x86 ;put arrow above second position
	sjmp pointtoposition
innotsecond:
	mov a, #0x88 ;put arrow above am/pm position
	sjmp pointtoposition
pointtoposition:
    lcall ?WriteCommand
	mov a, #0x0 ;draw arrow above said position
    lcall ?WriteData
	jnb p2.4,returnittime ;check for finish button
	jnb p2.5, returnitalarm ;check for finish button
	ljmp checkset
returnittime: jnb p2.4, $ ;stay until release
	mov day, setday ;set counter date
	mov hours, sethours
	mov min, setmin
	mov second, setsecond
		Set_Cursor(1, 1) 
	Send_Constant_String(#Initial_Message)
	ljmp loop
returnitalarm: jnb p2.5, $ ;stay until release
	mov alarmday, setday ;set alarm
	mov alarmhours, sethours
	mov alarmmin, setmin
	mov alarmsecond, setsecond
		Set_Cursor(1, 1) 
	Send_Constant_String(#Initial_Message)
	ljmp loop
setup:	
	jnb p2.0, $  ;stay until release
	mov a, position
	CJNE a, #0x00,nothoursU ;find position
	mov a, sethours ;position in hours
	CJNE a, #0x12,movuphour ;set to 00 if 12
	mov a, #0x01
	mov sethours, a
	ljmp checkset
movuphour:
	mov a, sethours ;add 1 to hour
	add a, #0x01
	da a
	mov sethours,a
	ljmp checkset
nothoursU:
	mov a, position 
	CJNE a, #0x01, notminsU ;find position
	mov a, setmin ;position in mins
	CJNE a, #0x59, movupmin ;set to 00 if 59
	mov a, #0x00
	mov setmin,a
	ljmp checkset
movupmin:
	mov a, setmin ;add 1 min
	add a, #0x01
	da a
	mov setmin,a
	ljmp checkset
notminsU:
	mov a, position 
	CJNE a, #0x02, notsecondsU ;find position
	mov a, setsecond;position in second
	CJNE a, #0x59, movupsecond ;set to 00 if 59
	mov a, #0x00
	da a
	mov setsecond, a
	ljmp checkset
movupsecond:
	mov a, setsecond ;add 1 sec
	add a, #0x01
	da a
	mov setsecond,a
	ljmp checkset
notsecondsU:
	ljmp notsecondsd;mid jump due to jnz/cjne
switch:
	mov setday, #0x1 ;set am
	Set_Cursor(2,9)
    Send_Constant_String(#AMMESSAGE)
	ljmp checkset
setdown:	
	jnb p2.1, $ ;stay untill button is release
	mov a, position
	CJNE a, #0x00,nothoursD ;find position
	mov a, sethours ;in hours
	CJNE a, #0x00,movdownhour 
	mov sethours, #0x12 ;set hours to 12 if 00
	ljmp checkset
movdownhour:
	mov a, sethours
	add a, #0x99 ;sub 1 from hours
	da a
	mov sethours,a
	ljmp checkset
switch1:ljmp switch ;mid jmp due to jnz/cjne
nothoursD:
	mov a, position
	CJNE a, #0x01, notminsD ;find position
	mov a, setmin ;position in min
	CJNE a, #0x00, movdownmin 
	mov setmin, #0x59 ;set min to 59 if 00
	ljmp checkset
movdownmin:
	mov a, setmin ;sub 1 from min
	add a, #0x99
	da a
	mov setmin,a
	ljmp checkset
notminsD:
	mov a, position
	CJNE a, #0x02, notsecondsD ;find position
	mov a, setsecond ;position in second
	CJNE a, #0x00, movdownseconds
	mov setsecond,#0x59 ;set seconds to 59 if 00
	ljmp checkset
movdownseconds:
	mov a, setsecond
	add a, #0x99 ;sub 1 from second
	da a
	mov setsecond,a
	ljmp checkset
notsecondsD:
	mov a, setday ; position in day
	CJNE a, #0x01, switch1 
	mov a, #0x0 ;set to pm if am
	mov setday,a
	Set_Cursor(2,9)
    Send_Constant_String(#PMMESSAGE)
	ljmp checkset
setright:
	jnb p2.3, $ ;stay untill button is release
	mov a, position ;find position
	CJNE a, #0x03,movright 
	mov position, #0x00 ; mov position to hours spot if in day
	ljmp checkset
movright:
	mov a, position ;mov to the right
	add a, #0x01
	mov position,a
	ljmp checkset
setleft:
	jnb p2.2, $ ;stay until button is release
	mov a, position ;find position
	CJNE a, #0x00,movleft 
	mov position, #0x03 ;mov position to days if in hours
	ljmp checkset
movleft:
	mov a, position ;mov position to left
	subb a, #0x01
	mov position,a
	ljmp checkset
	
setarrow:
	mov a, #0x40 ; add up and down arrows
    lcall ?WriteCommand
    mov a, #0x00
    lcall ?WriteData
    mov a, #0x04
    lcall ?WriteData  
    mov a, #0x04
    lcall ?WriteData 
    mov a, #0x04 
    lcall ?WriteData
    mov a, #0x15
    lcall ?WriteData 
    mov a, #0x0e
    lcall ?WriteData
    mov a, #0x04 
    lcall ?WriteData
    mov a, #0x00   
    lcall ?WriteData
    mov a, #0x00
    lcall ?WriteData
    mov a, #0x04
    lcall ?WriteData  
    mov a, #0x0e
    lcall ?WriteData 
    mov a, #0x15 
    lcall ?WriteData
    mov a, #0x04
    lcall ?WriteData 
    mov a, #0x04
    lcall ?WriteData
    mov a, #0x04 
    lcall ?WriteData
    mov a, #0x00   
    lcall ?WriteData
	RET
ALARMTIME:

lcall playmario
	Set_Cursor(2, 1)
    Send_Constant_String(#Initial_Message)
    	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
lcall genrandomnumber03
lcall FINDOPTION
	Set_Cursor(2, 1)
    Send_Constant_String(#Initial_Message)
lcall playmarioa
ljmp loop

playmario:
 
lcall SHOWTIME

lcall E2
    Wait_Milli_Seconds(#50)
    lcall SHOWTIME
lcall E2
    Wait_Milli_Seconds(#50)
    lcall SHOWTIME
lcall E2
    Wait_Milli_Seconds(#50)
    lcall SHOWTIME
lcall C2
    Wait_Milli_Seconds(#50)
    lcall SHOWTIME
lcall E2
    Wait_Milli_Seconds(#50)
    lcall SHOWTIME
lcall G2
    Wait_Milli_Seconds(#50)
    lcall SHOWTIME
lcall G1
    Wait_Milli_Seconds(#50)
lcall SHOWTIME
lcall C2
    Wait_Milli_Seconds(#50)
    lcall SHOWTIME
lcall G1
    Wait_Milli_Seconds(#50)
lcall SHOWTIME
lcall E1
    Wait_Milli_Seconds(#50)
    lcall SHOWTIME
lcall A1
    Wait_Milli_Seconds(#50)
    lcall SHOWTIME
lcall B1
    Wait_Milli_Seconds(#50)
    lcall SHOWTIME
lcall Bflat1
    Wait_Milli_Seconds(#50)
    lcall SHOWTIME
lcall A1
    Wait_Milli_Seconds(#50)
lcall SHOWTIME
    Wait_Milli_Seconds(#50)
    lcall G1
lcall SHOWTIME
ret

playmarioa:
lcall E2
    Wait_Milli_Seconds(#50)
lcall E2
    Wait_Milli_Seconds(#50)
lcall E2
mov a, #0x18
lcall ?WriteCommand
lcall C2
    Wait_Milli_Seconds(#50)
lcall E2
    Wait_Milli_Seconds(#50)
lcall G2
    Wait_Milli_Seconds(#50)
lcall G1
mov a, #0x18
lcall ?WriteCommand
lcall C2
    Wait_Milli_Seconds(#50)
lcall G1
    Wait_Milli_Seconds(#50)
lcall E1
    Wait_Milli_Seconds(#50)
lcall A1
mov a, #0x18
lcall ?WriteCommand
lcall B1
    Wait_Milli_Seconds(#50)
lcall Bflat1
    Wait_Milli_Seconds(#50)
lcall A1
    Wait_Milli_Seconds(#50)
lcall G1
mov a, #0x18
lcall ?WriteCommand
ljmp playmarioa
E1:;329.63 hz, 1/329.63=0.00303370  turn on and off in that time
mov R2, #83*2
LE1: mov R1, #135/2
	cpl p2.6

LE2: mov R0, #166
LE3: djnz R0, LE3 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, LE2 ; 22.51519us*135=3ms
    djnz R2, LE1 ; (3ms)*29=0.25s (approximately)

G1:;261.63 hz, 1/392.00=0.00255102 turn on and off in that time
mov R2, #33*2
LG1: mov R1, #113/2
	cpl p2.6
LG2: mov R0, #166
LG3: djnz R0, LG3 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, LG2 ; 22.51519us*113=2.55102ms
    djnz R2, LG1 ; (2ms)*125=0.25s (approximately)
ret

C2:;523.25 hz, 1/523.25=0.00191113 turn on and off in that time
mov R3, #2
LC0:mov R2, #131
LC1: mov R1, #85/2
	cpl p2.6
LC2: mov R0, #166
LC3: djnz R0, LC3 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, LC2 ; 22.51519us*85=1.91113ms
    djnz R2, LC1 ; (1ms)*131=0.25s (approximately)
    djnz R3,LC0
ret	


A1:;440.00 hz, 1/440.00=0.00227273 turn on and off in that time
mov R2, #110*2
LA1: mov R1, #101/2
	cpl p2.6
LA2: mov R0, #166
LA3: djnz R0, LA3 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, LA2 ; 22.51519us*101=2.27273ms
    djnz R2, LA1 ; (1ms)*110=0.25s (approximately)
ret	
B1:;493.88 hz, 1/493.88=0.00202478 turn on and off in that time
mov R2, #123*2
LB1: mov R1, #90/2
	cpl p2.6
LB2: mov R0, #166
LB3: djnz R0, LB3 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, LB2 ; 22.51519us*90=2.02478ms
    djnz R2, LB1 ; (2.02478ms)*123=0.25s (approximately)
ret		
Bflat1:;466.16 hz, 1/466.16=0.00214519 turn on and off in that time
mov R2, #117*2
LBF1: mov R1, #95/2
	cpl p2.6
LBF2: mov R0, #166
LBF3: djnz R0, LBF3 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, LBF2 ; 22.51519us*95=2.14519ms
    djnz R2, LBF1 ; (2.14519ms)*117=0.25s (approximately)
ret
G2:;783.99 hz, 1/783.99=0.00127553 turn on and off in that time
mov R3, #2
LG20:mov R2, #165
LG21: mov R1, #57/2
	cpl p2.6
LG22: mov R0, #166
LG23: djnz R0, LG23 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, LG22 ; 22.51519us*57=1.27553ms
    djnz R2, LG21 ; (1.51688ms)*165=0.25s (approximately)
        djnz R3,LG20
ret	
E2:;659.25 hz, 1/659.25=0.00151688 turn on and off in that time
mov R3, #2
LE20:mov R2, #165
LE21: mov R1, #67/2
	cpl p2.6
LE22: mov R0, #166
LE23: djnz R0, LE23 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, LE22 ; 22.51519us*67=1.51688ms
    djnz R2, LE21 ; (1.51688ms)*165=0.25s (approximately)
    djnz R3,LE20
ret		
SHOWTIME:
lcall CHECKTIME
Set_Cursor(2, 7)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(second) ;display second at 2,7
    Set_Cursor(2, 4)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(min) ; display min at 2,4
    Set_Cursor(2, 1)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(hours) ; display hours at 2,1

    Set_Cursor(2,9)
    mov a, day
    
    JNZ DISPLAYAM
    Send_Constant_String(#PMMESSAGE); display am/pm at 2,9
    ret
DISPLAYAM:
	Set_Cursor(2,9)
    Send_Constant_String(#AMMESSAGE); display am/pm at 2,9
    ljmp loop
    
CHECKTIME:       MOV a, second
	CJNE a, #0x60,returnnow ;makes it go back to 00, whole min has pass
	MOV second ,#0x00
	MOV a, min
	add a, #0x01 ;makes the min increase
	da a
	mov min, a
	CJNE a, #0x60, returnnow
	MOV min ,#0x00 ;makes it go back to 00, whole hour has pass
	MOV a, hours
	add a, #0x01 ;makes the hour increase
	da a
	mov hours, a
	CJNE a, #0x12, hour12shift
	Mov a, day 
	JNZ SwitchToPM ;switch to pm 
 	mov a, #0x01
	
	
    
hour12shift:	CJNE a, #0x13, returnnow 
	MOV hours ,#0x01 ;whole day has pass, goes to 1'clock
SwitchToPM: mov a, #0x0 ;switch to pm
	ret
returnnow: ret
genrandomnumber03:
	mov a, second
	mov b,#0x04
	div AB
	mov a, b
	ret
FINDOPTION:
	cjne a, #0x00, notOPTION0
	ljmp option0
notOPTION0:
	cjne a, #0x01, notOPTION1
	ljmp option1
notOPTION1:
	cjne a, #0x02, notOPTION2
	   ljmp option2 
notOPTION2:
	    Send_Constant_String(#Initial_Message)
    Wait_Milli_Seconds(#50)
	Set_Cursor(1,1)
    Send_Constant_String(#ready)
    Wait_Milli_Seconds(#250)
    Wait_Milli_Seconds(#250)
    Set_Cursor(1,1)
    Send_Constant_String(#Initial_Message)
    Wait_Milli_Seconds(#50)
    mov a, #0x84
    lcall ?WriteCommand
	mov a, #0x7E ;draw arrow above said position
    lcall ?WriteData
 	mov a, #0x0 ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x1 ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x7F ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x1 ;draw arrow above said position
    lcall ?WriteData
    ret
option0:	Send_Constant_String(#Initial_Message)
    Wait_Milli_Seconds(#50)
	Set_Cursor(1,1)
    Send_Constant_String(#ready)
    Wait_Milli_Seconds(#250)
    Wait_Milli_Seconds(#250)
    Set_Cursor(1,1)
    Send_Constant_String(#Initial_Message)
    Wait_Milli_Seconds(#50)
    mov a, #0x84 ;down down up right left
    lcall ?WriteCommand
	mov a, #0x0 ;draw arrow above said position
    lcall ?WriteData
 	mov a, #0x0 ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x1 ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x7E ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x7F ;draw arrow above said position
    lcall ?WriteData
    ret
option1:Send_Constant_String(#Initial_Message)
    Wait_Milli_Seconds(#50)
	Set_Cursor(1,1)
    Send_Constant_String(#ready)
    Wait_Milli_Seconds(#250)
    Wait_Milli_Seconds(#250)
    Set_Cursor(1,1)
    Send_Constant_String(#Initial_Message)
    Wait_Milli_Seconds(#50)
    mov a, #0x84
    lcall ?WriteCommand
	mov a, #0x1 ;draw arrow above said position
    lcall ?WriteData
 	mov a, #0x1 ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x7E ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x0 ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x7F ;draw arrow above said position
    lcall ?WriteData
    ret
option2:Send_Constant_String(#Initial_Message)
    Wait_Milli_Seconds(#50)
	Set_Cursor(1,1)
    Send_Constant_String(#ready)
    Wait_Milli_Seconds(#250)
    Wait_Milli_Seconds(#250)
    Set_Cursor(1,1)
    Send_Constant_String(#Initial_Message)
    Wait_Milli_Seconds(#50)
    mov a, #0x84
    lcall ?WriteCommand
	mov a, #0x7F ;draw arrow above said position
    lcall ?WriteData
 	mov a, #0x0 ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x1 ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x7E ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x0 ;draw arrow above said position
    lcall ?WriteData
    ret
playmario0: ;down down up right left
;check r
lcall E2
    Wait_Milli_Seconds(#50)
lcall E2
    Wait_Milli_Seconds(#50)
lcall E2
mov a, #0x18
lcall ?WriteCommand
lcall C2
    Wait_Milli_Seconds(#50)
lcall E2
    Wait_Milli_Seconds(#50)
lcall G2
    Wait_Milli_Seconds(#50)
lcall G1
mov a, #0x18
lcall ?WriteCommand
lcall C2
    Wait_Milli_Seconds(#50)
lcall G1
    Wait_Milli_Seconds(#50)
lcall E1
    Wait_Milli_Seconds(#50)
lcall A1
mov a, #0x18
lcall ?WriteCommand
lcall B1
    Wait_Milli_Seconds(#50)
lcall Bflat1
    Wait_Milli_Seconds(#50)
lcall A1
    Wait_Milli_Seconds(#50)
lcall G1
mov a, #0x18
lcall ?WriteCommand

lcall E2
    Wait_Milli_Seconds(#50)
lcall E2
    Wait_Milli_Seconds(#50)
lcall E2
mov a, #0x18
lcall ?WriteCommand
lcall C2
    Wait_Milli_Seconds(#50)
lcall E2
    Wait_Milli_Seconds(#50)
lcall G2
    Wait_Milli_Seconds(#50)
lcall G1
mov a, #0x18
lcall ?WriteCommand
lcall C2
    Wait_Milli_Seconds(#50)
lcall G1
    Wait_Milli_Seconds(#50)
lcall E1
    Wait_Milli_Seconds(#50)
lcall A1
mov a, #0x18
lcall ?WriteCommand
lcall B1
    Wait_Milli_Seconds(#50)
lcall Bflat1
    Wait_Milli_Seconds(#50)
lcall A1
    Wait_Milli_Seconds(#50)
lcall G1
mov a, #0x18
lcall ?WriteCommand
ljmp playmarioa
END



genrandomnumber03:
	mov a, second
	mov b,#0x04
	div AB
	mov a, b
	ret
FINDOPTION:
	cjne a, #0x00, notOPTION0
	ljmp option0
notOPTION0:
	cjne a, #0x01, notOPTION1
	ljmp option1
notOPTION1:
	cjne a, #0x02, notOPTION2
	   ljmp option2 
notOPTION2:
	    Send_Constant_String(#Initial_Message)
    Wait_Milli_Seconds(#50)
	Set_Cursor(1,1)
    Send_Constant_String(#ready)
    Wait_Milli_Seconds(#250)
    Wait_Milli_Seconds(#250)
    Set_Cursor(1,1)
    Send_Constant_String(#Initial_Message)
    Wait_Milli_Seconds(#50)
    mov a, #0x84
    lcall ?WriteCommand
	mov a, #0x7E ;draw arrow above said position
    lcall ?WriteData
 	mov a, #0x0 ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x1 ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x7F ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x1 ;draw arrow above said position
    lcall ?WriteData
    ret
option0:	Send_Constant_String(#Initial_Message)
    Wait_Milli_Seconds(#50)
	Set_Cursor(1,1)
    Send_Constant_String(#ready)
    Wait_Milli_Seconds(#250)
    Wait_Milli_Seconds(#250)
    Set_Cursor(1,1)
    Send_Constant_String(#Initial_Message)
    Wait_Milli_Seconds(#50)
    mov a, #0x84 ;down down up right left
    lcall ?WriteCommand
	mov a, #0x0 ;draw arrow above said position
    lcall ?WriteData
 	mov a, #0x0 ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x1 ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x7E ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x7F ;draw arrow above said position
    lcall ?WriteData
    ret
option1:Send_Constant_String(#Initial_Message)
    Wait_Milli_Seconds(#50)
	Set_Cursor(1,1)
    Send_Constant_String(#ready)
    Wait_Milli_Seconds(#250)
    Wait_Milli_Seconds(#250)
    Set_Cursor(1,1)
    Send_Constant_String(#Initial_Message)
    Wait_Milli_Seconds(#50)
    mov a, #0x84
    lcall ?WriteCommand
	mov a, #0x1 ;draw arrow above said position
    lcall ?WriteData
 	mov a, #0x1 ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x7E ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x0 ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x7F ;draw arrow above said position
    lcall ?WriteData
    ret
    
    option2:Send_Constant_String(#Initial_Message)
    Wait_Milli_Seconds(#50)
	Set_Cursor(1,1)
    Send_Constant_String(#ready)
    Wait_Milli_Seconds(#250)
    Wait_Milli_Seconds(#250)
    Set_Cursor(1,1)
    Send_Constant_String(#Initial_Message)
    Wait_Milli_Seconds(#50)
    mov a, #0x84
    lcall ?WriteCommand
	mov a, #0x7F ;draw arrow above said position
    lcall ?WriteData
 	mov a, #0x0 ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x1 ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x7E ;draw arrow above said position
    lcall ?WriteData
	mov a, #0x0 ;draw arrow above said position
    lcall ?WriteData
    ret