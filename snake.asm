include Irvine32.inc

.386
.model flat,stdcall
.stack 4096
ExitProcess proto,  dwExitCode:dword

; Constants
; ROWS and COLS must both be even integers
ROWS = 30
COLS = 60

; Game speed
TICK_MS = 70

DIRECTION_QUEUE_CAPACITY = 64
DIRECTION_QUEUE_END = sizeof direction_queue - type direction_queue
TILE_QUEUE_END = sizeof tile_queue - type tile_queue

; Console Directions
D_NORTH = 'w'
D_EAST  = 'd'
D_SOUTH = 's'
D_WEST  = 'a'

; Position indicators (what is in this space?)
P_EMPTY  = 0
P_PELLET = 1
P_SNAKE  = 2
P_BORDER = 3

; Macros
; GET_XY_OF_AX Takes a surrogate byte, which is either a register, variable, or pointer
; Returns (x, y) on the console of the position in the ax register in ah and al respectively
GET_XY_OF_AX macro surrogate
    mov surrogate, COLS
    div surrogate
endm

; All data should be assumed to be unsigned
; unless otherwise specified

.data?
direction_queue byte DIRECTION_QUEUE_CAPACITY dup(?)
previous_direction byte ?

.data
; Static Strings
two_spaces        byte "  ", 0
you_died_here     byte "<----- You died here.       ", 0
press_key_to_exit byte "Press the enter key to exit.", 0
you_are_here      byte "<---- You are here.     ", 0
instructions_1    byte "  Press w, a, s, d, or  ", 0
instructions_2    byte " an arrow key to begin. ", 0
instructions_clr  byte "                        ", 0

; 1D array where each byte represents a tile on the console.
console_tiles byte COLS dup(P_BORDER), ; Top row, all border
                   2 dup(P_BORDER), 2 dup(P_SNAKE), (COLS - 6) dup(P_EMPTY), 2 dup(P_BORDER), ; First row, snake in corner
                   (ROWS - 3) dup(2 dup(P_BORDER), (COLS - 4) dup(P_EMPTY), 2 dup(P_BORDER)), ; Rows 2..(COLS - 2), empty
                   COLS dup(P_BORDER) ; Final row, all border

; Stuff for managing the direction the snake will travel
direction_queue_front dword 0
direction_queue_rear dword 0
direction_queue_size dword 1

; Amount of units the snake has left to grow
snake_growth dword 0

; Current position of the snake, index
; Although snake_current_tile is a dword, the value inside it should only any
; value large than an unsigned word should be treated as a special value
snake_current_tile dword (COLS + 2)

; Queue of console_tiles to be removed
tile_queue dword (COLS + 2), (ROWS * COLS - 2) dup(?)
tile_queue_front dword 0
tile_queue_rear dword 0

; Is the game over yet?
gameover dword 0

.code
main proc
    ; Remember default text color
    mov eax, 0
    call GetTextColor
    push eax ; Pop at end of program

    ; Init
    mov eax, lightgray + (black shl 4)
    call SetTextColor
    call Clrscr
    call DrawBoard
    call Randomize

    ; Draw instructions
    mov eax, black + (white shl 4)
    call SetTextColor
    mov edx, 0104h
    call GotoXY
    mov edx, offset you_are_here
    call WriteString
    mov edx, 0204h
    call GotoXY
    mov edx, offset instructions_1
    call WriteString
    mov edx, 0304h
    call GotoXY
    mov edx, offset instructions_2
    call WriteString

; Wait to start the game
wait_for_first_keypress:
    mov eax, 1
    call Delay
    ; Read input while we're waiting
    call ReadKey
    jz wait_for_first_keypress
    call ConvertToDir
    jz wait_for_first_keypress
    mov byte ptr [offset direction_queue], al

    ; Clear instructions
    mov eax, lightgray + (black shl 4)
    call SetTextColor
    mov edx, 0104h
    call GotoXY
    mov edx, offset instructions_clr
    call WriteString
    mov edx, 0204h
    call GotoXY
    mov edx, offset instructions_clr
    call WriteString
    mov edx, 0304h
    call GotoXY
    mov edx, offset instructions_clr
    call WriteString

    ; Start the game
    call Pellet
    call Gameloop

    ; Game Over
    ; Retrieve the current tile the snake is on
    mov eax, snake_current_tile
    GET_XY_OF_AX dl
    mov dh, al
    mov dl, ah
    push dx

    ; Highlight the death position
    call GotoXY
    mov eax, green + (green shl 4)
    call SetTextColor
    mov edx, offset two_spaces
    call WriteString
    
    ; Print game over messages
    mov eax, black + (white shl 4)
    call SetTextColor
    mov edx, offset you_died_here
    call WriteString
    pop dx
    add dl, 2
    inc dh
    call GotoXY
    mov edx, offset press_key_to_exit
    call WriteString

    ; Wait for the user to press enter, then exit
waiting_for_exit:
    call ReadChar
    cmp al, 13 ; 13 is ASCII for return
    jne waiting_for_exit

    ; Return the command prompt back to normal
    pop eax
    call SetTextColor
    call ClrScr
    mov dx, 0
    call GotoXY
    invoke ExitProcess, 0
main endp

Gameloop proc uses eax ebx ecx edx
begin:
    call GetMseconds
    mov ecx, eax
tick_wait:
    mov eax, 1
    call Delay
    call ReadInDir

tick_calc:
    call GetMseconds
    sub eax, ecx
    cmp eax, TICK_MS
    ja end_tick
    jmp tick_wait

end_tick:
    call MoveSnake
    cmp gameover, 0
    je begin

    ret
Gameloop endp

MoveSnake proc uses eax ebx ecx edx
    cmp direction_queue_size, 0
    je from_prev ; Get dir from previous_direction in queue if empty
    dec direction_queue_size ; Otherwise, prepare to dequeue
    mov ebx, direction_queue_front
    mov al, byte ptr [ebx + offset direction_queue]
    mov previous_direction, al
    cmp direction_queue_front, DIRECTION_QUEUE_END
    je dir_wraparound
    add direction_queue_front, type direction_queue
    jmp calc_next
dir_wraparound:
    mov direction_queue_front, 0
    jmp calc_next
from_prev:
    mov al, previous_direction

; Calculate space of the snake's next move
calc_next:
    cmp al, D_NORTH
    je north
    cmp al, D_EAST
    je east
    cmp al, D_SOUTH
    je south
    jmp west ; only other possible direction

north:
    sub snake_current_tile, COLS
    jmp do_move
east:
    add snake_current_tile, 2
    jmp do_move
south:
    add snake_current_tile, COLS
    jmp do_move
west:
    sub snake_current_tile, 2

do_move:
    ; if next position is snake or border, game over
    ; if next position is pellet, move, then eat the pellet
    ; if next position is empty, just move
    mov eax, snake_current_tile
    mov cl, byte ptr [eax + offset console_tiles]
    cmp cl, P_SNAKE
    jb enq
    mov gameover, 1

enq:
    call ReadInDir
    ; Add new position to the rear of tile_queue
    cmp tile_queue_rear, TILE_QUEUE_END
    je enq_wraparound
    add tile_queue_rear, type tile_queue
    mov eax, tile_queue_rear
    mov ebx, snake_current_tile
    mov dword ptr [eax + offset tile_queue], ebx
    jmp enq_update
enq_wraparound:
    mov tile_queue_rear, 0
    mov ebx, snake_current_tile
    mov dword ptr [offset tile_queue], ebx
enq_update:
    mov eax, snake_current_tile
    mov word ptr [eax + offset console_tiles], (P_SNAKE shl 8) + P_SNAKE
    mov eax, lightgreen + (lightgreen shl 4)
    call SetTextColor
    mov eax, snake_current_tile
    GET_XY_OF_AX dl
    mov dh, al
    mov dl, ah
    call GotoXY
    mov edx, offset two_spaces
    call WriteString
    call ReadInDir

; Determine if the snake needs to grow or not
    cmp snake_growth, 0
    jbe deq
    dec snake_growth
    jmp determine_pellet

deq:
    ; Retrieve front of the tile_queue
    mov eax, tile_queue_front
    mov ebx, dword ptr [eax + offset tile_queue]
    ; Dequeue
    cmp tile_queue_front, TILE_QUEUE_END
    je deq_wraparound
    add tile_queue_front, type tile_queue
    jmp deq_update
deq_wraparound:
    mov tile_queue_front, 0
deq_update:
    mov word ptr [ebx + offset console_tiles], (P_EMPTY shl 8) + P_EMPTY
    mov eax, black + (black shl 4)
    call SetTextColor
    mov eax, ebx
    GET_XY_OF_AX dl
    mov dh, al
    mov dl, ah
    call GotoXY
    mov edx, offset two_spaces
    call WriteString
    call ReadInDir

determine_pellet:
    cmp cl, P_PELLET
    je eat_pellet
    ret

eat_pellet:
    call Pellet
    call ReadKey
    add snake_growth, 4
    ret
MoveSnake endp

Pellet proc uses eax edx
; Choose a random position for the pellet to go
chooserand:
    mov eax, (ROWS * COLS) - (2 * COLS)
    call RandomRange
    add eax, COLS
    test eax, 1
    jnz eax_odd
    jmp eax_even
eax_odd:
    dec eax
eax_even:
    ; Choose a different space if that one is already taken
    mov dl, byte ptr [eax + offset console_tiles]
    cmp dl, P_EMPTY
    jne chooserand
    
    ; Update everything
    mov word ptr [eax + offset console_tiles], (P_PELLET shl 8) + P_PELLET
    GET_XY_OF_AX dh
    mov dh, al
    mov dl, ah
    call GotoXY
    mov eax, lightred + (lightred shl 4)
    call SetTextColor
    mov edx, offset two_spaces
    call WriteString
    ret
Pellet endp

; Perfoms a no-wait read on the keyboard and, if a
; directional key was pressed, adds it to the dirquq
; You will find this function spacked thoughout the
; program after particularly time-consuming tasks in
; order to read in as much input as possible
ReadInDir proc
    pushad ; Yes, this is very overkill

    ; Read input while we're waiting
    call ReadKey
    jz _ret
    call ConvertToDir
    jz _ret

    ; Enqueue direction
    cmp direction_queue_size, 0
    je empty
    cmp direction_queue_size, DIRECTION_QUEUE_CAPACITY
    jae _ret ; If the queue is full skip adding onto it
    mov ebx, direction_queue_rear
    mov ah, byte ptr [ebx + offset direction_queue]
    call DirOppositeOrSame
    jz _ret
    cmp direction_queue_rear, DIRECTION_QUEUE_END
    je wraparound
    add direction_queue_rear, type direction_queue
    mov ebx, direction_queue_rear
    mov byte ptr [ebx + offset direction_queue], al
    inc direction_queue_size
    jmp _ret
wraparound:
    mov direction_queue_rear, 0
    mov byte ptr [offset direction_queue], al
    inc direction_queue_size
    jmp _ret
empty:
    mov ah, previous_direction
    call DirOppositeOrSame
    jz _ret
    inc direction_queue_size
    mov direction_queue_front, 0
    mov direction_queue_rear, 0
    mov byte ptr [offset direction_queue], al

_ret:
    popad
    ret
ReadInDir endp

; Reads the output of ReadKey and converts to a direction
; which it return in al, unless it is invalid, in which case
; it sets the zero flag
ConvertToDir proc
    cmp al, 0
    je special_key
    cmp al, 'w'
    je unset
    cmp al, 'd'
    je unset
    cmp al, 's'
    je unset
    cmp al, 'a'
    je unset
    xor al, al
    ret
special_key:
    cmp dx, 025h ; left arrow key
    je west
    cmp dx, 026h ; up
    je north
    cmp dx, 027h ; right
    je east
    cmp dx, 028h ; down
    je south
    xor al, al
    ret

north:
    mov al, D_NORTH
    jmp unset
east:
    mov al, D_EAST
    jmp unset
south:
    mov al, D_SOUTH
    jmp unset
west:
    mov al, D_WEST

unset:
    test al, al
    ret
ConvertToDir endp

; Expects two directions in ah and al respectivaly
; Sets the zero flag if they are opposite, doesn't otherwise
DirOppositeOrSame proc uses ax
    cmp al, D_EAST
    je east
    cmp al, D_WEST
    je west
    cmp al, D_SOUTH
    je south

north:
    cmp ah, D_SOUTH
    je set
    xor al, ah
    ret
east:
    cmp ah, D_WEST
    je set
    xor al, ah
    ret
west:
    cmp ah, D_EAST
    je set
    xor al, ah
    ret
south:
    cmp ah, D_NORTH
    je set
    xor al, ah
    ret

set:
    xor ax, ax
    ret
DirOppositeOrSame endp

DrawBoard proc uses eax ecx edx
    ; Set border color
    mov eax, blue + (blue shl 4)
    call SetTextColor

    ; Draw top border
    mov al, ' '
    mov ecx, COLS
drawloop_top:
    call WriteChar
    loop drawloop_top

    ; Draw side borders
    mov ecx, (ROWS - 2)
    mov dh, 1 ; store current row in ah
drawloop_mid:
    mov dl, 0
    call GotoXY
    call WriteChar ; left border
    call WriteChar
    mov dl, (COLS - 2)
    call GotoXY
    call WriteChar ; right border
    call WriteChar
    inc dh
    loop drawloop_mid

    ; Draw bottom border
    mov dh, (ROWS - 1)
    mov dl, 0
    call GotoXY
    mov ecx, COLS
drawloop_bottom:
    call WriteChar
    loop drawloop_bottom

    ; Draw Snake
    mov eax, lightgreen + (lightgreen shl 4)
    call SetTextColor

    mov edx, 0102h
    call GotoXY
    mov edx, offset two_spaces
    call WriteString

    ret
DrawBoard endp

end main