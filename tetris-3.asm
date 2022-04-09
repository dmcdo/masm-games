include Irvine32.inc

.386
.model flat,stdcall
.stack 4096
ExitProcess proto, dwExitCode:dword

ROWS = 20
COLS = 10

.data
; Strings
next_text byte "Next:", 0
two_spaces byte "  ", 0
gameover_msg1 byte "     Game over.     ", 0
gameover_msg2 byte "Press enter to quit.", 0

; Each tetramino has four rotations. Each bit represents a tile
; Format: (3, 0), (3, 1), (3, 2), (3, 3), (2, 0), ..., (0, 0)
tetramino_i dword 0000000011110000b, 0100010001000100b, 0000000011110000b, 0100010001000100b
tetramino_j dword 0000000011100010b, 0000011001000100b, 0000100011100000b, 0000010001001100b
tetramino_l dword 0000000011101000b, 0000010001000110b, 0000001011100000b, 0000110001000100b
tetramino_o dword 0000000001100110b, 0000000001100110b, 0000000001100110b, 0000000001100110b
tetramino_s dword 0000000001101100b, 0000010001100010b, 0000000001101100b, 0000010001100010b
tetramino_t dword 0000000011100100b, 0000010011000100b, 0000010011100000b, 0000010001100100b
tetramino_z dword 0000000011000110b, 0000001001100100b, 0000000011000110b, 0000001001100100b

tetramino_refs  dword offset tetramino_i,        offset tetramino_j,
                      offset tetramino_l,        offset tetramino_o,
                      offset tetramino_s,        offset tetramino_t,
                      offset tetramino_z

tetramino_colors byte cyan + (cyan shl 4),        red + (red shl 4),
                      green + (green shl 4),      lightMagenta + (lightMagenta shl 4),
                      brown + (brown shl 4),      lightBlue + (lightBlue shl 4),
                      magenta + (magenta shl 4)

; 2d byte array. Is there [x][y] occupied? 0 for no, non-zero for yes
; If yes, it stores a color code
tetris_board byte (ROWS * COLS) dup(0)

; (Decreases over time)
tick_speed dword 700

; Information about the current tetramino (with default vals just in case)
current_tetramino_color byte green + (green shl 4)
current_tetramino_offset dword offset tetramino_i
current_tetramino_rotation dword 0
current_x sbyte (COLS / 2) - 2
current_y sbyte -2

; For previewing the next tetramino
next_tetramino_color byte 0
next_tetramino_offset dword NULL

; Skip the current tick?
tick_skip dword 0

; Is the game over?
gameover dword 0

.code
main proc
    ; Remember default text color & set black background
    call GetTextColor
    push eax
    mov eax, black + (black shl 4)
    call SetTextColor
    call ClrScr

    ; Start the game
    call Randomize
    call DrawBoard
    call NextTetramino
    call GameLoop

    ; Print gameover message
    mov eax, white + (blue shl 4)
    call SetTextColor
    mov dx, ((ROWS / 2) shl 8) + 2
    call GotoXY
    mov edx, offset gameover_msg1
    call WriteString
    mov dx, ((ROWS / 2 + 1) shl 8) + 2
    call GotoXY
    mov edx, offset gameover_msg2
    call WriteString

    ; Quit when the user presses enter
waiting_for_exit:
    call ReadChar
    cmp al, 13 ; 13 is ASCII for return
    jne waiting_for_exit

    ; Restore text color
    pop eax
    call SetTextColor
    call ClrScr
    mov dx, 0
    call GotoXY

    invoke ExitProcess, 0
main endp

; Continutally loops until the game is over
; Doesn't preserve any registers
GameLoop proc
begin:
    .IF gameover != 0
        jmp _ret
    .ENDIF

    call GetMseconds
    mov ecx, eax
tick_wait:
    .IF tick_skip != 0
        mov tick_skip, 0
        jmp begin
    .ENDIF

    mov eax, 1
    call Delay
    call AcceptInput

tick_calc:
    call GetMseconds
    sub eax, ecx
    cmp eax, tick_speed
    ja end_tick
    jmp tick_wait

end_tick:
    ; Move current tetramino down
    mov dh, current_y
    mov dl, current_x
    inc dh
    mov esi, current_tetramino_offset
    mov eax, current_tetramino_rotation
    mov ecx, dword ptr [eax * type dword + esi]
    call CheckTetramino
    .IF eax == 0
        mov eax, 0 ; Draw over tetramino with black
        dec dh
        call DrawTetramino
        mov al, current_tetramino_color
        inc dh
        call DrawTetramino
        mov current_y, dh
    .ELSE
        dec dh
        mov eax, 0
        mov al, current_tetramino_color
        call UpdateTetramino
        call AutoLineClear
        call NextTetramino
    .ENDIF

    jmp begin

_ret:
    ret
GameLoop endp

; Goes through the tertis board and clear
; any complete lines
AutoLineClear proc uses esi ebx ecx edx
    mov esi, offset tetris_board
    mov dx, 0

    mov ecx, ROWS
rowloop:
    push ecx
    mov bl, 1
    mov ecx, COLS
colloop:
    .IF byte ptr [esi] == 0
        mov bl, 0
    .ENDIF
    inc dl
    inc esi
    loop colloop
    inc dh
    mov dl, 0

    .IF bl != 0
        ; Move all the lines down one
        call _ALCMove
    .ENDIF

    pop ecx
    loop rowloop

    ret
AutoLineClear endp

; Helper procedure for AutoLineClear
_ALCMove proc uses eax esi edx
    dec esi
    dec dh
    mov dl, COLS - 1
begin:
    ; Update
    mov al, byte ptr [esi - COLS]
    mov byte ptr [esi], al

    ; Draw
    push dx
    inc dh
    inc dl
    shl dl, 1
    call GotoXY
    .IF al == 0
        mov eax, black + (black shl 4)
        call SetTextColor
    .ELSE
        and eax, 0FFh
        call SetTextColor
    .ENDIF
    mov edx, offset two_spaces
    call WriteString
    pop dx

    ; Continue
    dec esi
    .IF sbyte ptr dl <= 0
        dec dh
        mov dl, COLS - 1
    .ELSE
        dec dl
    .ENDIF

    ; Loop
    .IF esi >= (offset tetris_board + COLS)
        jmp begin
    .ENDIF

; Clear out the top row
    mov dh, 0
    mov dl, COLS - 1
    mov esi, offset tetris_board + COLS - 1
    mov eax, black + (black shl 4)
    call SetTextColor
clear_top:
    mov byte ptr [esi], 0
    push dx
    inc dh
    inc dl
    shl dl, 1
    call GotoXY
    mov edx, offset two_spaces
    call WriteString

    pop dx
    dec esi
    .IF sbyte ptr dl <= 0
        dec dh
        mov dl, COLS - 1
    .ELSE
        dec dl
    .ENDIF

    .IF esi >= (offset tetris_board)
        jmp clear_top
    .ENDIF

    ret
_ALCMove endp

; Picks a random tetramino, sets it as the current
; one and puts it at the top
NextTetramino proc uses eax ecx esi
    .IF tick_speed > 100
        sub tick_speed, 5
    .ENDIF

    mov current_tetramino_rotation, 0
    mov current_y, -2
    mov current_x, (COLS / 2) - 2

    ; Initialize current and next
    .IF next_tetramino_offset == NULL
        mov eax, lengthof tetramino_refs
        call RandomRange
        mov esi, dword ptr [eax * type dword + offset tetramino_refs]
        mov next_tetramino_offset, esi
        mov cl, byte ptr [eax + offset tetramino_colors]
        mov next_tetramino_color, cl

        mov eax, lengthof tetramino_refs
        call RandomRange
        mov esi, dword ptr [eax * type dword + offset tetramino_refs]
        mov current_tetramino_offset, esi
        mov cl, byte ptr [eax + offset tetramino_colors]
        mov current_tetramino_color, cl

    ; Standard case, move next to current, get new next
    .ELSE
        mov eax, next_tetramino_offset
        mov current_tetramino_offset, eax
        mov al, next_tetramino_color
        mov current_tetramino_color, al

        mov eax, lengthof tetramino_refs
        call RandomRange
        mov esi, dword ptr [eax * type dword + offset tetramino_refs]
        mov next_tetramino_offset, esi
        mov cl, byte ptr [eax + offset tetramino_colors]
        mov next_tetramino_color, cl
    .ENDIF
    
    ; Place new tetramino
    mov dh, -2
    mov dl, (COLS / 2) - 2
    mov esi, current_tetramino_offset
    mov ecx, dword ptr [esi]

    call CheckTetramino
    .IF eax == 0
        mov eax, 0
        mov al, current_tetramino_color
        call DrawTetramino
    .ELSE
        dec dh
        mov eax, 0
        mov al, current_tetramino_color
        call DrawTetramino
        mov gameover, 1
        mov tick_skip, 1
    .ENDIF

    ; Preview next
    mov dh, 0
    mov dl, COLS + 3
    mov eax, 0
    mov ecx, 0FFh
    call DrawTetramino
    mov al, next_tetramino_color
    mov esi, next_tetramino_offset
    mov ecx, dword ptr [esi]
    call DrawTetramino

    ret
NextTetramino endp

; Read in input from the user
AcceptInput proc uses eax ecx edx esi
    call _AcceptInput
    ret
AcceptInput endp

_AcceptInput proc
    local tetramino_store:dword, rotation_store:dword
    call ReadKey
    jz _ret ; Exit if no key pressed

    ; If user hit spacebar, place the current tetramino
    .IF al == ' '
        mov dh, current_y
        mov dl, current_x
        mov esi, current_tetramino_offset
        mov eax, current_tetramino_rotation
        mov ecx, dword ptr [eax * type dword + esi]
        mov eax, black + (black shl 4)
        call DrawTetramino

        ; Keep looping until the current tetramino is at the bottom
    downloop:
        inc dh
        call CheckTetramino
        .IF eax == 0
            jmp downloop
        .ENDIF

        ; Place the tetramino
        dec dh
        mov eax, 0
        mov al, current_tetramino_color
        call DrawTetramino
        call UpdateTetramino
        call AutoLineClear
        call NextTetramino
        mov tick_skip, 1

    ; If special character (ie arrow keys)
    .ELSEIF al == 0
        ; left arrow key (move left)
        .IF dx == 025h
            mov dh, current_y
            mov dl, current_x
            dec dl
            mov esi, current_tetramino_offset
            mov eax, current_tetramino_rotation
            mov ecx, dword ptr [eax * type dword + esi]
            call CheckTetramino
            .IF eax == 0
                inc dl
                mov eax, 0 ; black
                call DrawTetramino
                dec dl
                mov al, current_tetramino_color
                call DrawTetramino
                mov current_x, dl
            .ENDIF

        ; Up arrow key (rotate)
        .ELSEIF dx == 026h
            mov dh, current_y
            mov dl, current_x
            mov esi, current_tetramino_offset
            mov eax, current_tetramino_rotation
            .IF eax >= 3
                mov eax, 0
            .ELSE
                inc eax
            .ENDIF
            mov rotation_store, eax
            mov ecx, dword ptr [eax * type dword + esi]
            call CheckTetramino
            .IF eax == 0
                mov tetramino_store, ecx
                mov eax, current_tetramino_rotation
                mov ecx, dword ptr [eax * type dword + esi]
                mov eax, 0 ; black
                call DrawTetramino
                mov ecx, tetramino_store
                mov al, current_tetramino_color
                call DrawTetramino
                mov eax, rotation_store
                mov current_tetramino_rotation, eax
            .ENDIF

        ; Right arrow key (move right)
        .ELSEIF dx == 027h
            mov dh, current_y
            mov dl, current_x
            inc dl
            mov esi, current_tetramino_offset
            mov eax, current_tetramino_rotation
            mov ecx, dword ptr [eax * type dword + esi]
            call CheckTetramino
            .IF eax == 0
                dec dl
                mov eax, 0 ; black
                call DrawTetramino
                inc dl
                mov al, current_tetramino_color
                call DrawTetramino
                mov current_x, dl
            .ENDIF

        ; Down arrow key (go faster)
        .ELSEIF dx == 028h
            mov dh, current_y
            mov dl, current_x
            inc dh
            mov esi, current_tetramino_offset
            mov eax, current_tetramino_rotation
            mov ecx, dword ptr [eax * type dword + esi]
            call CheckTetramino
            .IF eax == 0
                mov eax, 0 ; Black
                dec dh
                call DrawTetramino
                mov al, current_tetramino_color
                inc dh
                call DrawTetramino
                mov current_y, dh
                mov tick_skip, 1
            .ENDIF

        .ENDIF
    .ENDIF

_ret:
    ret
_AcceptInput endp

; Returns tetris_board[dh][dl] in bl
GetBoardCoord proc uses eax edx
    mov eax, 0
    mov al, dh
    mov bh, COLS
    mul bh
    mov ebx, 0
    mov bl, dl
    mov bl, byte ptr [ebx + eax + offset tetris_board]
    ret
GetBoardCoord endp

; Sets tetris_board[dh][dl] to the value in bl
SetBoardCoord proc uses eax ebx ecx edx
    ; Check if valid
    .IF sbyte ptr dh < 0 || sbyte ptr dh >= ROWS || \
        sbyte ptr dl < 0 || sbyte ptr dl >= COLS
        ret
    .ENDIF

    mov eax, 0
    mov al, dh
    mov ch, COLS
    mul ch
    mov ecx, 0
    mov cl, dl
    mov byte ptr [ecx + eax + offset tetris_board], bl
    ret
SetBoardCoord endp

; Updates tetris_board
; Params: tetramino in ecx, (x, y) in (dl, dh), color in eax
UpdateTetramino proc uses ebx ecx edx
    shl ecx, 16
    mov bl, al

uploop:
    push dx
    shl ecx, 1
    jnc i
    call SetBoardCoord
i:  inc dl
    shl ecx, 1
    jnc j
    call SetBoardCoord
j:  inc dl
    shl ecx, 1
    jnc k
    call SetBoardCoord
k:  inc dl
    shl ecx, 1
    jnc l
    call SetBoardCoord
l:  pop dx
    inc dh

    cmp ecx, 0
    jne uploop
    ret
UpdateTetramino endp

; Checks if a tetramino can be placed at a given x, y
; Expects the x, y in dl and dh and the tetramino dword in ecx
; Returns 0 in eax if valid, 1 if invalid
CheckTetramino proc uses ebx ecx edx
    call _CheckTetramino
    ret
CheckTetramino endp

; Helper procedure for CheckTetramino
_CheckTetramino proc
    local count:dword, dword_store:dword

    shl ecx, 16

chloop:
    ; If this row is bounds (y), check if any pieces are out of bounds
    .IF sbyte ptr dh < 0 || sbyte ptr dh >= ROWS
        mov dword_store, ecx
        and dword_store, (01111000000000000b shl 16)
        jnz invalid
        shl ecx, 4

    ; This row in in bounds, check each individual bit
    .ELSE
        mov count, 4
    elseloop:
        ; If this is out of bounds
        .IF sbyte ptr dl >= COLS || sbyte ptr dl < 0
            shl ecx, 1
            jc invalid

         ; If it's in bounds, check if there are no conflicts
        .ELSE
            call GetBoardCoord
            shl ecx, 1
            jnc _endif
            cmp bl, 0
            jne invalid
        _endif:
        .ENDIF

        ; Do the loop
        inc dl
        dec count
        jnz elseloop
        sub dl, 4
    .ENDIF
    .IF ecx != 0
        inc dh
        jmp chloop
    .ENDIF

    mov eax, 0
    jmp cleanup

invalid:
    mov eax, 1
cleanup:
    ret
_CheckTetramino endp

; Draws a tetramino dword
; Expects a y coordinate in dh, an x in dl, and a tetramino dword in ecx,
; and the color code to be used by SetTextColor in eax
DrawTetramino proc uses eax ecx edx
    call SetTextColor
    shl ecx, 16
    add dh, 1
    inc dl
    shl dl, 1


; Continually left shift ecx
; Every time the carry flag is set, draw at the approptiate x, y
drawloop:
    cmp dh, 0
    jl skip_nybble ; Skip negative y's
    push edx
    call GotoXY
    shl ecx, 1
    jnc i
    push edx
    mov edx, offset two_spaces
    call WriteString
    pop edx
i:  add dl, 2
    call GotoXY
    shl ecx, 1
    jnc j
    push edx
    mov edx, offset two_spaces
    call WriteString
    pop edx
j:  add dl, 2
    call GotoXY
    shl ecx, 1
    jnc k
    push edx
    mov edx, offset two_spaces
    call WriteString
    pop edx
k:  add dl, 2
    call GotoXY
    shl ecx, 1
    jnc l
    mov edx, offset two_spaces
    call WriteString
l:  pop edx
m:  inc dh
    cmp ecx, 0
    jne drawloop

    ret

skip_nybble:
    shl ecx, 4
    jmp m
DrawTetramino endp

DrawBoard proc
    ; Draw "next" tetramino thingy label
    mov eax, white + (black shl 4)
    call SetTextColor
    mov dh, 0 + 1
    mov dl, (COLS + 4) * 2
    call GotoXY
    mov edx, offset next_text
    call WriteString
    mov dx, 0
    call GotoXY

    ; Set border color
    mov eax, white + (white shl 4)
    call SetTextColor

    ; Draw top border
    mov ecx, COLS
    mov edx, offset two_spaces
    call WriteString
drawloop_top:
    call WriteString
    loop drawloop_top
    call WriteString

    ; Draw side borders
    mov ecx, ROWS
    mov dh, 1 ; col
drawloop_mid:
    mov dl, 0
    call GotoXY
    push edx
    mov edx, offset two_spaces
    call WriteString
    pop edx
    mov dl, 2 + (2 * COLS)
    call GotoXY
    push edx
    mov edx, offset two_spaces
    call WriteString
    pop edx
    inc dh
    loop drawloop_mid

; Draw bottom border
    mov dx, (ROWS + 1) shl 8
    call GotoXY
    mov ecx, COLS
    mov edx, offset two_spaces
    call WriteString
drawloop_bottom:
    call WriteString
    loop drawloop_bottom
    call WriteString

    ret
DrawBoard endp

end main