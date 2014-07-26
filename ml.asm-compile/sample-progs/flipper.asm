; Go up if our x-ordinate is even, or down if it is odd.

    GET_INDEX = 3
    GET_X     = 5

    int GET_INDEX          ; Get our ghost index in A.
    int GET_X          ; Get our x-ordinate in A.
    and a, 1        ; Zero all but least significant bit of A.
                    ; Now A is 0 if x-ordinate is even, or 1 if it is odd.
    mov b, a        ; Save A in B because we need to use A to set direction.
    mov a, 2        ; Move down by default.
    jeq skip, b, 1   ; Don't change anything if x-ordinate is odd.
    mov a,0        ; We only get here if x-ordinate was even, so move up.

skip:
    int 0          ; This is line 7, the target of the above jump. Now actually set the direction.
    hlt            ; Stop.

