
my_index equ [0]
my_mode  equ [1]
my_x	 equ [2]
my_y 	 equ [3]
my_dir 	 equ [4]

initialized equ [5]
seed equ [6]

target_x equ [7]
target_y equ [8]

abs_w    equ [9]
curr_dist  equ [10]



back_dir equ [20]


dir_up	equ 0
dir_rt	equ 1
dir_dn	equ 2
dir_lt	equ 3


        int 3
        my_index = a
        if a > 3 { a -= 4 }
        my_mode = a

        int 5
        my_x = a
        my_y = b

        a = my_index
        int 6
        my_dir = b

        ; back_dir = atpakaļgaitas virziens, tajā nekad nevarēs iet (tupiki neskaitās, paši nostrādās)

        back_dir = dir_up
        if my_dir = dir_up { back_dir = dir_dn }
        if my_dir = dir_rt { back_dir = dir_lt }
        if my_dir = dir_lt { back_dir = dir_rt }

        int 1

        target_x = a
        target_y = b

        c = my_index
        if c != 0 {
            ; 0 - iet tieši uz mērķi, ne dir_up
            call advance_target
        }

        call find_good_move

        hlt


find_good_move:
        c = my_x
        d = my_y
        e = dir_up

        call map_at_direction

        if a != 0 {
                if b != 0 {
                        a = dir_up
                        int 0
                        ret
                }
        }

        e = dir_rt
        call map_at_direction
        if a != 0 {
                if b != 0 {
                        a = dir_rt
                        int 0
                        ret
                }
        }


        e = dir_dn
        call map_at_direction
        if a != 0 {
                if b != 0 {
                        a = dir_dn
                        int 0
                        ret
                }
        }

        e = dir_lt
        call map_at_direction
        if a != 0 {
                if b != 0 {
                        a = dir_lt
                        int 0
                        ret
                }
        }

        ret


advance_target:
        ; in: c = dir
        if c = dir_lt {
           target_x -= 1 
           if target_x > 127 { target_x = 0 }
        }
        if c = dir_rt { target_x += 1 }
        if c = dir_up {
           target_y -= 1 
           if target_y > 127 { target_y = 0 }
        }
        if c = dir_dn { target_y += 1 }
        ret

map_at_direction:

        if e = back_dir {
            a = 0 ; wall at the back
            ret
        }

        ; in: c = x
        ; in: d = y
        ; in: e = dir
        ; out: a = map_square
        ; out: b = 1/0 gets closer to target?

        abs_w = c - target_x
        call abs
        curr_dist = abs_w
        abs_w = d - target_y
        call abs
        curr_dist += abs_w
        f = curr_dist

        a = c
        b = d

        if e = dir_lt { a -= 1 }
        if e = dir_rt { a += 1 }
        if e = dir_up { b -= 1 }
        if e = dir_dn { b += 1 }


        abs_w = a - target_x
        call abs
        curr_dist -= abs_w

        abs_w = b - target_y
        call abs
        curr_dist -= abs_w

        int 7

        ; ieliks b = 0 /1
        call slight_chance_of_moving
        if curr_dist < 127 {
                b = curr_dist
        }

        ret

abs:
; noabso to, kas ir abs_w variablē
        if abs_w > 127 {
                xor abs_w, 255
                and abs_w, 127
                add abs_w, 1
        }
        ret

slight_chance_of_moving:
    ; maza iespēja, ka b = 1
    ; liela iespēja, ka b = 0

    if initialized = 0 {
        initialized = 1
        seed = my_index
    }
    b = seed * 128
    or seed b

    b = seed / 32
    xor seed b

    b = seed * 8
    xor seed b

    b = 0
    if seed < 32 {
        b = 1
    }

    ret


