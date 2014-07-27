
my_index equ [0]
my_mode  equ [1]
my_x	 equ [2]
my_y 	 equ [3]
my_dir 	 equ [4]
my_vita	 equ [5]

initialized equ [6]
seed equ [7]

target_x equ [8]
target_y equ [9]

dist  equ [10]



back_dir equ [20]


dir_up	equ 0
dir_rt	equ 1
dir_dn	equ 2
dir_lt	equ 3


        int 3
        my_index = a
        if a >= 4 { a -= 4 }
        my_mode = a

        int 5
        my_x = a
        my_y = b

        a = my_index
        int 6
        my_vita = a
        my_dir = b

        ; back_dir = atpakaļgaitas virziens, tajā nekad nevarēs iet (tupiki neskaitās, paši nostrādās)

        back_dir = dir_up
        if my_dir = dir_up { back_dir = dir_dn }
        if my_dir = dir_rt { back_dir = dir_lt }
        if my_dir = dir_lt { back_dir = dir_rt }

        int 1

        target_x = a
        target_y = b
        if my_vita = 1 {
            ; kaut kādu citu koord, ar cerību, ka tīs nah
            target_x = b
            target_y = a
        }

        

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
                        jmp int_0_ret
                }
        }

        e = dir_rt
        call map_at_direction
        if a != 0 {
                if b != 0 {
                        a = dir_rt
                        jmp int_0_ret
                }
        }


        e = dir_dn
        call map_at_direction
        if a != 0 {
                if b != 0 {
                        a = dir_dn
                        jmp int_0_ret
                }
        }

        e = dir_lt
        call map_at_direction
        if a != 0 {
                if b != 0 {
                        a = dir_lt
                        int_0_ret:
                        int 0
                        ; ret
                }
        }

        ret


advance_target:
        ; in: c = dir
        if c = dir_lt {
           target_x -= 1 
           if target_x >= 127 { target_x = 0 }
        }
        if c = dir_rt { target_x += 1 }
        if c = dir_up {
           target_y -= 1 
           if target_y >= 127 { target_y = 0 }
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

        dist = 0
        if c >= target_x {
            g = c - target_x
            dist += g
        }
        if c <= target_x {
            g = target_x - c
            dist += g
        }
        if d >= target_y {
            g = d - target_y
            dist += g
        }
        if d <= target_y {
            g = target_y - d
            dist += g
        }

        f = dist

        a = c
        b = d

        if e = dir_lt { a -= 1 }
        if e = dir_rt { a += 1 }
        if e = dir_up { b -= 1 }
        if e = dir_dn { b += 1 }

        if a >= target_x {
            g = a - target_x
            dist -= g
        }
        if a <= target_x {
            g = target_x - a
            dist -= g
        }
        if b >= target_y {
            g = b - target_y
            dist -= g
        }
        if b <= target_y {
            g = target_y - b
            dist -= g
        }

        int 7

        ; ieliks b = 0 /1
        b = 0
        if f >= 9 {
            ; ja esam super-tuvu, tad bez muļķībām
            call slight_chance_of_moving
        }
        if dist <= 127 {
                b = dist
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
    if seed <= 31 {
        b = 1
    }

    ret


