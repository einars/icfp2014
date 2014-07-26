; load ghost x,y @ 0,1
ghost_x equ 0
ghost_y equ 1
player_x equ 2
player_y equ 3
vitality equ 4
direction equ 5


if a = b {
    b = a
}
if a > b {
    b += 1
    c += 1
    d += 1
}

int 3 
int 5 
[ghost_x] = a 
[ghost_y] = b 

; load player x,y @ 2,3

int 1
[player_x] = a
[player_y] = b

; load vitality, direction @ 4,5

int 3 
int 6
[vitality] = a
[direction] = b

; ghost x < player x @ 6

[6] = 2
jlt label1, [ghost_x], [2]
[6] = 4

label1:

; ghost y < player y @ 7 
[7] = 3
jlt label2, [ghost_y], [player_y]
[7] = 1

label2:

; ghost x <> player x @ 8
[8] = 0
jeq label3, [ghost_x], [player_x] 
[8] = 1

label3:

; if (ghost x <> player x) -> move left or right
jeq move_left_or_right, [8], 1

; else move up or down
a = [7]
int 0
jeq direction_selected, 1, 1 

; move left/right
move_left_or_right:
a = [6]
int 0

direction_selected:

hlt
