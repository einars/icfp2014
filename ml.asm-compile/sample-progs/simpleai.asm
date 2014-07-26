; load ghost x,y @ 0,1
ghost_x = 0
ghost_y = 1
player_x = 2
player_y = 3
vitality = 4
direction = 5

int 3 
int 5 
mov [ghost_x], a 
mov [ghost_y], b 

; load player x,y @ 2,3

int 1
mov [player_x], a
mov [player_y], b

; load vitality, direction @ 4,5

int 3 
int 6
mov [vitality], a
mov [direction], b

; ghost x < player x @ 6

mov [6], 2
jlt label1, [ghost_x], [2]
mov [6], 4

label1:

; ghost y < player y @ 7 
mov [7], 3
jlt label2, [ghost_y], [player_y]
mov [7], 1

label2:

; ghost x <> player x @ 8
mov [8], 0
jeq label3, [ghost_x], [player_x] 
mov [8], 1

label3:

; if (ghost x <> player x) -> move left or right
jeq move_left_or_right, [8], 1

; else move up or down
mov a, [7]
int 0
jeq direction_selected, 1, 1 

; move left/right
move_left_or_right:
mov a, [6]
int 0

direction_selected:

hlt
