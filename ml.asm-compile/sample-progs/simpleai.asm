; load ghost x,y @ 0,1
int 3 
int 5 
mov [0], a 
mov [1], b 

; load player x,y @ 2,3

int 1
mov [2], a
mov [3], b

; load vitality, direction @ 4,5

int 3 
int 6
mov [2], a
mov [3], b

; ghost x < player x @ 6

mov [6], 2
jlt label1, [0], [2]
mov [6], 4

label1:

; ghost y < player y @ 7 
mov [7], 3
jlt label2, [1], [3]
mov [7], 1

label2:

; ghost x <> player x @ 8
mov [8], 0
jeq label3, [0], [2] 
mov [8], 1

label3:

; if (ghost x <> player x) -> move left or right
jeq move_left_or_right, [8], 1

; else move up or down
move a, [7]
int 0
jeq direction_selected, 1, 1 

; move left/right
move_left_or_right:
mov a, [6]
int 0

direction_selected:

hlt
