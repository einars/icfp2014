; load ghost x,y @ 0,1
ghost_x equ 0
ghost_y equ 1
player_x equ 2
player_y equ 3
vitality equ 4
direction equ 5

a = 1

if a = direction {
    b = a
    a = direction
}
if a > b {
    b += 1
    c += 1
    d += 1
}
