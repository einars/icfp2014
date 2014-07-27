; load ghost x,y @ 0,1
counter equ [0]

counter = 0

while counter < 5 {
    call inc_counter
}

a = counter
int 8
hlt

inc_counter:
    counter += 1
    ret
