; load ghost x,y @ 0,1
counter equ [0]

counter = 0

call inc_counter
call inc_counter
call inc_counter

; būtu jābūt 9
a = counter
int 8
hlt

; inkrīzo par 3
inc_counter:
    counter += 1
    call inc_counter_by_2
    ret

inc_counter_by_2:
    counter += 2
    ret

