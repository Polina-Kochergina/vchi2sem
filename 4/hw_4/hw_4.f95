program hw_4
use func
use odu
implicit none
!character(2) file_num
integer i
real a, b

allocate(x(0:n),y1(0:n),y2(0:n),y3(0:n))
a = 0
b = 1

read(*,*) n

h = (b-a)/n
x(0) = a


  do i = 1, n
  	x(i) = x(i-1) + h
  enddo
print *, x

y1(0) = (b-a)/2; y2(0) = y1(0); y3(0) = y1(0)

print *, x(0), x(n), y1(0), y2(0), y3(0)

!do i = 1, file_num
  !write(file_num, '(i2).dat') i
  !open(1, file = file_num)
  !	f = f0(i)
!	call euler(f)
!	call rungeKutta(f)
!	call Adams(f)
print *, "Точки x:", x(0:n)	
call euler(f1)
call rungeKutta(f1)
call adams(f1)

!print *, "Eyler y(x):", y1(0:n)
!print *, "Runge-Kutta y(x):", y2(0:n)
!print *, "Adams y(x):", y3(0:n)
!print *, x(0), x(n), a, b, y1(0), y2(0), y3(0)



end program
