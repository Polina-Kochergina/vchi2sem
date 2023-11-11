program shooting
use mymod
implicit none

real, allocatable, dimension(:) :: x, y, s, l, dy
integer i, n
real h, y0, yn, t, eps

eps = 0.001
n = 100
h = pi/n
allocate(x(0:n), y(0:n), s(0:n), l(0:n), dy(0:n))


x(0) = 0.0
x(n) = pi

do i = 1, n-1
	x(i) = x(i-1) + h
enddo

y(0) = 0
y(n) = exp(pi) - 8.0*exp(2*pi)/5.0 - 3.0/5.0
dy(0) = 1.
dy(1) = 2.


t = y(n)
l(0) = dy(0)
l(1) = dy(1)


do i = 0,100
	dy(0)=l(i)
	call ODU(f, x, y, dy, n)
	
	s(i)=y(n)

	if (abs(t - s(i)) > eps) then
		continue
	else
		print*, i
		exit
	endif
	
	if (i>=1) then
		l(i+1)=l(i)-(s(i)-t)*(l(i)-l(i-1))/(s(i)-s(i-1))
	endif
enddo



open(1, file = 'output')
write(1, 100) ' X ', 'Y '
do i = 0, n
	write(1,*) x(i), y(i), dy(i)
enddo
100 format(7x, a, 14x, a)
close(1)

contains

function f(x, y, dy)
implicit none
real f, x, y, dy
	f = 3.0*dy - 2.0*y + 2.0*sin(x)
end function


end program
