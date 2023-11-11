program main
implicit none

real, allocatable, dimension(:) :: alpha, beta, f0, x, y
integer i, n
real, parameter :: pi = acos(-1.0)
real h, p, q, a, b, c, y0, yn


n = 100
h = pi/n
allocate(alpha(n), beta(n), f0(0:n), x(0:n), y(0:n))

p = -3.
q = 2.

a = 2.0 - p*h
b = -2.0*(2.0-q*h**2)
c = 2.0+p*h

x(0) = 0.0
x(n) = pi

do i = 1, n-1
	x(i) = x(i-1) + h
enddo

do i=0,n
	f0(i) = 2.0*f(x(i))*h**2
enddo

y0 = 0
yn = exp(pi) - 8*exp(2*pi)/5 - 3./5.



f0(n)=f0(n)-a*yn

alpha(1) = -c/b
beta(1) = f0(0)/b

do i = 2, n
	alpha(i) = -c/(a*alpha(i-1) + b)
	beta(i) = (f0(i-1)- a*beta(i-1))/(a*alpha(i-1) + b)
	
enddo

y(n) = (f0(n) - a*beta(n))/(a*alpha(n) + b)


do i = n-1, 1, -1
	y(i) = alpha(i+1)*y(i+1) + beta(i+1)

enddo




open(1, file = 'output')
write(1, 100) ' X ', 'Y '
do i=0,n
	write(1,*) x(i), y(i)
enddo
100 format(7x, a, 14x, a)
close(1)

contains

function f(x)
implicit none
real f, x
	f = -2*sin(x)
end function


end program
