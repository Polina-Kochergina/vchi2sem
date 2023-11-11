program mnk
use mymod
implicit none
real(8) sumx, sumy, sumxy, sumx2, sumy2, lny, xlny, lnx, lnx2, ylnx, x4, x3, x2y, sum1x, sum1x2, sumy_x, col
integer n, i, j, idx
real(8), allocatable, dimension(:) :: X, Y, abc5, abc1, abc2, abc3, abc4, b1, b2, b3, b4, b5
real(8), dimension(5) :: e2
real(8) A1(2,2), A2(2,2), A3(2,2), A4(2,2), A5(3,3)
character(len=10), dimension(5) :: name_f

name_f =[character(len=10) :: "linear", "hiperbolic", "logarithmic", "exponentional", "quadratic"]

open(1, file = 'input')

n = 10
col = n


allocate (X(n))
	read(1,*) X

allocate (Y(n))
	read(1,*) Y 
close(1)


! линейная ф.

sumx = sum(X)
sumy = sum(Y)

sumx2 = sum(x**2)
sumy2 = sum(y**2)
sumxy = sum(x*y)



allocate (b1(2), b2(2), b3(2), b4(2), b5(3), abc1(2), abc5(3), abc2(2), abc3(2), abc4(2))
a1(1,:) = (/sumx2, sumx/)
a1(2,:) = (/sumx, col/)
b1 = (/sumxy, sumy/)

abc1 = abc(A1, b1, 2)



do i = 1, n
	e2(1) = e2(1) + (y(i) - abc1(1)*x(i) - abc1(2))**2
enddo

!гмперболическая ф.

sum1x = sum(1./x)
sum1x2 = sum(1./x**2)
sumy_x = sum(y/x)


a2(1,:) = (/sum1x2, sum1x/)
a2(2,:) = (/sum1x, col/)
b2 = (/sumy_x, sumy/)

abc2 = abc(A2, b2, 2)

do i = 1, n
	e2(2) = e2(2) + (y(i) - abc2(1)/x(i) - abc2(2))**2
enddo



!логарифм
lnx = sum(log(x))
lnx2 = sum(log(x**2))
ylnx = sum(y*log(x))

a3(1,:) = (/lnx2, lnx/)
a3(2,:) = (/lnx, col/)
b3 = (/ylnx, sumy/)

abc3 = abc(A3, b3, 2)
abc3(1) = log(abc3(1))
e2(3) = 0
do i = 1, n
	e2(3) = e2(3) + (y(i) - abc3(1)*log(x(i)) - abc3(2))**2
enddo



!exp

lny = sum(log(y))
xlny = sum(x*log(y))

a4(1,:) = (/sumx2, sumx/)
a4(2,:) = (/sumx, col/)
b4 = (/xlny, lny/)	

abc4 = abc(A4, b4, 2)

e2(4) = 0
do i = 1, n
	e2(4) = e2(4) + (y(i) - exp(abc4(2))*exp(abc4(1)*x(i)))**2
enddo


!квадратичная ф.

x4 = sum(x**4)
x3 = sum(x**3)
x2y = sum((x**2)*y)


a5(1,:) = (/x4, x3, sumx2/)
a5(2,:) = (/x3, sumx2, sumx/)
a5(3,:) = (/sumx2, sumx, col/)		
b5 = (/x2y, sumxy, sumy/)


abc5 = abc(A5, b5, 3)

e2(5) = 0
do i = 1, n
	e2(5) = e2(5) + (y(i) - abc5(1)*x(i)**2 - abc5(2)*x(i) - abc5(3))**2
enddo

write(*,*) e2


idx = minloc(e2, 1)
write(*,*) name_f(idx), "function"

end program







