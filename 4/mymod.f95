module mymod
implicit none
integer, parameter :: mp=8


contains 


function f0(x,y)
implicit none
real(mp) x, y, f0
	f0 = (1 + 2*x)/(y**2)
end function

function f2(x,y)
implicit none
real(mp) x, f2, y
	f2 = 4*x - 2*y
end function

function f3(x,y)
implicit none
real(mp) x, f3, y
	f3 = cos(x-y)
end function

function f4(x,y)
implicit none
real(mp) x, f4, y
	f4 = exp(x) - y
end function

function f5(x,y)
implicit none
real(mp) x, f5, y
	f5 = sin(x) - y
end function

function f6(x,y)
implicit none
real(mp) x, f6, y
	f6 = (1 + y**2)/(1 + x**2)
end function

function f7(x,y)
implicit none
real(mp) x, f7, y
	f7 = 2*x - y
end function

function f8(x,y)
implicit none
real(mp) x, f8, y
	f8 = 3*x - 2*y + 5
end function

function f9(x,y)
implicit none
real(mp) x, f9, y
	f9 = exp(2*x)
end function

function f10(x,y)
implicit none
real(mp) x, f10, y
	f10 = -2*y/(y**2 - 6*x)
end function


!---------------------------------------------


subroutine ODY(f1, a, b, n, p)
implicit none
real(mp) a, b, h, k0, k1, k2, k3, f1
integer n, i, p
character(10) num
real(mp), allocatable, dimension(:) :: Y, X

allocate (Y(0:n-1), X(0:n-1))

h = (b-a)/n
x(0) = a

do i = 0, n-2
	x(i+1) = x(i) + h
enddo
y(0) = (b-a)/2


write(num, "(I2.2)") p


print*, 1
open(p, file=trim("./DATA/data")//trim(num)//trim(".dat"))
	write(p,*) X
	write(p,*) ' '

print*, 2

write(p,*) 'Метод Эйлера:'

do i = 0, n-1
	y(i+1) = y(i) + h*f1(x(i), y(i))
enddo
write(p,*) Y


print*, 3

write(p,*) 'Метод Рунге-Кутты:'

do i = 0, n-1
	k0 = h*f1(x(i),y(i))
	k1 = h*f1(x(i)+h/2, y(i) + k0/2)
	k2 = h*f1(x(i)+h/2, y(i) + k1/2)
	k3 = h*f1(x(i)+h, y(i)+k2)
	!write(*,*) ' k =', k0, k1, k2, k3
	y(i+1) = y(i) + 1.0/6*(k0 + 2*k1 + 2*k2 + k3)
enddo
write(p,*) Y


print*, 4

write(p,*) 'Метод Адамса:'

do i = 3, n-1
	y(i+1) = y(i) + h*(55/24*f1(x(i),y(i)) - 59/24*f1(x(i-1), y(i-1)) +37/24*f1(x(i-2),y(i-2)) -9/24*f1(x(i-3),y(i-3)))
enddo
write(p,*) Y


print*, 5
close(p)


end subroutine



end module mymod
