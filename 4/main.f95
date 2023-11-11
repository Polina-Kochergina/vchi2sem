program task1
use mymod
implicit none
real(mp) a, b, h, k0, k1, k2, k3
integer n, i
real(mp), allocatable, dimension(:) :: Y, X

read(*,*) a, b
read(*,*) n

call system("mkdir DATA")

call ODY(f0, a, b, n, 1)
call ODY(f2, a, b, n, 2)
call ODY(f3, a, b, n, 3)
call ODY(f4, a, b, n, 4)
call ODY(f5, a, b, n, 5)
call ODY(f6, a, b, n, 6)
call ODY(f7, a, b, n, 7)
call ODY(f8, a, b, n, 8)
call ODY(f9, a, b, n, 9)
call ODY(f10, a, b, n, 10)

end program

