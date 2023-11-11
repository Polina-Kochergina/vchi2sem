program task1
use mymod
implicit none
real(8) a, b, g, I
integer n
write(*,*) 'enter a, b, n'	
read(*,*) a, b, n

call gauss(f,a,b,n,g)


I=ff(b)-ff(a)		! ----------Ньютон-Лейбниц

write(*,*) n
write(*,*) 'int(x**2)) = ', I, a, 'to', b
write (*,*) 'Gauss = ', g


end program
