program task1
use mymod
implicit none
real(8) a, b, simm, rect, trp, I
integer n
write(*,*) 'enter a, b, n'			!'введите начало промежутка, конец промежутка и количество разбиений'
read(*,*) a, b, n

call sim(f,a,b,n,simm)
call rectan(f,a,b,n,rect)
call trap(f, a, b, n, trp)

I=ff(b)-ff(a)  ! ----------Ньютон-Лейбниц

write(*,*) n
write(*,*) 'int(x**2)) = ', I, a, 'to', b
write (*,*) 'Simpson = ', simm
write (*,*) 'Trapeze = ', trp
write (*,*) 'Rectangle = ', rect

end program
