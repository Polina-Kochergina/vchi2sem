program task2_2
use mymod
implicit none
real(8) a, b, g, I, l1,l2, c, delta, e,l
integer n

write(*,*) 'enter a, b, n'	
read(*,*) a, b, n
write(*,*) 'enter c'	
read(*,*) c

delta=0.001
e=0.0001
call gauss(f, c-delta, c+delta, n, l)

write(*,*) 'epsilon = ', e

do while ( abs(l) > 0.5*e) 
	delta = delta/2
	call gauss(f, c-delta, c+delta, n, l)
	write(*,*) delta, l
end do

call gauss(f, a, c-delta, n, l1)
call gauss(f, c+delta, b, n, l2)
write(*,*) l1, l2
g = l1 + l2
write(*,*) "integral f(x) = pi = 3,1415926..  from -inf to inf"
write (*,*) 'Gauss = ', g

write(*,*) 'delta =', delta

end program
