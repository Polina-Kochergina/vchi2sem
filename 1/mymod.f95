module mymod
implicit none
integer, parameter :: mp=8


contains 
function f(x)				! подынтегральная функция
implicit none
real(mp) x, f
	f = x**2
end function

function ff(x) 			! первообразная заданной функции f(x) для проверки
implicit none
real(mp) x, ff
	ff = (x**3)/3
end function



subroutine sim(f, a, b, n, simm)			! Метод Симпсона
!implicit none
integer i,n
real(mp) a, b, f, simm, h, s1, s2, x
h = (b-a)/n
s1 = 0
s2 = 0
	do i = 0, n-1
    	x = a + i*h
    		if (mod(i,2) == 0) then
        		s1 = s1 + f(x)
    		else
        		s2 = s2 + f(x)
    		endif
	enddo
simm = (h/3) * (f(a) + f(b) + 4*s1 + 2*s2)
end subroutine sim



subroutine rectan(f, a, b, n, rect)		!прямоугольник
implicit none
integer n, i
real(mp) a, b, f, rect, h, x
h = (b-a)/n
rect = 0
	do i = 1, n
	    x = a + (i-0.5)*h
	    rect = rect + f(x)
	enddo
rect = rect*h
end subroutine rectan



subroutine trap(f, a, b, n, trp)		! трапеция
implicit none
integer n, i
real(mp) a, b, f, trp, h, s, x
h = (b-a)/n
s = (f(a) + f(b))/2
	do i = 2, n
	    x = a + (i-1)*h
	    s = s + f(x)
	enddo
trp = s*h
end subroutine trap

end module mymod
