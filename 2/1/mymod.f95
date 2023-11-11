module mymod
implicit none
integer, parameter :: mp=8


contains 
function f(x)
implicit none
real(mp) x, f
	f = x**2
end function

function ff(x)
implicit none
real(mp) x, ff
	ff = (x**3)/3
end function

subroutine gauss(f,a,b,n,g)
implicit none
integer i,n
real(mp) a,b,f,h,x, g
h=(b-a)/n
g=0
do i=1, n
	g=g+h/2*(f((2*a+h*(1+(i-1)*2))/2 - 3**(0.5)/6*h) + f((2*a+h*(1+(i-1)*2))/2 + 3**(0.5)/6*h))
enddo
end subroutine gauss


end module mymod
