module mymod
implicit none
integer, parameter :: mp=8


contains 
function f(x)
implicit none
real(mp) x, f
	f = sin(x)/x
end function

function ff(x)
implicit none
real(mp) x, ff
	ff = x - (x**3)/18 + x**5/600
end function

subroutine gauss(f,a,b,n,s)
implicit none
integer i,n
real(mp) a,b,f,h,x,s
h=(b-a)/n
s=0
do i=1, n
	s=s+h/2*(f((2*a+h*(1+(i-1)*2))/2 - 3**(0.5)/6*h) + f((2*a+h*(1+(i-1)*2))/2 + 3**(0.5)/6*h))
enddo
end subroutine gauss


end module mymod
