module mymod
implicit none
real, parameter :: pi = acos(-1.0)

contains 

subroutine ODU(f1, x, y, dy, n)
implicit none
real h, k0, k1, k2, k3, f1, q0, q1, q2, q3
integer n, i
real :: Y(0:n), X(0:n), dY(0:n)


h = pi/n

do i = 0, n-1

	k0 = h*f1(x(i), y(i), dy(i))
	q0 = h*dy(i)
	k1 = h*f1(x(i)+h/2, y(i) + q0/2, dy(i)+k0/2.0)
	q1 = h*(dy(i) + k0/2.0)
	k2 = h*f1(x(i)+h/2, y(i) + q1/2, dy(i) + k1/2.0)
	q2 = h*(dy(i) + k1/2.0)
	k3 = h*f1(x(i)+h, y(i) + q2, dy(i) +k2)
	q3 = h*(dy(i) + k2*h)
	!write(*,*) ' k =', k0, k1, k2, k3
	y(i+1) = y(i) + 1.0/6.0*(q0 + 2*q1 + 2*q2 + q3)
	dy(i+1) = dy(i) + 1.0/6.0*(k0 + 2*k1 + 2*k2 + k3)
enddo
print*, q0
end subroutine

end module mymod
