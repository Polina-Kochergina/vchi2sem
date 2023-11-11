module odu
implicit none
real, public ::  h
integer, public :: n
real, public, allocatable :: x(:), y1(:), y2(:), y3(:)


contains


subroutine euler(f)
real f
integer i
!allocate(x(0:n), y1(0:n))
print *, "Eyler y(x):"
  do i = 0, n-1
  	y1(i+1) = y1(i) + h*f(x(i), y1(i))
  	print *, x(i+1), y1(i+1)
  enddo
!print *, "Eyler y(x):", y1
!deallocate(x, y1)
end subroutine


subroutine rungeKutta(f)
real f, k0, k1, k2, k3
integer i
!allocate(x(0:n), y2(0:n))
print *, "Runge-Kutta y(x):"
  do i = 0, n-1
	k0 = h*f(x(i), y2(i))
	k1 = h*f(x(i) + h/2, y2(i) + k0/2)
	k2 = h*f(x(i) + h/2, y2(i) + k1/2)
	k3 = h*f(x(i) + h, y2(i) + k2)
	y2(i+1) = y2(i) + (k0 + 2*k1 + 2*k2 +k3)/6.0
	print *, x(i+1), y2(i+1)
  enddo
!print *, "Runge-Kutta y(x):", y2
!deallocate(x, y2)
end subroutine


subroutine adams(f)
real f
integer i
!allocate(x(0:n), y3(0:n))
print *, "Adams y(x):"
  y3(1) = y3(0) + h*(f(x(0), y3(0))*55/24.0)
  y3(2) = y3(1) + h*(f(x(1), y3(1))*55/24.0 - f(x(0), y3(0))*59/24.0)
  y3(3) = y3(2) + h*(f(x(2), y3(2))*55/24.0 - f(x(1), y3(1))*59/24.0 + f(x(0), y3(0))*37/24.0)
  do i = 3, n-1
	y3(i+1) = y3(i) + h*(f(x(i), y3(i))*55/24.0 - f(x(i-1), y3(i-1))*59/24.0 + f(x(i-2), y3(i-2))*37/24.0 - f(x(i-3), y3(i-3))*9/24.0)
	print *, x(i+1), y3(i+1)
  enddo
!print *, "Adams y(x):", y3
!deallocate(x, y3)
end subroutine



end module	
