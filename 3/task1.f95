program task1
implicit none
real S, Sq
integer n, i, k
real, allocatable, dimension(:,:) :: A, X


read(*,*) n
allocate (A(2,n), X(2,n))
call random_number (A)
write(*,*) A
do i = 1 , n						!---масштабирование отрезка [0,1]
	x(1,i) = (-4+8)/2 + (8+4)*A(1,i)/2 		! [-4,8]
	x(2,i) = (-3+9)/2 + (3+9)*A(2,i)/2 		! [-3,9]
enddo
write(*,*) x


k = 0
do i=1, n 
if (sqrt((x(1,i) - 2)**2 + (x(2,i) - 3)**2) <= 6) then
	k = k + 1
end if
write(*,*) k
enddo

Sq = 144					! --- площадь квадрата 12х12
S = Sq * k/n
write(*,*) S

end program

