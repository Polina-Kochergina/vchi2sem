program task2
implicit none
real V, V_1
integer n, i, k
real, allocatable, dimension(:,:) :: A, X


read(*,*) n
allocate (A(2,n), X(2,n))
call random_number (A)
write(*,*) A
do i = 1 , n					! --- масштабирование отрезка [0,1) в отрезак [a,b)
	x(1,i) =  2.667*A(1,i)			! from 0 to 2.667
	x(2,i) =  -2 + 4*A(2,i)		! from -2 to 2
enddo 
write(*,*) x(2,:)



k = 0
do i=1, n 
if (x(2,i)**2 <= 2*x(1,i) .and. x(2,i)**2 <= 16 - 6*x(1,i)) then
	k = k + 1
	write(*,*) k
end if
enddo

V = 10*2.667*4	
write(*,*) V				! наша фигура вписана в параллелепипед со сторонами
V_1 = V * k/n					! 10х4х2.667, но наша фигура ограничена плоскостями || XOY,
write(*,*) V_1					! поэтому можем спроецировать на XOY, и тогда k/n будет таким же,
						! если б нужно было б найти площадь фигуры ограниченную кривыми
						! y**2 = 16-6x    и   y**2 = 2x, поэтому я генирирую только x и y

end program
