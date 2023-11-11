program Jacobirotation
implicit none
real(8) phi, t, eps, amax
character(10) num 
integer n, i, j, k, m, l
real(8), allocatable, dimension(:,:) :: A, E, H, U
real(8), allocatable, dimension(:) :: lambda
real(8), parameter :: pi = atan(-1.0)

open(1, file = 'input')
read(1,*) n
allocate(A(n,n))
Read(1,*) A
close(1)

eps = 0.01

allocate(E(n,n), H(n,n), U(n,n), lambda(n))

do i = 1, n
	do j = 1, n
		if (i == j) then
		E(i,j) = 1
		else
		E(i,j) = 0
		endif
	enddo
enddo


amax = 0

do k = 1, 10

	do i = 1, n
		do j = i+1, n
			if (amax < a(i,j)) then
				amax = a(i,j)
				m = i
				l = j
			endif
		enddo
	enddo
	
	
	
		
	if (a(m,m) == a(l,l)) then
		phi = pi/4
	else	
		phi = atan(2*amax/(a(m,m) - a(l,l)))/2
	endif
		!write(*,*) phi
		
	H = E
	H(m,m) = cos(phi)
	H(l,l) = cos(phi)
	H(m,l) = -sin(phi)
	H(l,m) = sin(phi)
		
	A = matmul(transpose(H),A)
	A = matmul(A,H)
		
	if (k==1) then
	   U=H
	else
	   U = matmul(U,H)
	endif
		
		
	t = sum(A**2)
	do i = 1, n
		t = t - a(i,i)**2
	enddo
		!write(*,*) t
	if (t < eps**2) then
		write(*,*) "good!"
		write(*,*) k
		exit
	endif	
		
		
enddo

do i = 1, n
write(num, "(I1.1)") i
write(*,*) trim("v")//trim(num), U(i,:)
enddo

do i = 1, n
lambda(i) = a(i,i)
enddo

do i = 1, n
write(num, "(I1.1)") i
write(*,*) trim("lambda")//trim(num), "  =", lambda(i)
enddo

end program
