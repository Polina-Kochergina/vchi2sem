program QR
implicit none
real(8) t, eps
character(10) num 
integer n, i, j, k, m, l
real(8), allocatable, dimension(:,:) :: A, E, Q, H, v


open(1, file = 'input')
read(1,*) n
allocate(A(n,n))
Read(1,*) A
close(1)


eps = 0.01

allocate(E(n,n), H(n,n), Q(n,n), v(n,n))

do i = 1, n
	do j = 1, n
		if (i == j) then
		E(i,j) = 1
		else
		E(i,j) = 0
		endif
	enddo
enddo

do k = 1, 1000
Q = E
	do i = 1, n-1
		v(i,1) = a(i,i) + sign(1d0,a(i,i))*sqrt(sum(A(i:n,i)**2))
		v(i+1:n, 1) = a(i+1:n,i)
		H = E - 2*(matmul(v, transpose(v))/sum(v**2))
		A = matmul(H,A)
		Q = transpose(matmul(Q, H))
	
	enddo
  
	A = matmul(A,transpose(Q))

	t=0
	
	do m = 1, n
		do l = m + 1, n
			t = t + a(l,m)**2
		enddo
	enddo
	
	if (t > eps**2) then
		continue
	else
		!print*, k
		exit
	endif
	
enddo



do i = 1, n
write(num, "(I1.1)") i
write(*,*) trim("lambda")//trim(num), "  =", a(i,i)
enddo

end program
