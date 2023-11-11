program task4
use mymod
implicit none
real(mp) a, b, h, k0, k1, k2, k3, m
integer n, i
real(mp), allocatable, dimension(:) :: Y, X

write(*,*) "введите концы промежутка a и b и кол-во узлов n"
read(*,*) a, b
read(*,*) n


call system("mkdir DATA")

!call ODY(f, a, b, n, i, m)    ! f --- функция (лежат в модуле)
				! a, b --- интервал 
				! n --- количество х и y, по хорошему h = (b-a)/n должен быть
				! не больше 0.1, следовательно n зависит от длины промежутка (a,b)
				! i --- номер/название файла в который выводятся результаты
				! m --- задача Коши для a, т.е --- y_0: y(a) = m 
m = a

call ODY(f0, a, b, n, 1, m) 
call ODY(f2, a, b, n, 2, m) 
call ODY(f3, a, b, n, 3, m) 
call ODY(f4, 1._mp, 5._mp, n, 4, m) ! а = 1, b = 5, т.к решение ~ exp(x) и при больших х быстро уходит на бесконечность
call ODY(f5, a, b, n, 5, m) 
call ODY(f6, a, b, n, 6, m) 
call ODY(f7, a, b, n, 7, m)
call ODY(f8, a, b, n, 8, m)
call ODY(f9, a, b, n, 9, m)
call ODY(f10, a, b, n, 10, m)

write(*,*) "результаты  в директории DATA"

end program

