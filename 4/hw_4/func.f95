module func
implicit none


contains

function f1(x,y)
real f1, x, y
f1 = (1-2*x)/y**2
end function

function f2(x,y)
real f2, x, y
f2 = 4*x - 2*y
end function

function f3(x,y)
real f3, x, y
f3 = cos(x-y)
end function

function f4(x,y)
real f4, x, y
f4 = exp(x) - y
end function

function f5(x,y)
real f5, x, y
f5 = sin(x) - y
end function

function f6(x,y)
real f6, x, y
f6 = (1 + y**2)/(1 + x**2)
end function

function f7(x,y)
real f7, x, y
f7 = 2*x - y
end function

function f8(x,y)
real f8, x, y
f8 = 3*x - 2*y +5
end function

function f9(x,y)
real f9, x, y
f9 = exp(2*x) - 1
end function

function f10(x,y)
real f10, x, y
f10 = -2*y/(y**2 - 6*x)
end function

end module
