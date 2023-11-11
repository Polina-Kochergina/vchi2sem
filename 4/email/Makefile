comp = gfortran

pattern = *.f95
source = ${wildcard ${pattern}}
obj = ${patsubst %.f95, %.o, ${source}}


main: ${obj}
	${comp} $^ -o $@

%.mod %.o: %.f95
	touch $@
	${comp} -c $<


main.o: mymod.mod 



run: main
	./main
result: main
	touch result
	./main>result
clear:
	rm -f *.o *.out *.mod* main
	rm -rf DATA

