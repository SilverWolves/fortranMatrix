! Team 6 - Fortran 2003 
!
! This code can be viewed in Notepad++ or any known Fortran ide, however
! we primarily used text editors and compiled via command line.
!
! To download the compiler needed to run Fortran2003 program 
! go to https://gcc.gnu.org/wiki/GFortranBinaries find operating system 
! and find the highlighted word "installer". Download and run and compiler
! will set itself up, it should set up the path itself as well.
!
! Command line, find location of fortran file, to compile type
! gfortran -o nameOfDest nameOfSrc.f03
!
! this command creates a file named "nameOfDest.exe" and can be ran with 
! either "nameOfDest.exe" or just "nameOfDest"




program test_cpu_time					

	implicit none													! disables an old feature of fortran 
																	! that treats all variables that start with i, j, k, l, m and n 
																	! as integers and all others as real arguments. 
  
	Real :: start, finish											! create two floating point variables
	real, dimension(1000, 1000) :: matrixA, MatrixB, resultMatrix	! create three 2D arrays (column wise in Fortran)
	integer :: i, j													! create two integers
	
   
	do i=1,1000														! Loop control structure in Fortran
		do j = 1, 1000												! filling both Matrices with random values
			call random_number( matrixA(i, j) )
			CALL RANDOM_NUMBER( matrixB(i, j) )
		end do
	end do
   
	PRINT '("Multiplying 1000x1000 Matrices in Fortran")'			! printing a prompt to let user know prg is running

	call Cpu_time(start)											! gettin cpu time
   
	call matriXMultiply(MAtrixA, MatrixB, resultMatrix)				! calling subroutine

	call cpu_time(finish)											! getting cpu time
										
	print '("Time = ",i7," ms.")', INT((finish-start)*1000)			! printing subroutine run time
	
end program test_cpu_time



subroutine matrixMultiply(matrixA, matrixB, resultMatrix) 			! subroutine to multiply matrices
implicit none

	real, dimension(1000,1000) :: matrixA, MatrixB, resultMatrix	! have to identify types within functions or subroutine
	integer :: i, j, k												! integers to use with loops
   
	do i=1,1000														! loops to handle matrix multiplication 
		do j = 1, 1000
			resultMatrix(i, j) = 0									! all result matrix info to be originally set to zero (0)
			do k = 1, 1000
				resultMatrix(i, j) = matrixA(i, k) + matrixB(k, j)	! actual multiplication
			end do
		end do
	end do
return  															! return is not neccessary its just good for when ending a subroutine or function in Fortran  
end subroutine matrixMultiply										! subroutine ends 										