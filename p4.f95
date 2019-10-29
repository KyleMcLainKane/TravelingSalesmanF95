!
! Program 4
! Traveling salesman problem in FORTRAN 95
! CS-320-01
! 10-30-19
! Author: Kyle McLain Kane, cssc0498
!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! PROGRAM P4
!             Traveling Salesman Problem in FORTRAN 95
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM P4

IMPLICIT NONE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Variable declarations
INTEGER :: status, i, j, cities, temp, distance, permutations = 0
INTEGER :: bestDistance = 99999
CHARACTER(20) :: filename,tempName
CHARACTER(20), ALLOCATABLE, DIMENSION(:) :: city				!1D array for the name of the cities
INTEGER, ALLOCATABLE, DIMENSION(:,:) :: dist_table 				!2D array for the values between points
INTEGER, ALLOCATABLE, DIMENSION (:) :: path, best_path
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Open the file and read number of cities
PRINT *, "Program #4, cssc0498, Kyle McLain Kane"
PRINT *, "Enter file name:"
READ *, filename								!Currently being used as a scan for the file to open

OPEN(UNIT=10, FILE=filename, STATUS="OLD", ACTION="READ", IOSTAT=status)

if(status /= 0) THEN								!Checks to see if a correct file is inputted
    	PRINT *, "ERROR, could not open file for writing."
    	stop
    
END IF

!Reads in the amount of cities
read(10, *) cities
		
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Allocate memory for all needed arrays

!Based off number of cities from past part, 
! allocate the memory for the dist_table,
!Fortran is column major order

allocate ( dist_table(cities,cities) )    					!Allocating the table like this because it is cities X cities  (2D array)
allocate ( city(cities) )  							!Single array
allocate ( path(cities) )
allocate ( best_path(cities) )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Fill in arrays from data file

do i = 1, cities 
	path(i) = i
	READ(10, '(a)') city(i)						!Have to use (a) to get both strings if space in name   
		 
	do j = 1, cities                					!Reading the values in the table
		READ(10, *) dist_table(i,j)   
		                     
	end do  
	     
end do 
 
close(10) 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Use recursion to find minimal distance

! Permute all but home city
CALL permute(2, cities)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Print formatted output (COL major)
DO i = 1, cities
	IF(i == 1)THEN												!Starting at the first case
		WRITE(*,*) city(1), " to ", city(best_path(i+1)), "--", dist_table(best_path(i+1),1) 
	ELSE IF(i == cities) THEN										!Printing out the final stop to home
		WRITE(*,*) city(best_path(i)), " to ", city(1), "--", dist_table(1, best_path(i))
	ELSE													!Normal travel between cities
		WRITE(*,*) city(best_path(i)), " to ", city(best_path(i+1)), "--", dist_table(best_path(i+1),best_path(i))
	END IF	
END DO
PRINT *, ""
PRINT *, "Best distance is: ", bestDistance, " miles"
PRINT *, "Number of permutations: ", permutations
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Format labels
100 FORMAT (I6)
200 FORMAT (A)

CONTAINS
!Permute function
RECURSIVE SUBROUTINE permute(first, last)
INTEGER :: first, last, i, tem
	
	!Base Case
	IF (first == last) THEN
		distance = dist_table(1,path(2))
		
		DO i=2, last-1
			distance = distance + dist_table(path(i), path(i+1))
		END DO
		
		! Get distance from last city back home
		distance = distance + dist_table(path(last),path(1))
		
		!Incrementing permutations
		permutations = permutations + 1
	
		!Logging the best distance and route
		IF(distance < bestDistance) THEN
			bestDistance = distance
			              			
			DO i=2, cities
				best_path(i) = path(i)
				
			END DO
		END IF		
			
	ELSE
	
		!Mixing it up
		DO i=first, last
		
			!Swapping
			tem=path(first)
			path(first)=path(i)
			path(i)=tem
	
			!Recursion reduction step
			CALL permute(first+1,last)
			
			!Swapping
			tem=path(first)
			path(first)=path(i)
			path(i)=tem
			
		END DO	
	END IF 
	 
END SUBROUTINE permute       

END PROGRAM P4
