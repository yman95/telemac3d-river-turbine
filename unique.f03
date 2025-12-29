LOGICAL,DIMENSION(:), ALLOCATABLE :: mask
INTEGER,DIMENSION(:), ALLOCATABLE :: index_vector
INTEGER                           :: i,j,num
LOGICAL,intent(in),optional       :: sort 
num=size(a)  
ALLOCATE(mask(num)) 
mask = .TRUE.

DO i=num,2,-1
mask(i)=.NOT.(ANY(a(:i-1)==a(i)))
ENDDO
unique=a(PACK([(i,i=1,num)],mask))
IF(PRESENT(sort)) THEN
  IF(sort) CALL quicksort(unique)
ELSE
  CALL quicksort(unique)
ENDIF

