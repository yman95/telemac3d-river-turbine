MODULE FILE_CSV
  IMPLICIT NONE
  CONTAINS
    SUBROUTINE Write_CSV_2D(file,a)
      CHARACTER(LEN=*), INTENT(IN)    :: file
      REAL*8, DIMENSION(:,:), INTENT(IN) :: a
      INTEGER                          :: I
      INTEGER,PARAMETER :: unit=10
      
      OPEN(unit=unit,file=file,status='unknown')
      DO I=1,size(a,1)
        WRITE(unit, '(*(F15.8 : ", "))') a(I,:)
      ENDDO
      CLOSE(unit)
    END SUBROUTINE
END MODULE FILE_CSV