MODULE TurbinesModule
  USE TurbineModule, ONLY:Turbine

  IMPLICIT NONE
  INTEGER                    :: NTEC
  TYPE(Turbine), ALLOCATABLE :: TEC(:)
  
  CONTAINS
    SUBROUTINE readTEC(F1ID,stat)
    ! 
    ! Read TEC text file
    !
      INTEGER,INTENT(IN) :: F1ID
      INTEGER,INTENT(OUT) :: stat
      
      INTEGER :: io
      INTEGER :: ITEC
      CHARACTER (144) :: LINE
      
      CHARACTER*20 :: name
      REAL*8 :: d,t,x,y,z,rx,ry,rz
      
      stat=0
      
      ! Find number of turbines in the text file
      NTEC=0
      REWIND(F1ID)
      DO 
        READ(F1ID,FMT='(A)',IOSTAT=io)LINE
        IF (io > 0) stat=1  
        IF (io > 0) RETURN
        IF (io < 0) EXIT
        IF(LINE(1:1).NE.'#'.AND.LINE(1:1).NE.' ') THEN
          print *, LINE
          NTEC = NTEC + 1              
        ENDIF
      ENDDO
      
      ALLOCATE(TEC(NTEC))
      
      ! Save turbine parameters
      ITEC=0
      REWIND(F1ID)
      DO 
        READ(F1ID,FMT='(A)',IOSTAT=io)LINE
        IF (io > 0) stat=1  
        IF (io > 0) RETURN
        IF (io < 0) EXIT
        IF(LINE(1:1).NE.'#'.AND.LINE(1:1).NE.' ') THEN
          ITEC=ITEC+1
          READ(LINE,*,IOSTAT=io)name,d,t,x,y,z,rx,ry,rz 
          IF (io > 0) stat=1  
          IF (io > 0) RETURN
          IF (io < 0) EXIT
          CALL TEC(ITEC)%initialize(d,t,x,y,z,rx,ry,rz)
        ENDIF
      ENDDO
      RETURN
    END SUBROUTINE 
    
    
    SUBROUTINE readCD(F2ID,stat)
    ! 
    ! Read CD csv file
    !
      INTEGER,INTENT(IN)  :: F2ID
      INTEGER,INTENT(OUT) :: stat
      INTEGER             :: ITEC
      print *, "Number of Turbines = ", NTEC
      DO ITEC=1,NTEC
        CALL TEC(ITEC)%readCD(F2ID,stat)
      ENDDO
    END SUBROUTINE
    
   
    
    ! INTEGER,POINTER,INTENT(IN) :: meshPointer ! TODO, review this
END MODULE