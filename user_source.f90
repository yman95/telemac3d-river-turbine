!                    *****************
                     SUBROUTINE SOURCE &
!                    *****************
!
    (S0U,S0V,S0W,S1U,S1V,S1W,&
     UN3,VN3,WSN3,WN3, &
     VOLU,VOLUN,T3,NPOIN3,NTRAC,LT,AT,DT,PRIVE,NONHYD, &
     NPOIN2,NSCE,ISCE,KSCE,QSCE,USCE,VSCE,MAXSCE)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PREPARES SOURCE TERMS FOR DIFFUSION OF TRACERS.
!
!history  CDG/SOGREAH
!+        **/06/2001
!+
!+   TRACER SOURCES
!
!history  J-M HERVOUET (LNHE)
!+        29/08/2008
!+        V5P6
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME
!| DT             |-->| TIME STEP
!| ISCE           |-->| NODE ADRESSES IN 2D MESH FOR SOURCES
!| KSCE           |<->| NUMBER OF PLANE FOR SOURCES
!| LT             |-->| ITERATION NUMBER
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| NONHYD         |-->| LOGICAL FOR NON-HYDROSTATIC OPTION
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF POINTS IN THE MESH
!| NSCE           |-->| NUMBER OF GIVEN POINTS FOR SOURCES
!| NTRAC          |-->| NUMBER OF TRACERS
!| PRIVE          |-->| BLOCK OF ARRAYS FOR USER
!| QSCE           |-->| WATER DISCHARGE OF SOURCES
!| S0U            |<->| EXPLICIT SOURCE TERMS ON VELOCITIES U
!| S0V            |<->| EXPLICIT SOURCE TERMS ON VELOCITIES V
!| S0W            |<->| EXPLICIT SOURCE TERMS ON VELOCITIES W
!| S1U            |<->| IMPLICIT SOURCE TERMS ON VELOCITIES U
!| S1V            |<->| IMPLICIT SOURCE TERMS ON VELOCITIES V
!| S1W            |<->| IMPLICIT SOURCE TERMS ON VELOCITIES W
!| T3             |<->| WORK ARRAY: NOT USED
!| UN3            |-->| COMPONENTS OF VELOCITY AT PREVIOUS TIME STEP
!| USCE           |-->| VELOCITY FOR SOURCE
!| VN3            |-->| COMPONENTS OF VELOCITY AT PREVIOUS TIME STEP
!| VOLU           |-->| VOLUME AROUND POINTS AT TIME N+1
!| VOLUN          |-->| VOLUME AROUND POINTS AT TIME N
!| VSCE           |-->| VELOCITY FOR SOURCE
!| WN3            |-->| COMPONENTS OF VELOCITY AT PREVIOUS TIME STEP
!| WSN3           |-->| SIGMA-TRANSFORMED VERTICAL VELOCITY COMPONENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
  USE BIEF
  USE DECLARATIONS_SPECIAL
  USE DECLARATIONS_TELEMAC3D, ONLY :MESH2D,MESH3D,T3D_FILES,T3DFO1,T3DFO2,NPLAN,PRIVE1,UNSV3D
  USE TurbinesModule, ONLY:readTEC,readCD,TEC,NTEC,Turbine
  USE FILE_CSV, ONLY:Write_CSV_2D
  USE ModulePolyarea
  IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
  INTEGER, INTENT(IN)           :: NPOIN3, NTRAC, LT, MAXSCE
!
  TYPE(BIEF_OBJ), INTENT(IN)    :: UN3, VN3, WSN3, WN3
  TYPE(BIEF_OBJ), INTENT(INOUT) :: S0U, S0V, S1U, S1V, S0W, S1W
  TYPE(BIEF_OBJ), INTENT(INOUT) :: T3
  TYPE(BIEF_OBJ), INTENT(IN)    :: VOLU, VOLUN,PRIVE
!
  DOUBLE PRECISION, INTENT(IN)  :: AT,DT
  LOGICAL, INTENT(IN)           :: NONHYD
!
  INTEGER, INTENT(IN)           :: NPOIN2
  INTEGER, INTENT(IN)           :: NSCE
  INTEGER, INTENT(IN)           :: ISCE(NSCE)
  INTEGER, INTENT(IN)           :: KSCE(NSCE)
  DOUBLE PRECISION, INTENT(IN)  :: QSCE(NSCE)
  DOUBLE PRECISION, INTENT(IN)  :: USCE(NSCE)
  DOUBLE PRECISION, INTENT(IN)  :: VSCE(NSCE)
!
!-----------------------------------------------------------------------
!     ALLOCATE THE EXTRA VARIABLES FOR TEC
!-----------------------------------------------------------------------
! FORTRAN ARRAY(ROW:COL)
  
  INTEGER :: FO1,FO2,stat
  INTEGER :: I,J,I3,ITEC,INODE
  INTEGER :: K,L,cnt2,numInTurbine,idwArrayIndex
  DOUBLE PRECISION :: effX,effY,effZ,sumFU,sumFV,sumFW,cnt1,nodesInTubine
  DOUBLE PRECISION :: TcpA,TECVolu,dist,sumEffX,sumEffY,sumEffZ
  DOUBLE PRECISION :: effXRatio,effYRatio,effZRatio
  DOUBLE PRECISION :: cdinPointX,cdinPointY,cdinPointZ,cnt3
  DOUBLE PRECISION :: X,Y,Z,TECAvgV,TECAvgU,TECAvgW,searchr,closest
  DOUBLE PRECISION :: IDWSumX, IDWSumY, IDWSumZ, IDWSumDistInverse
  DOUBLE PRECISION :: turTol
  INTEGER,ALLOCATABLE :: meshIds(:),meshIdsIDW(:)
  DOUBLE PRECISION, ALLOCATABLE :: updateArray(:,:) !(I3,x,y,z,Cd in x,y,z)
  DOUBLE PRECISION, ALLOCATABLE :: idwArray(:,:) !(I3,x,y,z,Cd in x,y,z)
  LOGICAL :: pointsInRange,idwFlag
  TYPE(Turbine) ::temptec
  CHARACTER(len=20) :: filename
!-----------------------------------------------------------------------
!     VARIABLES NOTE
!-----------------------------------------------------------------------
! I, J, K, L: variables for loop
! cnt1 : counting variable
! trk1 : tracking variable
! IDWArray(:,:):  I3, dist, weight, effX, effY, effZ
! searchr : search radius

!-----------------------------------------------------------------------
!     INITIALIZATION TURBINES AT FIRST ITERATION
!-----------------------------------------------------------------------
  IF(LT.EQ.1) THEN
    print *, "------------First Iteration--------------"
    IF(T3D_FILES(T3DFO1)%NAME(1:1).EQ.' ') THEN
     WRITE(LU,*) "SOURCE: FORMATTED DATA FILE 1 IS REQUIRED"
     CALL PLANTE(1)
     STOP
    ENDIF
    IF(T3D_FILES(T3DFO2)%NAME(1:1).EQ.' ') THEN
     WRITE(LU,*) "SOURCE: FORMATTED DATA FILE 2 IS REQUIRED"
     CALL PLANTE(1)
     STOP
    ENDIF
    
    FO1=T3D_FILES(T3DFO1)%LU
    FO2=T3D_FILES(T3DFO2)%LU
    
    CALL readTEC(FO1,stat)
    IF(stat>0) THEN
      WRITE(LU,*) "ERROR in readTEC"
      CALL PLANTE(1)
      STOP
    ENDIF
  
    CALL readCD(FO2,stat)
    IF(stat>0)THEN
      WRITE(LU,*) "ERROR in readCD"
      CALL PLANTE(1)
      STOP
    ENDIF
    
    ! Save XY nodes
    DO ITEC=1,NTEC
      temptec=TEC(ITEC)
      INODE=0
      
      DO I=1,MESH2D%NPOIN
        IF(INPOLY(MESH2D%X%R(I), MESH2D%Y%R(I), temptec%xy(:,1), temptec%xy(:,2),size(temptec%xy,1))) THEN
          INODE = INODE+1
        ENDIF
      ENDDO
      
      ALLOCATE(meshIds(INODE))
      
      
      
      
      INODE=0
      DO I=1,MESH2D%NPOIN
        IF(INPOLY(MESH2D%X%R(I), MESH2D%Y%R(I), temptec%xy(:,1), &
        temptec%xy(:,2),size(temptec%xy,1))) THEN
          INODE = INODE+1
          meshIds(INODE)=I
        ENDIF
      ! print *, "temptec%xy(:,1)", temptec%xy(:,1)
      ENDDO
      CALL TEC(ITEC)%saveMeshIds(meshIds)
      
      DEALLOCATE(meshIds)
      
      INODE=0
      DO I=1,MESH2D%NPOIN
        IF(INPOLY(MESH2D%X%R(I), MESH2D%Y%R(I), temptec%xydummy(:,1),&
        temptec%xydummy(:,2),size(temptec%xy,1))) THEN
          INODE = INODE+1
        ENDIF
      ENDDO
      
      ALLOCATE(meshIdsIDW(INODE))
      
      INODE=0
      DO I=1,MESH2D%NPOIN
        IF(INPOLY(MESH2D%X%R(I), MESH2D%Y%R(I), temptec%xydummy(:,1),&
        temptec%xydummy(:,2),size(temptec%xy,1))) THEN
          INODE = INODE+1
          meshIdsIDW(INODE)=I
        ENDIF
      ENDDO
      CALL TEC(ITEC)%saveMeshIdsIDW(meshIdsIDW)
      
      DEALLOCATE(meshIdsIDW)
      
	  write(filename, '(A,I0,A)') 'xy', ITEC, '.csv'
      CALL Write_CSV_2D(filename,TEC(ITEC)%xy)
	  write(filename, '(A,I0,A)') 'yz', ITEC, '.csv'
      CALL Write_CSV_2D(filename,TEC(ITEC)%yz)
	  write(filename, '(A,I0,A)') 'xz', ITEC, '.csv'
      CALL Write_CSV_2D(filename,TEC(ITEC)%xz)
      
      
    ENDDO ! ITEC=1,NTEC
    
  ENDIF ! LT.EQ.1
  
!-----------------------------------------------------------------------      
!     INITIALIZATION OF SOURCE TERMS
!-----------------------------------------------------------------------       
  S1U%TYPR='Q'
  S1V%TYPR='Q'

  IF(NONHYD) THEN
    S0W%TYPR='0'
    S1W%TYPR='Q'
  ENDIF 
  
  DO I=1,NPOIN3
     S1U%R(I)=0.D0
     S1V%R(I)=0.D0
     IF(NONHYD) THEN ! IF NOT HYDROSTATIC CASE
      S1W%R(I)=0.D0
     ENDIF
     PRIVE1%R(I) = 0.D0
  ENDDO ! END OF DO I=1,NPOIN3
!-----------------------------------------------------------------------
!     SET SOURCE TERMS
!-----------------------------------------------------------------------
      
      print *, "Time step =", LT
      DO ITEC=1,NTEC
      temptec=TEC(ITEC)
      TcpA = 0.D00
      !-----------------------------------------------------------------------
      !     SET Variables
      searchr = 0.08D0  !search radius of the IDW method
      TcpA = 0.0768D0!Turbine centerplane area
      !-----------------------------------------------------------------------
      IDWSumX = 0.D0  !the sum of effX, used in IDW
      IDWSumY = 0.D0  !the sum of effY, used in IDW
      IDWSumZ = 0.D0  !the sum of effZ, used in IDW
      IDWSumDistInverse = 0.D0 !the sum of distance inversed, used in IDW
      ALLOCATE(updateArray(25000,7)) !(I3,x,y,z,Cd in x,y,z)
      ALLOCATE(idwArray(1000,7))
      print *, "temptec%length", temptec%length
      TECVolu = 0.D0  !Turbine volume
      TECAvgU = 0.D0  !average fluid velocity inside the turbine in x direciton
      TECAvgV = 0.D0  !average fluid velocity inside the turbine in y direciton
      TECAvgW = 0.D0  !average fluid velocity inside the turbine in z direciton
      sumFU = 0.D0    !calculated sum of drag forces in x direciton
      sumFV = 0.D0    !calculated sum of drag forces in y direciton
      sumFW = 0.D0    !calculated sum of drag forces in z direciton
      effX = 0.D0     !effective drag coefficient of a node in x direction
      effY = 0.D0     !effective drag coefficient of a node in y direction
      effZ = 0.D0     !effective drag coefficient of a node in z direction
      dist = 0.D0     !distance between an EPTM node and a TELEMAC node
      cnt1 = 0.D0     !counter1
      sumEffX = 0.D0  !Sum of all effective drag coefficient in the X direction in updateArray
      sumEffY = 0.D0  !Sum of all effective drag coefficient in the Y direction in updateArray
      sumEffZ = 0.D0  !Sum of all effective drag coefficient in the Z direction in updateArray
      L = 1           !loop variable
      updateArray = 0.D0  !the array which contains all nodes that have a drag coefficient and their source term need to be calculated each loop
      closest = -1.D0 !closest distance between an EPTM node and a TELEMAC node, used in assigning Eff. Cd
      pointsInRange = .FALSE. !points flag
      cnt2 = 1        !counter2
      nodesInTubine = 0.D0
      effXRatio = 0.D0!Ratio between the sum of effX in TELEMAC and input reference
      effYRatio = 0.D0!Ratio between the sum of effY in TELEMAC and input reference
      effZRatio = 0.D0!Ratio between the sum of effZ in TELEMAC and input reference
      idwArray = 0.D0
      cdinPointX = 0.D0
      cdinPointY = 0.D0
      cdinPointZ = 0.D0
      cnt3 = 0.D0
      idwArrayIndex = 1
      idwFlag = .False.
      turTol = 1.0d-2
      
      !print *,  temptec%xz(:,:)
      !print *, "-------------------------------------"
      !print *,  temptec%yz(:,:)
      !print *, "-------------------------------------"
      !Adjust temptec XYZ position from TEC rotation
     ! DO L=1, size(temptec%xz,1)
    !    IF (L.EQ.1) THEN
    !      temptec%xz(L,2) = temptec%xz(L,2) + temptec%z * 1.2D0
    !    ELSEIF (L.EQ.5) THEN
    !      temptec%xz(L,2) = temptec%xz(L,2) + temptec%z * 1.2D0
    !    ELSE
    !      temptec%xz(L,2) = temptec%xz(L,2) + temptec%z
    !    ENDIF
    !  ENDDO
      
    !  DO L=1, size(temptec%yz,1)
    !    IF (L.GT.3) THEN
    !      temptec%yz(L,2) = temptec%yz(L,2) + temptec%z * 1.2D0
    !    ELSE
    !     temptec%yz(L,2) = temptec%yz(L,2) + temptec%z
    !    ENDIF
    !  ENDDO
      
      !print *,  temptec%xz(:,:)
      !print *, "-------------------------------------"
      !print *,  temptec%yz(:,:)
      !print *, "-------------------------------------"
      !print *,  temptec%xy(:,2)
      !print *, "-------------------------------------"
      !print *,  temptec%xyz(:,2)
      print *, "max X val temptec%xyz", maxval(temptec%xyz(:,1))
	  print *, "min X val temptec%xyz", minval(temptec%xyz(:,1))
	  print *, "max Y val temptec%xyz", maxval(temptec%xyz(:,2))
	  print *, "min Y val temptec%xyz", minval(temptec%xyz(:,2))
      !print *, "size(temptec%xz,1)", size(temptec%xz,1)
      !print *, "size(temptec%yz,1)", size(temptec%yz,1)
      !print *, "temptec%z", temptec%z
      !print *, "size(temptec%xy,1)", size(temptec%xy,1)
      
      
      !Assign every EPTM Cd to its closest TELEMAC node
      !updateArray holds the assigned TELEMAC nodes
      print *, "NPLAN", NPLAN
      DO K=1, size(temptec%xyz(:,1))!For every EPTM node
      
      DO INODE=1,size(temptec%meshIdsIDW) !For every TELEMAC node
      I = temptec%meshIdsIDW(INODE)
        I3=I+3*NPOIN2
        
        !calculate distance between EPTM nodes and TELEMAC nodes
        dist = DSQRT((MESH3D%X%R(I3)-temptec%xyz(K,1))**2&
        +(MESH3D%Y%R(I3)-temptec%xyz(K,2))**2&
        +(MESH3D%Z%R(I3)-temptec%xyz(K,3))**2)
        
        IF (closest .EQ. -1.D0) THEN !First node
        closest = dist
        updateArray(L,1) = I3
        updateArray(L,2) = MESH3D%X%R(I3)
        updateArray(L,3) = MESH3D%Y%R(I3)
        updateArray(L,4) = MESH3D%Z%R(I3)
        updateArray(L,5:7) = temptec%cd(K,1:3)
        
        ELSEIF (dist .LT. closest) THEN 
        closest = dist
        updateArray(L,1) = I3
        updateArray(L,2) = MESH3D%X%R(I3)
        updateArray(L,3) = MESH3D%Y%R(I3)
        updateArray(L,4) = MESH3D%Z%R(I3)
        updateArray(L,5:7) = temptec%cd(K,1:3)
        
        ENDIF !IF (closest .EQ. -1.D0) THEN

      ENDDO !INODE=1,size(temptec%meshIdsIDW) !For every TELEMAC node
      
        L = L + 1
        !parameter reset
        closest = -1.D0
        
      ENDDO !DO K=1, size(temptec%xyz(:,1)) !For every EPTM node
      
      
      !Average the multiple Cds on the same TELEMAC node
      !And copy them to idwArray
      DO L=1, size(updateArray(:,1)) ! For each updateArray point
        DO K=1, size(updateArray(:,1)) !Checks with every updateArray point
          IF ((updateArray(L,1).EQ.updateArray(K,1)).AND.(updateArray(L,1).NE.0.D0)) THEN
            cdinPointX = cdinPointX + updateArray(K,5)
            cdinPointY = cdinPointY + updateArray(K,6)
            cdinPointZ = cdinPointZ + updateArray(K,7)
            cnt3 = cnt3+1.D0 !counts TELEMAC points that have multiple Cds on them
            IF (updateArray(L,1).NE.updateArray(K,1)) THEN
              updateArray(K,1:7) = 0.D0
            ENDIF
          ENDIF
        ENDDO !K inner loop
        cdinPointX = cdinPointX/cnt3
        cdinPointY = cdinPointY/cnt3
        cdinPointZ = cdinPointZ/cnt3
        DO J=1, idwArrayIndex
          IF (updateArray(L,1).EQ.idwArray(J,1)) THEN
            exit
          ENDIF
          IF ((updateArray(L,1).NE.idwArray(J,1)).AND.(idwArray(J,1).EQ.0.D0)) THEN
            idwArray(idwArrayIndex,1:4) = updateArray(L,1:4)
            idwArray(idwArrayIndex,5) = cdinPointX
            idwArray(idwArrayIndex,6) = cdinPointY
            idwArray(idwArrayIndex,7) = cdinPointZ
            idwArrayIndex = idwArrayIndex + 1
            
          ENDIF
        ENDDO
        cnt3 = 1.D0 !Counter3 reset
        cdinPointX = 0.D0
        cdinPointY = 0.D0
        cdinPointZ = 0.D0
      ENDDO !L outer loop
      
      print *, "idwArrayIndex", idwArrayIndex
      
      !write(*, '(F8.5)', '(F8.5)') idwArray(:,3)
     ! do i = 1, idwArrayIndex
     !   write(*, '(F6.2, F6.2, F6.2)') idwArray(i, 2), idwArray(i, 3), idwArray(i, 4)
     ! end do
      updateArray = 0.D0
      
      
      
      !idwArray has all TELEMAC points with averaged EPTM Cds (rim/blade path)
      
      !Turbine volume and average velocity calculation
      DO INODE=1,size(temptec%meshIds) !For every TELEMAC node
        I = temptec%meshIds(INODE)
        DO J=1,NPLAN !For every TELEMAC node
          I3=I+(J-1)*NPOIN2
          X=MESH3D%X%R(I3)
          Y=MESH3D%Y%R(I3)
          Z=MESH3D%Z%R(I3)
          !print *,"x,y,z",X,Y,Z
          !print *,"temptec x,y,z", temptec%x, temptec%y, temptec%z

          IF(INPOLY(Y,Z, temptec%yz(:,1), temptec%yz(:,2), size(temptec%yz,1)).AND.&
            INPOLY(X,Z, temptec%xz(:,1), temptec%xz(:,2), size(temptec%xz,1)).AND.&
            INPOLY(X,Y, temptec%xy(:,1),temptec%xy(:,2), size (temptec%xy,1))) THEN
            !Calculate TEC Total Volume and TEC Averaged U,V,W
            TECVolu = TECVolu + VOLU%R(I3)
            TECAvgU = TECAvgU + UN3%R(I3)
            TECAvgV = TECAvgV + VN3%R(I3)
            TECAvgW = TECAvgW + WN3%R(I3)
            updateArray(cnt2,1) = I3
            updateArray(cnt2,2) = MESH3D%X%R(I3)
            updateArray(cnt2,3) = MESH3D%Y%R(I3)
            updateArray(cnt2,4) = MESH3D%Z%R(I3)
            cnt2 = cnt2 + 1
            cnt1 = cnt1 + 1.D0
          ENDIF
        ENDDO !J=1,NPLAN
      ENDDO !INODE=1,size(temptec%meshIds)
      
      IF (cnt1 .NE. 0.D0) THEN
      nodesInTubine = cnt1
        TECAvgU = TECAvgU/cnt1
        TECAvgV = TECAvgV/cnt1
        TECAvgW = TECAvgW/cnt1
      ELSE
        print *, "cnt1 is 0, no points in turbine captured"
      ENDIF
      print *, "# of TELEMAC nodes in the turbine region", cnt1
      print *, "cnt2", cnt2
      !updateArray now has all nodes in the Turbine region (without the distributed Cds)
      
      
      !Overwriting points with existing Cd values
      DO L=1, idwArrayIndex - 1
        updateArray(cnt2,2:7)=idwArray(L,2:7)
        cnt2 = cnt2 + 1
      ENDDO
      print *,"# of updateArray points", cnt2

      !updateArray now has Cds at the end of the array
      !Need to overwrite 0s before IDW
      !Do an if check to overwrite in IDW
      !idwArrayIndex is the last element in idwArray+1
      !cnt2 is the last element in updateArray+1
      
      !idwArrayIndex = 1
      !Find the last non zero element in idwArray
      !DO L=1, size(idwArray(:,1))
      !  IF (idwArray(L,1).NE.0.D0)THEN
      !    IF (idwArray(L+1,1).EQ.0.D0)THEN
      !      idwArrayIndex = idwArrayIndex +1
      !      print *, "end element of idwArray", idwArrayIndex
      !    ENDIF
      !  ENDIF
      !ENDDO
      
      !DO L=1, cnt2+1
      !  write(*, '(7F15.6)') idwArray(L, :)
      !ENDDO
      
      
      
      idwArray = 0.D0
      idwArrayIndex = 1
      !IDW on TELEMAC mesh for smoothing within the Turbine region
      !dist between idwArray's coordinates and MESH3D%Y%R(I3) 
	  !FOR TSH TSI: ONE NODE, X=0.0 Y=0.026
	  !FOR TSM: TWO NODE, X=0.0 & 1.92 Y=0.026
	  !FOR TSN: TWO NODES,
	  !FOR TSJ, TSK, TSL: TWO NODES, X=0.0 and  Y=0.311 & -0.316
	  !FOR TSO: THREE NODES, X=0.0 and  Y=0.311 & -0.316, x=0.96 Y=0.026
      DO L=1, cnt2
      IF ((updateArray(L,2)+turTol.GT.0.0D0).AND.(updateArray(L,2)-turTol.LT.0.0D0).AND. &
	   (updateArray(L,3)+turTol.GT.-0.316D0).AND. (updateArray(L,3)-turTol.LT.-0.316D0).AND. &
       (updateArray(L,4).EQ.0.48D0)) THEN
        idwArray(idwArrayIndex,1:7) = updateArray(L,1:7)
        idwArray(idwArrayIndex,5)=-18.13217258D0
        print *, "!!!!!!!idw center update", idwArray(idwArrayIndex,1), idwArray(idwArrayIndex,5)
        idwArrayIndex = idwArrayIndex + 1
        
		
      ELSEIF ((updateArray(L,2)+turTol.GT.0.0D0).AND.(updateArray(L,2)-turTol.LT.0.0D0).AND. &
	   (updateArray(L,3)+turTol.GT.0.311D0).AND. (updateArray(L,3)-turTol.LT.0.311D0).AND. &
       (updateArray(L,4).EQ.0.48D0)) THEN
	    idwArray(idwArrayIndex,1:7) = updateArray(L,1:7)
        idwArray(idwArrayIndex,5)=-18.13217258D0
		print *, "!!!!!!!idw center update", idwArray(idwArrayIndex,1), idwArray(idwArrayIndex,5)
        idwArrayIndex = idwArrayIndex + 1
		
	  ELSEIF ((updateArray(L,2)+turTol.GT.0.96D0).AND.(updateArray(L,2)-turTol.LT.0.96D0).AND. &
	   (updateArray(L,3)+turTol.GT.0.026D0).AND. (updateArray(L,3)-turTol.LT.0.026D0).AND. &
       (updateArray(L,4).EQ.0.48D0)) THEN
	    idwArray(idwArrayIndex,1:7) = updateArray(L,1:7)
        idwArray(idwArrayIndex,5)=-18.13217258D0
		print *, "!!!!!!!idw center update", idwArray(idwArrayIndex,1), idwArray(idwArrayIndex,5)
        idwArrayIndex = idwArrayIndex + 1
		
	   ELSE
        DO J=1, cnt2
          dist = DSQRT((updateArray(L,2)-updateArray(J,2))**2&
               +(updateArray(L,3)-updateArray(J,3))**2&
               +(updateArray(L,4)-updateArray(J,4))**2)
          !print *, "dist", dist
          IF (dist.GT.0.D0 .AND. dist.LT.searchr) THEN
            IDWSumX = IDWSumX + (updateArray(J,5))/(dist**2)
            IDWSumY = IDWSumY + (updateArray(J,6))/(dist**2)
            IDWSumZ = IDWSumZ + (updateArray(J,7))/(dist**2)
            IDWSumDistInverse = IDWSumDistInverse + (1/dist**2)
          ENDIF
        ENDDO
        IF (IDWSumDistInverse .GT. 0.D0) THEN
          idwArray(idwArrayIndex,1:4) = updateArray(L,1:4)
          idwArray(idwArrayIndex,5) = IDWSumX/IDWSumDistInverse
          idwArray(idwArrayIndex,6) = IDWSumY/IDWSumDistInverse
          idwArray(idwArrayIndex,7) = IDWSumZ/IDWSumDistInverse
          idwArrayIndex = idwArrayIndex + 1
          
          IF (idwArrayIndex .GT. 1000) THEN
            print *, "Max idwArray size reached"
          ENDIF
        ENDIF
        !parameter reset
        IDWSumX = 0.D0
        IDWSumY = 0.D0
        IDWSumZ = 0.D0
        IDWSumDistInverse = 0.D0
      ENDIF
      ENDDO
      
      
        print *, "total num of points in idwArray after IDW", idwArrayIndex
      
        !After IDW, a previous empty node will have value i.e. overpredicting
        !Add the previous empty TELEMAC node to idwArray
        !Apply coefficient after
        
      
        
      DO L=1, idwArrayIndex-1
        sumEffX = sumEffX + idwArray(L,5)
        sumEffY = sumEffY + idwArray(L,6)
        sumEffZ = sumEffZ + idwArray(L,7)
      ENDDO
        print *, "Sum of EffX in idwArray ", sumEffX
        print *, "Sum of EffY in idwArray ", sumEffY
        print *, "Sum of EffZ in idwArray ", sumEffZ
        !effXRatio = 1225288.755D0/sumEffX
        !effYRatio = 213115.5335D0/sumEffY
        !effZRatio = 4.6749D0/sumEffZ
        effXRatio = -29.511265D0 !EPTM total V / TELEMAC total V
        effYRatio = -29.511265D0 !D_eptm1=6m,D_eptm2=5.88m,D_tele1=0.32m,D_tele2=0.31m
        effZRatio = -29.511265D0 !(113.097-108.619)*0.003)/(0.019792*0.023(lab blade chord length))
      DO L=1, idwArrayIndex-1
          idwArray(L,5) = abs(idwArray(L,5) * effXRatio)
          idwArray(L,6) = abs(idwArray(L,6) * effYRatio)
          idwArray(L,7) = abs(idwArray(L,7) * effZRatio)
      ENDDO
        !print *, "effZRatio: ", effZRatio
        sumEffX = 0.D0
        sumEffY = 0.D0
        sumEffZ = 0.D0
      DO L=1, size(idwArray(:,1)) 
        sumEffX = sumEffX + idwArray(L,5)
        sumEffY = sumEffY + idwArray(L,6)
        sumEffZ = sumEffZ + idwArray(L,7)
      ENDDO
      print *, "Adjusted EffX ", sumEffX
      print *, "Adjusted EffY ", sumEffY
      print *, "Adjusted EffZ ", sumEffZ
      
      
     ! DO L = 1, size(idwArray(:,1)) 
	!	IF (idwArray(L,2)/=0.d0 .OR. idwArray(L,3)/=0.d0 .OR. idwArray(L,1)/=0.d0) THEN
	!		write(*,'(F10.4, F10.4, F10.4)') idwArray(L,2),idwArray(L,3),idwArray(L,5)
	!	ENDIF
	  !ENDDO
      !Source term update
      cnt2 = 1
      DO L = 1, size(idwArray(:,1)) 
        IF (idwArray(L,1) .NE. 0.D0) THEN
        cnt2 = cnt2 + 1
        I3 = idwArray(L,1)
        effX = idwArray(L,5)
        effY = idwArray(L,6)
        effZ = idwArray(L,7)
        
        !center I3 ID 313454 for TSH and TSI
        
        S1U%R(I3)=(0.5D0*(effX)*TcpA*TECAvgU)

        S1V%R(I3)=(0.5D0*(effY)*TcpA*TECAvgV)
             
        S1W%R(I3)=(0.5D0*(effZ)*TcpA*TECAvgW)
        
        PRIVE1%R(I3)=effX
        
        !S1U%R(I3)=(0.5D0*(effX)*TcpA*TECAvgU)/(nodesInTubine)
        !S1V%R(I3)=(0.5D0*(effY)*TcpA*TECAvgV)
        !S1W%R(I3)=(0.5D0*(effZ)*TcpA*TECAvgW) 
        !/(nodesInTubine*2)
        !*(VOLU%R(I3)/TECVolu)
        
        
        !print *, "S1U%R(I3)", S1U%R(I3), "TcpA", TcpA, "TECAvgU", TECAvgU
        !print *, "VOLU%R(I3)", VOLU%R(I3), "TECVolu", TECVolu
        !IF (S1U%R(I3) .NE. 0.D0) THEN
        !print *, "S1U%R(I3)", S1U%R(I3),S1V%R(I3),S1W%R(I3)
        !print *, "effx,", effX
        !ENDIF

        
        IF (S1U%R(I3) .NE. 0.D0) THEN
        sumFU = sumFU + S1U%R(I3)*TECAvgU !*1000 (from rho)/1000 in kN
        sumFV = sumFV + S1V%R(I3)*TECAvgU
        sumFW = sumFW + S1W%R(I3)*TECAvgU
        
        !sumEffX = sumEffX + effX*VOLU%R(I3)
        !print *, "A(I3)", TcpA*(VOLU%R(I3)/TECVolu)
        
        ENDIF
        
        ELSEIF (idwArray(L,1) .EQ. 0.D0) THEN
          CYCLE
        ENDIF
      ENDDO !size(idwArray(:,1)) Drag term updating process
      
      
      print '(A, F18.6,F10.4,F10.4)', "Drag force of turbine = ", sumFU, sumFV, sumFW
      print '(A, F12.8,F12.8,F12.8,F12.8)', "VOLU%R(I3), TECVolu, TcpA, VolRatio ", VOLU%R(I3),TECVolu,TcpA,(VOLU%R(I3)/TECVolu)
      print '(A, F14.8,F14.8,F14.8)', "TEC Avg Velocity in U = ", TECAvgU, TECAvgV, TECAvgW
      
      !parameter reset
      TECAvgU = 0.D0
      TECAvgV = 0.D0
      TECAvgW =0.D0
      sumFU = 0.D0
      sumFV = 0.D0
      sumFW = 0.D0
      cnt1 = 0.D0
      L=1
      cnt2 = 1
      effX = 0.D0
      effY = 0.D0
      effZ = 0.D0
      sumEffX = 0.D0
      sumEffY = 0.D0
      sumEffZ = 0.D0
      effXRatio = 0.D0
      effYRatio = 0.D0
      effZRatio = 0.D0
      
      DEALLOCATE(updateArray)
      DEALLOCATE(idwArray)

  ENDDO ! ITEC=1,NTEC
  
  !print *, "End of subroutine"
  
  print *, "------------------------------------------------------------"
END SUBROUTINE
