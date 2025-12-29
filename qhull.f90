MODULE ModuleQHull
  USE ModuleSort,ONLY:quicksort
  USE MathModule,ONLY:unique
  CONTAINS
    PURE REAL*8 FUNCTION cross(v1,v2,v3) result(c)
      REAL*8,DIMENSION(2),INTENT(in) :: v1,v2,v3
    	c =  (v2(1)-v1(1))*(v3(2)-v1(2))-(v2(2)-v1(2))*(v3(1)-v1(1))
    END FUNCTION cross
    
    FUNCTION BBOX(P) result(R)
      REAL*8,DIMENSION(:,:) :: P
      REAL*8,DIMENSION(size(P,2)*2) :: R
      IF(size(P,2) == 2) THEN
        R(1)=MINVAL(P(:,1))
        R(2)=MINVAL(P(:,2))
        R(3)=MAXVAL(P(:,1))
        R(4)=MAXVAL(P(:,2))
        RETURN
      ENDIF
      R(1)=MINVAL(P(:,1))
      R(2)=MINVAL(P(:,2))
      R(3)=MINVAL(P(:,4))
      R(4)=MAXVAL(P(:,1))
      R(5)=MAXVAL(P(:,2))
      R(6)=MAXVAL(P(:,3))
    END FUNCTION
    
    FUNCTION QHull(P) result(H)
      REAL*8,DIMENSION(:,:) :: P
      REAL*8,DIMENSION(:,:),ALLOCATABLE :: H
      INTEGER             :: n,nH
      REAL*8,DIMENSION(0:size(P,1)-1,2) :: U,L
      
      INTEGER             :: i,iL,iU
      n = size(P,1)
    	 
	    IF (n <= 1)THEN
	      ALLOCATE(H(size(P,1),size(P,2)))
	      H=P
	      RETURN
	    END IF
	    
	    CALL quicksort(P)
	    
	    iL=0
	    L= -HUGE(1.)
	    DO i=0,n-1
	      DO WHILE (iL >= 2.D0 .AND.cross(L(iL-2,:), L(iL-1,:), P(i+1,:)) <= 0.D0)
	       L(iL,:) = -HUGE(1.)
	       iL=iL-1
	      ENDDO
	      L(iL,:)=P(i+1,:)
	      iL=iL+1
	    ENDDO
      iU=0
	    U= HUGE(1.)
	    DO i=n-1,0,-1
	      DO WHILE (iU >= 2.D0 .AND. cross(U(iU-2,:), U(iU-1,:), P(i+1,:)) <= 0.D0)
	        U(iU,:) = HUGE(1.D0)
	        iU=iU-1
	      ENDDO
	      U(iU,:)=P(i+1,:)
	      iU=iU+1
	    ENDDO
	    
      iL = iL-1
      iU = iU-1
      ! nH  = iL+iU+1 ! Save first point, don't need this for Telemac
      nH  = iL+iU
      
      ALLOCATE(H(nH,2))
      H   = 0.D0
      
      ! NOTE: save values in Hull
      H(1:iL,:) = L(0:iL-1,:)
      H(iL+1:iL+iU,:) = U(0:iU-1,:)
      ! H=unique(H,sort=.False.)
      
      ! NOTE: save first value twice, don't need this for Telemac
      ! H(iL+iU+1,:) = H(1,:)
      
    END FUNCTION
    
END MODULE