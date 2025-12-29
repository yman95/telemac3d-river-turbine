MODULE TurbineModule
  ! USE MathModule, ONLY:PI
  USE MathModule, ONLY:unique,PI,radians
  USE ModuleMat4, ONLY:Mat4
  USE ModuleQHull, ONLY:QHull
  IMPLICIT NONE
  
  ! TYPE MeshNode
  !   ! id    
  !   ! x Pointer
  !   ! y Pointer
  !   ! z Pointer
    
    
  !   CONTAINS
  !       procedure :: init
  !       ! Init()
  
  ! END TYPE MeshNode
  
  
  TYPE Turbine
    TYPE(Mat4)                        :: mat4
    REAL*8,DIMENSION(:,:),ALLOCATABLE :: rtz,cd,xyz,xy,yz,xz,xydummy,yzdummy,xzdummy
    REAL*8,DIMENSION(:),ALLOCATABLE   :: v
    INTEGER,DIMENSION(:),ALLOCATABLE  :: meshIds,meshIdsIDW
    
    REAL*8,DIMENSION(6)               :: bbox
    REAL*8,DIMENSION(3)               :: origin
    REAL*8                            :: d,t,x,y,z,rx,ry,rz,length
    
    CONTAINS
      procedure :: initialize
      procedure :: readCD
      procedure :: Translate
      procedure :: TranslateNOORIGIN
      procedure :: Scale
      procedure :: Rotatex
      procedure :: Rotatey
      procedure :: Rotatez
      procedure :: Transform
      procedure :: ResetOrigin
      procedure :: getBbox      
      procedure :: Deallocate
      procedure :: saveMeshIds
      procedure :: saveMeshIdsIDW
  END TYPE Turbine
  
  CONTAINS
    SUBROUTINE initialize(this,d,t,x,y,z,rx,ry,rz)
      CLASS(Turbine)     :: this
      REAL*8,INTENT(IN)  :: d,t,x,y,z,rx,ry,rz
      
      ! Turbine properties
      this%d = d ! Diameter
      this%t = t ! Blade thickness 
      
      ! Turbine Centroid
      this%x = x 
      this%y = y
      this%z = z
      
      
      
      !print *,rx,ry,rz
      ! Turbine Rotation
      this%rx = radians(rx)
      this%ry = radians(ry)
      this%rz = radians(rz)
      !print *,this%rx,this%ry,this%rz
      
    END SUBROUTINE
    
    SUBROUTINE readCD(this,F2ID,stat)
    ! 
    ! Read CD csv file
    ! Assume Shaft first and than Shell
    ! Compute xyz and volume
    !
      CLASS(Turbine)     :: this
      INTEGER,INTENT(IN)  :: F2ID

      
      INTEGER,INTENT(OUT) :: stat
      INTEGER             :: io
      INTEGER             :: NLINE
      INTEGER             :: I,J,K,NSHAFT,NSHELL
      REAL*8              :: dz,radiusO,radiusI,dr,theta,area,length
      CHARACTER*16        :: C ! Dummy header
      CHARACTER (144)     :: LINE
      
      REAL*8,ALLOCATABLE,DIMENSION(:) :: r,t,z

      stat=0
      REWIND(F2ID)
      READ(F2ID,*)C,C,C,C,C,C
      NLINE=0
      DO 
        READ(F2ID,FMT='(A)',IOSTAT=io)LINE
        IF (io > 0) stat=1  
        IF (io > 0) RETURN
        IF (io < 0) EXIT
        NLINE = NLINE + 1              
      ENDDO
      
      ALLOCATE (this%rtz(NLINE, 3))
      ALLOCATE (this%cd(NLINE, 3))
      ALLOCATE (this%xyz(NLINE, 3))
      ALLOCATE (this%v(NLINE))
      
      REWIND(F2ID)
      READ(F2ID,*)C,C,C,C,C,C
      DO I = 1, NLINE
        READ(F2ID,*,IOSTAT=io)this%rtz(I,:),this%cd(I,:)
        IF (io > 0) stat=1  
        IF (io > 0) RETURN
        IF (io < 0) EXIT
      ENDDO
      
      ! Get unique r values (radius)
      r=unique(this%rtz(:,1))
      t=unique(this%rtz(:,2))
      z=unique(this%rtz(:,3))
      
      dz = z(2)-z(1)   ! Assume uniform dz for each point
      theta= t(2)-t(1) ! Assume uniform dt or theta for each point
      
      ! Shaft
      z=PACK(this%rtz(:,3),this%rtz(:,1) == r(1))
      NSHAFT = size(z)
      NSHELL = NLINE - NSHAFT
      
      this%xyz(1:NSHAFT,1)=0.0D0
      this%xyz(1:NSHAFT,2)=0.0D0
      this%xyz(1:NSHAFT,3)=z
      this%v(1:NSHAFT)=dz*PI*r(1)**2.0D0
      
      ! Shell
      this%xyz(NSHAFT:, 1) = r(2) * COS(this%rtz(NSHAFT:,2))
      this%xyz(NSHAFT:, 2) = r(2) * SIN(this%rtz(NSHAFT:,2))
      this%xyz(NSHAFT:, 3) = this%rtz(NSHAFT:,3)
      
      ! Shell - Compute volume for each point 
      theta=atan2(sin(theta), cos(theta))
      radiusO=r(2)
      radiusI=radiusO-this%t
      dr = radiusO-radiusI
      area=0.5D0*theta*(radiusI+radiusO)*dr
      this%v(NSHAFT:) = dz*area    
      
      !length
      this%length = maxval(this%xyz(:,3))-minval(this%xyz(:,3))
      !print *, "length= ",this%length
      
      CALL this%mat4%Reset()
      CALL this%Translate([0.D0,.0D0,-MAXVAL(z)*0.5D0])
      CALL this%ResetOrigin()
      
      CALL this%Scale(this%d)
      CALL this%Rotatex(this%rx)
      CALL this%Rotatey(this%ry)
      CALL this%Rotatez(this%rz)
      
       !print *,this%d
       !print *,this%rx
       !print *,this%ry
       !print *,this%rz
      
      CALL this%Translate([this%x,this%y,this%z])
      
      this%xy=QHull(this%xyz(:,[1,2]))
      this%yz=QHull(this%xyz(:,[2,3]))
      this%xz=QHull(this%xyz(:,[1,3]))
      
      CALL this%Scale(11.D-1)
      !CALL this%Translate([this%x,this%y,this%z])
      this%xydummy=QHull(this%xyz(:,[1,2]))
      this%yzdummy=QHull(this%xyz(:,[2,3]))
      this%xzdummy=QHull(this%xyz(:,[1,3]))
      CALL this%Scale(909.D-3)
      !CALL this%Translate([-this%x,-this%y,-this%z])
      
      
    END SUBROUTINE
    
    SUBROUTINE saveMeshIds(this,ids)
      CLASS(Turbine)     :: this
      INTEGER,INTENT(in),DIMENSION(:),ALLOCATABLE :: ids
      this%meshIds=ids
    END SUBROUTINE
    
    SUBROUTINE saveMeshIdsIDW(this,ids)
      CLASS(Turbine)     :: this
      INTEGER,INTENT(in),DIMENSION(:),ALLOCATABLE :: ids
      this%meshIdsIDW=ids
    END SUBROUTINE
    ! SUBROUTINE getInsideXY(this,MESH2D,NPOIN2)
    !   CLASS(Turbine),INTENT(INOUT) :: this
    !   INTEGER :: I
      
    !   MESH2D%X%R
      
      
    ! END SUBROUTINE

    ! SUBROUTINE getInsideZ(this)
    !   CLASS(Turbine),INTENT(INOUT) :: this
      
    ! END SUBROUTINE    
    
    ! SUBROUTINE computeDragForce(this)
    !   CLASS(Turbine),INTENT(INOUT) :: this
      
    ! END SUBROUTINE
    
    SUBROUTINE ResetOrigin(this)
      CLASS(Turbine),INTENT(INOUT) :: this
      this%origin=[0.,0.,0.]
    END SUBROUTINE
    
    SUBROUTINE Translate(this,x)
      CLASS(Turbine),INTENT(INOUT) :: this
      REAL*8, DIMENSION(3),INTENT(in) :: x
      CALL this%mat4%Translate(x)
      CALL this%Transform()
      this%origin=x
    END SUBROUTINE
    
    SUBROUTINE TranslateNOORIGIN(this,x)
      CLASS(Turbine),INTENT(INOUT) :: this
      REAL*8, DIMENSION(3),INTENT(in) :: x
      CALL this%mat4%Translate(x)
      CALL this%Transform()
     
    END SUBROUTINE
    
    SUBROUTINE Scale(this,x)
      CLASS(Turbine),INTENT(INOUT) :: this
      REAL*8,INTENT(in)          :: x
      CALL this%TranslateNOORIGIN(-this%origin)
      CALL this%mat4%Scale([x,x,x])
      CALL this%Transform()
      CALL this%TranslateNOORIGIN(this%origin)
    END SUBROUTINE
    
    SUBROUTINE Rotatex(this,x)
      CLASS(Turbine),INTENT(INOUT) :: this
      REAL*8,INTENT(in) :: x
      CALL this%mat4%Rotatex(x)
      CALL this%Transform()
    END SUBROUTINE
    
    SUBROUTINE Rotatey(this,x)
      CLASS(Turbine),INTENT(INOUT) :: this
      REAL*8,INTENT(in) :: x
      CALL this%mat4%Rotatey(x)
      CALL this%Transform()
    END SUBROUTINE
    
    SUBROUTINE Rotatez(this,x)
      CLASS(Turbine),INTENT(INOUT) :: this
      REAL*8,INTENT(in) :: x
      CALL this%mat4%Rotatez(x)
      CALL this%Transform()
    END SUBROUTINE
    
    SUBROUTINE Transform(this)
      CLASS(Turbine),INTENT(INOUT) :: this
      CALL this%mat4%Transform(this%xyz)
      CALL this%mat4%Reset()
      CALL this%getBbox()
    END SUBROUTINE

    SUBROUTINE getBbox(this)
      CLASS(Turbine)           :: this
      this%bbox(1)=MINVAL(this%xyz(:,1))
      this%bbox(2)=MINVAL(this%xyz(:,2))
      this%bbox(3)=MINVAL(this%xyz(:,3))
      this%bbox(4)=MAXVAL(this%xyz(:,1))
      this%bbox(5)=MAXVAL(this%xyz(:,2))
      this%bbox(6)=MAXVAL(this%xyz(:,3))
    END SUBROUTINE
    
    SUBROUTINE Deallocate(this)
      CLASS(Turbine) :: this
      DEALLOCATE(this%rtz)
      DEALLOCATE(this%xyz)
      DEALLOCATE(this%cd)
      DEALLOCATE(this%v)
    END SUBROUTINE
    
END MODULE TurbineModule