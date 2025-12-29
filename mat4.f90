MODULE ModuleMat4
  TYPE Mat4
    REAL*8,DIMENSION(4,4) :: x
    CONTAINS
      procedure :: Reset
      procedure :: Translate
      procedure :: Scale
      procedure :: Rotatex
      procedure :: Rotatey
      procedure :: Rotatez
      procedure :: Transform
  END TYPE
  
  CONTAINS
    SUBROUTINE Reset(this)
      CLASS(Mat4),INTENT(INOUT) :: this
      INTEGER I
      this%x = 0.D0
      FORALL(I = 1:4) this%x(I,I) = 1.D0
    END SUBROUTINE
    
    SUBROUTINE Translate(this,x)
      CLASS(Mat4),INTENT(INOUT) :: this
      REAL*8, DIMENSION(3),INTENT(in) :: x
      this%x(1,4) = x(1)
      this%x(2,4) = x(2)
      this%x(3,4) = x(3)
    END SUBROUTINE
    
    SUBROUTINE Scale(this,x)
      CLASS(Mat4),INTENT(INOUT) :: this
      REAL*8, DIMENSION(3),INTENT(in) :: x
      this%x(1,1) = x(1)
      this%x(2,2) = x(2)
      this%x(3,3) = x(3)
    END SUBROUTINE    

    SUBROUTINE Rotatex(this,x)
      CLASS(Mat4),INTENT(INOUT) :: this
      REAL*8,INTENT(in) :: x
      this%x(2,2) = COS(x)
      this%x(3,3) = COS(x)
      this%x(2,3) = -SIN(x)
      this%x(3,2) = SIN(x)
    END SUBROUTINE
    
    SUBROUTINE Rotatey(this,x)
      CLASS(Mat4),INTENT(INOUT) :: this
      REAL*8,INTENT(in) :: x
      this%x(1,1) = COS(x)
      this%x(3,3) = COS(x)
      this%x(1,3) = -SIN(x)
      this%x(3,1) = SIN(x)
    END SUBROUTINE
    
    SUBROUTINE Rotatez(this,x)
      CLASS(Mat4),INTENT(INOUT) :: this
      REAL*8,INTENT(in) :: x
      this%x(1,1) = COS(x)
      this%x(2,2) = COS(x)
      this%x(1,2) = -SIN(x)
      this%x(2,1) = SIN(x)
    END SUBROUTINE
    
    SUBROUTINE Transform(this,x)
      CLASS(Mat4),INTENT(IN) :: this
      REAL*8,DIMENSION(:,:),INTENT(INOUT) :: x
      x=vec3(TRANSPOSE(MATMUL(this%x, TRANSPOSE(vec4(x)))))
    END SUBROUTINE
  
    FUNCTION vec4(vec3)
      REAL*8, DIMENSION(:,:),INTENT(in) :: vec3
      REAL*8, DIMENSION(size(vec3,1),4) :: vec4
      vec4(:,:3) = vec3
      vec4(:,4)  = 1.0D0
    END FUNCTION vec4
    
    FUNCTION vec3(vec4)
      REAL*8, DIMENSION(:,:),INTENT(in) :: vec4
      REAL*8, DIMENSION(size(vec4,1),3) :: vec3
      vec3=vec4(:,:3)
    END FUNCTION vec3
  
END MODULE