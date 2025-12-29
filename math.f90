MODULE MathModule
  USE ModuleSort, ONLY:quicksort
  IMPLICIT NONE
  
  REAL*8, PARAMETER :: PI        = 3.1415927410125732D0     
  REAL*8, PARAMETER :: PI2       = 2.0D0 * PI
  REAL*8, PARAMETER :: Degree180 = 180.0D0
  REAL*8, PARAMETER :: R_to_D    = Degree180/PI
  REAL*8, PARAMETER :: D_to_R    = PI/Degree180
  
  INTERFACE unique
    MODULE PROCEDURE runique
    MODULE PROCEDURE dunique
    MODULE PROCEDURE runique2
    MODULE PROCEDURE dunique2
  END INTERFACE unique 
  
  CONTAINS
    PURE FUNCTION  degrees(r)
      REAL*8, INTENT(IN) :: r
      REAL*8             :: degrees
      degrees = r * R_to_D
    END FUNCTION  degrees
    
    PURE FUNCTION  radians(d)
      REAL*8, INTENT(IN) :: d
      REAL*8             :: radians
      radians = d * D_to_R
    END FUNCTION  radians
    
    FUNCTION runique(a,sort) result(unique)
      IMPLICIT NONE
      REAL, DIMENSION(:)              :: a
      REAL, DIMENSION(:), ALLOCATABLE :: unique
      INCLUDE 'unique.f03'
    END FUNCTION runique     
    
    FUNCTION dunique(a,sort) result(unique)
      IMPLICIT NONE
      REAL*8, DIMENSION(:)              :: a
      REAL*8, DIMENSION(:), ALLOCATABLE :: unique
      INCLUDE 'unique.f03'
    END FUNCTION dunique
  
    FUNCTION runique2(a,sort) result(unique)
      IMPLICIT NONE
      REAL, DIMENSION(:,:)              :: a
      REAL, DIMENSION(:,:), ALLOCATABLE :: unique
      INCLUDE 'unique2.f03'
    END FUNCTION runique2     
    
    FUNCTION dunique2(a,sort) result(unique)
      IMPLICIT NONE
      REAL*8, DIMENSION(:,:)              :: a
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: unique
      INCLUDE 'unique2.f03'
    END FUNCTION dunique2  
  
END MODULE