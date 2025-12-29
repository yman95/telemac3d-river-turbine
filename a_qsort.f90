MODULE ModuleSort

  INTERFACE quicksort
   MODULE PROCEDURE rquicksort
   MODULE PROCEDURE rquicksort2
   MODULE PROCEDURE dquicksort
   MODULE PROCEDURE dquicksort2
  END INTERFACE quicksort
  
  INTERFACE compare
   MODULE PROCEDURE rcompare
   MODULE PROCEDURE dcompare
   MODULE PROCEDURE rcompare2
   MODULE PROCEDURE dcompare2
  END INTERFACE compare 
  
  
  CONTAINS
    LOGICAL FUNCTION rcompare(a, b) result(c)
      REAL, intent(in) :: a, b
      c = a<b
    END FUNCTION
    
    LOGICAL FUNCTION dcompare(a, b) result(c)
      REAL*8, intent(in) :: a, b
      c = a<b
    END FUNCTION 
    
    LOGICAL FUNCTION rcompare2(a, b) result(c)
      REAL, intent(in) :: a(:), b(:)
      INTEGER :: I
      DO I=1,SIZE(a)
        c=a(I)<b(I)
        IF(.NOT. a(I)==b(I))RETURN
      ENDDO
    END FUNCTION
    
    LOGICAL FUNCTION dcompare2(a, b) result(c)
      REAL*8, intent(in) :: a(:), b(:)
      INTEGER :: I
      DO I=1,SIZE(a)
        c=a(I)<b(I)
        IF(.NOT. a(I)==b(I))RETURN
      ENDDO
    END FUNCTION
    
    RECURSIVE SUBROUTINE rquicksort(a,f,l)
      IMPLICIT NONE
      REAL a(:)
      REAL x, t
      INCLUDE 'quicksort_include.f03'
    END SUBROUTINE rquicksort
    
    RECURSIVE SUBROUTINE rquicksort2(a,f,l)
      IMPLICIT NONE
      REAL a(:,:)
      REAL x(size(a,2)), t(size(a,2))
      INCLUDE 'quicksort_include2.f03'
    END SUBROUTINE rquicksort2
    
    RECURSIVE SUBROUTINE dquicksort(a,f,l)
      IMPLICIT NONE
      REAL*8 a(:)
      REAL*8 x, t
      INCLUDE 'quicksort_include.f03'
    END SUBROUTINE dquicksort
    
    RECURSIVE SUBROUTINE dquicksort2(a,f,l)
      IMPLICIT NONE
      REAL*8 a(:,:)
      REAL*8 x(size(a,2)), t(size(a,2))
      INCLUDE 'quicksort_include2.f03'
    END SUBROUTINE dquicksort2    
END MODULE