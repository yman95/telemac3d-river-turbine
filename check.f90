MODULE CheckModule
  IMPLICIT NONE
  INTERFACE Check
    MODULE PROCEDURE checki
    MODULE PROCEDURE checkr
    MODULE PROCEDURE checkd  
    MODULE PROCEDURE icheck
    MODULE PROCEDURE rcheck
    MODULE PROCEDURE dcheck
    MODULE PROCEDURE icheck2
    MODULE PROCEDURE rcheck2
    MODULE PROCEDURE dcheck2   
  END INTERFACE Check
  
  CONTAINS
    FUNCTION checki(a,b)
  		INTEGER, intent(in) :: a,b
  		LOGICAL checki
  		checki=a.eq.b
    END FUNCTION checki
   
    FUNCTION checkr(a,b)
  		REAL, intent(in) :: a,b
  		LOGICAL checkr
  		checkr=a.eq.b
    END FUNCTION checkr
    
    FUNCTION checkd(a,b)
  		REAL*8, intent(in) :: a,b
  		LOGICAL checkd
  		checkd=a.eq.b
    END FUNCTION checkd        

    FUNCTION icheck(a,b)
  		INTEGER, intent(in) :: a(:),b(:)
  		LOGICAL c(size(a))
  		LOGICAL icheck
  		WHERE(a == b)c=.TRUE.
  		icheck=ALL(c)
    END FUNCTION icheck
    
    FUNCTION rcheck(a,b)
  		REAL, intent(in) :: a(:),b(:)
  		LOGICAL c(size(a))
  		LOGICAL rcheck
  		WHERE(a == b)c=.TRUE.
  		rcheck=ALL(c)
    END FUNCTION rcheck
    
    FUNCTION dcheck(a,b)
		  REAL*8, intent(in) :: a(:),b(:)
  		LOGICAL c(size(a))
  		LOGICAL dcheck
  		WHERE(a == b)c=.TRUE.
  		dcheck=ALL(c)
    END FUNCTION dcheck

    FUNCTION icheck2(a,b)
  		INTEGER, intent(in) :: a(:,:),b(:,:)
  		LOGICAL c(size(a,1),size(a,2))
  		LOGICAL icheck2
  		WHERE(a == b)c=.TRUE.
  		icheck2=ALL(c)
    END FUNCTION icheck2
    
    FUNCTION rcheck2(a,b)
  		REAL, intent(in) :: a(:,:),b(:,:)
  		LOGICAL c(size(a,1),size(a,2))
  		LOGICAL rcheck2
  		WHERE(a == b)c=.TRUE.
  		rcheck2=ALL(c)
    END FUNCTION rcheck2
    
    FUNCTION dcheck2(a,b)
		  REAL*8, intent(in) :: a(:,:),b(:,:)
  		LOGICAL c(size(a,1),size(a,2))
  		LOGICAL dcheck2
  		WHERE(a == b)c=.TRUE.
  		dcheck2=ALL(c)
    END FUNCTION dcheck2    
    
END MODULE