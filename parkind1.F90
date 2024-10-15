MODULE PARKIND1
  !
  !     *** Define usual kinds for strong typing ***
  !
  IMPLICIT NONE
  SAVE
  !
  !     Integer Kinds
  !     -------------
  INTEGER, PARAMETER :: JPIM = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER :: JPIB = SELECTED_INT_KIND(12)
  INTEGER, PARAMETER :: JPRB = SELECTED_REAL_KIND(13,300)
  
  END MODULE PARKIND1
  