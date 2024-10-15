MODULE INV_TRANS_FIELD_API_MODULE
USE FIELD_MODULE
USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

CONTAINS

! Spectral to grid space transformation
SUBROUTINE INV_TRANS_FIELD_API(SPVORS,SPDIVS,SPSCALARS, &
                             & US, VS, VORS,DIVS,SCALARS, &
                             & DUS, DVS, DSCALARS, DSCALARS_NS, &
                             & VSETUVS, VSETS, &
                             & LDACC) 

TYPE(FIELD_BASIC_PTR),OPTIONAL  :: SPVORS(:), SPDIVS(:)        !Spectral vector fields : vorticity and divergence fields (in)
TYPE(FIELD_BASIC_PTR),OPTIONAL  :: SPSCALARS(:)                !Spectral scalar fields (in)

TYPE(FIELD_BASIC_PTR),OPTIONAL  :: US(:),VS(:)                 !Grid vector fields     (out)
TYPE(FIELD_BASIC_PTR),OPTIONAL  :: VORS(:),DIVS(:)             !Grid vector fields :vorticity and divergence     (out)
TYPE(FIELD_BASIC_PTR),OPTIONAL  :: SCALARS(:)                  !Grid scalar fields     (out)

TYPE(FIELD_BASIC_PTR),OPTIONAL  :: DUS(:),DVS(:)               !Grid vector fields derivatives EW (out)
TYPE(FIELD_BASIC_PTR),OPTIONAL  :: DSCALARS(:), DSCALARS_NS(:) !Grid scalar fields derivatives NS(out)

INTEGER(KIND=JPIM),OPTIONAL     :: VSETUVS(:)                   !Meta data vector fields
INTEGER(KIND=JPIM),OPTIONAL     :: VSETS(:)                     !Meta data scalar fields

LOGICAL, OPTIONAL :: LDACC
! Local variables

! Temporary arrays

TYPE(FIELD_1RB_VIEW), ALLOCATABLE  :: SPVORL(:), SPDIVL(:)         
TYPE(FIELD_1RB_VIEW), ALLOCATABLE  :: SPSCALARL(:)                 

TYPE(FIELD_2RB_VIEW), ALLOCATABLE  :: UL(:),VL(:)                  
TYPE(FIELD_1RB_VIEW), ALLOCATABLE  :: VORL(:),DIVL(:)              
TYPE(FIELD_2RB_VIEW), ALLOCATABLE  :: SCALARL(:)                   

TYPE(FIELD_2RB_VIEW), ALLOCATABLE :: DUL(:),DVL(:)                 
TYPE(FIELD_2RB_VIEW), ALLOCATABLE :: DSCALARL(:), DSCALARS_NL(:) 


REAL(KIND=JPRB),ALLOCATABLE :: ZSPVOR(:,:),ZSPDIV(:,:)              ! Spectral vector fields (in)
REAL(KIND=JPRB),ALLOCATABLE :: ZSPSCALAR(:,:)                       ! Spectral scalar fields (in)
REAL(KIND=JPRB),ALLOCATABLE :: ZGPUV(:,:,:,:)                       ! Grid vector fields (out)
REAL(KIND=JPRB),ALLOCATABLE :: ZGP(:,:,:)                           ! Grid scalar fields (out)

INTEGER(KIND=JPIM)          :: IFLDSUV                              ! Number of input spectral vector fields
INTEGER(KIND=JPIM)          :: IFLDS                                ! Number of input spectral scalar fields
INTEGER(KIND=JPIM)          :: IFLDG                                ! Number of output scalar fields
INTEGER(KIND=JPIM)          :: IFLDGUV                              ! Number of output vector fields

INTEGER(KIND=JPIM)          :: ISPEC2                               ! Size of spectral fields (truncation)
INTEGER(KIND=JPIM)          :: IPROMA,IGPBLKS                       ! Size of output grid fields

INTEGER(KIND=JPIM)          :: JFLD                                 ! field counter
LOGICAL          :: LDSCDERS                             ! indicating if derivatives of scalar variables are req.
LOGICAL          :: LDVORGP                              ! indicating if grid-point vorticity is req.
LOGICAL          :: LDDIVGP                              ! indicating if grid-point divergence is req.
LOGICAL          :: LDUVDER                              ! indicating if E-W derivatives of u and v are req.

#include "inv_trans.h"

! 1. VECTOR FIELDS TRANSFORMATION

! Check if all provided vector field information is consistent
IF (PRESENT(US) .NEQV. PRESENT(VS))STOP "US/VS"
IF (PRESENT(US) .NEQV. PRESENT(SPVORS))STOP "US/VORS"
IF (PRESENT(US) .NEQV. PRESENT(SPDIVS))STOP "US/DIVS"
IF (PRESENT(US) .NEQV. PRESENT(VSETUVS))STOP "US/VSETUVS"

! Do we have vector fields?
IF (PRESENT(US)) THEN
  IF ((SIZE(US)/= SIZE(VS)).OR.(SIZE(US)/= SIZE(SPDIVS)).OR.(SIZE(US)/= SIZE(SPVORS))) THEN
    PRINT*,"INVALID SIZES:"
    PRINT*," US = ",SIZE(US)
    PRINT*," VS = ",SIZE(VS)
    PRINT*," VORS = ",SIZE(SPVORS)
    PRINT*," DIVS = ",SIZE(SPDIVS)    
    STOP 1
  ENDIF

  SPVORL = LS (SPVORS, LDACC)
  SPDIVL = LS (SPDIVS, LDACC)

  UL = LG (US)
  VL = LG (VS)

  IF ((SIZE (UL) /= SIZE (VL)) .OR. (SIZE (VORL) /= SIZE (DIVL)) .OR. (SIZE (UL) /= SIZE (VSETUVS))) THEN
    PRINT*,"INVALID SIZES:"
    PRINT *, " UL = ", SIZE (UL)
    PRINT *, " VL = ", SIZE (VL)
    PRINT *, " VORL = ", SIZE (VORL)
    PRINT *, " DIVL = ", SIZE (DIVL)
    PRINT *, " VSETUVS = ", SIZE (VSETUVS)
    STOP 1
  ENDIF

  IPROMA = SIZE(UL(1)%V,1)
  IGPBLKS = SIZE(UL(1)%V,2)
  
  IFLDGUV = SIZE(UL)         ! Number of vector fields
  ISPEC2 = SIZE(SPVORL(1)%V,1) ! Size of spectral fields
  IFLDSUV = SIZE(SPVORL)       ! Number of input spectral vector fields
    
  ! Allocate vector field input in spectral space
  ALLOCATE(ZSPVOR(ISPEC2,IFLDSUV))  
  ALLOCATE(ZSPDIV(ISPEC2,IFLDSUV))

  ! Allocate vector field output in grid space
  ALLOCATE(ZGPUV(IPROMA,IFLDGUV,2,IGPBLKS)) 
  
  ! Copy from fields to temporary arrays (1D copy thanks to FIELD VIEW)
  DO JFLD=1,IFLDSUV
    ZSPVOR(:,JFLD) = SPVORL(JFLD)%V(:)
    ZSPDIV(:,JFLD) = SPDIVL(JFLD)%V(:)
  ENDDO
ELSE
  ! US is not provided, we do not have to compute the corresponding vector output
  IFLDSUV = 0
  ALLOCATE(ZGPUV(0,IFLDSUV,2,0),ZSPVOR(0,0),ZSPDIV(0,0))
ENDIF

! 2. SCALAR FIELDS TRANSFORMATION

! Check if all provided scalar field information is consistent
IF (PRESENT(SCALARS) .NEQV. PRESENT(SPSCALARS))STOP "GRID/SPEC"
IF (PRESENT(SCALARS) .NEQV. PRESENT(VSETS))STOP "GRID/VSETS"

IF (PRESENT(SCALARS)) THEN

  IF ((SIZE(SCALARS)/= SIZE(SPSCALARS))) THEN
    PRINT*," SIZE(GRID) = ",SIZE(SCALARS)
    PRINT*," SIZE(SPEC) = ",SIZE(SPSCALARS)    
    STOP 1
  ENDIF


  SCALARL = LG (SCALARS, LDACC)
  SPSCALARL = LS (SPSCALARS, LDACC)

  IF (SIZE (SCALARL) /= SIZE (VSETS)) THEN
    PRINT *, " SIZE (SCALARL) = ", SIZE (SCALARL)
    PRINT *, " SIZE (SPSCALARS) = ", SIZE (SPSCALARS)
    PRINT *, " SIZE (VSETS) = ", SIZE (VSETS)
    STOP 1
  ENDIF

  IPROMA = SIZE(SCALARL(1)%V,1)
  IGPBLKS = SIZE(SCALARL(1)%V,2)
  IFLDG = SIZE(SCALARL) ! Number of output scalar fields in grid space

  ISPEC2 = SIZE(SPSCALARL(1)%V,1)
  IFLDS = SIZE(SPSCALARL)  ! Number of input scalar fields in spectral space

  ! Allocate scalar field input in spectral space
  ALLOCATE(ZSPSCALAR(ISPEC2,IFLDS))
  
  ! Allocate scalar field output in grid space
  ALLOCATE(ZGP(IPROMA,IFLDG,IGPBLKS))
  
 ! Copy scalar spectral fields to temporary arrays (1D copy thanks to FIELD VIEW)
  DO JFLD=1,IFLDS
    ZSPSCALAR(:,JFLD) = SPSCALARL(JFLD)%V(:)
  ENDDO
ELSE
  IFLDG = 0
  ALLOCATE(ZGP(0,IFLDG,0),ZSPSCALAR(0,0))
ENDIF

! 3. CALL INV_TRANS

CALL INV_TRANS(PSPVOR = ZSPVOR,PSPDIV = ZSPDIV,PGPUV = ZGPUV,KVSETUV = VSETUVS, &
             & PSPSC2 = ZSPSCALAR,PGP2 = ZGP,KVSETSC2 = VSETS, &
             & KPROMA = IPROMA, NGPBLKS = IGPBLKS) 

! 4. Copy back data to fields

! Copy vector fields back from temporary vector arrays
DO JFLD=1,IFLDGUV
  UL(JFLD)%V(:,:) = ZGPUV(:,JFLD,1,:)
  VL(JFLD)%V(:,:) = ZGPUV(:,JFLD,2,:)
ENDDO

! Copy scalar fields back from temporary scalar arrays
DO JFLD=1,IFLDG
  SCALARL(JFLD)%V(:,:) = ZGP(:,JFLD,:)
ENDDO

END SUBROUTINE INV_TRANS_FIELD_API

END MODULE INV_TRANS_FIELD_API_MODULE