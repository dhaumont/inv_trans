MODULE INV_TRANS_FIELD_API_MODULE
USE FIELD_MODULE
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE PRINT_DEBUG_FIELD_API

IMPLICIT NONE

ABSTRACT INTERFACE

SUBROUTINE FSPGL_INTF (KM,KSL,KDGL,KFIELDS,PR1MU2,PFIELD,&
                     & KPTRU,KFLDUV,KFLDSC,KFLDPTRUV)

USE PARKIND1  ,ONLY : JPIM     ,JPRB

INTEGER(KIND=JPIM),INTENT(IN)           :: KM
INTEGER(KIND=JPIM),INTENT(IN)           :: KSL
INTEGER(KIND=JPIM),INTENT(IN)           :: KDGL
REAL(KIND=JPRB)   ,INTENT(IN)           :: PR1MU2(KDGL)
INTEGER(KIND=JPIM),INTENT(IN)           :: KFIELDS
REAL(KIND=JPRB)   ,INTENT(INOUT),TARGET :: PFIELD(2*KFIELDS,0:KDGL+1)
INTEGER(KIND=JPIM),INTENT(IN)           :: KPTRU
INTEGER(KIND=JPIM),INTENT(IN)           :: KFLDUV
INTEGER(KIND=JPIM),INTENT(IN)           :: KFLDSC
INTEGER(KIND=JPIM),INTENT(IN)           :: KFLDPTRUV(KFLDUV)

END SUBROUTINE FSPGL_INTF

END INTERFACE

CONTAINS

! Spectral to grid space transformation
SUBROUTINE INV_TRANS_FIELD_API(SPVORS,SPDIVS,SPSCALARS, &
                             & US, VS, VORS,DIVS,SCALARS, &
                             & DUS, DVS, DSCALARS, DSCALARS_NS, &
                             & VSETUVS, VSETS, &
                             & LDACC, LDVERBOSE, &
			                       & FSPGL_PROC)
			                      

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
LOGICAL, OPTIONAL :: LDVERBOSE
PROCEDURE (FSPGL_INTF), OPTIONAL :: FSPGL_PROC

! Local variables

! Temporary arrays

TYPE(FIELD_1RB_VIEW), ALLOCATABLE  :: SPVORL(:), SPDIVL(:)         
TYPE(FIELD_1RB_VIEW), ALLOCATABLE  :: SPSCALARL(:)                 

TYPE(FIELD_2RB_VIEW), ALLOCATABLE  :: UL(:),VL(:)                  
TYPE(FIELD_2RB_VIEW), ALLOCATABLE  :: VORL(:),DIVL(:)              
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
INTEGER(KIND=JPIM)          :: KFLDG                                ! Size of output scalar fields array
INTEGER(KIND=JPIM)          :: IFLDGUV                              ! Number of output vector fields
INTEGER(KIND=JPIM)          :: KFLDGUV                              ! Size of output vector fields array

INTEGER(KIND=JPIM)          :: OFFSET                             
INTEGER(KIND=JPIM)          :: ISPEC2                               ! Size of spectral fields (truncation)
INTEGER(KIND=JPIM)          :: IPROMA,IGPBLKS                       ! Size of output grid fields

INTEGER(KIND=JPIM)          :: JFLD                                 ! field counter
LOGICAL          :: LDSCDERS                                        ! indicating if derivatives of scalar variables are req.
LOGICAL          :: LDVORGP                                         ! indicating if grid-point vorticity is req.
LOGICAL          :: LDDIVGP                                         ! indicating if grid-point divergence is req.
LOGICAL          :: LDUVDER                                         ! indicating if E-W derivatives of u and v are req.
LOGICAL          :: LLVERBOSE                                       ! indicating if verbose output is req.
#include "inv_trans.h"


LLVERBOSE = .FALSE.
IF (PRESENT(LDVERBOSE))  LLVERBOSE = LDVERBOSE

! 1. VECTOR FIELDS TRANSFORMATION

! Check if all provided vector field information is consistent
IF (PRESENT(US) .NEQV. PRESENT(VS))STOP "US/VS"
IF (PRESENT(US) .NEQV. PRESENT(SPVORS))STOP "US/VORS"
IF (PRESENT(US) .NEQV. PRESENT(SPDIVS))STOP "US/DIVS"
IF (PRESENT(US) .NEQV. PRESENT(VSETUVS))STOP "US/VSETUVS"

KFLDGUV = 0
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

  IF ((SIZE (UL) /= SIZE (VL)) .OR. (SIZE (SPVORL) /= SIZE (SPDIVL)) .OR. (SIZE (UL) /= SIZE (VSETUVS))) THEN
    PRINT*,"INVALID SIZES:"
    WRITE(*,*) " UL = ", SIZE (UL)
    WRITE(*,*) " VL = ", SIZE (VL)
    WRITE(*,*) " SPVORL = ", SIZE (SPVORL)
    WRITE(*,*) " SPDIVL = ", SIZE (SPDIVL)
    WRITE(*,*) " VSETUVS = ", SIZE (VSETUVS)
    STOP 1
  ENDIF

  IPROMA = SIZE(UL(1)%V,1)
  IGPBLKS = SIZE(UL(1)%V,2)
  
  IFLDGUV = SIZE(UL)           ! Number of vector fields

  ISPEC2 = SIZE(SPVORL(1)%V,1) ! Size of spectral fields
  IFLDSUV = SIZE(SPVORL)       ! Number of input spectral vector fields
  
  LDVORGP = .FALSE.                              
  LDDIVGP = .FALSE.                             
  LDUVDER  = .FALSE.                            
  LDSCDERS = .FALSE. 

  KFLDGUV = KFLDGUV + 2


  IF (PRESENT(DUS) .AND. PRESENT(DVS))    THEN
     WRITE(*,*) "DUS/DVS PRESENT"
     LDUVDER = .TRUE.
     KFLDGUV = KFLDGUV + 2
  ENDIF
  IF (PRESENT(VORS)) THEN
    WRITE(*,*) "VORS PRESENT"
     LDVORGP = .TRUE.
     KFLDGUV = KFLDGUV + 1
  ENDIF
  IF (PRESENT(DIVS)) THEN
    WRITE(*,*) "DIVS PRESENT"
     LDUVDER = .TRUE.
     KFLDGUV = KFLDGUV + 1
  ENDIF
  
  ! Allocate vector field input in spectral space
  ALLOCATE(ZSPVOR(ISPEC2,IFLDSUV))  
  ALLOCATE(ZSPDIV(ISPEC2,IFLDSUV))

  ! Allocate vector field output in grid space
  ALLOCATE(ZGPUV(IPROMA,IFLDGUV,KFLDGUV,IGPBLKS)) 
  
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
    WRITE(*,*) " SIZE (SCALARL) = ", SIZE (SCALARL)
    WRITE(*,*) " SIZE (SPSCALARS) = ", SIZE (SPSCALARS)
    WRITE(*,*) " SIZE (VSETS) = ", SIZE (VSETS)
    STOP 1
  ENDIF

  IPROMA = SIZE(SCALARL(1)%V,1)
  IGPBLKS = SIZE(SCALARL(1)%V,2)
  IFLDG = SIZE(SCALARL) ! Number of output scalar fields in grid space

  ISPEC2 = SIZE(SPSCALARL(1)%V,1)
  IFLDS = SIZE(SPSCALARL)  ! Number of input scalar fields in spectral space
  
  KFLDG = IFLDG
  IF (PRESENT(DSCALARS) .AND. PRESENT(DSCALARS_NS)) THEN
    WRITE(*,*) "DSCALARS/DSCALARS_NS PRESENT"
     LDSCDERS = .TRUE.
     KFLDG = KFLDG + 2 * IFLDG
  ENDIF

  ! Allocate scalar field input in spectral space
  ALLOCATE(ZSPSCALAR(ISPEC2,IFLDS))
  
  ! Allocate scalar field output in grid space
  ALLOCATE(ZGP(IPROMA,KFLDG,IGPBLKS))
  
 ! Copy scalar spectral fields to temporary arrays (1D copy thanks to FIELD VIEW)
  DO JFLD=1,IFLDS
    ZSPSCALAR(:,JFLD) = SPSCALARL(JFLD)%V(:)
  ENDDO
ELSE
  IFLDG = 0
  ALLOCATE(ZGP(0,IFLDG,0),ZSPSCALAR(0,0))
ENDIF

! 3. CALL INV_TRANS

IF (LLVERBOSE)  THEN
  CALL PRINT_DEBUG_INV_TRANS_FIELD_API(ZSPVOR, ZSPDIV, ZSPSCALAR, ZGPUV, ZGP, &
                  & SPVORS,SPDIVS,SPSCALARS, &
                  & US, VS, VORS,DIVS,SCALARS, &
                  & DUS, DVS, DSCALARS, DSCALARS_NS, &
                  & VSETUVS, VSETS, IFLDSUV,IFLDS,IFLDG,IFLDGUV,LDSCDERS,LDVORGP,LDDIVGP, LDUVDER, &
                  & ISPEC2, IPROMA,IGPBLKS,KFLDG,KFLDGUV)
ENDIF

IF (PRESENT (FSPGL_PROC)) THEN
	CALL INV_TRANS(PSPVOR = ZSPVOR,PSPDIV = ZSPDIV,PGPUV = ZGPUV,KVSETUV = VSETUVS, &
	             & PSPSC2 = ZSPSCALAR,PGP2 = ZGP,KVSETSC2 = VSETS, &
	             & KPROMA = IPROMA,FSPGL_PROC=FSPGL_PROC, NGPBLKS = IGPBLKS) 
ELSE
	CALL INV_TRANS(PSPVOR = ZSPVOR,PSPDIV = ZSPDIV,PGPUV = ZGPUV,KVSETUV = VSETUVS, &
	             & PSPSC2 = ZSPSCALAR,PGP2 = ZGP,KVSETSC2 = VSETS, &
	             & KPROMA = IPROMA, NGPBLKS = IGPBLKS) 
ENDIF
! 4. Copy back data to fields

! Copy vector fields back from temporary vector arrays
OFFSET = 1
DO JFLD=1,IFLDGUV
  UL(JFLD)%V(:,:) = ZGPUV(:,JFLD,OFFSET,:)
  VL(JFLD)%V(:,:) = ZGPUV(:,JFLD,OFFSET + 1,:)
ENDDO
OFFSET = OFFSET + 2
! copy derivatives, divergences and vorticities back from temporary vector arrays

IF (LDUVDER) THEN
  DO JFLD=1,IFLDGUV
    DUL(JFLD)%V(:,:) = ZGPUV(:,JFLD,OFFSET ,:)
    DVL(JFLD)%V(:,:) = ZGPUV(:,JFLD,OFFSET + 1 ,:)
  ENDDO
  OFFSET = OFFSET + 2
ENDIF
  
IF (LDVORGP) THEN
  DO JFLD=1,IFLDGUV
    VORL(JFLD)%V(:,:) = ZGPUV(:,JFLD,OFFSET,:)
  ENDDO  
  OFFSET = OFFSET + 1
ENDIF

IF (LDDIVGP) THEN
  DO JFLD=1,IFLDGUV
    DIVL(JFLD)%V(:,:) = ZGPUV(:,JFLD,OFFSET,:)
  ENDDO
  OFFSET = OFFSET + 1
ENDIF

! Copy scalar fields back from temporary scalar arrays
DO JFLD=1,IFLDG
  SCALARL(JFLD)%V(:,:) = ZGP(:,JFLD,:)
ENDDO

OFFSET = IFLDG
IF (LDSCDERS) THEN
  DO JFLD=1,IFLDG
    DSCALARL(JFLD)%V(:,:) = ZGP(:,OFFSET+JFLD,:)
    DSCALARS_NL(JFLD)%V(:,:) = ZGP(:,2 * OFFSET+JFLD,:)
  ENDDO
ENDIF

END SUBROUTINE INV_TRANS_FIELD_API

END MODULE INV_TRANS_FIELD_API_MODULE
