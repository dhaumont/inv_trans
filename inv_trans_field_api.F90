MODULE INV_TRANS_FIELD_API_MODULE
USE FIELD_MODULE
USE PARKIND1  ,ONLY : JPIM     ,JPRB

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
    PRINT *, " UL = ", SIZE (UL)
    PRINT *, " VL = ", SIZE (VL)
    PRINT *, " SPVORL = ", SIZE (SPVORL)
    PRINT *, " SPDIVL = ", SIZE (SPDIVL)
    PRINT *, " VSETUVS = ", SIZE (VSETUVS)
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

  KFLDGUV = IFLDGUV


  IF (PRESENT(DUS) .AND. PRESENT(DVS))    THEN
     PRINT *, "DUS/DVS PRESENT"
     LDUVDER = .TRUE.
     KFLDGUV = KFLDGUV + 2 * IFLDGUV
  ENDIF
  IF (PRESENT(VORS)) THEN
    PRINT *, "VORS PRESENT"
     LDVORGP = .TRUE.
     KFLDGUV = KFLDGUV + IFLDGUV
  ENDIF
  IF (PRESENT(DIVS)) THEN
    PRINT *, "DIVS PRESENT"
     LDUVDER = .TRUE.
     KFLDGUV = KFLDGUV + IFLDGUV
  ENDIF
  
  ! Allocate vector field input in spectral space
  ALLOCATE(ZSPVOR(ISPEC2,IFLDSUV))  
  ALLOCATE(ZSPDIV(ISPEC2,IFLDSUV))

  ! Allocate vector field output in grid space
  ALLOCATE(ZGPUV(IPROMA,KFLDGUV,2,IGPBLKS)) 
  
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
  
  KFLDG = IFLDG
  IF (PRESENT(DSCALARS) .AND. PRESENT(DSCALARS_NS)) THEN
    PRINT *, "DSCALARS/DSCALARS_NS PRESENT"
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
  CALL PRINT_DEBUG(ZSPVOR, ZSPDIV, ZSPSCALAR, ZGPUV, ZGP, &
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
DO JFLD=1,IFLDGUV
  UL(JFLD)%V(:,:) = ZGPUV(:,JFLD,1,:)
  VL(JFLD)%V(:,:) = ZGPUV(:,JFLD,2,:)
ENDDO

! copy derivatives, divergences and vorticities back from temporary vector arrays
OFFSET = IFLDGUV
IF (LDUVDER) THEN
  DO JFLD=1,IFLDGUV
    DUL(JFLD)%V(:,:) = ZGPUV(:,OFFSET+JFLD,1,:)
    DVL(JFLD)%V(:,:) = ZGPUV(:,OFFSET+JFLD,2,:)
  ENDDO
  OFFSET = OFFSET + IFLDGUV
ENDIF
  
IF (LDVORGP) THEN
  DO JFLD=1,IFLDGUV
    VORL(JFLD)%V(:,:) = ZGPUV(:,OFFSET+JFLD,1,:)
  ENDDO
  OFFSET = OFFSET + IFLDGUV
ENDIF

IF (LDDIVGP) THEN
  DO JFLD=1,IFLDGUV
    DIVL(JFLD)%V(:,:) = ZGPUV(:,OFFSET+JFLD,1,:)
  ENDDO
  OFFSET = OFFSET + IFLDGUV
ENDIF

! Copy scalar fields back from temporary scalar arrays
DO JFLD=1,IFLDG
  SCALARL(JFLD)%V(:,:) = ZGP(:,JFLD,:)
ENDDO

OFFSET = IFLDG

IF (LDSCDERS) THEN
  DO JFLD=1,IFLDG
    DSCALARL(JFLD)%V(:,:) = ZGP(:,OFFSET+JFLD,:)
  ENDDO
  DO JFLD=1,IFLDG
    DSCALARS_NL(JFLD)%V(:,:) = ZGP(:,OFFSET+JFLD,:)
  ENDDO
  OFFSET = OFFSET + IFLDGUV
ENDIF

END SUBROUTINE INV_TRANS_FIELD_API

SUBROUTINE PRINT_DEBUG(ZSPVOR, ZSPDIV, ZSPSCALAR, ZGPUV, ZGP, SPVORS,SPDIVS,SPSCALARS, &
                      & US, VS, VORS,DIVS,SCALARS, &
                      & DUS, DVS, DSCALARS, DSCALARS_NS, &
                      & VSETUVS, VSETS, IFLDSUV,IFLDS,IFLDG,IFLDGUV,LDSCDERS,LDVORGP,LDDIVGP, LDUVDER, &
                      & ISPEC2, NPROMA,IGPBLKS,KFLDG,KFLDGUV)


REAL(KIND=JPRB),ALLOCATABLE :: ZSPVOR(:,:),ZSPDIV(:,:)              ! Spectral vector fields (in)
REAL(KIND=JPRB),ALLOCATABLE :: ZSPSCALAR(:,:)                       ! Spectral scalar fields (in)
REAL(KIND=JPRB),ALLOCATABLE :: ZGPUV(:,:,:,:)                       ! Grid vector fields (out)
REAL(KIND=JPRB),ALLOCATABLE :: ZGP(:,:,:)                           ! Grid scalar fields (out)

TYPE(FIELD_BASIC_PTR),OPTIONAL  :: SPVORS(:), SPDIVS(:)        !Spectral vector fields : vorticity and divergence fields (in)
TYPE(FIELD_BASIC_PTR),OPTIONAL  :: SPSCALARS(:)                !Spectral scalar fields (in)

TYPE(FIELD_BASIC_PTR),OPTIONAL  :: US(:),VS(:)                 !Grid vector fields     (out)
TYPE(FIELD_BASIC_PTR),OPTIONAL  :: VORS(:),DIVS(:)             !Grid vector fields :vorticity and divergence     (out)
TYPE(FIELD_BASIC_PTR),OPTIONAL  :: SCALARS(:)                  !Grid scalar fields     (out)

TYPE(FIELD_BASIC_PTR),OPTIONAL  :: DUS(:),DVS(:)               !Grid vector fields derivatives EW (out)
TYPE(FIELD_BASIC_PTR),OPTIONAL  :: DSCALARS(:), DSCALARS_NS(:) !Grid scalar fields derivatives NS(out)

INTEGER(KIND=JPIM),OPTIONAL     :: VSETUVS(:)                   !Meta data vector fields
INTEGER(KIND=JPIM),OPTIONAL     :: VSETS(:)                     !Meta data scalar fields


  INTEGER(KIND=JPIM), INTENT(IN)          :: IFLDSUV                              ! Number of input spectral vector fields
  INTEGER(KIND=JPIM), INTENT(IN)          :: IFLDS                                ! Number of input spectral scalar fields
  INTEGER(KIND=JPIM), INTENT(IN)          :: IFLDG                                ! Number of output scalar fields
  INTEGER(KIND=JPIM), INTENT(IN)          :: KFLDG                                ! Size of output scalar fields array
  INTEGER(KIND=JPIM), INTENT(IN)          :: IFLDGUV                              ! Number of output vector fields
  INTEGER(KIND=JPIM), INTENT(IN)          :: KFLDGUV                              ! Size of output vector fields array

  INTEGER(KIND=JPIM), INTENT(IN)          :: ISPEC2                               ! Size of spectral fields (truncation)
  INTEGER(KIND=JPIM), INTENT(IN)          :: NPROMA,IGPBLKS                       ! Size of NPROMA and number of blocs

  LOGICAL, INTENT(IN)          :: LDSCDERS                                        ! indicating if derivatives of scalar variables are req.
  LOGICAL, INTENT(IN)          :: LDVORGP                                         ! indicating if grid-point vorticity is req.
  LOGICAL, INTENT(IN)          :: LDDIVGP                                         ! indicating if grid-point divergence is req.
  LOGICAL , INTENT(IN)         :: LDUVDER                                         ! indicating if E-W derivatives of u and v are req.

  PRINT *, "NPROMA", NPROMA
  PRINT *, "NGPBLKS", IGPBLKS
  PRINT *, "ISPEC2 (Size of spectral fields)", ISPEC2
  PRINT *, "LDSCDERS (Compute grid-point derivatives of scalar fields)", LDSCDERS
  PRINT *, "LDUVDER  (Compute grid-point derivatives of vector fields)", LDUVDER
  PRINT *, "LDVORGP  (Compute grid-point vorticity)    ", LDVORGP
  PRINT *, "LDDIVGP  (Compute grid-point divergence)   ", LDDIVGP

  PRINT *, ""
  PRINT *, "Input spectral vector fields  "
  IF (PRESENT(SPVORS)) CALL PRINT_DEBUG_FIELDS(SPVORS, "SPVORS and SPDIVS (in)", (/"ISPEC2 ", "NLEV   "/))
  PRINT *, "                                          ========"
  PRINT *, "=> IFLDSUV:                         ", IFLDSUV
  PRINT *, "Size of ZSPVOR [SPEC, NFIELDS]", UBOUND(ZSPVOR) - LBOUND(ZSPVOR) + 1
  PRINT *, "Size of ZSPDIV [SPEC, NFIELDS]", UBOUND(ZSPDIV) - LBOUND(ZSPDIV) + 1

  PRINT *, ""
  PRINT *, "Input spectral scalar fields"
  IF (PRESENT(SPSCALARS)) CALL PRINT_DEBUG_FIELDS(SPSCALARS,"SPSCALARS(in)", (/"ISPEC2 ", "NLEV   "/))  
  PRINT *, "                                          ========"
  PRINT *, "=> IFLDS:                            ", IFLDS
  PRINT *, "Size of ZSPSCALAR [SPEC, NFIELDS]", UBOUND(ZSPSCALAR) - LBOUND(ZSPSCALAR) + 1

  PRINT *, ""
  PRINT *, "#Output grid-point vector fields"
  IF (PRESENT(US)) CALL PRINT_DEBUG_FIELDS(US,"US  and VS (out)", (/"NPROMA ","NLEV   ", "NGPBLKS"/))
  IF (PRESENT(VORS)) CALL PRINT_DEBUG_FIELDS(VORS,"VORS (out)")
  IF (PRESENT(DIVS)) CALL PRINT_DEBUG_FIELDS(DIVS,"DIVS (out)")
  IF (PRESENT(DUS)) CALL PRINT_DEBUG_FIELDS(DUS, "DUS (out)")
  IF (PRESENT(DVS)) CALL PRINT_DEBUG_FIELDS(DVS, "DVS (out)")
  PRINT *, "                                          ========"
  PRINT *, "=> IFLDGUV                          ", IFLDGUV
  PRINT *, "Size of PGPUV [NPROMA, NLEV, NFIELDS, NGPBLKS]", UBOUND(ZGPUV) - LBOUND(ZGPUV) + 1

  PRINT *, ""
  PRINT *, "#Output grid-point scalar fields"
  IF (PRESENT(SCALARS)) CALL PRINT_DEBUG_FIELDS(SCALARS, "SCALARS (out)", (/"NPROMA ", "NLEV   ", "NGPBLKS"/))
  IF (PRESENT(DSCALARS)) CALL PRINT_DEBUG_FIELDS(DSCALARS, "DSCALARS (out)")
  IF (PRESENT(DSCALARS_NS)) CALL PRINT_DEBUG_FIELDS(DSCALARS_NS,"DSCALARS_NS (out)")
  PRINT *, "                                          ========"
  PRINT *, "=> IFLDG                            ", IFLDG
  PRINT *, "Size of PGP [NPROMA, NLEV, NGPBLKS]", UBOUND(ZGP) - LBOUND(ZGP) + 1

  

  

END SUBROUTINE PRINT_DEBUG

END MODULE INV_TRANS_FIELD_API_MODULE
