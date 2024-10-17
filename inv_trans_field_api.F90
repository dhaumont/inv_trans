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

  CALL PRINT_DEBUG_HEADER("DIMENSIONS")
  
  WRITE(*,'(A11,I4,A2)') "NPROMA   | ", NPROMA,  ' |'
  WRITE(*,'(A11,I4,A2)') "NBLKS    | ", IGPBLKS, ' |'
  WRITE(*,'(A11,I4,A2)') "NSPECS   | ", ISPEC2,  ' |'
  WRITE(*,'(A11,L4,A2)') "LDUVDER  | ", LDUVDER,  ' |'
  WRITE(*,'(A11,L4,A2)') "LDVORGP  | ", LDVORGP,  ' |'
  WRITE(*,'(A11,L4,A2)') "LDDIVGP  | ", LDDIVGP,  ' |'
  WRITE(*,'(A11,L4,A2)') "LDSCDERS | ", LDSCDERS,  ' |'
  CALL PRINT_DEBUG_FOOTER()
  
  CALL PRINT_DEBUG_HEADER("Input spectral vector fields:", SIZE(SPVORS))
  
  IF (PRESENT(SPVORS)) THEN
    CALL PRINT_DEBUG_FIELDS(SPVORS, "SPVORS", (/"NSPECS ", "NLEV   "/))
    CALL PRINT_DEBUG_SUM(IFLDSUV)
  ENDIF

  IF (PRESENT(SPVORS)) THEN 
    CALL PRINT_DEBUG_FIELDS(SPDIVS, "SPDIVS", (/"NSPECS ", "NLEV   "/))
    CALL PRINT_DEBUG_SUM(IFLDSUV)
  ENDIF

  CALL PRINT_DEBUG_SEPARATOR()
  CALL PRINT_DEBUG_2D(ZSPVOR, "ZSPVOR", "| NSPECS | NFIELDS|")
  CALL PRINT_DEBUG_2D(ZSPDIV, "ZSPDIV")
  
  CALL PRINT_DEBUG_HEADER("Input spectral scalar fields:", SIZE(SPSCALARS))
  
  IF (PRESENT(SPSCALARS)) THEN
    CALL PRINT_DEBUG_FIELDS(SPSCALARS,"SPSCALARS", (/"NSPECS ", "NLEV   "/))  
    CALL PRINT_DEBUG_SUM(IFLDS)
  ENDIF
    
  CALL PRINT_DEBUG_SEPARATOR()
  CALL PRINT_DEBUG_2D(ZSPSCALAR, "ZSPSCALAR", "| NSPECS | NFIELDS|")
  CALL PRINT_DEBUG_FOOTER()
  
  CALL PRINT_DEBUG_HEADER("Output grid-point vector fields:", SIZE(US))
  
  IF (PRESENT(US)) THEN
    CALL PRINT_DEBUG_FIELDS(US,"US", (/"NPROMA ","NLEV   ", "NBLKS  "/))
    CALL PRINT_DEBUG_SUM(IFLDGUV)
  ENDIF

  IF (PRESENT(US))  THEN
   CALL PRINT_DEBUG_FIELDS(US,"VS", (/"NPROMA ","NLEV   ", "NBLKS  "/))
   CALL PRINT_DEBUG_SUM(IFLDGUV)
  ENDIF
  
  
  IF (PRESENT(VORS)) CALL PRINT_DEBUG_FIELDS(VORS,"VORS")
  IF (PRESENT(DIVS)) CALL PRINT_DEBUG_FIELDS(DIVS,"DIVS")
  IF (PRESENT(DUS)) CALL PRINT_DEBUG_FIELDS(DUS, "DUS")
  IF (PRESENT(DVS)) CALL PRINT_DEBUG_FIELDS(DVS, "DVS")

  CALL PRINT_DEBUG_SEPARATOR()
  CALL PRINT_DEBUG_4D(ZGPUV, "ZGPUV", "| NPROMA | NLEVS  | NFIELDS| NBLKS  |")
  CALL PRINT_DEBUG_FOOTER()
  
  CALL PRINT_DEBUG_HEADER("Output grid-point scalar fields:", SIZE(SCALARS))
  IF (PRESENT(SCALARS)) CALL PRINT_DEBUG_FIELDS(SCALARS, "SCALARS", (/"NPROMA ", "NLEV   ", "NBLKS  "/))
  IF (PRESENT(DSCALARS)) CALL PRINT_DEBUG_FIELDS(DSCALARS, "DSCALARS")
  IF (PRESENT(DSCALARS_NS)) CALL PRINT_DEBUG_FIELDS(DSCALARS_NS,"DSCALARS_NS")
  
  CALL PRINT_DEBUG_SUM(IFLDG)

  CALL PRINT_DEBUG_SEPARATOR()
  CALL PRINT_DEBUG_3D(ZGP,  "ZGP", "| NPROMA | NLEVS  | NBLKS  |")
  CALL PRINT_DEBUG_FOOTER()
  

END SUBROUTINE PRINT_DEBUG

SUBROUTINE PRINT_DEBUG_1D(A1, NAME, HEADER)
  REAL(KIND=JPRB),INTENT(IN) :: A1(:)
  CHARACTER(LEN=*), INTENT(IN) :: NAME
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: HEADER

END SUBROUTINE PRINT_DEBUG_1D

SUBROUTINE PRINT_DEBUG_2D(A2, NAME, HEADER)
  REAL(KIND=JPRB),INTENT(IN) :: A2(:,:)
  CHARACTER(LEN=*), INTENT(IN) :: NAME
  CHARACTER(LEN=*), OPTIONAL,INTENT(IN) :: HEADER
  IF (PRESENT(HEADER))  WRITE(*,'(8X,A)') HEADER
  WRITE(*,'(A7,A3,I6,A3,I6, A3, I6, A2)') NAME, " | ", &
        & UBOUND(A2,1) - LBOUND(A2,1) + 1, " | " , &
        & UBOUND(A2,2) - LBOUND(A2,2) + 1, " |"
END SUBROUTINE PRINT_DEBUG_2D

SUBROUTINE PRINT_DEBUG_3D(A3, NAME, HEADER)
    REAL(KIND=JPRB),INTENT(IN) :: A3(:,:,:)
    CHARACTER(LEN=*), INTENT(IN) :: NAME
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: HEADER
  IF (PRESENT(HEADER))WRITE(*,'(8X,A)') HEADER
  WRITE(*,'(A7,A3,I6,A3,I6, A3, I6, A2)') NAME, " | ", &
        & UBOUND(A3,1) - LBOUND(A3,1) + 1, " | " , &
        & UBOUND(A3,2) - LBOUND(A3,2) + 1, " | ", &
        & UBOUND(A3,3) - LBOUND(A3,3) + 1, " |"
END SUBROUTINE PRINT_DEBUG_3D

SUBROUTINE PRINT_DEBUG_4D(A4, NAME, HEADER)
  REAL(KIND=JPRB),INTENT(IN) :: A4(:,:,:,:)
  CHARACTER(LEN=*), INTENT(IN) :: NAME
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: HEADER
  IF (PRESENT(HEADER))WRITE(*,'(8X,A)') HEADER
  WRITE(*,'(A7,A3,I6,A3,I6, A3, I6, A3, I6, A2)') NAME, " | ", &
        & UBOUND(A4,1) - LBOUND(A4,1) + 1, " | ", &
        & UBOUND(A4,2) - LBOUND(A4,2) + 1, " | ", &
        & UBOUND(A4,3) - LBOUND(A4,3) + 1, " | ", &
        & UBOUND(A4,4) - LBOUND(A4,4) + 1, " |"
  
END SUBROUTINE PRINT_DEBUG_4D

SUBROUTINE PRINT_DEBUG_HEADER(DESCRIPTION, NUMBER)
  CHARACTER(LEN=*), INTENT(IN) :: DESCRIPTION
  INTEGER, INTENT(IN), OPTIONAL :: NUMBER

    WRITE(*,*) "-----------------------------------------------"
    IF (PRESENT(NUMBER)) THEN
      WRITE(*,*) "#", DESCRIPTION, NUMBER, "#"
    ELSE
      WRITE(*,*) "#", DESCRIPTION, "#"
    ENDIF
    WRITE(*,*) "-----------------------------------------------"
    
END SUBROUTINE PRINT_DEBUG_HEADER

SUBROUTINE PRINT_DEBUG_FOOTER()
  WRITE(*,*) "-----------------------------------------------"
  WRITE(*,*) ""
END SUBROUTINE PRINT_DEBUG_FOOTER

SUBROUTINE PRINT_DEBUG_SEPARATOR()
  WRITE(*,*) "............."
  WRITE(*,*) ""
END SUBROUTINE PRINT_DEBUG_SEPARATOR

SUBROUTINE PRINT_DEBUG_SUM(N)
  INTEGER, INTENT(IN) :: N
  WRITE(*,'(31X,A8)') "========"
  WRITE(*,'(33X,I4)')  N

END SUBROUTINE PRINT_DEBUG_SUM
END MODULE INV_TRANS_FIELD_API_MODULE
