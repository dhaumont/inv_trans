MODULE PRINT_DEBUG_FIELD_API

  USE PARKIND1  ,ONLY : JPIM     ,JPRB
  USE FIELD_MODULE

  IMPLICIT NONE

  CONTAINS

  SUBROUTINE PRINT_DEBUG_FIELDS(YLFL, FIELD_NAME, DIM_NAMES)
    TYPE (FIELD_BASIC_PTR) :: YLFL (:)
    INTEGER(KIND=JPIM) ::  JFLD
    INTEGER(KIND=JPIM) :: ILBOUNDS (5), IUBOUNDS (5)
    CHARACTER(LEN=*) :: FIELD_NAME
    CHARACTER(LEN=*), OPTIONAL :: DIM_NAMES(:)
    INTEGER :: N_DIMS
  
    N_DIMS = 0

    IF (PRESENT(DIM_NAMES)) THEN
      N_DIMS = SIZE(DIM_NAMES)
    ENDIF
  
    IF (N_DIMS == 1 ) THEN
      WRITE (*, "(A16,8X,A2, A4,A2)") &
            & FIELD_NAME, "| ", DIM_NAMES(1), " |" 
    ELSE IF (N_DIMS == 2 ) THEN
      WRITE (*, "(A16,8X,A2, A4,A3, A4,A2)") &
              & FIELD_NAME, "| ", DIM_NAMES(1), " | ", DIM_NAMES(2), " |" 
    ELSE IF (N_DIMS == 3 ) THEN
      WRITE (*, "(A16,8X,A2, A4,A3, A4,A3,A4, A2)") &
              & FIELD_NAME, "| ", &
              & DIM_NAMES(1), " | ", DIM_NAMES(2), " | ",  DIM_NAMES(3), " |"
  
    ENDIF

  DO JFLD = 1, SIZE (YLFL)
    
    SELECT TYPE (YLF => YLFL (JFLD)%PTR)
      CLASS IS (FIELD_1RB)
        CALL YLF%GET_DIMS (LBOUNDS=ILBOUNDS, UBOUNDS=IUBOUNDS)
           IF (N_DIMS == 2 ) THEN
          WRITE (*,"(A1, I3, A1, 1X, A9, 9X, &
              &      A1, 1X,I4, A10)") &
              &     "[",JFLD, "]", "FIELD_1RB",&
              &     "|", IUBOUNDS(1)-ILBOUNDS(1) + 1, ' |   (1)| '
        ENDIF
      CLASS IS (FIELD_2RB)
        CALL YLF%GET_DIMS (LBOUNDS=ILBOUNDS, UBOUNDS=IUBOUNDS)
        IF (N_DIMS == 3 ) THEN
          WRITE (*,"(A1, I3, A1, 1X, A9, 9X,&
                 &  A2, I4, A10, I4 ,A2)") &
                 & "[",JFLD, "]", "FIELD_2RB",  &
                 &  "| ", IUBOUNDS(1)-ILBOUNDS(1) + 1, ' |   (1)| ', IUBOUNDS(2)-ILBOUNDS(2) + 1," |"
        ELSE IF (N_DIMS == 2 ) THEN
          WRITE (*,"(A1, I3, A1, 1X,  A9, 9X, &
                  &  A2, I4,A3,I4,A2)") &
                  & "[",JFLD, "]", "FIELD_2RB",& 
                  & "| ",IUBOUNDS(1)-ILBOUNDS(1) + 1, " | ",IUBOUNDS(2)-ILBOUNDS(2) + 1," |"
          
        ENDIF
      CLASS IS (FIELD_3RB)
        CALL YLF%GET_DIMS (LBOUNDS=ILBOUNDS, UBOUNDS=IUBOUNDS)

        WRITE (*,"(A1, I3, A1, 1X,  A9, 9X, &
                &  A2, I4,A3,I4,A3,I4,A2)") &
               & "[",JFLD, "]", "FIELD_3RB",& 
              & "| ",IUBOUNDS(1)-ILBOUNDS(1) + 1, " | ",IUBOUNDS(2)-ILBOUNDS(2) + 1," | ",IUBOUNDS(3)-ILBOUNDS(3) + 1, " |"

      CLASS DEFAULT
        STOP 1
    END SELECT
  
  ENDDO
END SUBROUTINE PRINT_DEBUG_FIELDS

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

SUBROUTINE PRINT_DEBUG_INV_TRANS_FIELD_API(ZSPVOR, ZSPDIV, ZSPSCALAR, ZGPUV, ZGP, SPVORS,SPDIVS,SPSCALARS, &
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

END SUBROUTINE PRINT_DEBUG_INV_TRANS_FIELD_API
 
END MODULE PRINT_DEBUG_FIELD_API
  