C=======================================================================
      SUBROUTINE FLOOD_EVAP(XLAI, EO, EF)

!-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL EO, EF, EF85, XLAI

!-----------------------------------------------------------------------
!     EF = EO*(1.0-0.45*XLAI)
      EF = EO*(1.0-0.53*XLAI)  !prevents discontinuity
      IF (XLAI .GT. 0.85) THEN
!       EF85 = EO * 0.62  !EF at XLAI=0.85, prevents discontinuity
!       EF = AMIN1(EF85, EO/1.1*EXP(-0.60*XLAI))
        EF = EO/1.1*EXP(-0.60*XLAI)
      ENDIF

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE FLOOD_EVAP
C=======================================================================
