C=======================================================================
      SUBROUTINE ESUP(EOS, SUMES1, SUMES2, U, ES, T)
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
!     INPUT VARIABLES:
      REAL EOS, U
!-----------------------------------------------------------------------
!     INPUT/OUTPUT VARIABLES:
      REAL SUMES1
!-----------------------------------------------------------------------
!     OUTPUT VARIABLES:
      REAL ES, SUMES2, T
!-----------------------------------------------------------------------
C  Calculate stage 1 soil evaporation
C    If the sum for stage 1 soil evaporation (SUMES1) is larger than
C    stage 1 evaporation limit (U), start stage 2 soil evaporation (SUMES2)
C    and adjust soil evaporation (ES)
C-----------------------------------------------------------------------
      SUMES1 = SUMES1 + EOS
      IF (SUMES1 .GT. U) THEN
        ES = EOS - 0.4 * (SUMES1 - U)
        SUMES2 = 0.6 * (SUMES1 - U)
        T = (SUMES2/3.5)**2
        SUMES1 = U
      ELSE
        ES = EOS
      ENDIF

      END SUBROUTINE ESUP
