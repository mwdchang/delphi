C=======================================================================

      REAL FUNCTION DECL(DAYL,XLAT)

!-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL DAYL,DL,PI,RAD,SC,XLAT1,XLAT
      PARAMETER (PI=3.14159, RAD=PI/180.0)

!-----------------------------------------------------------------------
C     Limit value of XLAT and DL to prevent zeros.

      IF (XLAT .GE. 0.0 .AND. XLAT .LT. 0.1) THEN
        XLAT1 = 0.1
        DL = 12.00573
      ELSE IF (XLAT .LT. 0.0 .AND. XLAT .GT. -0.1) THEN
        XLAT1 = -0.1
        DL = 11.99427
      ELSE
        DL = DAYL
        XLAT1 = XLAT
      ENDIF

C     Calculation of declination of sun.

      SC = SIN((DL-12.0)/24.0*PI)
      XLAT1 = TAN(RAD*XLAT1)
      DECL = ATAN(SC/XLAT1) / RAD

      RETURN
      END FUNCTION DECL

C=======================================================================
