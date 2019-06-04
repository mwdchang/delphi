C=======================================================================

      SUBROUTINE SOLAR(
     &    DAYL, DEC, SRAD, XLAT,                          !Input
     &    CLOUDS, ISINB, S0N)                             !Output

!-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL AMTRCS,AMTRD,CCOS,CLOUDS,DAYL,DEC,DSINB,ISINB,
     &  PI,RAD,SCLEAR,S0D,S0N,SC,SOC,SRAD,SRADJ,SSIN,XLAT
      PARAMETER (AMTRCS=0.77, PI = 3.14159, RAD = PI/180.0, SC=1368.0)

!-----------------------------------------------------------------------
C     Initialization
      SRADJ = SRAD * 1.0E6

C     Sun angles.  SOC limited for latitudes above polar circles.
      SSIN = SIN(RAD*DEC) * SIN(RAD*XLAT)
      CCOS = COS(RAD*DEC) * COS(RAD*XLAT)
      SOC = SSIN / CCOS
      SOC = MIN(MAX(SOC,-1.0),1.0)

C     Integral of SINB over day (sec) (Eqn. 18).
      DSINB = 3600.0 * (DAYL*SSIN + 24.0/PI*CCOS*SQRT(1.0-SOC**2))

C     Set normal (perp.) extra-terrestial radiation equal to
C     average solar constant (elliptical orbit ignored).
      S0N = SC

C     Calculate daily atmospheric transmission from global
C     and normal extra-terrestial radiation.
      S0D = S0N * DSINB
      AMTRD = SRADJ / S0D

C     Calculate clear sky radiation (0.77*S0D) in MJ.
      SCLEAR = AMTRCS * S0D * 1.E-6

C     Calculate daily cloudiness factor (range = 0-1).
      CLOUDS = MIN(MAX(1.0-SRAD/SCLEAR,0.0),1.0)

C     Integral in Eqn. 6 (used in HMET)
      ISINB = 3600.0 * (DAYL*(SSIN+0.4*(SSIN**2+0.5*CCOS**2)) +
     &  24.0/PI*CCOS*(1.0+1.5*0.4*SSIN)*SQRT(1.0-SOC**2))

      RETURN
      END SUBROUTINE SOLAR
C=======================================================================
