C=======================================================================
      SUBROUTINE SNOWFALL (DYNAMIC,
     &    TMAX, RAIN,                                     !Input
     &    SNOW, WATAVL)                                   !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types,
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      INTEGER DYNAMIC
      REAL TMAX, RAIN, SNOW, WATAVL, SNOMLT

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      SNOW = 0.0
!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      SNOMLT = 0.0
      IF (TMAX  .GT. 1.0) THEN
         SNOMLT = TMAX + RAIN*0.4
         IF (SNOMLT .GT. SNOW) THEN
             SNOMLT = SNOW
         ENDIF

         SNOW   = SNOW - SNOMLT
         WATAVL = RAIN + SNOMLT
      ELSE
         SNOW   = SNOW   + RAIN
         WATAVL = 0.0
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE SNOWFALL
