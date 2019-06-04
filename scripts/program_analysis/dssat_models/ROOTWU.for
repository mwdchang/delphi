C=======================================================================
      SUBROUTINE ROOTWU(DYNAMIC,
     &    DLAYR, LL, NLAYR, PORMIN, RLV, RWUMX, SAT, SW,  !Input
     &    RWU, TRWUP)                                     !Output

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types,
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE
C-----------------------------------------------------------------------
      INTEGER DYNAMIC

      INTEGER L, NLAYR

      REAL SWEXF, TRWUP
      REAL SWCON1, SWCON3, PORMIN, RWUMX
      REAL DLAYR(NL), LL(NL), RLV(NL), RWU(NL)
      REAL SAT(NL), SW(NL), SWCON2(NL), TSS(NL)
      REAL DENOMINATOR

      PARAMETER (SWCON1 = 1.32E-3)
      PARAMETER (SWCON3 = 7.01)

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
C     Compute SWCON2 for each soil layer.  Adjust SWCON2 for extremely
C     high LL to avoid water uptake limitations.
!-----------------------------------------------------------------------
      SWCON2 = 0.0
      RWU    = 0.0
      TSS    = 0.0
      TRWUP  = 0.0



!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
      TRWUP  = 0.0
      DO L = 1,NLAYR
        SWCON2(L) = 120. - 250. * LL(L)
        IF (LL(L) .GT. 0.30) SWCON2(L) = 45.0
        RWU(L) = 0.0   !WDB - CIMMYT 2002
      ENDDO

      DO L = 1,NLAYR
        IF (RLV(L) .LE. 0.00001 .OR. SW(L) .LE. LL(L)) THEN
          RWU(L) = 0.
        ELSE
          IF (RLV(L) > EXP(SWCON3)) THEN
            DENOMINATOR = SWCON3 - ALOG(SWCON3)
          ELSE
            DENOMINATOR = SWCON3 - ALOG(RLV(L))
          ENDIF

          RWU(L) = SWCON1*EXP(MIN((SWCON2(L)*(SW(L)-LL(L))),40.))/
     &      DENOMINATOR
!           Previous denominator - could explode for large RLV - problem with RLV?
!     &      (SWCON3-ALOG(RLV(L)))
!           RWU in cm3[water]/cm[root]-d

C-----------------------------------------------------------------------
C           PORMIN = MINIMUM PORE SPACE  REQUIRED FOR SUPPLYING OXYGEN
C                TO ROOTS FOR OPTIMAL GROWTH AND FUNCTION
C     TSS(L) = number of days soil layer L has been saturated
C-----------------------------------------------------------------------
!         CHP 6/27/2011 Add check for SW ~= SAT and PORMIN = 0.0 (Flooded rice)
!         IF ((SAT(L)-SW(L)) .GE. PORMIN) THEN
          IF ((SAT(L)-SW(L)) .GE. PORMIN .OR. PORMIN < 1.E-6) THEN
             TSS(L) = 0.
          ELSE
             TSS(L) = TSS(L) + 1.
          ENDIF
C-----------------------------------------------------------------------
C           Delay of 2 days after soil layer is saturated before root
C           water uptake is affected
C-----------------------------------------------------------------------
          IF (TSS(L) .GT. 2.) THEN
             SWEXF = (SAT(L)-SW(L))/PORMIN
             SWEXF = MAX(SWEXF,0.0)
          ELSE
             SWEXF = 1.0
          ENDIF
          SWEXF = MIN(SWEXF,1.0)
          RWU(L) = MIN(RWU(L),RWUMX*SWEXF)
          RWU(L) = MIN(RWU(L),RWUMX)
        ENDIF
        RWU(L) = RWU(L) * DLAYR(L) * RLV(L)
!       cm[water]   cm3[water]   cm3[soil]   cm[root]
!       --------- = ---------- * --------- * ---------
!           d       cm[root]-d   cm2[soil]   cm3[soil]

        TRWUP  = TRWUP + RWU(L)     !cm/d
      ENDDO

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE ROOTWU
