C=======================================================================
      SUBROUTINE PETPEN(
     &    CLOUDS, EORATIO, MSALB, SRAD, TAVG, TDEW,       !Input
     &    TMAX, TMIN, VAPR, WINDSP, WINDHT, XHLAI,        !Input
     &    EO)                                             !Output
!-----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------
!     INPUT VARIABLES:
      REAL CLOUDS, EORATIO, MSALB, SRAD, TAVG, TDEW, TMAX, TMIN,
     &        WINDSP, XHLAI, WINDSP_M
!-----------------------------------------------------------------------
!     OUTPUT VARIABLES:
      REAL EO
!-----------------------------------------------------------------------
!     LOCAL VARIABLES:
      REAL ALBEDO, EAIR, ESAT, G, LHVAP, PSYCON, RADB,
     &  RNET, RNETMG, S, TK4,
     &  VHCAIR, VPD, DAIR, RT, ET0, KC, WINDHT, VAPR
      REAL SHAIR, PATM, SBZCON
      REAL k, d, REFHT, Zom, Zoh, ra, rl, rs    !added for PenMon
!      REAL alt_RADB, Tprev
!      INTEGER THREEDAYCOUNT
!      REAL    THREEDAYAVG(3)

!     PARAMETER (WINDHT = 2.0)
C     PARAMETER (SHAIR = 1005.0)
      PARAMETER (SHAIR = 0.001005)  !changed for PenMon to MJ/kg/K
      PARAMETER (PATM = 101300.0)
!      PARAMETER (SBZCON=4.093E-9)  !(MJ/m2/d)
      PARAMETER (SBZCON=4.903E-9)   !(MJ/K4/m2/d) fixed constant 5/6/02

      REAL VPSLOP, VPSAT

      LHVAP = (2501.0-2.373*TAVG) * 1000.0                ! J/kg
C     PSYCON = SHAIR * PATM / (0.622*LHVAP)               ! Pa/K
      PSYCON = SHAIR * PATM / (0.622*LHVAP) * 1000000     ! Pa/K

!     Previous code:
      ESAT = (VPSAT(TMAX)+VPSAT(TMIN)) / 2.0              ! Pa
      EAIR = VPSAT(TDEW)                                  ! Pa

!     If actual vapor pressure is available, use it.
      IF (VAPR > 1.E-6) THEN
        EAIR = VAPR * 1000.
      ENDIF

      VPD = MAX(0.0, ESAT - EAIR)                         ! Pa
      S = (VPSLOP(TMAX)+VPSLOP(TMIN)) / 2.0               ! Pa/K
      RT = 8.314 * (TAVG + 273.0)                         ! N.m/mol
      DAIR = 0.028966*(PATM-0.387*EAIR)/RT                ! kg/m3
C BAD DAIR = 0.1 * 18.0 / RT * ((PATM  -EAIR)/0.622 + EAIR)   ! kg/m3
      VHCAIR = DAIR * SHAIR    !not used                  ! J/m3

C     Convert windspeed to 2 m reference height.
!     Do this conversion in WEATHR and send out 2m windspeed
!     CHP 11/26/01
!      WIND2 = WINDSP * (2.0/WINDHT)**0.2

C       Calculate aerodynamic resistance (ra).
C       ra (d/m) = {ln[zm-d/zom]*ln[zh-d/zoh]}/(k^2*uz)
C       zm = ht.wind measurement (m), zh = ht.humidity measurement (m),
C       zom,zoh=rooughness length of momentum, heat and vapor x-fer (m)
C       k=von Karman's constant 0.41, uz=WINDSP @ z m/d,
C       d = zero plane displacement height (m)

        REFHT = 0.12                 !arbitrary for testing PenMon
        WINDSP_M = WINDSP*(1000.)     !Converts km/d to m/d
        k = 0.41                     !von Karman's constant

!       was 2/3, which (for integers) results in zero!!
!       SSJ 9/19/2006 added the decimals
        !d = (2/3)*REFHT
        d = (2./3.)*REFHT

        Zom = 0.123*REFHT
        Zoh = 0.1*Zom
        ra = (LOG((WINDHT-d)/Zom)*LOG((WINDHT-d)/Zoh))/((k**2)*WINDSP_M)

C       Calculate surface resistance (rs).
C       rs = rl/LAIactive       rs (s m^-1),
C       rl = bulk stomatal resistance of the well-illuminated leaf (s m^-1)

        rl = 100           !value assummed from FAO grass reference
        rs = rl/(0.5*2.88) !0.5*XHLAI assumes half of LA is contributing
C                          !  to heat/vapor transfer
        rs = rs/86400      !converts (s m^-1 to d/m)

C     Calculate net radiation (MJ/m2/d).  By FAO method 1990. EAIR is divided
C       by 1000 to convert Pa to KPa.

c     MJ, 2007-04-11
c     --------------
c     There appears to be no support for soil heat flux (G), apart
c     from the variable already existing; it is just always set to
c     0, for some reason.
c     Here is the (improved) CANEGRO method for calculating G
c     (Allen, R.G. et al 1989,
c     'Operational Estimates of Reference Evapotranspiration',
c     Agronomy Journal Vol. 81, No. 4),
c     http://www.kimberly.uidaho.edu/water/papers/evapotranspiration/
c                   Allen_Operational_Estimates_Reference_ET_1989.pdf
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c         3-day sum of average temperature:
c          IF (THREEDAYCOUNT .LT. 1) THEN
c             Initialise
c              THREEDAYCOUNT = 1
c              THREEDAYAVG   = Tavg
c          ELSE IF (THREEDAYCOUNT .GE. 3) THEN
c              THREEDAYCOUNT = 1
c          ELSE
c              THREEDAYCOUNT = THREEDAYCOUNT + 1
c          ENDIF
c          THREEDAYAVG(THREEDAYCOUNT) = Tavg
c          Tprev = SUM(THREEDAYAVG)/3.
c          G = (Tavg-Tprev) * 0.38
c     --------------
c     MJ, 2007-04-12:
c     :::::::::::::::
c     FAO suggests that G be set to 0.  Oh well.
c     ------------------------------------------

      G = 0.0
      IF (XHLAI .LE. 0.0) THEN
        ALBEDO = MSALB
      ELSE
C  KJB NOTE THAT REFERENCE IS ALWAYS ALBEDO FIXED TO 0.23,  OLD PEN VARIED
C  THE ALBEDO WITH LAI.  WHAT DO WE WANT?  IS THIS PART OF THE REASON THAT
C  KC IS NEEDED WITH THE REFERENCE FORMULATION?
C       ALBEDO = 0.23-(0.23-SALB)*EXP(-0.75*XHLAI)
        ALBEDO = 0.23
      ENDIF

      TK4 = ((TMAX+273.)**4+(TMIN+273.)**4) / 2.0
C
C     BELOW WAS THE OLD PENMAN, DIFFERENT CLOUDS METHOD, EAIR CHG IS GOOD
C     RADB = SBZCON * TK4 * (0.4 - 0.005 * SQRT(EAIR)) *
C    &        (1.1 * (1. - CLOUDS) - 0.1)
C

      RADB = SBZCON * TK4 * (0.34 - 0.14 * SQRT(EAIR/1000)) *
     &        (1.35 * (1. - CLOUDS) - 0.35)

      RNET= (1.0-ALBEDO)*SRAD - RADB

C     Compute EO using Penman-Montieth

      RNETMG = (RNET-G)
C     !MJ/m2/d
        ET0 = ((S*RNETMG + (DAIR*SHAIR*VPD)/ra)/(S+PSYCON*(1+rs/ra)))
C     !Converts MJ/m2/d to mm/d
        ET0 = ET0/ (LHVAP / 1000000.)
        IF (XHLAI .LE. 6.0) THEN
        XHLAI = XHLAI
        ELSE
        XHLAI = 6.0
        ENDIF
C   KJB LATER, NEED TO PUT VARIABLE IN PLACE OF 1.1
!      KC=1.0+(1.1-1.0)*XHLAI/6.0
      KC=1.0+(EORATIO-1.0)*XHLAI/6.0
      EO=ET0*KC
C     EO=ET0
        EO = MAX(EO,0.0)
!###  EO = MAX(EO,0.0)   !gives error in DECRAT_C
      EO = MAX(EO,0.0001)

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE PETPEN
