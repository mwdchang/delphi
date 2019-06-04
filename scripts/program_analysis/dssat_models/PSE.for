!=======================================================================

      SUBROUTINE PSE(EO, KSEVAP, XLAI, EOS)

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE
      SAVE

!     CHARACTER*1 MEINF
      REAL EO, XLAI, EOS
      REAL KSEVAP
      REAL KE, REFET

      KE = 0
      REFET = 0
!-----------------------------------------------------------------------
!     Potential soil evaporation based on leaf area index and potential
!         evapotranspiration.

! LAH JULY 2, 2003
      IF (KE .GE. 0.0) THEN
        EOS = KE * REFET !KRT added for ASCE dual Kc ET approach
      ELSEIF (KSEVAP .LE. 0.0) THEN

!       Old computation:
        IF (XLAI .LE. 1.0) THEN   !<-------!
!         EOS = EO*(1.0 - 0.43*XLAI)       !
!         get rid of discontinuity:
          EOS = EO*(1.0 - 0.39*XLAI)       !
        ELSE                               !-> old code
          EOS = EO/1.1*EXP(-0.4*XLAI)      !
        ENDIF    !<------------------------!

      ELSE
        EOS = EO*EXP(-KSEVAP*XLAI) !<------- Tony's new code 07/02/2003
      ENDIF


!     Ken's new computation: 01/03/2003
!      EOS = EO*EXP(-0.50*XLAI)

!     Note from Tony:  this is not the same as the old computation and
!       may cause different results.  We need to re-evaluate.
!     Probably should use KEP here.

      EOS = MAX(EOS,0.0)

      RETURN
      END SUBROUTINE PSE
