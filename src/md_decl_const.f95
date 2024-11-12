! This module contains the constant parameters for the 3PGmix model
! All the parameters are ordered in the alphabetical ordered
! V.Trotsiuk [trotsiuk@fld.czu.cz]

module mod_decl_const

    implicit none

    real(kind=kind(0.0d0)), parameter :: Pi = 3.141592654d0
    real(kind=kind(0.0d0)), parameter :: ln2 = 0.693147181d0

    real(kind=kind(0.0d0)), parameter :: e20 = 2.2d0            ! rate of change of saturated VP with T at 20C
!    real(kind=kind(0.0d0)), parameter :: Qa = -90.0d0, Qb = 0.8d0    ! intercept & slope of net v. solar radiation relationship (W/m2)

!    real(kind=kind(0.0d0)), parameter :: gDM_mol = 24.d0
!    real(kind=kind(0.0d0)), parameter :: molPAR_MJ = 2.30d0
    real(kind=kind(0.0d0)), parameter :: MaxSoilCond = 0.00250d0

    real(kind=kind(0.0d0)), parameter :: lambda = 2460000.0d0   ! latent heat of vapourisation of H2O (J/kg)
    real(kind=kind(0.0d0)), parameter :: rhoAir = 1.2d0         ! density of air, kg/m3
    real(kind=kind(0.0d0)), parameter :: VPDconv = 0.000622d0   ! convert VPD to saturation deficit = 18/29/1000

    integer, dimension(12), parameter :: dayOfYear = (/15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349/)
    integer, dimension(12), parameter :: daysInMonth = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)



! Parameters for self-thinning as rates of change                !20241106
    real(kind=kind(0.0d0)), parameter :: beta0 = -20.4d0         !20241106
    real(kind=kind(0.0d0)), parameter :: betaB = 1.86d0             !20241106
    real(kind=kind(0.0d0)), parameter :: betaN = 2.6d0          !20241106
    real(kind=kind(0.0d0)), parameter :: betafN = 0d0         !20241106
    real(kind=kind(0.0d0)), parameter :: betafT = 0d0         !20241106
    real(kind=kind(0.0d0)), parameter :: betafPhys = 0d0     !20241106

    !integer :: mort_model = 1     ! 1 - 3PGpjs; 2 - 3PGmix       !20241106

! To be provided in the "species" inputs because they are species specific and not appropriate as site inputs
    real(kind=kind(0.0d0)), parameter :: lt_fN = 0.9d0             !20241106 long-term valules of modifiers
    real(kind=kind(0.0d0)), parameter :: lt_fT = 0.9d0             !20241106
    real(kind=kind(0.0d0)), parameter :: lt_fPhys = 0.9d0          !20241106


end module mod_decl_const