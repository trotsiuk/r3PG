! Declaration file for all the variables used in the program
! Structure
! Group
!    site; species; climate; parameters
! Inputs are ordered by the order they are provided for the program
! Other variables are ordered by group and alphabeticaly

!***************************************
! INPUT

! Site data ----------------------------
real(kind=kind(0.0d0)) :: lat                             ! site latitude
integer :: soil_class                           ! soil parameters for soil class
real(kind=kind(0.0d0)) :: asw                             ! available soil water
real(kind=kind(0.0d0)) :: asw_max                         ! maximum available soil water
real(kind=kind(0.0d0)) :: asw_min                         ! minimum available soil water
integer :: year_i                               ! initial year when the simulations starts
integer :: month_i                              ! initial month when the simulation starts
integer :: altitude                             ! altitude of the site location, m
real(kind=kind(0.0d0)) :: lt_mod_mths           ! length of time used to calculate the long-term modifiers for background mortality !20251114

! Species data -------------------------
integer, dimension(n_sp) :: year_p              ! year when species was planted
integer, dimension(n_sp) :: month_p             ! month when species was planted
real(kind=kind(0.0d0)), dimension(n_sp) :: fertility      ! initial site fertility rating for a given species
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_foliage_i ! initial foliage biomass for a species
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_root_i    ! initial root biomass for a species
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_stem_i    ! initial stem biomass for a species
real(kind=kind(0.0d0)), dimension(n_sp) :: stems_n_i      ! initial stand stocking for a species
real(kind=kind(0.0d0)), dimension(n_sp) :: lt_fN      ! long-term value of fN modifier for a given species         !20241106 !20251114
!real(kind=kind(0.0d0)), dimension(n_sp) :: lt_fT      ! long-term value of fT modifier for a given species         !20241106 !20251114
!real(kind=kind(0.0d0)), dimension(n_sp) :: lt_fPhys   ! long-term value of fPhysmod modifier for a given species   !20241106 !20251114

! Long-term modifier state variables                                                !20251114
integer, parameter :: n_sp_max = 20                                                 !20251114
logical :: lt_initialised(n_sp_max)    ! flags for cohorts                          !20251114
real(kind=8) :: lt_fT(n_sp_max)        ! long-term temperature modifier             !20251114
real(kind=8) :: lt_fPhys(n_sp_max)     ! long-term physiological modifier           !20251114
integer :: LTwin                       ! user-provided long-term window (months)    !20251114
! Rolling-history arrays                                                            !20251114
real(kind=8) :: fT_hist(n_sp_max, lt_mod_mths)                                      !20251114
real(kind=8) :: fPhys_hist(n_sp_max, lt_mod_mths)                                   !20251114
integer      :: hist_ptr(n_sp_max)                                                  !20251114


! Climate ------------------------------
real(kind=kind(0.0d0)), dimension(n_m) :: tmp_min         ! minimum daily temperature
real(kind=kind(0.0d0)), dimension(n_m) :: tmp_max         ! maximum daily temperature
real(kind=kind(0.0d0)), dimension(n_m) :: tmp_ave
real(kind=kind(0.0d0)), dimension(n_m) :: prcp            ! monthly precipitation sum
real(kind=kind(0.0d0)), dimension(n_m) :: solar_rad       ! mean daily incident solar radiation
real(kind=kind(0.0d0)), dimension(n_m) :: frost_days      ! number of frost days per month
real(kind=kind(0.0d0)), dimension(n_m) :: vpd_day
real(kind=kind(0.0d0)), dimension(n_m) :: co2             ! atmospheric CO2
real(kind=kind(0.0d0)), dimension(n_m) :: d13catm         ! added d13C of atmospheric CO2 (per mil)


! Parameters ---------------------------

! Biomass partitioning and turnover
real(kind=kind(0.0d0)), dimension(n_sp) :: pFS2           ! Foliage:stem partitioning ratio at D=2 cm
real(kind=kind(0.0d0)), dimension(n_sp) :: pFS20          ! Foliage:stem partitioning ratio at D=20 cm
real(kind=kind(0.0d0)), dimension(n_sp) :: aWs            ! Constant in the stem mass v. diam. relationship
real(kind=kind(0.0d0)), dimension(n_sp) :: nWs            ! Power in the stem mass v. diam. relationship
real(kind=kind(0.0d0)), dimension(n_sp) :: pRn            ! Minimum fraction of NPP to roots
real(kind=kind(0.0d0)), dimension(n_sp) :: pRx            ! Maximum fraction of NPP to roots
real(kind=kind(0.0d0)), dimension(n_sp) :: gammaF1        ! Coefficients in monthly litterfall rate
real(kind=kind(0.0d0)), dimension(n_sp) :: gammaF0        ! Coefficients in monthly litterfall rate
real(kind=kind(0.0d0)), dimension(n_sp) :: tgammaF        ! Coefficients in monthly litterfall rate
real(kind=kind(0.0d0)), dimension(n_sp) :: gammaR         ! Average monthly root turnover rate
integer, dimension(n_sp) :: leafgrow            ! If deciduous, leaves are produced at end of this month
integer, dimension(n_sp) :: leaffall            ! If deciduous, leaves all fall at start of this month

! NPP & conductance modifiers
real(kind=kind(0.0d0)), dimension(n_sp) :: Tmin           ! Minimum temperature for growth
real(kind=kind(0.0d0)), dimension(n_sp) :: Topt           ! Optimum temperature for growth
real(kind=kind(0.0d0)), dimension(n_sp) :: Tmax           ! Maximum temperature for growth
real(kind=kind(0.0d0)), dimension(n_sp) :: kF             ! Days production lost per frost day
real(kind=kind(0.0d0)), dimension(n_sp) :: SWconst0       ! Moisture ratio deficit for fq = 0.5
real(kind=kind(0.0d0)), dimension(n_sp) :: SWpower0       ! Power of moisture ratio deficit
real(kind=kind(0.0d0)), dimension(n_sp) :: fCalpha700     ! Assimilation enhancement factor at 700 ppm
real(kind=kind(0.0d0)), dimension(n_sp) :: fCg700         ! Canopy conductance enhancement factor at 700 ppm
real(kind=kind(0.0d0)), dimension(n_sp) :: m0             ! Value of 'm' when FR = 0
real(kind=kind(0.0d0)), dimension(n_sp) :: fN0            ! Value of 'fNutr' when FR = 0
real(kind=kind(0.0d0)), dimension(n_sp) :: fNn            ! Power of (1-FR) in 'fNutr'
real(kind=kind(0.0d0)), dimension(n_sp) :: MaxAge         ! Maximum stand age used in age modifier
real(kind=kind(0.0d0)), dimension(n_sp) :: nAge           ! Power of relative age in function for f_age
real(kind=kind(0.0d0)), dimension(n_sp) :: rAge           ! Relative age to give f_age = 0.5

! Stem mortality & self-thinning
real(kind=kind(0.0d0)), dimension(n_sp) :: gammaN1        ! Mortality rate for large t
real(kind=kind(0.0d0)), dimension(n_sp) :: gammaN0        ! Seedling mortality rate (t = 0)
real(kind=kind(0.0d0)), dimension(n_sp) :: tgammaN        ! Age at which mortality rate has median value
real(kind=kind(0.0d0)), dimension(n_sp) :: ngammaN        ! Shape of mortality response
real(kind=kind(0.0d0)), dimension(n_sp) :: wSx1000        ! Max. stem mass per tree @ 1000 trees/hectare
real(kind=kind(0.0d0)), dimension(n_sp) :: thinPower      ! Power in self-thinning rule
real(kind=kind(0.0d0)), dimension(n_sp) :: beta0          ! constant for self-thinning when mort_model = 2       !20241106
real(kind=kind(0.0d0)), dimension(n_sp) :: betaB          ! Power for B when mort_model = 2                      !20241106
real(kind=kind(0.0d0)), dimension(n_sp) :: betaN          ! Power in tree density when mort_model = 2            !20241106
real(kind=kind(0.0d0)), dimension(n_sp) :: betafN         ! Power in fertility modifier when mort_model = 2      !20241106
real(kind=kind(0.0d0)), dimension(n_sp) :: betafT         ! Power in temperature modifier when mort_model = 2    !20241106
real(kind=kind(0.0d0)), dimension(n_sp) :: betafPhys      ! Power in physmod modifier when mort_model = 2        !20241106
real(kind=kind(0.0d0)), dimension(n_sp) :: mF             ! Fraction mean single-tree foliage biomass lost per dead tree
real(kind=kind(0.0d0)), dimension(n_sp) :: mR             ! Fraction mean single-tree root biomass lost per dead tree
real(kind=kind(0.0d0)), dimension(n_sp) :: mS             ! Fraction mean single-tree stem biomass lost per dead tree

! Canopy structure and processes
real(kind=kind(0.0d0)), dimension(n_sp) :: SLA0           ! Specific leaf area at age 0
real(kind=kind(0.0d0)), dimension(n_sp) :: SLA1           ! Specific leaf area for mature leaves
real(kind=kind(0.0d0)), dimension(n_sp) :: tSLA           ! Age at which specific leaf area = (SLA0+SLA1)/2
real(kind=kind(0.0d0)), dimension(n_sp) :: k              ! Extinction coefficient for absorption of PAR by canopy
real(kind=kind(0.0d0)), dimension(n_sp) :: fullCanAge     ! Age at canopy closure
real(kind=kind(0.0d0)), dimension(n_sp) :: MaxIntcptn     ! Maximum proportion of rainfall evaporated from canopy
real(kind=kind(0.0d0)), dimension(n_sp) :: LAImaxIntcptn  ! LAI for maximum rainfall interception
real(kind=kind(0.0d0)), dimension(n_sp) :: cVPD           ! DF LAI for 50% reduction of VPD in canopy
real(kind=kind(0.0d0)), dimension(n_sp) :: alphaCx        ! Canopy quantum efficiency
real(kind=kind(0.0d0)), dimension(n_sp) :: y              ! Ratio NPP/GPP
real(kind=kind(0.0d0)), dimension(n_sp) :: MinCond        ! Minimum canopy conductance
real(kind=kind(0.0d0)), dimension(n_sp) :: MaxCond        ! Maximum canopy conductance
real(kind=kind(0.0d0)), dimension(n_sp) :: LAIgcx         ! LAI for maximum canopy conductance
real(kind=kind(0.0d0)), dimension(n_sp) :: CoeffCond      ! Defines stomatal response to VPD
real(kind=kind(0.0d0)), dimension(n_sp) :: BLcond         ! Canopy boundary layer conductance
real(kind=kind(0.0d0)), dimension(n_sp) :: RGcGW          ! The ratio of diffusivities of CO2 and water vapour in air
real(kind=kind(0.0d0)), dimension(n_sp) :: D13CTissueDif  ! d13C difference of modelled tissue and new photosynthate
real(kind=kind(0.0d0)), dimension(n_sp) :: aFracDiffu     ! Fractionation against 13C in diffusion
real(kind=kind(0.0d0)), dimension(n_sp) :: bFracRubi      ! Enzymatic fractionation by Rubisco

! Wood and stand properties
real(kind=kind(0.0d0)), dimension(n_sp) :: fracBB0        ! Branch and bark fraction at age 0
real(kind=kind(0.0d0)), dimension(n_sp) :: fracBB1        ! Branch and bark fraction for mature stands
real(kind=kind(0.0d0)), dimension(n_sp) :: tBB            ! Age at which fracBB = (fracBB0+fracBB1)/2
real(kind=kind(0.0d0)), dimension(n_sp) :: rho0           ! Minimum basic density - for young trees
real(kind=kind(0.0d0)), dimension(n_sp) :: rho1           ! Maximum basic density - for older trees
real(kind=kind(0.0d0)), dimension(n_sp) :: tRho           ! Age at which rho = (rhoMin+rhoMax)/2
integer, dimension(n_sp) :: CrownShape          !***DF crown shape of a given species; 1=cone, 2=ellipsoid, 3=half-ellipsoid, 4=rectangular

! Height and Wolume
real(kind=kind(0.0d0)), dimension(n_sp) :: aH, nHB, nHC
real(kind=kind(0.0d0)), dimension(n_sp) :: aV, nVB, nVH, nVBH
real(kind=kind(0.0d0)), dimension(n_sp) :: aK, nKB, nKH, nKC, nKrh
real(kind=kind(0.0d0)), dimension(n_sp) :: aHL, nHLB, nHLL, nHLC, nHLrh

! Delta 13
real(kind=kind(0.0d0)), dimension(n_sp) :: Qa, Qb
real(kind=kind(0.0d0)), dimension(n_sp) :: gDM_mol, molPAR_MJ

! Bias correction
real(kind=kind(0.0d0)), dimension(n_sp) :: Dscale0, DscaleB, Dscalerh, Dscalet, DscaleC
real(kind=kind(0.0d0)), dimension(n_sp) :: Dshape0, DshapeB, Dshaperh, Dshapet, DshapeC
real(kind=kind(0.0d0)), dimension(n_sp) :: Dlocation0, DlocationB, Dlocationrh, Dlocationt, DlocationC
real(kind=kind(0.0d0)), dimension(n_sp) :: wsscale0, wsscaleB, wsscalerh, wsscalet, wsscaleC
real(kind=kind(0.0d0)), dimension(n_sp) :: wsshape0, wsshapeB, wsshaperh, wsshapet, wsshapeC
real(kind=kind(0.0d0)), dimension(n_sp) :: wslocation0, wslocationB, wslocationrh, wslocationt, wslocationC

!***************************************
! DERIVED VARIABLES

! Helpers ------------------------------
integer :: i = 1                                ! indexing for species
integer :: ii = 1                               ! indexing for month (row of climatic data)
integer :: month = 1
integer :: b_n = 2                              ! how many times to iterate for bias correction
integer :: n = 1                                ! count for bias correction
logical :: b_cor = .TRUE.                            ! if something has changed and we need to correct bias

! Climatic variables -------------
real(kind=kind(0.0d0)), dimension(12) :: adjSolarZenithAngle
real(kind=kind(0.0d0)), dimension(12) :: day_length
integer, dimension(n_m) :: month_vector        ! A vector of month which will be used for the simulation

! Stand variables ----------------
real(kind=kind(0.0d0)), dimension(n_m, n_sp) :: age     ! Age of each species and month
real(kind=kind(0.0d0)), dimension(n_m, n_sp) :: age_m   ! Age of each species used for calculating modifiers (one month less than s_age)
real(kind=kind(0.0d0)), dimension(n_sp) :: stems_n
real(kind=kind(0.0d0)), dimension(n_sp) :: stems_n_ha     ! potential number of stems per ha for monoculture equivalent
real(kind=kind(0.0d0)), dimension(n_sp) :: stems_n_total            ! total number of trees per ha for the whole stand per active cohort, used when mort_model = 2 !20241106
real(kind=kind(0.0d0)), dimension(n_sp) :: mort_thinn_total            ! total number of trees per ha that die when mort_model = 2 !20241106



real(kind=kind(0.0d0)), dimension(n_sp) :: basal_area     ! stand level basal area
real(kind=kind(0.0d0)), dimension(n_sp) :: basal_area_prop    ! proportion of basal area
real(kind=kind(0.0d0)), dimension(n_sp) :: dbh            ! average tree DBH, cm

real(kind=kind(0.0d0)), dimension(n_sp) :: dbh_total            ! average tree DBH for the whole stand, cm (weighted average by basal_area_prop, and dbh_total = dbh for even-aged monocultures) !20241106
real(kind=kind(0.0d0)), dimension(n_sp) :: dbh_total_prev       ! average tree DBH for the whole stand, cm, of previous time step !20241106

real(kind=kind(0.0d0)), dimension(n_sp) :: basal_area_total
real(kind=kind(0.0d0)), dimension(n_sp) :: dbh_prev            ! average tree DBH, cm, of the previous time step, used when mort_model = 2 !20241106

real(kind=kind(0.0d0)), dimension(n_sp) :: height         ! average tree height, m
real(kind=kind(0.0d0)) :: Height_max

real(kind=kind(0.0d0)), dimension(n_sp) :: crown_length   !***DF mean live-crown length (m) of a species
real(kind=kind(0.0d0)), dimension(n_sp) :: crown_width    ! ***DF mean crown diameter (m)

real(kind=kind(0.0d0)), dimension(n_sp) :: volume
real(kind=kind(0.0d0)), dimension(n_sp) :: volume_mai
real(kind=kind(0.0d0)), dimension(n_sp) :: volume_old
real(kind=kind(0.0d0)), dimension(n_sp) :: volume_cum
real(kind=kind(0.0d0)), dimension(n_sp) :: volume_change

real(kind=kind(0.0d0)), dimension(n_sp) :: competition_total

real(kind=kind(0.0d0)), dimension(n_m, n_sp) :: SLA       ! Specific leaf area
real(kind=kind(0.0d0)), dimension(n_m, n_sp) :: fracBB    ! Fraction of stem biomass as branch and bark
real(kind=kind(0.0d0)), dimension(n_m, n_sp) :: wood_density  ! Whole-tree basic density


! Canopy variables ---------------
real(kind=kind(0.0d0)), dimension(n_sp) :: LAI            ! Canopy LAI (mean annual LAI if output time step is annual, and final year LAI if step is whole rotation)
real(kind=kind(0.0d0)), dimension(n_sp) :: lai_total      ! total competition of the forest
real(kind=kind(0.0d0)), dimension(n_sp) :: LAI_per        ! species specific proportion of lai
real(kind=kind(0.0d0)), dimension(n_sp) :: lai_above      ! leaf area above the given species
real(kind=kind(0.0d0)), dimension(n_sp) :: canopy_vol_frac
real(kind=kind(0.0d0)), dimension(n_sp) :: lai_sa_ratio !the ratio of mean tree leaf area (m2) to crownSA (m2)
integer, dimension(n_sp) :: layer_id


! Stocks variables ---------------
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_foliage   ! Foliage biomass
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_foliage_debt
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_root      ! Root biomass
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_stem      ! Stem biomass, including branches and bark
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_tree      ! average tree stem mass
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_tree_max  ! Max. mean tree stem mass at current stocking

real(kind=kind(0.0d0)), dimension(n_sp) :: biom_incr_foliage
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_incr_root
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_incr_stem

real(kind=kind(0.0d0)), dimension(n_sp) :: biom_loss_foliage  ! Litter fall
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_loss_root


! Modifiers ----------------------
real(kind=kind(0.0d0)), dimension(n_m, n_sp) :: f_age     ! Age related modifier
real(kind=kind(0.0d0)), dimension(n_m, n_sp) :: f_tmp     ! Temperature modifier
real(kind=kind(0.0d0)), dimension(n_m, n_sp) :: f_tmp_gc  ! gc canopy conductance modifier as in Feikema et al 2010 FEM 260,663–678
real(kind=kind(0.0d0)), dimension(n_m, n_sp) :: f_frost   ! Frost modifier
real(kind=kind(0.0d0)), dimension(n_m, n_sp) :: f_calpha  !
real(kind=kind(0.0d0)), dimension(n_m, n_sp) :: f_cg      !

real(kind=kind(0.0d0)), dimension(n_sp) :: f_vpd
real(kind=kind(0.0d0)), dimension(n_sp) :: f_sw
real(kind=kind(0.0d0)), dimension(n_sp) :: f_nutr
real(kind=kind(0.0d0)), dimension(n_sp) :: f_phys

real(kind=kind(0.0d0)), dimension(n_sp) :: lt_fN_ave ! weighted average (by basal area) long-term average lt_fN, used when mort_model = 2 !20241106
real(kind=kind(0.0d0)), dimension(n_sp) :: lt_fT_ave ! weighted average (by basal area) long-term average lt_fT, used when mort_model = 2 !20241106
real(kind=kind(0.0d0)), dimension(n_sp) :: lt_fPhys_ave  ! weighted average (by basal area) long-term average lt_fPhys, used when mort_model = 2 !20241106


! Production ---------------------
real(kind=kind(0.0d0)), dimension(n_sp) :: pfsConst       ! Derived from pFS2 and PFS20
real(kind=kind(0.0d0)), dimension(n_sp) :: pfsPower       ! Derived from pFS2 and PFS20

real(kind=kind(0.0d0)), dimension(n_sp) :: pFS
real(kind=kind(0.0d0)), dimension(n_sp) :: fi             !***DF the proportion of above canopy PAR absorbed by each species
real(kind=kind(0.0d0)), dimension(n_sp) :: lambda_h       !Constant to account for horizontal canopy heterogeneity such as gaps between trees and the change in zenith angle (and shading) with latitude and season (see Equations 2 and 5 of Forrester et al., 2014, Forest Ecosystems, 1:17)
real(kind=kind(0.0d0)), dimension(n_sp) :: lambda_v       !Constant to partition light between species and to account for vertical canopy heterogeneity (see Equations 2 and 3 of Forrester et al., 2014, Forest Ecosystems, 1:17)

real(kind=kind(0.0d0)), dimension(n_sp) :: npp_fract_root
real(kind=kind(0.0d0)), dimension(n_sp) :: npp_fract_stem
real(kind=kind(0.0d0)), dimension(n_sp) :: npp_fract_foliage

real(kind=kind(0.0d0)), dimension(n_sp) :: apar            ! RADint
real(kind=kind(0.0d0)), dimension(n_sp) :: aero_resist    ! # 'DF aerodynamic resistance within the canopy at the height of the given species (s m-1)
real(kind=kind(0.0d0)), dimension(n_sp) :: VPD_sp         ! # 'DF VPD around the crowns of the given species
real(kind=kind(0.0d0)), dimension(n_sp) :: alpha_c        ! Canopy quantum efficiency after modifiers
real(kind=kind(0.0d0)), dimension(n_sp) :: epsilon    ! Light-use efficiency based on GPP
real(kind=kind(0.0d0)), dimension(n_sp) :: epsilon_gpp    ! Light-use efficiency based on GPP
real(kind=kind(0.0d0)), dimension(n_sp) :: epsilon_npp    ! Light-use efficiency based on NPP
real(kind=kind(0.0d0)), dimension(n_sp) :: epsilon_biom_stem !Light-use efficiency based on stem biomass (increment in WS)
real(kind=kind(0.0d0)), dimension(n_sp) :: GPP
real(kind=kind(0.0d0)), dimension(n_sp) :: NPP
real(kind=kind(0.0d0)), dimension(n_sp) :: NPP_f          ! the full NPP before substraction of depth
real(kind=kind(0.0d0)), dimension(n_sp) :: gC
real(kind=kind(0.0d0)), dimension(n_sp) :: conduct_canopy
real(kind=kind(0.0d0)), dimension(n_sp) :: m


! Mortality ----------------------
real(kind=kind(0.0d0)), dimension(n_sp) :: mort_stress     ! Number of trees that died due to stress-related mortality
real(kind=kind(0.0d0)), dimension(n_sp) :: mort_thinn      ! Number of trees that died due to density-dependent mortality

real(kind=kind(0.0d0)), dimension(n_m, n_sp) :: gammaN
real(kind=kind(0.0d0)), dimension(n_m, n_sp) :: gammaF


! Management mortality
integer, dimension(n_sp) :: t_n, d_n ! currnet thinnign and defoliation number
real(kind=kind(0.0d0)) :: manag_remove_prop                              ! proportion to be removed during the management based on the trees (manag_model = 1) or biomass (manag_model = 2) 20250314
real(kind=kind(0.0d0)), dimension(3) :: manag_remove_prop_compartment    ! proportion of each compartment (stem, root, foliage) to be removed suring hte management.  20250314

real(kind=kind(0.0d0)), dimension(n_sp) :: stems_loss_manag      ! 20250314
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_loss_stem_manag      ! 20250314
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_loss_root_manag      ! 20250314
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_loss_foliage_manag      ! 20250314

real(kind=kind(0.0d0)), dimension(n_sp) :: stems_loss_stress      ! 20250314
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_loss_stem_stress      ! 20250314
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_loss_root_stress      ! 20250314
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_loss_foliage_stress      ! 20250314

real(kind=kind(0.0d0)), dimension(n_sp) :: stems_loss_density      ! 20250314
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_loss_stem_density      ! 20250314
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_loss_root_density      ! 20250314
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_loss_foliage_density      ! 20250314

real(kind=kind(0.0d0)), dimension(n_sp) :: stems_loss_def
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_loss_stem_def
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_loss_root_def
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_loss_foliage_def

real(kind=kind(0.0d0)) :: stems_loss_total      ! Sum of mort_stress, mort_defol, and mort_manag for all cohorts combined !20250301

! Defoliation
integer, dimension(n_sp) :: def_type ! current defoliation type for a given species
real(kind=kind(0.0d0)), dimension(n_sp) :: biom_foliage_adj_pre_def !Adjusted pre defoliation foliage mass
real(kind=kind(0.0d0)), dimension(n_sp) :: def_recover_t !Time neede to recover from the defoliation
real(kind=kind(0.0d0)), dimension(n_sp) :: age_last_def_event ! Last defoliation event age
real(kind=kind(0.0d0)), dimension(n_sp) :: def_test_var

! Water use ----------------------
real(kind=kind(0.0d0)), dimension(n_sp) :: SWconst         ! soil parameters for soil class
real(kind=kind(0.0d0)), dimension(n_sp) :: SWpower         ! soil parameters for soil class
real(kind=kind(0.0d0)) :: Irrig
real(kind=kind(0.0d0)) :: poolFractn                       ! Determines fraction of excess water that remains on site
real(kind=kind(0.0d0)) :: water_runoff_polled              ! current stored runoff

real(kind=kind(0.0d0)) :: irrig_supl
real(kind=kind(0.0d0)) :: prcp_runoff
real(kind=kind(0.0d0)) :: excessSW

real(kind=kind(0.0d0)) :: conduct_soil


! Transpiration
real(kind=kind(0.0d0)), dimension(n_sp) :: transp_veg       ! Traspiration from the forest
real(kind=kind(0.0d0)) :: evapotra_soil
real(kind=kind(0.0d0)) :: transp_total

real(kind=kind(0.0d0)), dimension(n_sp) :: prcp_interc_fract
real(kind=kind(0.0d0)), dimension(n_sp) :: prcp_interc
real(kind=kind(0.0d0)) :: prcp_interc_total                 ! total rain interception

real(kind=kind(0.0d0)) :: evapo_transp
real(kind=kind(0.0d0)) :: f_transp_scale                    !***DF scales GPP and NPP down if evapotranspiration is greater than ASW

real(kind=kind(0.0d0)), dimension(n_sp) :: WUE
real(kind=kind(0.0d0)), dimension(n_sp) :: WUE_transp       !***DF


! Wood Delta --------------------
real(kind=kind(0.0d0)), dimension(n_sp) :: fCalphax
real(kind=kind(0.0d0)), dimension(n_sp) :: fCg0

real(kind=kind(0.0d0)) :: air_pressure                    !Air pressure (kPa)
real(kind=kind(0.0d0)), dimension(n_sp) :: GPP_molsec     !GPP per second (mol/m2 s)
real(kind=kind(0.0d0)), dimension(n_sp) :: Gw_mol         !Canopy conductance for water vapor in mol/m2s
real(kind=kind(0.0d0)), dimension(n_sp) :: Gc_mol         !Canopy conductance for CO2 in mol/m2s
real(kind=kind(0.0d0)), dimension(n_sp) :: canopy_cover
real(kind=kind(0.0d0)), dimension(n_sp) :: InterCi        !intercellular CO2 concentration
real(kind=kind(0.0d0)), dimension(n_sp) :: D13CNewPS
real(kind=kind(0.0d0)), dimension(n_sp) :: D13CTissue


! Weibull -----------------------
real(kind=kind(0.0d0)), dimension(15, n_sp) :: bias_scale


! Settings ----------------------
integer :: light_model               ! 1 - 3PGpjs; 2 - 3PGmix 2 - 3PGmix
integer :: transp_model              !1 - 3PGpjs; 2 - 3PGmix
integer :: phys_model                !1 - 3PGpjs; 2 - 3PGmix
integer :: height_model              !1 - linear; 2 - non - linear
integer :: correct_bias              !0 - no; 1 - 3PGmix
integer :: calculate_d13c             !0 - no; 1 - 3PGmix
integer :: mort_model                !1 - 3PGpjs; 2 - 3PGmix !20241106
integer :: manag_model               !1 - 3PGpjs(based on tree number); 2 - 3PGmix !20241106