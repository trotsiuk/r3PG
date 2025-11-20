module mod_3PG

    use, intrinsic :: iso_c_binding, only: c_double, c_int, c_bool
    use mod_decl_const

    implicit none
    private
    public :: s_3PG_f

contains

    subroutine s_3PG_f ( siteInputs, speciesInputs, forcingInputs, managementInputs, defoliationInputs, &
        pars_i, pars_b, n_sp, n_m, n_man, t_t, n_def, d_t, settings, output) bind(C, name = "s_3PG_f_")

        implicit none

        !********************************************************************************************
        !   Declaration

        ! Number of species and month
        integer(kind=c_int), intent(in) :: n_m
        integer(kind=c_int), intent(in) :: n_sp
        integer(kind=c_int), intent(in) :: n_man, n_def ! number of management and defoliation interventions
        integer(kind=c_int), dimension(n_sp), intent(in) :: t_t, d_t! number of management and defoliation interventions
        integer(kind=c_int), dimension(8), intent(in) :: settings    ! settings for the models                !20241106

        ! Initial, forcing, parameters
        real(kind=c_double), dimension(9), intent(in) :: siteInputs                            !20251114
        real(kind=c_double), dimension(n_sp,7), intent(in) :: speciesInputs                    !20241106 !20251114
        real(kind=c_double), dimension(n_man,6,n_sp), intent(in) :: managementInputs           !20251114
        real(kind=c_double), dimension(n_def,9,n_sp), intent(in) :: defoliationInputs
        real(kind=c_double), dimension(n_m,9), intent(in) :: forcingInputs
        real(kind=c_double), dimension(90,n_sp), intent(in) :: pars_i                          !20241106
        real(kind=c_double), dimension(30,n_sp), intent(in) :: pars_b

!integer :: mm !20251114

        ! Output array
        real(kind=c_double), dimension(n_m,n_sp,11,20), intent(inout) :: output

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! this is only for printing the history
!! Declare these once at the top of your subroutine
!integer :: kk, ios
!character(len=256) :: filenameT, filenameP
!integer :: isp, jj
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!! Temporary variables for long-term modifiers
!integer :: stat, s_index, m_lt!, m      ! loop variables
!real(kind=8), dimension(n_sp) :: m_tmp
!integer(kind=8) :: approx_bytes
!real(kind=8) :: approx_mb
! Temporary variables for long-term modifiers
real(kind=8) :: f_sw_tmp, f_vpd_tmp, f_phys_tmp, vpd_mean !20251114



        ! Variables, Parameters, Constants
        include 'i_decl_var.h'

        include 'i_read_input.h'
        include 'i_read_param.h'
        include 'i_read_param_sizeDist.h'

        ! Initialization
        include 'i_init_var.h'


        !*************************************************************************************
        ! INITIALISATION (Age independent)

        ! Day-length calculations
        adjSolarZenithAngle(:) = f_get_solarangle( Lat )

        day_length(:) = 86400.d0 * f_get_daylength( Lat ) !Seconds

        ! CO2 modifiers helpers
        fCalphax(:) = fCalpha700(:) / (2.d0 - fCalpha700(:))
        fCg0(:) = fCg700(:) / (2.d0 * fCg700(:) - 1.d0)

        ! Generate the sequence of months
        ! we need it to calculate the frost days and limit it
        month = month_i
        do i = 1, n_m
            month_vector(i) = month
            month = mod(month, 12) + 1
        end do

        ! Temperature --------
        do i = 1, n_sp
            ! calculate temperature response function to apply to alphaCx
            f_tmp(:,i) = ((tmp_ave(:) - Tmin(i)) / (Topt(i) - Tmin(i))) * &
                ((Tmax(i) - tmp_ave(:)) / (Tmax(i) - Topt(i))) ** ((Tmax(i) - Topt(i)) / (Topt(i) - Tmin(i)))

            where( tmp_ave(:) <= Tmin(i) .or. tmp_ave(:) >= Tmax(i) )
                f_tmp(:,i) = 0.d0
            end where

            ! calculate temperature response function to apply to gc (uses mean of Tx and Tav instead of Tav, Feikema et al 2010)
            f_tmp_gc(:,i) = (((tmp_ave(:) + tmp_max(:)) / 2 - Tmin(i)) / (Topt(i) - Tmin(i))) * &
                ((Tmax(i) - (tmp_ave(:) + tmp_max(:)) / 2) / (Tmax(i) - Topt(i))) ** ((Tmax(i) - Topt(i)) / (Topt(i) - Tmin(i)))

            where( (tmp_ave(:) + tmp_max(:)) / 2 <= Tmin(i) .or. (tmp_ave(:) + tmp_max(:)) / 2 >= Tmax(i) )
                f_tmp_gc(:,i) = 0.d0
            end where

            ! frost modifier
            f_frost(:,i) = 1.d0 - kF(i) * ( frost_days(:) / daysInMonth(month_vector(:)) )

            ! CO2 modifiers
            f_calpha(:,i) = fCalphax(i) * co2(:) / (350.d0 * (fCalphax(i) - 1.d0) + co2(:))
            f_cg(:,i) = fCg0(i) / (1.d0 + (fCg0(i) - 1.d0) * co2(:) / 350.d0)

        end do

        ! air pressure
        air_pressure = 101.3d0 * Exp(-1.d0 * altitude / 8200.d0)


        ! SOIL WATER --------
        ! Assign the SWconst and SWpower parameters for this soil class
        if ( soil_class > 0.d0 ) then
            ! Standard soil type
            SWconst(:) = 0.8d0 - 0.10d0 * soil_class
            SWpower(:) = 11.d0 - 2.d0 * soil_class
        elseIf ( soil_class < 0.d0 ) then
            ! Use supplied parameters
            SWconst(:) = SWconst0(:)
            SWpower(:) = SWpower0(:)
        else
            ! No soil-water effects
            SWconst(:) = 999
            SWpower(:) = SWpower0(:)
        end if

        ! Initial ASW must be between min and max ASW
        if (asw_min > asw_max) then
            asw_min = asw_max
        end if

        asw = max( min( asw, asw_max ), asw_min )

        ! Silvicultural events are currently not active
        Irrig = 0.d0
        water_runoff_polled = 0.d0
        poolFractn = 0.d0
        poolFractn = max(0.d0, min(1.d0, poolFractn))


        ! NUTRITIONS --------
        ! Check fN(FR) for no effect: fNn = 0 ==> fN(FR)=1 for all FR
        where( fNn(:) == 0.d0 ) fN0(:) = 1.d0


        ! Partitioning  --------
        pfsPower(:) = Log( pFS20(:) / pFS2(:) ) / Log( 20.d0 / 2.d0 )
        pfsConst(:) = pFS2(:) / 2.d0 ** pfsPower(:)



        ! INITIALISATION (Age dependent)---------------------
        ! Calculate the species specific modifiers
        do i = 1, n_sp
            age(:,i) = 12.d0 * ( year_i - year_p(i) ) + month_i - month_p(i) - 1.d0 !
            age(:,i) =  ( age(:,i) + int( (/(i, i=1, n_m)/) ) ) / 12.d0 ! translate to years
            age_m(:,i) =  age(:,i) - 1.d0/12.d0
            age_m(1,i) =  age(1,i)

            SLA(:,i) = f_exp( n_m, age_m(:,i), SLA0(i), SLA1(i), tSLA(i), 2.d0)
            fracBB(:,i) = f_exp( n_m, age_m(:,i), fracBB0(i), fracBB1(i), tBB(i), 1.d0)
            wood_density(:,i) = f_exp( n_m, age_m(:,i), rho0(i), rho1(i), tRho(i), 1.d0)
            gammaN(:,i) = f_exp( n_m, age(:,i), gammaN0(i), gammaN1(i), tgammaN(i), ngammaN(i))

            gammaF(:,i) = f_exp_foliage( n_m, age_m(:,i), gammaF1(i), gammaF0(i), tgammaF(i))


            ! age modifier
            if (nAge(i) == 0.d0) then
                f_age(:,i) = 1.d0
            else
                ! I'm not declaring relative age, but directly put it inside
                f_age(:,i) = 1.d0 / (1.d0 + ( (age_m(:,i) / MaxAge(i) ) / rAge(i)) ** nAge(i))
            end if

            !age(:,i) = age(:,i) + 1.d0 ! that how the VBA works

        end do


        ! INITIALISATION (Stand)---------------------
        ii = 1
        month = month_i

        where (age(ii,:) >= 0.d0 )
          stems_n(:) = stems_n_i(:)
          biom_stem(:) = biom_stem_i(:)
          biom_foliage(:) = biom_foliage_i(:)
          biom_root(:) = biom_root_i(:)
        end where

        ! Check if this is the dormant period or previous/following period is dormant
        ! to allocate foliage if needed, etc.
        do i = 1, n_sp
            ! if this is a dormant month
            if( f_dormant(month, leafgrow(i), leaffall(i)) .eqv. .TRUE. ) then
                biom_foliage_debt(i)= biom_foliage(i)
                biom_foliage(i) = 0.d0
            end if
        end do

        ! Initial stand characteristics
        where (age(ii,:) >= 0.d0 )
          biom_tree(:) = biom_stem(:) * 1000.d0 / stems_n(:)  ! kg/tree
          dbh(:) = ( biom_tree(:) / aWs(:)) ** (1.d0 / nWs(:))
          basal_area(:) = dbh(:) ** 2.d0 / 4.d0 * Pi * stems_n(:) / 10000.d0
          lai(:) =  biom_foliage(:) * SLA(ii,:) * 0.1d0
        end where

        competition_total(:) = sum( wood_density(ii,:) * basal_area(:) )


        if( height_model .eq. 1 ) then
            height(:) = aH(:) * dbh(:) ** nHB(:) * competition_total(:) ** nHC(:)
        else if ( height_model .eq. 2 ) then
            height(:) = Hd(:) + aH(:) * Exp(1.d0)**(-nHB(:)/dbh(:)) + nHC(:) * competition_total(:) * dbh(:) !20251114
        else if ( height_model .eq. 3 ) then
            height(:) = Hd(:) + (dbh(:) ** aH(:)) / (nHB(:) + nHC(:) * (dbh(:) ** aH(:))) !20251114
        end if

        ! Correct the bias
        do n = 1, b_n
            competition_total(:) = sum( wood_density(ii,:) * basal_area(:) )

            call s_sizeDist_correct(n_sp, age(ii,:), stems_n(:), biom_tree(:), competition_total(:), lai(:), &
                correct_bias, height_model,  pars_i(69:86,:), pars_b, aWs(:), nWs(:), pfsPower(:), pfsConst(:), &  !20241106
                dbh(:), basal_area(:), height(:), crown_length(:), crown_width(:), pFS(:), bias_scale(:,:) )
        end do



        Height_max = maxval( height(:), mask=lai(:)>0.d0 )

        ! Volume and Volume increment
        volume(:) = biom_stem(:) * (1.d0 - fracBB(ii,:)) / wood_density(ii,:)
        where( aV(:) > 0 ) volume(:) = aV(:) * dbh(:) ** nVB(:) * height(:) ** nVH(:) * &
                (dbh(:) * dbh(:) * height(:)) ** nVBH(:) * stems_n(:)

        volume_cum(:) = volume(:)
        volume_old(:) = volume(:)

        volume_mai(:) = volume_cum(:) / age(ii,:)

        basal_area_prop(:) = basal_area(:) / sum( basal_area(:) )












!-----------------------------
! Long-term modifiers initialization
!-----------------------------
if (.not. allocated(lt_fT)) allocate(lt_fT(n_sp))
lt_fT(:) = 1.0d0

if (.not. allocated(lt_fPhys)) allocate(lt_fPhys(n_sp))
lt_fPhys(:) = 1.0d0

if (.not. allocated(fT_hist)) allocate(fT_hist(n_sp, lt_mod_mths))
fT_hist(:,:) = 1.0d0

if (.not. allocated(fPhys_hist)) allocate(fPhys_hist(n_sp, lt_mod_mths))
fPhys_hist(:,:) = 1.0d0

if (.not. allocated(hist_ptr)) allocate(hist_ptr(n_sp))


do i = 1, n_sp

    !---------------------------
    ! 1) Temperature (lt_fT)
    !---------------------------
    lt_fT(i) = sum(f_tmp(1:lt_mod_mths, i)) / real(lt_mod_mths, kind=8)
    fT_hist(i,1:lt_mod_mths) = lt_fT(i)

    !---------------------------
    ! 2) Physiological (lt_fPhys)
    ! Exclude the first month of VPD (to avoid initial zeros)
    !---------------------------
    vpd_mean = sum(vpd_day(2:lt_mod_mths)) / real(lt_mod_mths - 1, kind=8)

    ! ASW uses the constant directly
    f_sw_tmp  = 1.d0 / (1.d0 + ((1.d0 - 1.d0) / SWconst(i)) ** SWpower(i)) !1.d0 - 1.d0 is because ASW = asw_max
    f_vpd_tmp = exp(-CoeffCond(i) * vpd_mean)

    if (phys_model .eq. 1) then
        f_phys_tmp = min(f_sw_tmp, f_vpd_tmp)
    else
        f_phys_tmp = f_sw_tmp * f_vpd_tmp
    end if

    lt_fPhys(i) = f_phys_tmp

    ! Ensure the entire row contains the correct long-term modifier
    fPhys_hist(i,1:lt_mod_mths) = f_phys_tmp

    !---------------------------
    ! Initialize circular buffer pointer
    !---------------------------
    hist_ptr(i) = lt_mod_mths   ! start at the last element

end do





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!do isp = 1, n_sp
!!!
!!!    !---------------------------
!!!    ! 1) Temperature (lt_fT)
!!!    !---------------------------
!!!    lt_fT(isp) = sum(f_tmp(1:lt_mod_mths, isp)) / real(lt_mod_mths, kind=8)
!!!    fT_hist(isp,1:lt_mod_mths) = lt_fT(isp)
!!!
!!!    !---------------------------
!!!    ! 2) Physiological (lt_fPhys)
!!!    !---------------------------
!!!    vpd_mean = sum(vpd_day(2:lt_mod_mths)) / real(lt_mod_mths - 1, kind=8)
!!!    f_sw_tmp  = 1.d0 / (1.d0 + ((1.d0 - 1.d0) / SWconst(isp)) ** SWpower(isp)) !1.d0 - 1.d0 is because ASW = asw_max
!!!    f_vpd_tmp = exp(-CoeffCond(isp) * vpd_mean)
!!!
!!!    if (phys_model .eq. 1) then
!!!        f_phys_tmp = min(f_sw_tmp, f_vpd_tmp)
!!!    else
!!!        f_phys_tmp = f_sw_tmp * f_vpd_tmp
!!!    end if
!!!
!!!    lt_fPhys(isp) = f_phys_tmp
!!!
!!!    !---------------------------
!!!    ! Write debug CSV for this species
!!!    !---------------------------
!!!    write(filenameP, '(A,I0,A)') 'debug_fPhys_species_', isp, '.csv'
!!!    open(unit=400, file=filenameP, status='replace', action='write', iostat=ios)
!!!    if (ios /= 0) stop 'Error opening debug CSV'
!!!
!!!    write(400, '(A,F12.6)') 'asw_max=', asw_max
!!!    write(400, '(A,F12.6)') 'SWconst=', SWconst(isp)
!!!    write(400, '(A,F12.6)') 'SWpower=', SWpower(isp)
!!!    write(400, '(A,F12.6)') 'vpd_mean=', vpd_mean
!!!    write(400, '(A,F12.6)') 'f_sw_tmp=', f_sw_tmp
!!!    write(400, '(A,F12.6)') 'f_vpd_tmp=', f_vpd_tmp
!!!    write(400, '(A,F12.6)') 'f_phys_tmp=', f_phys_tmp
!!!    write(400, '(A,F12.6)') 'lt_fPhys=', lt_fPhys(isp)
!!!
!!!    close(400)
!!!
!!!    !---------------------------
!!!    ! Fill entire row of fPhys_hist
!!!    !---------------------------
!!!    do jj = 1, lt_mod_mths
!!!        fPhys_hist(isp,jj) = f_phys_tmp
!!!    end do
!!!
!!!    !---------------------------
!!!    ! Initialize circular buffer pointer
!!!    !---------------------------
!!!    hist_ptr(isp) = lt_mod_mths
!!!
!!!end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!do isp = 1, n_sp
!
!    !---------------------------
!    ! 1) Temperature (lt_fT)
!    !---------------------------
!    lt_fT(isp) = sum(f_tmp(1:lt_mod_mths, isp)) / real(lt_mod_mths, kind=8)
!    fT_hist(isp,1:lt_mod_mths) = lt_fT(isp)
!
!    !---------------------------
!    ! 2) Physiological (lt_fPhys)
!    !---------------------------
!    vpd_mean = sum(vpd_day(2:lt_mod_mths)) / real(lt_mod_mths - 1, kind=8)
!    f_sw_tmp  = 1.d0 / (1.d0 + ((1.d0 - 1.d0) / SWconst(isp)) ** SWpower(isp))  ! ASW = asw_max
!    f_vpd_tmp = exp(-CoeffCond(isp) * vpd_mean)
!
!    if (phys_model .eq. 1) then
!        f_phys_tmp = min(f_sw_tmp, f_vpd_tmp)
!    else
!        f_phys_tmp = f_sw_tmp * f_vpd_tmp
!    end if
!
!    lt_fPhys(isp) = f_phys_tmp
!
!    !---------------------------
!    ! Fill entire row of fPhys_hist
!    !---------------------------
!    do jj = 1, lt_mod_mths
!        fPhys_hist(isp,jj) = f_phys_tmp
!    end do
!
!    !---------------------------
!    ! Write debug CSV for this species
!    !---------------------------
!    write(filenameP, '(A,I0,A)') 'debug_fPhys_species_', isp, '.csv'
!    open(unit=400, file=filenameP, status='replace', action='write', iostat=ios)
!    if (ios /= 0) stop 'Error opening debug CSV'
!
!    write(400, '(A,F12.6)') 'asw_max=', asw_max
!    write(400, '(A,F12.6)') 'SWconst=', SWconst(isp)
!    write(400, '(A,F12.6)') 'SWpower=', SWpower(isp)
!    write(400, '(A,F12.6)') 'vpd_mean=', vpd_mean
!    write(400, '(A,F12.6)') 'f_sw_tmp=', f_sw_tmp
!    write(400, '(A,F12.6)') 'f_vpd_tmp=', f_vpd_tmp
!    write(400, '(A,F12.6)') 'f_phys_tmp=', f_phys_tmp
!    write(400, '(A,F12.6)') 'lt_fPhys=', lt_fPhys(isp)
!
!    !---------------------------
!    ! Write the full fPhys_hist row for this species
!    !---------------------------
!    write(400, '(A)') 'fPhys_hist row:'
!    do jj = 1, lt_mod_mths
!        write(400, '(F12.6)', advance='no') fPhys_hist(isp,jj)
!        if (mod(jj,10) == 0) write(400,*)
!    end do
!    write(400,*)
!
!    close(400)
!
!    !---------------------------
!    ! Initialize circular buffer pointer
!    !---------------------------
!    hist_ptr(isp) = lt_mod_mths
!
!end do











        ! INITIALISATION (Write output)---------------------
        include 'i_write_out.h'



        !*************************************************************************************
        ! Monthly simulations

        do ii = 2, n_m

            ! month update
            month = month + int(1)

            if (month > 12) then
                month = int(1)
            end if

            ! Add new cohort ----------------------------------------------------------------------
            where (age(ii,:) .eq. 0.d0 )
              stems_n(:) = stems_n_i(:)
              biom_stem(:) = biom_stem_i(:)
              biom_foliage(:) = biom_foliage_i(:)
              biom_root(:) = biom_root_i(:)
            end where

            if( any(age(ii,:) .eq. 0.d0) ) then
              b_cor = .TRUE.
            end if


            ! Test for dormancy ----------------------------------------------------------------------

            ! If this is first month after dormancy we need to make potential LAI, so the
            ! PAR absorbption can be applied, otherwise it will be sero.
            ! In the end of the month we will re-calculate it based on the actual values
            ! Also, for any cohort is still recovering from a defoliation event (receiving from stored
            ! carborhydrates or altered partitioning), add that biomass.

            do i = 1, n_sp
                if( f_dormant(month, leafgrow(i), leaffall(i)) .eqv. .FALSE. ) then
                    if( f_dormant(month-1, leafgrow(i), leaffall(i)) .eqv. .TRUE. ) then
                        lai(i) =  biom_foliage_debt(i) * SLA(ii,i) * 0.1d0
                        b_cor = .TRUE.
                    end if
                end if

                ! If this is first dormant month, then we set WF to 0 and move everything to the dept
                if( f_dormant(month, leafgrow(i), leaffall(i)) .eqv. .TRUE. ) then
                    if( f_dormant(month-1, leafgrow(i), leaffall(i)) .eqv. .FALSE. ) then
                        biom_foliage_debt(i)= biom_foliage(i)
                        biom_foliage(i) = 0.d0
                        lai(i) =  0.d0
                        b_cor = .TRUE.
                    end if
                end if

            end do

            ! If any cohorts are recovering from a defoliation event, check whether they finished recovering
            ! after the last growth using new NPP. ! 20250301
            ! if ( t_recover(i) > 0 .and. age(ii,i) >= age_last_def_event(i) + t_recover(i)/12.d0 ) then ! t_recover(i) > 0 (not 0.0d0) indicates that there has been a defoliation event
            !         t_recover(i) = 0.0d0
            !     end if


            !     if( defoliation_type(i) == 2 .and. age(ii,i) > age_last_def_event(i) + 1.d0/12.d0 .and. &
            !     biom_foliage(i) + biom_stem(i) >= adj_pre_def_foliage_mass(i) ) then                                               ! coppice
            !         t_recover(i) = 0.0d0
            !     end if
            !     if( defoliation_type(i) == 1 .and. age(ii,i) >= age_last_def_event(i) + 1.d0/12.d0 .and. &
            !     biom_foliage(i) >= adj_pre_def_foliage_mass(i) ) then                                              ! prune
            !         t_recover(i) = 0.0d0
            !     end if
            !     if( defoliation_type(i) == 3 .and. age(ii,i) >= age_last_def_event(i) + 1.d0/12.d0 .and. &
            !     biom_foliage(i) >= adj_pre_def_foliage_mass(i) ) then                               ! epicormic
            !         t_recover(i) = 0.0d0
            !     end if

            !****** We shall call this only if the any of the above is TRUE
            if ( b_cor .eqv. .TRUE. ) then
                do n = 1, b_n
                    competition_total(:) = sum( wood_density(ii,:) * basal_area(:) )

                    call s_sizeDist_correct(n_sp, age(ii,:), stems_n(:), biom_tree(:), competition_total(:), lai(:), &
                        correct_bias, height_model,  pars_i(69:86,:), pars_b, aWs(:), nWs(:), pfsPower(:), pfsConst(:), &         !20241106
                        dbh(:), basal_area(:), height(:), crown_length(:), crown_width(:), pFS(:), bias_scale(:,:) )
                end do
                b_cor = .FALSE.
            end if

            !Radiation and assimilation ----------------------------------------------------------------------
            if ( light_model .eq. int(1) ) then
                call s_light_3pgpjs ( n_sp, age_m(ii,:), fullCanAge(:), k(:), lai(:), &
                    solar_rad(ii), daysInMonth(month), &
                    canopy_cover(:), apar(:) )

                VPD_sp(:) = vpd_day(ii)

            else if ( light_model .eq. int(2) ) then

                ! Calculate the absorbed PAR. If this is first month, then it will be only potential
                call s_light_3pgmix ( n_sp, height(:), crown_length(:), crown_width(:), lai(:), stems_n(:), &
                    solar_rad(ii), CrownShape(:), k(:), gammaAPAR(:), adjSolarZenithAngle(month), daysInMonth(month), &
                    apar(:), lai_above(:), fi(:), lambda_v(:), lambda_h(:), canopy_vol_frac(:), layer_id(:), lai_sa_ratio(:))

                VPD_sp(:) = vpd_day(ii) * Exp(lai_above(:) * (-Log(2.d0)) / cVPD(:))
            end if


            ! Determine the various environmental modifiers which were not calculated before
            ! calculate VPD modifier
            ! Get within-canopy climatic conditions this is exponential function
            Height_max = maxval( height(:), mask=lai(:)>0.d0 )

            ! but since BLcond is a vector we can't use the expF
            aero_resist(:) = (1.d0 / BLcond(:)) + (5.d0 * sum( lai(:) ) - (1.d0 / BLcond(:))) * &
                Exp(-ln2 * ( height(:) / (Height_max / 2.d0)) ** 2.d0)
            ! if this is the highest tree
            where( height(:) == Height_max)
                aero_resist(:) = 1.d0 / BLcond(:)
            end where
            ! Check for dormancy
            where( lai(:) .eq. 0.d0)
                aero_resist(:) = 0.d0
            end where

            f_vpd(:) = Exp( -CoeffCond(:) * VPD_sp(:))

            ! soil water modifier
            f_sw(:) = 1.d0 / (1.d0 + ((1.d0 -  ASW / asw_max) / SWconst(:)) ** SWpower(:))

            ! soil nutrition modifier
            f_nutr(:) = 1.d0 - (1.d0 - fN0(:)) * (1.d0 - fertility(:)) ** fNn(:)
            where( fNn(:) == 0.d0 ) f_nutr(:) = 1.d0

            ! calculate physiological modifier applied to conductance and alphaCx.
            if ( phys_model .eq. int(1) ) then

                f_phys(:) = min( f_vpd(:), f_sw(:) ) * f_age(ii,:)
                f_tmp_gc(ii,:) = 1.d0

            else if ( phys_model .eq. int(2) ) then

                f_phys(:) = f_vpd(:) * f_sw(:) * f_age(ii,:)

            end if






!!-------------------------------------------------------------
!! Monthly update of long-term modifiers
!!-------------------------------------------------------------
!do i = 1, n_sp
!
!    ! Skip species not yet planted
!    if (age(month, i) < 0.d0) cycle
!
!    ! Update circular buffer index
!    hist_ptr(i) = mod(hist_ptr(i), lt_mod_mths) + 1
!
!    !---------------------------
!    ! 1) Temperature (lt_fT)
!    !---------------------------
!    fT_hist(i, hist_ptr(i)) = f_tmp(month, i)
!    lt_fT(i) = sum(fT_hist(i, 1:lt_mod_mths)) / real(lt_mod_mths, kind=8)
!
!    !---------------------------
!    ! 2) Physiological (lt_fPhys)
!    !---------------------------
!    fPhys_hist(i, hist_ptr(i)) = f_phys(i)   ! use precomputed f_phys for this month
!    lt_fPhys(i) = sum(fPhys_hist(i, 1:lt_mod_mths)) / real(lt_mod_mths, kind=8)
!
!end do



!-------------------------------------------------------------
! Monthly update of long-term modifiers
!-------------------------------------------------------------
do i = 1, n_sp

    ! Skip species not yet planted
    if (age(ii, i) < 0.d0) cycle

    ! Update circular buffer index
    hist_ptr(i) = mod(hist_ptr(i), lt_mod_mths) + 1

    !---------------------------
    ! 1) Temperature (lt_fT)
    !---------------------------
    fT_hist(i, hist_ptr(i)) = f_tmp(ii, i)
    lt_fT(i) = sum(fT_hist(i, 1:lt_mod_mths)) / real(lt_mod_mths, kind=8)

    !---------------------------
    ! 2) Physiological (lt_fPhys) — exclude age effect
    !---------------------------
    fPhys_hist(i, hist_ptr(i)) = f_phys(i) / f_age(ii, i)
    lt_fPhys(i) = sum(fPhys_hist(i, 1:lt_mod_mths)) / real(lt_mod_mths, kind=8)

end do

!!-------------------------------------------------------------
!! Debug: save first 5 months of fT_hist and fPhys_hist
!!-------------------------------------------------------------
!! ------------------------------
!        ! Optional: Save fT_hist and fPhys_hist for first 5 months
!        ! ------------------------------
!        if (ii <= 125) then
!            do i = 1, n_sp
!                ! File names
!                write(filenameT, '(A,I0,A,I0,A)') 'fT_hist_sp', i, '_month', ii, '.csv'
!                write(filenameP, '(A,I0,A,I0,A)') 'fPhys_hist_sp', i, '_month', ii, '.csv'
!
!                ! Open files for writing
!                open(unit=301, file=filenameT, status='replace', action='write', iostat=ios)
!                if (ios /= 0) stop 'Error opening fT_hist file'
!
!                open(unit=302, file=filenameP, status='replace', action='write', iostat=ios)
!                if (ios /= 0) stop 'Error opening fPhys_hist file'
!
!                ! Write the lt_mod_mths elements of the circular buffer
!                do kk = 1, lt_mod_mths
!                    write(301, '(F12.6)') fT_hist(i, kk)
!                    write(302, '(F12.6)') fPhys_hist(i, kk)
!                end do
!
!                close(301)
!                close(302)
!            end do
!        end if

























            ! Calculate assimilation before the water balance is done
            alpha_c(:) = alphaCx(:) * f_nutr(:) * f_tmp(ii,:) * f_frost(ii,:) * f_calpha(ii,:) * f_phys(:)
            where( lai(:) == 0.d0 ) alpha_c(:) = 0.d0
            epsilon(:) = gDM_mol * molPAR_MJ * alpha_c(:)
            GPP(:) = epsilon(:) * apar(:) / 100        ! tDM/ha (apar is MJ/m^2)
            NPP(:) = GPP(:) * y(:)                       ! assumes respiratory rate is constant



            ! Water Balance ----------------------------------------------------------------------
            ! Calculate each specie proportion
            lai_total(:) = sum( lai(:) )
            lai_per(:) = lai(:) / lai_total(:)
            where( lai_total(:) .eq. 0.d0 ) lai_per(:) = 0.d0

            ! Calculate conductance
            gC(:) = MaxCond(:)
            where( lai_total(:) <= LAIgcx(:) )
                gC(:) = MinCond(:) + (MaxCond(:) - MinCond(:)) * lai_total(:) / LAIgcx(:)
            end where

            conduct_canopy(:) = gC(:) * lai_per(:) * f_phys(:) * f_tmp_gc(ii,:) * f_cg(ii,:)
            conduct_soil = MaxSoilCond * ASW / asw_max


            ! Calculate transpiration
            if ( transp_model .eq. int(1) ) then

                call s_transpiration_3pgpjs( n_sp, solar_rad(ii), day_length(month), VPD_sp(:), BLcond(:), &
                    conduct_canopy(:), daysInMonth(month), Qa, Qb, &
                    transp_veg(:))
                evapotra_soil = 0.d0

            else if ( transp_model .eq. int(2) ) then

                call s_transpiration_3pgmix( n_sp, solar_rad(ii), vpd_day(ii), day_length(month), daysInMonth(month), &
                    lai(:), fi(:), VPD_sp(:), aero_resist(:), conduct_canopy(:), conduct_soil, Qa, Qb, &
                    transp_veg(:), evapotra_soil)

            end if

            transp_total = sum( transp_veg(:) ) + evapotra_soil


            ! rainfall interception
            prcp_interc_fract(:) = MaxIntcptn(:)
            where (LAImaxIntcptn(:) > 0.d0)
                prcp_interc_fract(:) = MaxIntcptn(:) * min(1.d0, lai_total(:) / LAImaxIntcptn(:)) * LAI_per(:)
            end where

            prcp_interc(:) = prcp(ii) * prcp_interc_fract(:)
            prcp_interc_total = sum( prcp_interc(:) )

            ! Do soil water balance Need to constrain irrigation only to the growing season
            ASW = ASW + prcp(ii) + (100.d0 * Irrig / 12.0d0) + water_runoff_polled
            evapo_transp = min( ASW, transp_total + prcp_interc_total)  !ET can not exceed ASW
            excessSW = max(ASW - evapo_transp - asw_max, 0.d0)
            ASW = ASW - evapo_transp - excessSW
            water_runoff_polled = poolFractn * excessSW
            prcp_runoff = (1.d0 - poolFractn) * excessSW

            if (ASW < asw_min) then
                irrig_supl = asw_min - ASW
                ASW = asw_min
            end if


            if ( ( transp_total + prcp_interc_total ) == 0 ) then
                !this might be close to 0 if the only existing species is dormant during this month
                ! (it will include the soil evaporation if Apply3PGpjswaterbalance = no)
                f_transp_scale = 1.
            else
                f_transp_scale = evapo_transp / (transp_total + prcp_interc_total)  !scales NPP and GPP
            end if

            ! correct for actual ET
            GPP = GPP * f_transp_scale
            NPP = NPP * f_transp_scale
            NPP_f = NPP


            if ( transp_total > 0 .and. f_transp_scale < 1 ) then
                ! a different scaler is required for transpiration because all of the scaling needs
                ! to be done to the transpiration and not to the RainIntcpth, which occurs regardless of the growth
                transp_veg(:) = (evapo_transp - prcp_interc_total) / transp_total * transp_veg(:)
                evapotra_soil = (evapo_transp - prcp_interc_total) / transp_total * evapotra_soil
            end if


            ! NEED TO CROSS CHECK THIS PART, DON'T FULLY AGREE WITH IT
            if ( evapo_transp /= 0.d0 .and. n_sp == 1 ) then
                ! in case ET is zero! Also, for mixtures it is not possible to calculate WUE based on
                ! ET because the soil evaporation cannot simply be divided between species.
                WUE(:) = 100.d0 * NPP(:) / evapo_transp
            else
                WUE(:) = 0.d0
            end if

            WUE_transp(:) = 0.d0
            where ( transp_veg(:) > 0.d0 )
                WUE_transp(:) = 100.d0 * NPP(:) / transp_veg(:)
            end where


            if ( calculate_d13c .eq. int(1) ) then
                ! d13C module ----------------------------------------------------------------------
                ! Calculating d13C - This is based on Wei et al. 2014 (Plant, Cell and Environment 37, 82-100)
                ! and Wei et al. 2014 (Forest Ecology and Management 313, 69-82). This is simply calculated from
                ! other variables and has no influence on any processes

                !convert GPP (currently in tDM/ha/month) to GPP in mol/m2/s.
                GPP_molsec(:) = GPP(:) * 100.d0 / ( daysInMonth(month) * 24.0d0 * 3600.0d0 * gDM_mol)

                !Canopy conductance for water vapour in mol/m2s, unit conversion (CanCond is m/s)
                Gw_mol(:) = conduct_canopy(:) * 44.6d0 * (273.15d0 / (273.15d0 + tmp_ave(ii) ) ) * (air_pressure / 101.3d0)

                ! Canopy conductance for CO2 in mol/m2s
                ! This calculation needs to consider the area covered by leaves as opposed to the total ground area of the stand.
                ! The explanation that Wei et al. provided for adding the "/Maximum(0.0000001, CanCover)" is
                ! that 3PG is a big leaf leaf model for conductance and the leaf area is assumed to be evenly distributed
                ! across the land area. So GwMol is divided by Maximum(0.0000001, CanCover) to convert the conductance
                ! to the area covered by the leaves only, which is smaller than the land area if the canopy has not
                ! closed. If the original light model has been selected then a CanCover value has already been calculated
                ! although Wei et al. also warn against using d13C calculations in stands with CanCover < 1.
                ! If the new light model has been selected then CanCover still needs to be calculated.

                canopy_cover(:) = (stems_n(:) * (crown_width(:) + 0.25d0) ** 2.d0) / 10000.d0
                where( canopy_cover(:) > 1.d0) canopy_cover(:) = 1.d0

                Gc_mol(:) = Gw_mol(:) * RGcGW(:) / max(0.0000001d0, canopy_cover(:))

                !Calculating monthly average intercellular CO2 concentration. Ci = Ca - A/g
                InterCi(:) = CO2(ii) * 0.000001d0 - GPP_molsec(:) / Gc_mol(:)

                !Calculating monthly d13C of new photosynthate, = d13Catm- a-(b-a) (ci/ca)
                D13CNewPS(:) = d13Catm(ii) - aFracDiffu(:) - (bFracRubi(:) - aFracDiffu(:)) * (InterCi(:) / (CO2(ii) * 0.000001d0))
                D13CTissue(:) = D13CNewPS(:) + D13CTissueDif(:)

                ! correct for dormancy
                where( Gc_mol(:) .eq. 0.d0 )
                    InterCi(:) = 0.d0
                    D13CNewPS(:) = 0.d0
                    D13CTissue(:) = 0.d0
                end where

            end if


            ! Biomass increment and loss module ----------------------------------------------
            ! determine biomass increments and losses
            m(:) = m0(:) + (1.d0 - m0(:)) * fertility(:)

            npp_fract_root(:) = pRx(:) * pRn(:) / (pRn(:) + (pRx(:) - pRn(:)) * f_phys * m(:))
            npp_fract_stem(:) = (1.d0 - npp_fract_root(:)) / (1.d0 + pFS(:))
            npp_fract_foliage(:) = 1.d0 - npp_fract_root(:) - npp_fract_stem(:)


            do i = 1, n_sp

                !  Dormant period -----------
                if ( f_dormant(month, leafgrow(i), leaffall(i)) .eqv. .TRUE. ) then

                    ! There is no increment. But if this is the first dormant period then there is litterfall
                    if ( f_dormant(month-1, leafgrow(i), leaffall(i))  .eqv. .TRUE. ) then
                        biom_loss_foliage(i) = 0.d0
                    else
                        biom_loss_foliage(i) = biom_foliage_debt(i)
                    end if

                    biom_loss_root(i) = 0.d0


                    ! No changes during dormant period
                    biom_incr_foliage(i) = 0.d0
                    biom_incr_root(i) = 0.d0
                    biom_incr_stem(i) = 0.d0

                else

                    ! Leaves are now re-created from carbohydrates and the NPP is
                    ! distributed amont the compartments
                    if( biom_foliage(i) == 0.d0 ) then
                        biom_foliage(i) = biom_foliage_debt(i)
                        biom_foliage_debt(i) = 0.d0
                    end if

                    ! Calculate biomass loss
                    biom_loss_foliage(i) = gammaF(ii, i) * biom_foliage(i)
                    biom_loss_root(i) = gammaR(i) * biom_root(i)


                    ! Calculate biomass increments
                    biom_incr_foliage(i) = NPP(i) * npp_fract_foliage(i)
                    biom_incr_root(i) = NPP(i) * npp_fract_root(i)
                    biom_incr_stem(i) = NPP(i) * npp_fract_stem(i)


                    ! Calculate end-of-month biomass
                    biom_foliage(i) = biom_foliage(i) + biom_incr_foliage(i) - biom_loss_foliage(i)
                    biom_root(i) = biom_root(i) + biom_incr_root(i) - biom_loss_root(i)
                    biom_stem(i) = biom_stem(i) + biom_incr_stem(i)

                end if

            end do

            ! Correct the bias
            biom_tree(:) = biom_stem(:) * 1000.d0 / stems_n(:)  ! kg/tree
            where( stems_n(:) .eq. 0.d0 ) biom_tree(:) = 0.d0

            lai(:) =  biom_foliage(:) * SLA(ii,:) * 0.1d0

            do n = 1, b_n
                competition_total(:) = sum( wood_density(ii,:) * basal_area(:) )

                call s_sizeDist_correct(n_sp, age(ii,:), stems_n(:), biom_tree(:), competition_total(:), lai(:), &
                    correct_bias, height_model,  pars_i(69:86,:), pars_b, aWs(:), nWs(:), pfsPower(:), pfsConst(:), &    !20241106
                    dbh(:), basal_area(:), height(:), crown_length(:), crown_width(:), pFS(:), bias_scale(:,:) )
            end do

            ! Volume and Volume increment
            ! This is done before thinning and mortality part
            volume(:) = biom_stem(:) * (1.d0 - fracBB(ii,:)) / wood_density(ii,:)
            where( aV(:) > 0 ) volume(:) = aV(:) * dbh(:) ** nVB(:) * height(:) ** nVH(:) * &
                    (dbh(:) * dbh(:) * height(:)) ** nVBH(:) * stems_n(:)

            volume_change(:) = volume(:) - volume_old(:)
            where( lai(:) .eq. 0.d0 ) volume_change(:) = 0.d0
            where( volume_change(:) .le. 0.d0 ) volume_change(:) = 0.d0

            volume_cum(:) = volume_cum(:) + volume_change(:)
            volume_old(:) = volume(:)

            volume_mai(:) = volume_cum(:) / age(ii,:)



            ! Management -------------------------------------------------------------------------
            !reset mortality value !20250301
            stems_loss_manag(:) = 0.d0
            biom_loss_stem_manag(:) = 0.d0
            biom_loss_root_manag(:) = 0.d0
            biom_loss_foliage_manag(:) = 0.d0

            do i = 1, n_sp

                if( t_t(i) > 0 ) then

                    if(t_n(i) <= t_t(i)) then

                        if( age(ii,i) >= managementInputs(t_n(i),1,i) ) then

                            if( stems_n(i) > managementInputs(t_n(i),2,i) .OR. (manag_model .EQ. 2) ) then !20251114

                                ! Calculate the proportion of management based on the stems (manag_model = 1, default)
                                ! or biomass (manag_model = 2) !20250314
                                if ( manag_model .eq. int(1) ) then
                                    manag_remove_prop = (stems_n(i) - managementInputs(t_n(i),2,i) ) / stems_n(i)
                                else
                                    manag_remove_prop = 1.d0 - managementInputs(t_n(i),6,i) !20251114
                                end if

                                ! recalculate the proportion to be removed for each compartment separately
                                ! positions in the vector 1=stem, 2-root, 3 = foliage
                                manag_remove_prop_compartment(:) = manag_remove_prop * managementInputs(t_n(i),3:5,i)

                                ! if the stand is thinned from above, then the ratios (F, R and S) of stem,
                                ! foliage and roots to be removed relative to the mean tree in the stand
                                ! will be >1. If the product of this ratio and delN is >= 1 then the new
                                ! WF, WR or WS will be < 0, which is impossible. Therefore, make sure this is >= 0.
                                ! 20250314 it shall be >= 1 (not > 1). Since if this equal 1 all trees will be removed
                                ! Therefore we set all the compartments proportion to 1 meaning that everything will be removed

                                if( maxval( manag_remove_prop_compartment(:) ) >= 1.d0 ) then
                                    manag_remove_prop_compartment(:) = 1.d0
                                    manag_remove_prop = 1.d0
                                end if

                                ! Calculate the losses in management & the stand values after management
                                stems_loss_manag(i) = stems_n(i) * manag_remove_prop
                                biom_loss_stem_manag(i) = biom_stem(i) * manag_remove_prop_compartment(1)
                                biom_loss_root_manag(i) = biom_root(i) * manag_remove_prop_compartment(2)

                                stems_n(i) = stems_n(i) - stems_loss_manag(i)
                                biom_stem(i) = biom_stem(i) - biom_loss_stem_manag(i)
                                biom_root(i) = biom_root(i) - biom_loss_root_manag(i)

                                if( f_dormant(month, leafgrow(i), leaffall(i)) .eqv. .TRUE. ) then
                                    biom_loss_foliage_manag(i) = biom_foliage_debt(i) * manag_remove_prop_compartment(3)
                                    biom_foliage_debt(i) = biom_foliage_debt(i) - biom_loss_foliage_manag(i)
                                else
                                    biom_loss_foliage_manag(i) = biom_foliage(i) * manag_remove_prop_compartment(3)
                                    biom_foliage(i) = biom_foliage(i) - biom_loss_foliage_manag(i)
                                end if

                                b_cor = .TRUE.

                            end if

                            t_n(i) = t_n(i) + 1

                        end if

                    end if

                end if

            end do

            ! Correct the bias
            if ( b_cor .eqv. .TRUE. ) then

                biom_tree(:) = biom_stem(:) * 1000.d0 / stems_n(:)  ! kg/tree
                where( stems_n(:) .eq. 0.d0 ) biom_tree(:) = 0.d0
                lai(:) =  biom_foliage(:) * SLA(ii,:) * 0.1d0

                do n = 1, b_n
                    competition_total(:) = sum( wood_density(ii,:) * basal_area(:) )

                    call s_sizeDist_correct(n_sp, age(ii,:), stems_n(:), biom_tree(:), competition_total(:), lai(:), &
                        correct_bias, height_model,  pars_i(69:86,:), pars_b, aWs(:), nWs(:), pfsPower(:), pfsConst(:), &    !20241106
                        dbh(:), basal_area(:), height(:), crown_length(:), crown_width(:), pFS(:), bias_scale(:,:) )
                end do

                ! Adjust the old wolume after thinning
                volume(:) = biom_stem(:) * (1.d0 - fracBB(ii,:)) / wood_density(ii,:)
                where( aV(:) > 0 ) volume(:) = aV(:) * dbh(:) ** nVB(:) * height(:) ** nVH(:) * &
                    (dbh(:) * dbh(:) * height(:)) ** nVBH(:) * stems_n(:)

                volume_old(:) = volume(:)

                b_cor = .FALSE.
            end if


            ! Defoliation --------------------------------------------------------------------------
            !reset defoliation value
            stems_loss_def(:) = 0.d0
            biom_loss_stem_def(:) = 0.d0
            biom_loss_root_def(:) = 0.d0
            biom_loss_foliage_def(:) = 0.d0
            def_type(:) = 0

            do i = 1, n_sp


                if( d_t(i) > 0 ) then

                    if(d_n(i) <= d_t(i)) then

                        if( age(ii,i) >= defoliationInputs(d_n(i),1,i) ) then

                            ! Check wether we need to put the defoliation type back to default after first month
                            def_type(i) = int( defoliationInputs(d_n(i),2,i))
                            def_recover_t(i) = defoliationInputs(d_n(i),7,i)

                            ! Adjust pre-defoliation foliage mass
                            if( def_type(i) == 1 .or. def_type(i) == 3 ) then
                                biom_foliage_adj_pre_def(i) = biom_foliage(i) * defoliationInputs(d_n(i),3,i)
                            else if (def_type(i) == 2 ) then ! copice event
                                biom_foliage_adj_pre_def(i) = biom_foliage(i) * defoliationInputs(d_n(i),5,i)
                                ! coppice_sr_ratio(i) = biom_stem(i) / biom_root(i)

                                !! TODO we need to re-calcualte the f_age after this
                                age(ii,i)  = 1.d0 / 12.d0

                            else if (def_type(i) == 4) then ! stand replacing
                                def_recover_t(i) = 0.d0
                            else
                                biom_foliage_adj_pre_def(i) = 0.0d0
                            end if

                            age_last_def_event(i) = age(ii, i)

                            if( f_dormant(month, leafgrow(i), leaffall(i)) .eqv. .TRUE.) then
                                ! foliage debt depends on stems that can resprout !20250301
                                biom_loss_foliage_def(i) = biom_foliage_debt(i) * (1.d0 - defoliationInputs(d_n(i),3,i)) !defol_stem_mass_prop_retained
                                biom_foliage_debt(i) = biom_foliage_debt(i) - biom_loss_foliage_def(i)

                                ! if a prune or epicormic event occured during dormant season, then no foliage could have been removed
                                if ( def_type(i) == 1 .or. def_type(i) == 3 ) then
                                    def_recover_t(i) = 0.d0
                                end if
                            else
                                !biom_loss_foliage_def(i) = biom_foliage(i) * (1.d0 - defoliationInputs(d_n(i),3,i)) * (1.d0 - defoliationInputs(d_n(i),4,i)) !defol_stem_mass_prop_retained
                                biom_loss_foliage_def(i) = biom_foliage(i) * (1.d0 - defoliationInputs(d_n(i),3,i)) &
                         * (1.d0 - defoliationInputs(d_n(i),4,i)) !defol_stem_mass_prop_retained
                                biom_foliage(i) = biom_foliage(i) - biom_loss_foliage_def(i)
                            end if

                            biom_loss_stem_def(i) = biom_stem(i) * (1.d0 - defoliationInputs(d_n(i),3,i))
                            biom_loss_root_def(i) = biom_root(i) * (1.d0 - defoliationInputs(d_n(i),5,i))

                            biom_stem(i) = biom_stem(i) - biom_loss_stem_def(i)
                            biom_root(i) = biom_root(i) - biom_loss_root_def(i)

                            ! if root biomass declined, there was mortality, so update stems_n
                            if( defoliationInputs(d_n(i),5,i) < 1 ) then
                                ! save the current value of mort_defol(i), before it is changed, so that the losses due to thinning can be calculated below.
                                !mort_defol(i) = stems_n(i)

                                ! if smaller trees are killed, then the ratio of stem,
                                ! to be removed relative to the mean tree in the stand
                                ! will be < 1. If the defol_stem_mass_prop_retained/ratio is > 1 then the new
                                ! stems_n will be > pre-disturbance stems_n, which is impossible. Therefore, make sure defol_stem_mass_prop_retained/ratio <= 1.
                                ! note that this is based on stem fraction, not root or foliage fractions
                                if(  defoliationInputs(d_n(i),3,i) / defoliationInputs(d_n(i),6,i) > 1.d0 ) then ! this would mean post stems_n > pre stems_n

                                    biom_loss_stem_def(i) = 0.d0
                                    !stems_n(i) = stems_n(i)
                                    ! this we need to keep, so we don't remove the stems if no mortality
                                else
                                    !biom_loss_stem_def(i) = stems_n(i) * (1.d0 - defoliationInputs(d_n(i),5,i)) /  defoliationInputs(d_n(i),6,i)
                                    biom_loss_stem_def(i) = stems_n(i) * (1.d0 - defoliationInputs(d_n(i),5,i)) &
                                    /  defoliationInputs(d_n(i),6,i)
                                    !stems_n(i) = stems_n(i) * defol_root_mass_prop_retained / defol_stem
                                    stems_n(i) = biom_loss_stem_def(i)
                                end if
                            end if

                            b_cor = .TRUE.

                            d_n(i) = d_n(i) + 1

                        end if

                    end if

                end if

            end do



            ! Mortality --------------------------------------------------------------------------

            ! Stress related ------------------
            !reset mortality value !20250301
            stems_loss_stress(:) = 0.d0
            biom_loss_stem_stress(:) = 0.d0
            biom_loss_root_stress(:) = 0.d0
            biom_loss_foliage_stress(:) = 0.d0

            do i = 1, n_sp
                if( f_dormant(month, leafgrow(i), leaffall(i)) .eqv. .FALSE.) then

                    if ( gammaN(ii,i) > 0.d0 ) then

                        stems_loss_stress(i) = gammaN(ii,i) * stems_n(i) / 12.d0 /100.d0
                        stems_loss_stress(i) = min( stems_loss_stress(i), stems_n(i)) ! Mortality can't be more than available

                        biom_loss_stem_stress(i) = mS(i) * biom_stem(i) * stems_loss_stress(i) / stems_n(i)
                        biom_loss_root_stress(i) = mR(i) * biom_root(i) * stems_loss_stress(i) / stems_n(i)
                        biom_loss_foliage_stress(i) = mF(i) * biom_foliage(i) * stems_loss_stress(i) / stems_n(i)

                        stems_n(i) = stems_n(i) - stems_loss_stress(i)
                        biom_stem(i) = biom_stem(i) - biom_loss_stem_stress(i)
                        biom_root(i) = biom_root(i) - biom_loss_root_stress(i)
                        biom_foliage(i) = biom_foliage(i) -  biom_loss_foliage_stress(i)

                        b_cor = .TRUE.

                    end if
                end if
            end do


            ! Correct the bias
            if ( b_cor .eqv. .TRUE. ) then

                biom_tree(:) = biom_stem(:) * 1000.d0 / stems_n(:)  ! kg/tree
                where( stems_n(:) .eq. 0.d0 ) biom_tree(:) = 0.d0
                lai(:) =  biom_foliage(:) * SLA(ii,:) * 0.1d0

                do n = 1, b_n
                    competition_total(:) = sum( wood_density(ii,:) * basal_area(:) )

                    call s_sizeDist_correct(n_sp, age(ii,:), stems_n(:), biom_tree(:), competition_total(:), lai(:), &
                        correct_bias, height_model,  pars_i(69:86,:), pars_b, aWs(:), nWs(:), pfsPower(:), pfsConst(:), &    !20241106
                        dbh(:), basal_area(:), height(:), crown_length(:), crown_width(:), pFS(:), bias_scale(:,:) )
                end do

                b_cor = .FALSE.
            end if

            ! Self-thinning / Density dependent related ------------------
            stems_loss_density(:) = 0.d0
            biom_loss_stem_density(:) = 0.d0
            biom_loss_root_density(:) = 0.d0
            biom_loss_foliage_density(:) = 0.d0

            basal_area_prop(:) = basal_area(:) / sum( basal_area(:) )
            stems_n_ha(:) = stems_n(:) / basal_area_prop(:)

            ! Get the total numbers of the alived trees for the further calculations
            stems_n_total(:) = sum( stems_n (:) )
            basal_area_total(:) = sum( basal_area (:) )
            dbh_total(:) = sum( dbh(:) * stems_n(:) ) / stems_n_total(:)

            ! Calculate the weighted average of the Long-term modifiers when the mort_model = 2 !20241211
            lt_fN_ave(:) = sum( lt_fN(:) * basal_area_prop(:) )
            lt_fT_ave(:) = sum( lt_fT(:) * basal_area_prop(:) )
            lt_fPhys_ave(:) = sum( lt_fPhys(:) * basal_area_prop(:) )

            biom_tree_max(:) = wSx1000(:) * (1000.d0 / stems_n_ha(:)) ** thinPower(:)

            ! do not calculate density-dependent mortality for any cohorts if there was already mortality for any single cohort (because dbh_prev(:) and dbh_total_prev will be inappropriate) ! 20250301
            stems_loss_total = sum(stems_loss_manag(:)  + stems_loss_stress(:)) !+ mort_defol(:)
            !stems_loss_total = 0.0d0

            if( stems_loss_total < 1.0e-6 ) then

                do i = 1, n_sp

                    if( f_dormant(month, leafgrow(i), leaffall(i)) .eqv. .FALSE.) then

                        if ( mort_model .eq. int(1) ) then        !20241106

                            if ( biom_tree_max(i) < biom_tree(i) ) then

                                stems_loss_density(i) = f_get_mortality( stems_n_ha(i), biom_stem(i) / basal_area_prop(i) , &
                                mS(i), wSx1000(i), thinPower(i) ) * basal_area_prop(i)

                            end if

                        else if ( mort_model .eq. int(2) ) then !20241106

                            mort_thinn_total(i) = ( (stems_n_total(i) - ( &
                                stems_n_total(i) ** (1 - betaN(i)) + Exp(beta0(i)) * (1 - betaN(i)) / (betaB(i) + 1) * &
                                (dbh_total_prev(i) ** (betaB(i) + 1) * lt_fN_ave(i) ** betafN(i) * lt_fT_ave(i) ** betafT(i) * &
                                lt_fPhys_ave(i) ** betafPhys(i) - dbh_total(i) ** (betaB(i) + 1) * lt_fN_ave(i) ** betafN(i) * &
                                lt_fT_ave(i) ** betafT(i) * lt_fPhys_ave(i) ** betafPhys(i))) ** (1 / (1 - betaN(i))) ))

                            stems_loss_density(i) = mort_thinn_total(i) * Pi * dbh_total(i) * dbh_total(i) / 40000 / &
                                basal_area_total(i) * basal_area(i) / (Pi * dbh(i) * dbh(i) / 40000)

                            !b_cor = .TRUE. !20241106

                        end if !20241106

                        ! It happends that somethines stems_loss_density provide negative values
                        ! this shall be neglected

                        if( stems_loss_density(i) <= 0.d0) then
                            stems_loss_density(i) = 0.d0
                        end if


                        if( stems_loss_density(i) > 0.d0) then !20241106

                            biom_loss_stem_density(i) = mS(i) * biom_stem(i) * stems_loss_density(i) / stems_n(i)
                            biom_loss_root_density(i) = mR(i) * biom_root(i) * stems_loss_density(i) / stems_n(i)
                            biom_loss_foliage_density(i) = mF(i) * biom_foliage(i) * stems_loss_density(i) / stems_n(i)

                            stems_n(i) = stems_n(i) - stems_loss_density(i)
                            biom_stem(i) = biom_stem(i) - biom_loss_stem_density(i)
                            biom_root(i) = biom_root(i) - biom_loss_root_density(i)
                            biom_foliage(i) = biom_foliage(i) - biom_loss_foliage_density(i)

                            b_cor = .TRUE. !20241106

                        end if

                        if( stems_n(i) <= 0) then !20241118
                            biom_foliage(i) = 0.d0
                            biom_root(i) = 0.d0
                            biom_stem(i) = 0.d0
                            stems_n(i) = 0.d0
                        end if

                    end if
                end do

            end if


            ! Correct the bias
            if ( b_cor .eqv. .TRUE. ) then

                biom_tree(:) = biom_stem(:) * 1000.d0 / stems_n(:)  ! kg/tree
                where( stems_n(:) .eq. 0.d0 ) biom_tree(:) = 0.d0
                lai(:) =  biom_foliage(:) * SLA(ii,:) * 0.1d0

                do n = 1, b_n
                    competition_total(:) = sum( wood_density(ii,:) * basal_area(:) )

                    call s_sizeDist_correct(n_sp, age(ii,:), stems_n(:), biom_tree(:), competition_total(:), lai(:), &
                        correct_bias, height_model,  pars_i(69:86,:), pars_b, aWs(:), nWs(:), pfsPower(:), pfsConst(:), &    !20241106
                        dbh(:), basal_area(:), height(:), crown_length(:), crown_width(:), pFS(:), bias_scale(:,:) )
                end do

                b_cor = .FALSE.
            end if



            ! Additional calculations ------------------
            basal_area_prop(:) = basal_area(:) / sum( basal_area(:) )

            ! Used when mort_model = 2   !20241106
            dbh_prev(:) = dbh(:)
            dbh_total_prev = dbh_total(:)

            ! Efficiency
            epsilon_gpp(:) = 100 * gpp(:) / apar(:)
            epsilon_npp(:) = 100 * npp_f(:) / apar(:)
            epsilon_biom_stem(:) = 100 * biom_incr_stem(:) / apar(:)

            where( apar(:) .eq. 0.d0 )
                epsilon_gpp(:) = 0.d0
                epsilon_npp(:) = 0.d0
                epsilon_biom_stem(:) = 0.d0
            end where

            ! Save end of the month results
            include 'i_write_out.h'

        end do

    end subroutine s_3PG_f

    !*************************************************************************************
    ! FUNCTIONS

    function f_dormant(month, leafgrow, leaffall) result( out )

        implicit none

        ! input
        integer, intent(in) :: month, leafgrow, leaffall

        ! output
        logical :: out

        out = .FALSE.

        ! This is called if the leafgrow parameter is not 0, and hence the species is Deciduous
        ! This is true if "currentmonth" is part of the dormant season
        if ( leafgrow > leaffall ) then
            ! check which hemisphere
            if  ( month >= leaffall .and. month <= leafgrow ) then ! growing at winter
                out = .TRUE.
            end if
        else if ( leafgrow < leaffall ) then
            if ( month < leafgrow .or. month >= leaffall ) then ! growing at summer
                out = .TRUE.
            end if
        end if

    end function f_dormant


    function f_exp(n_m, x, g0, gx, tg, ng) result( out )

        implicit none

        ! input
        integer, intent(in) :: n_m
        real(kind=kind(0.0d0)), dimension(n_m), intent(in) :: x
        real(kind=kind(0.0d0)), intent(in) :: g0, gx, tg, ng

        ! output
        real(kind=kind(0.0d0)), dimension(n_m) :: out

        out(:) = gx

        if ( tg /= 0.d0 ) then
            out(:) = gx + (g0 - gx) * Exp(-ln2 * ( x(:) / tg) ** ng)
        end if

    end function f_exp


    function f_exp_foliage(n_m, x, f1, f0, tg) result( out )

        implicit none

        ! input
        integer, intent(in) :: n_m
        real(kind=kind(0.0d0)), dimension(n_m), intent(in) :: x
        real(kind=kind(0.0d0)), intent(in) :: f1, f0, tg

        ! output
        real(kind=kind(0.0d0)), dimension(n_m) :: out

        ! local
        real(kind=kind(0.0d0)) :: kg

        if( tg * f1 == 0.d0 ) then
            out(:) = f1
        else
            kg = 12.d0 * Log(1.d0 + f1 / f0) / tg
            out(:) = f1 * f0 / (f0 + (f1 - f0) * Exp(-kg * x))
        end if

    end function f_exp_foliage


    function f_gamma_dist( x, n ) result( out )

        implicit none

        ! input
        integer, intent(in) :: n
        real(kind=kind(0.0d0)), dimension(n), intent(in) :: x

        ! output
        real(kind=kind(0.0d0)), dimension(n) :: out

        out = x ** (x - 0.5d0) * 2.718282d0 ** (-x) * (2.d0 * Pi) ** (0.5d0) * &
            (1.d0 + 1.d0 / (12.d0 * x) + 1.d0 / (288.d0 * x ** 2.d0) - 139.d0 / (51840.d0 * x ** 3.d0) - &
            571.d0 / (2488320.d0 * x ** 4.d0))

    end function f_gamma_dist


    function f_get_daylength( Lat ) result( day_length )
        ! Day-length calculations

        implicit none

        ! input
        real(kind=kind(0.0d0)), intent(in) :: Lat

        ! output
        real(kind=kind(0.0d0)), dimension(12) :: day_length

        ! local
        real(kind=kind(0.0d0)) :: SLAt, cLat
        real(kind=kind(0.0d0)), dimension(12) :: sinDec, cosH0


        SLAt = sin(Pi * Lat / 180.d0)
        cLat = cos(Pi * Lat / 180.d0)
        sinDec(:) = 0.4d0 * sin(0.0172d0 * (dayOfYear(:) - 80.d0) )
        cosH0(:) = -sinDec(:) * SLAt / (cLat * sqrt(1.d0 - (sinDec(:)) ** 2.d0))

        day_length(:) = Acos(cosH0(:)) / Pi

        where( cosH0 > 1.d0 ) day_length = 0.d0
        where( cosH0 < -1.d0 ) day_length = 1.d0

    end function f_get_daylength


    function f_get_layer ( n_sp, height, Heightcrown) result(layer_id)
        ! function to allocate each tree to the layer based on height and crown heigh
        ! First layer (1) is the highest
        ! According to Forrester, D.I., Guisasola, R., Tang, X. et al. For. Ecosyst. (2014) 1: 17.
        ! Calculations based on example https://it.mathworks.com/matlabcentral/answers/366626-overlapping-time-intervals

        implicit none

        integer, intent(in) :: n_sp ! number of species
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: height, Heightcrown

        ! output
        integer, dimension(n_sp) :: layer_id ! array of layer id

        ! local
        real(kind=kind(0.0d0)), dimension( n_sp*2 ) :: Height_all
        integer, dimension( n_sp*2 ) :: Height_ind
        integer, dimension( n_sp*2 ) :: ones,  ones_sum ! vector of 1, 0, -1 for calculation
        real(kind=kind(0.0d0)), allocatable, dimension(:) :: Height_layer ! maximum height of each layer

        integer :: i
        integer :: n_l

        ! Sort all height and crown heigh
        Height_all = [Heightcrown(:), height(:)] ! put height and crown beginning into vector
        Height_ind = f_orderId(Height_all) ! sort the array

        ! Assign index order for further calculations
        ones(:) = -1
        ones(1:n_sp) = 1
        ones = ones(Height_ind)

    !   cumulative sum
        ones_sum = 0
        do i = 1, n_sp*2
            if (i == 1) then
                ones_sum(i) = ones(i)
            else
                ones_sum(i) = ones_sum(i-1) + ones(i)
            end if
        end do

        ! Max height of each layer
        n_l = count(ones_sum == 0)
        allocate( Height_layer(n_l) )
        Height_layer(:) = 0
        Height_layer = Height_all(PACK(Height_ind, ones_sum == 0))

        ! Assign layer to each species
        layer_id(:) = 1
        if( n_l > 1 ) then
            do i = 1, n_l-1
                where ( height(:) > Height_layer(i) ) layer_id(:) = i+1
            end do
        end if

        deallocate( Height_layer )

        ! revert the order, so highest trees are 1 layer and lowest is n
        layer_id(:) = maxval( layer_id(:) ) - layer_id(:) + 1

    end function f_get_layer


    function f_get_layer_sum ( n_sp, nLayers, x, layer_id) result (y)
        ! function to sum any array x, based on the vector of layers id

        implicit none

        ! input
        integer, intent(in) :: n_sp, nLayers ! number of species and layers
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: x
        integer, dimension(n_sp), intent(in) :: layer_id

        ! output
        real(kind=kind(0.0d0)), dimension(n_sp) :: y

        ! local
        integer :: i = 1

        y(:) = 0.d0

        do i = 1, nLayers
            where ( layer_id(:) == i )
                y(:) = sum(x(:), mask=layer_id(:)==i)
            end where
        end do

    end function f_get_layer_sum


    function f_get_mortality(stems_n, WS, mS, wSx1000, thinPower) result(mort_n)
        ! calculate the mortality

        implicit none

        !input
        real(kind=kind(0.0d0)), intent(in) :: stems_n, WS, mS, wSx1000, thinPower

        ! output
        real(kind=kind(0.0d0)) :: mort_n

        ! local
        real(kind=kind(0.0d0)), parameter :: accuracy = 1.d0 / 1000.d0
        integer :: i
        real(kind=kind(0.0d0)) :: fN,dfN,dN,n,x1,x2


        n = stems_n / 1000.d0
        x1 = 1000.d0 * mS * WS / stems_n
        i = 0

        do
            i = i + 1

            if (n <= 0.d0) exit !added in 3PG+

            x2 = wSx1000 * n ** (1.d0 - thinPower)
            fN = x2 - x1 * n - (1.d0 - mS) * WS
            dfN = (1.d0 - thinPower) * x2 / n - x1
            dN = -fN / dfN
            n = n + dN

            if (abs(dN) <= accuracy .Or. i >= 5) exit

        end do

        mort_n = stems_n - 1000.d0 * n

    end function f_get_mortality


    function f_get_solarangle( Lat ) result( solarangle )

        implicit none

        ! input
        real(kind=kind(0.0d0)), intent(in) :: Lat

        ! output
        real(kind=kind(0.0d0)), dimension(12) :: solarangle

        ! local
        real(kind=kind(0.0d0)) :: secondxaxisintercept, firstxaxisintercept
        real(kind=kind(0.0d0)), dimension(12) :: gamma, declinationangle, szaprep, solarzenithangle


        secondxaxisintercept = 0.0018d0 * Lat ** 3.d0 - 0.0031d0 * Lat ** 2.d0 + 2.3826d0 * Lat + 266.62d0
        firstxaxisintercept = -0.0018d0 * Lat ** 3.d0 + 0.0021d0 * Lat ** 2.d0 - 2.3459d0 * Lat + 80.097d0

        gamma(:) = 2.d0 * Pi / 365.d0 * ( dayOfYear(:) - 1.d0)

        declinationangle(:) = 0.006918d0 - (0.399912d0 * Cos(gamma(:))) + 0.070257d0 * Sin(gamma(:)) - &
            0.006758d0 * Cos(2.d0 * gamma(:)) + 0.000907d0 * Sin(2.d0 * gamma(:)) - 0.002697d0 * Cos(3.d0 * gamma(:)) + &
            0.00148d0 * Sin(3.d0 * gamma(:))

        szaprep(:) = Sin(Pi / 180.d0 * Lat * ( -1.d0) ) * Sin(declinationangle(:)) + &
            Cos(Pi / 180.d0 * Lat * (-1.d0) ) * Cos(declinationangle(:))
        solarzenithangle(:) = 180.d0 / Pi * (Atan(-szaprep(:) / ((-szaprep(:) * szaprep(:) + 1.d0) ** 0.5d0)) + 2.d0 * Atan(1.d0))

        solarangle(:) = solarzenithangle(:)

        if ( Lat >= 0.d0 .and. Lat <= 23.4d0) Then
            !the zenith angle only needs to be adjusted if the lat is between about -23.4 and 23.4
            where( dayOfYear(:) > secondxaxisintercept .or. dayOfYear(:) < firstxaxisintercept )
                solarangle(:) = -1.d0 * solarzenithangle(:)
            end where
        end if

        if (  Lat >= -23.4d0 .and. Lat < 0.d0 ) Then
            !the zenith angle only needs to be adjusted if the lat is between about -23.4 and 23.4
            where( dayOfYear(:) > firstxaxisintercept .and. dayOfYear(:) < secondxaxisintercept )
                solarangle(:) = -1.d0 * solarzenithangle(:)
            end where
        end if

    end function f_get_solarangle


    function f_orderId(x) result(id)
        ! Returns the indices that would sort an array.

        implicit none

        ! input
        real(kind=kind(0.0d0)), intent(in) :: x(:)       ! array of numbers

        ! output
        integer :: id( size(x) )            ! indices into the array 'x' that sort it

        ! local
        integer :: i, n, imin, temp1        ! helpers
        real(kind=kind(0.0d0)) :: temp2
        real(kind=kind(0.0d0)) :: x2( size(x) )

        x2 = x
        n = size(x)

        do i = 1, n
            id(i) = i
        end do

        do i = 1, n-1
            ! find ith smallest in 'a'
            imin = minloc(x2(i:),1) + i - 1
            ! swap to position i in 'a' and 'b', if not already there
            if (imin /= i) then
                temp2 = x2(i); x2(i) = x2(imin); x2(imin) = temp2
                temp1 = id(i); id(i) = id(imin); id(imin) = temp1
            end if
        end do
    end function f_orderId


    function p_min_max ( x, mn, mx, n ) result( out )
        ! correct the values to be within the minimum and maximum range

        implicit none

        ! input
        integer, intent(in) :: n
        real(kind=kind(0.0d0)), intent(in) :: mn, mx
        real(kind=kind(0.0d0)), dimension(n) :: x

        ! output
        real(kind=kind(0.0d0)), dimension(n) :: out

        where( x(:) > mx) x(:) = mx
        where( x(:) < mn) x(:) = mn

        out = x

    end function p_min_max


    subroutine s_light_3pgpjs ( n_sp, age, fullCanAge, k, lai, solar_rad, days_in_month, &
        canopy_cover, apar )

        implicit none

        ! input
        integer, intent(in) :: n_sp ! number of species
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: age
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: fullCanAge     ! Age at canopy closure
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: k
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: lai
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: solar_rad
        integer, dimension(n_sp), intent(in) :: days_in_month

        ! output
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: canopy_cover
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: apar

        ! Additional variables for calculation distribution
        real(kind=kind(0.0d0)), dimension(n_sp) :: RADt ! Total available radiation
        real(kind=kind(0.0d0)), dimension(n_sp) :: lightIntcptn


        canopy_cover(:) = 1.d0
        where (fullCanAge(:) > 0.d0 .and. age(:) < fullCanAge(:) )
            canopy_cover(:) = (age(:) + 0.01d0) / fullCanAge(:)
        end where

        lightIntcptn = (1.d0 - (Exp(-k * lai / canopy_cover)))

        RADt = solar_rad * days_in_month ! MJ m-2 month-1
        apar = RADt * lightIntcptn * canopy_cover

    end subroutine s_light_3pgpjs


    subroutine s_light_3pgmix ( n_sp, height, crown_length, crown_width, lai, stems_n, solar_rad, &
        CrownShape, k, gammaAPAR, solarAngle,days_in_month, &
        apar, lai_above, fi, lambda_v, lambda_h, canopy_vol_frac, layer_id, lai_sa_ratio)               !20251114

        ! Subroutine calculate the apar for the mixed species forest
        ! It first allocate each species to a specific layer based on height and crown length
        ! and then distribute the light between those layers

        ! If LAI is equal to 0, this is an indicator that the species is currently in the dormant period


        implicit none

        ! input
        integer, intent(in) :: n_sp ! number of species
        real(kind=kind(0.0d0)), dimension(n_sp) :: height ! i'm not putting the intent(in) here as we modify those variables later
        real(kind=kind(0.0d0)), dimension(n_sp) :: crown_length ! i'm not putting the intent(in) here as we modify those variables later
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: crown_width
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: lai
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: stems_n
        real(kind=kind(0.0d0)), intent(in) :: solar_rad
        integer, dimension(n_sp), intent(in) :: CrownShape   !***DF crown shape of a given species; 1=cone, 2=ellipsoid, 3=half-ellipsoid, 4=rectangular
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: k
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: gammaAPAR !20251114

        real(kind=kind(0.0d0)), intent(in) :: solarAngle
        integer, intent(in) :: days_in_month

        ! output
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: apar
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: lai_above !leaf area above the given species
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: fi !***DF the proportion of above canopy apar absorbed by each species
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: lambda_v       !Constant to partition light between species and to account for vertical canopy heterogeneity (see Equations 2 and 3 of Forrester et al., 2014, Forest Ecosystems, 1:17)
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: lambda_h         !Constant to account for horizontal canopy heterogeneity such as gaps between trees and the change in zenith angle (and shading) with latitude and season (see Equations 2 and 5 of Forrester et al., 2014, Forest Ecosystems, 1:17)
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: canopy_vol_frac !Fraction of canopy space (between lowest crown crown height to tallest height) filled by crowns
        integer, dimension(n_sp), intent(out) :: layer_id
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: lai_sa_ratio !the ratio of mean tree leaf area (m2) to crownSA (m2)

        ! Additional variables for calculation distribution
        integer :: i
        real(kind=kind(0.0d0)), dimension(n_sp) :: Heightmidcrown    !mean height of the middle of the crown (height - height to crown base)/2 + height to crown base       !***DF
        real(kind=kind(0.0d0)), dimension(n_sp) :: Heightcrown ! height of the crown begining
        real(kind=kind(0.0d0)), dimension(n_sp) :: CrownSA  !mean crown surface area (m2) of a species
        real(kind=kind(0.0d0)), dimension(n_sp) :: Crownvolume   !***DF the crown volume of a given species
        integer :: nLayers ! number of layers
        real(kind=kind(0.0d0)), dimension(n_sp) :: Height_max_l
        real(kind=kind(0.0d0)), dimension(n_sp) :: Heightcrown_min_l
        real(kind=kind(0.0d0)), dimension(n_sp) :: Heightmidcrown_l ! maximum and minimum height of layer
        real(kind=kind(0.0d0)), dimension(n_sp) :: Heightmidcrown_r !ratio of the mid height of the crown of a given species to the mid height of a canopy layer
        real(kind=kind(0.0d0)), dimension(n_sp) :: kL_l          !sum of k x L for all species within the given layer
        real(kind=kind(0.0d0)), dimension(n_sp) :: lambdaV_l     ! sum of lambda_v per layer
        real(kind=kind(0.0d0)), dimension(n_sp) :: kLSweightedave   !calculates the contribution each species makes to the sum of all kLS products in a given layer (see Equation 6 of Forrester et al., 2014, Forest Ecosystems, 1:17)
        real(kind=kind(0.0d0)), dimension(n_sp) :: aparl  !The absorbed apar for the given layer
        real(kind=kind(0.0d0)) :: RADt ! Total available radiation
        real(kind=kind(0.0d0)), dimension(n_sp) :: LAI_l ! Layer LAI


real(kind=kind(0.0d0)) :: height_wtav_LAI ! weighted average height of each cohort, where the weighting is by LAI !20251114
real(kind=kind(0.0d0)), dimension(n_sp) :: height_rel_wt ! height of cohort relative to the weighted average (by LAI) height of all cohorts


        ! initialization
        CrownSA(:) = 0.d0
        Crownvolume(:) = 0.d0
        Height_max_l(:) = 0.d0
        Heightcrown_min_l(:) = 0.d0
        aparl(:) = 0.d0
        apar(:) = 0.d0
        lai_above(:) = 0.d0

        !Calculate the mid crown height, crown surface and volume
        ! check if species is dormant
        ! where( lai(:) == 0 )
        !    height(:) = 0.d0
        !    crown_length(:) = 0.d0
        ! end where

        Heightcrown(:) = height(:) - crown_length(:)
        Heightmidcrown(:) = height(:) - crown_length(:) / 2


        ! Calculate the crown area and volume
        ! We only do it for species that have LAI, otherwise it stays 0 as was initialized above
        do i = 1, n_sp
            if( lai(i) > 0.d0 ) then
                if( CrownShape(i) == int(1) ) then !cone shaped
                    CrownSA(i) = Pi * ((crown_width(i) / 2.d0) ** 2.d0) + Pi * crown_width(i) / 2.d0 * &
                        (((crown_width(i) / 2.d0) ** 2.d0) + crown_length(i) ** 2.d0) ** 0.5d0
                    Crownvolume(i) = Pi * crown_width(i) * crown_width(i) * crown_length(i) / 12.d0
                else if( CrownShape(i) == int(2) ) then !ellipsoid
                    CrownSA(i) = 4.d0 * Pi * ((((crown_width(i) / 2.d0) ** 1.6075d0) * ((crown_width(i) / 2.d0) ** 1.6075d0) + &
                        ((crown_width(i) / 2.d0) ** 1.6075d0) * ((crown_length(i) / 2.d0) ** 1.6075d0) + &
                        ((crown_width(i) / 2.d0) ** 1.6075d0) * ((crown_length(i) / 2.d0) ** 1.6075d0)) / 3.d0) ** (1.d0 / 1.6075d0)
                    Crownvolume(i) = Pi * crown_width(i) * crown_width(i) * crown_length(i) * 4.d0 / 24.d0
                else if( CrownShape(i) == int(3) ) then !half-ellipsoid
                    CrownSA(i) = Pi * ((crown_width(i) / 2.d0) ** 2.d0) + (4.d0 * Pi * ((((crown_width(i) / 2.d0) ** 1.6075d0) * &
                        ((crown_width(i) / 2.d0) ** 1.6075d0) + ((crown_width(i) / 2.d0) ** 1.6075d0) * &
                        ((crown_length(i)) ** 1.6075d0) + ((crown_width(i) / 2.d0) ** 1.6075d0) * &
                        ((crown_length(i)) ** 1.6075d0)) / 3.d0) ** (1 / 1.6075d0)) / 2.d0
                    Crownvolume(i) = Pi * crown_width(i) * crown_width(i) * crown_length(i) * 4.d0 / 24.d0
                else if( CrownShape(i) == int(4) ) then !rectangular
                    CrownSA(i) = crown_width(i) * crown_width(i) * 2.d0 + crown_width(i) * crown_length(i) * 4.d0
                    Crownvolume(i) = crown_width(i) * crown_width(i) * crown_length(i)
                end if
            end if
        end do


        !calculate the ratio of tree leaf area to crown surface area restrict kLS to 1
        lai_sa_ratio(:) = lai(:) * 10000.d0 / stems_n(:) / CrownSA(:)
        where ( lai(:) == 0.d0 ) lai_sa_ratio(:) = 0.d0


        ! separate trees into layers
        layer_id(:) = f_get_layer(n_sp, height(:), Heightcrown(:) )
        !where ( lai(:) == 0.0d0 ) layer_id(:) = -1.d0
        nLayers = maxval( layer_id(:) )


        ! Now calculate the proportion of the canopy space that is filled by the crowns. The canopy space is the
        ! volume between the top and bottom of a layer that is filled by crowns in that layer.
        ! We calculate it only for the trees that have LAI and are in that particular year. Thus the tree can be in that
        ! layer, but currently will not have LAI
        do i = 1, nLayers
            where ( layer_id(:) == i )
                Height_max_l(:) = maxval(height(:), mask=layer_id(:) .eq. i .and. lai(:) .ne. 0.d0)
                Heightcrown_min_l(:) = minval(Heightcrown(:), mask=layer_id(:) .eq. i .and. lai(:) .ne. 0.d0)
            end where
        end do


        ! sum the canopy volume fraction per layer and save it at each species
        canopy_vol_frac(:) = Crownvolume(:) * stems_n(:) / ( (Height_max_l(:) - Heightcrown_min_l(:)) * 10000.d0)
        canopy_vol_frac(:) = f_get_layer_sum(n_sp, nLayers, canopy_vol_frac(:), layer_id(:))

        ! if the canopy volume fraction is < 0.01 (very small seedlings) then it is outside the range of the model there is no need for lambda_h so, make canopy_vol_frac = 0.01
        !where( canopy_vol_frac(:) < 0.01d0 ) canopy_vol_frac(:) = 0.01d0

        Heightmidcrown_l(:) = Heightcrown_min_l(:) + ( Height_max_l(:) - Heightcrown_min_l(:) ) / 2.d0

        !determine the ratio between the mid height of the given species and the mid height of the layer.
        Heightmidcrown_r(:) = Heightmidcrown(:) / Heightmidcrown_l(:)

        ! Calculate the sum of kL for all species in a layer
        kL_l(:) =  k(:) * lai(:)
        kL_l(:) = f_get_layer_sum(n_sp, nLayers, kL_l(:), layer_id(:))


        ! Constant to partition light between species and to account for vertical canopy heterogeneity
        ! (see Equations 2 and 3 of Forrester et al., 2014, Forest Ecosystems, 1:17)
        lambda_v(:) = 0.012306d0 + 0.2366090d0 * k(:) * LAI(:) / kL_l(:) + 0.029118d0 * Heightmidcrown_r(:) + &
            0.608381d0 * k(:) * LAI(:) / kL_l(:) * Heightmidcrown_r(:)

        ! check for dormant
        where ( lai(:) == 0.d0 )
            lambda_v(:) = 0.d0
        end where

        ! make sure the sum of all lambda_v = 1
        lambdaV_l(:) = f_get_layer_sum(n_sp, nLayers, lambda_v(:), layer_id(:))

        where( lambdaV_l(:) .ne. 0.d0 )
            lambda_v(:) = lambda_v(:) / lambdaV_l(:)
        end where



        ! Calculate the weighted kLS based on kL/sumkL
        kLSweightedave(:) = k(:) * lai_sa_ratio(:) * k(:) * lai(:) / kL_l(:)
        kLSweightedave(:) = f_get_layer_sum( n_sp, nLayers, kLSweightedave(:), layer_id(:))
        ! the kLS should not be greater than 1 (based on the data used to fit the light model in Forrester et al. 2014)
        ! This is because when there is a high k then LS is likely to be small.
        where( kLSweightedave(:) > 1.d0) kLSweightedave(:) = 1.d0

        !Constant to account for horizontal canopy heterogeneity such as gaps between trees and the change in zenith angle (and shading) with latitude and season (see Equations 2 and 5 of Forrester et al., 2014, Forest Ecosystems, 1:17)
        lambda_h(:) = 0.8285d0 + ((1.09498d0 - 0.781928d0 * kLSweightedave(:)) * 0.1d0 ** (canopy_vol_frac(:))) - &
            0.6714096d0 * 0.1d0 ** (canopy_vol_frac(:))
        if ( solarAngle > 30.d0 ) then
            lambda_h(:) = lambda_h(:) + 0.00097d0 * 1.08259d0 ** solarAngle
        end if
        ! check for dormant
        where ( lai(:) == 0.0d0 )
            lambda_h(:) = 0.0d0
        end where


        RADt = solar_rad * days_in_month ! MJ m-2 month-1
        do i = 1, nLayers
            where ( layer_id(:) == i )
                aparl(:) = RADt * (1.d0 - 2.71828182845905d0 ** (-kL_l(:)))
            end where
            RADt = RADt - maxval(aparl(:), mask=layer_id(:)==i ) ! subtract the layer RAD from total
        end do

        ! ***DF this used to have month in it but this whole sub is run each month so month is now redundant here.
        apar(:) = aparl(:) * lambda_h(:) * lambda_v(:)

        ! The proportion of above canopy apar absorbed by each species. This is used for net radiation calculations in the gettranspiration sub
        fi(:) = apar(:) / (solar_rad * days_in_month)

        ! calculate the LAI above the given species for within canopy VPD calculations
        LAI_l = f_get_layer_sum(n_sp, nLayers, LAI(:), layer_id(:))

        ! now calculate the LAI of all layers above and part of the current layer if the species
        ! is in the lower half of the layer then also take the proportion of the LAI above
        ! the proportion is based on the Relative height of the mid crown

        do i = 1, n_sp
            lai_above(i) = sum( lai(:), mask = layer_id(:) < layer_id(i) )
            if ( Heightmidcrown_r(i) < 0.9999999999999d0 ) then
                lai_above(i) =  lai_above(i) + sum( LAI(:), mask = layer_id(:) == layer_id(i) ) * ( 1.d0-Heightmidcrown_r(i) )
            end if
        end do


! if there is more than 1 cohort, redistribute some of the remaining PAR to the shorter species assuming they are generally in gaps
! rather than under horizontally homogeneous canopies of the overstorey species

height_wtav_LAI = sum( height(:) * lai(:) ) / sum( max(lai(:), 1.0d-12) ) !20251114
height_rel_wt(:) = height(:)/height_wtav_LAI



    end subroutine s_light_3pgmix


    subroutine s_transpiration_3pgpjs ( n_sp, solar_rad, day_length, VPD_sp, BLcond, conduct_canopy, days_in_month, Qa, Qb, &
            transp_veg)

        implicit none

        ! input
        integer, intent(in) :: n_sp ! number of species
        real(kind=kind(0.0d0)), intent(in) :: solar_rad
        real(kind=kind(0.0d0)), intent(in) ::  day_length
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: VPD_sp
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: BLcond
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: conduct_canopy
        integer, intent(in) :: days_in_month
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: Qa, Qb

        ! output
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: transp_veg

        ! derived variables
        real(kind=kind(0.0d0)), dimension(n_sp) :: netRad
        real(kind=kind(0.0d0)), dimension(n_sp) :: defTerm
        real(kind=kind(0.0d0)), dimension(n_sp) :: div


        if( sum(VPD_sp(:)) == 0.d0 ) then
            transp_veg(:) = 0.d0

        else

            ! In the norther latitudes the radiation shall not go below there if
            ! there is a short daylangs
            if ( day_length > 0.d0 ) then
                netRad = Qa + Qb * (solar_rad * 1.d6 / day_length)
            else
                netRad = 0.d0          ! no short-wave input during polar night
            endif

            !netRad(:) = max(netRad(:), 0.d0) ! net radiation can't be negative
            !SolarRad in MJ/m2/day ---> * 10^6 J/m2/day ---> /day_length converts to only daytime period ---> W/m2
            defTerm(:) = rhoAir * lambda * (VPDconv * VPD_sp(:)) * BLcond(:)
            div(:) = conduct_canopy(:) * (1.d0 + e20) + BLcond(:)

            transp_veg(:) = days_in_month * conduct_canopy(:) * (e20 * netRad(:) + defTerm(:)) / div(:) / lambda * day_length
            ! in J/m2/s then the "/lambda*h" converts to kg/m2/day and the days in month then coverts this to kg/m2/month
            transp_veg(:) = max(0.d0, transp_veg(:)) ! transpiration can't be negative

        end if

    end subroutine s_transpiration_3pgpjs


    subroutine s_transpiration_3pgmix ( n_sp, solar_rad, vpd_day, day_length, days_in_month, lai, fi, VPD_sp, &
            aero_resist, conduct_canopy, conduct_soil, Qa, Qb, &
            transp_veg, evapotra_soil)

        implicit none

        ! input
        integer, intent(in) :: n_sp ! number of species

        real(kind=kind(0.0d0)), intent(in) :: solar_rad
        real(kind=kind(0.0d0)), intent(in) :: vpd_day
        real(kind=kind(0.0d0)), intent(in) ::  day_length
        integer, intent(in) :: days_in_month
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: lai
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: fi
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: VPD_sp
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: aero_resist
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: conduct_canopy
        real(kind=kind(0.0d0)), intent(in) ::  conduct_soil
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: Qa, Qb

        ! output
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: transp_veg
        real(kind=kind(0.0d0)), intent(out) :: evapotra_soil

        ! derived variables
        real(kind=kind(0.0d0)), dimension(n_sp) :: netRad
        real(kind=kind(0.0d0)), dimension(n_sp) :: defTerm
        real(kind=kind(0.0d0)), dimension(n_sp) :: div
        real(kind=kind(0.0d0)) :: lai_total ! here it is a number, while in the main subroutine it is a vector
        real(kind=kind(0.0d0)) :: netRad_so
        real(kind=kind(0.0d0)) :: defTerm_so
        real(kind=kind(0.0d0)) :: div_so ! ending `so` mean soil

        ! Species level calculations ---
        ! the within canopy aero_resist and VPDspecies have been calculated using information from the light submodel
        ! and from the calculation of the modifiers. The netrad for each species is calculated
        ! using the fi (proportion of PAR absorbed by the given species) and is calculated by the light submodel.

        if( sum(lai(:)) == 0.d0 ) then
            transp_veg(:) = 0.d0
        else
            netRad(:) = (Qa + Qb * (solar_rad * 10.d0 ** 6.d0 / day_length)) * fi(:)
            !netRad(:) = max(netRad(:), 0.d0) ! net radiation can't be negative
            !SolarRad in MJ/m2/day ---> * 10^6 J/m2/day ---> /day_length converts to only daytime period ---> W/m2
            defTerm(:) = rhoAir * lambda * (VPDconv * VPD_sp(:)) / aero_resist(:)
            div(:) = conduct_canopy(:) * (1.d0 + e20) + 1.d0 / aero_resist(:)

            transp_veg(:) = days_in_month * conduct_canopy(:) * (e20 * netRad(:) + defTerm(:)) / div(:) / lambda * day_length
            ! in J/m2/s then the "/lambda*h" converts to kg/m2/day and the days in month then coverts this to kg/m2/month

            where( lai(:) == 0.d0 )
                transp_veg(:) = 0.d0
            end where

        end if

        ! now get the soil evaporation (soil aero_resist = 5 * lai_total, and VPD of soil = VPD * Exp(lai_total * -Log(2) / 5))
        lai_total = sum( LAI(:) )

        if( lai_total > 0 ) then
            defTerm_so = rhoAir * lambda * (VPDconv * (vpd_day * Exp(lai_total * (-ln2) / 5.d0))) / (5.d0 * lai_total)
            div_so = conduct_soil * (1.d0 + e20) + 1.d0 / (5.d0 * lai_total)
        else
            !defTerm_so = 0.d0
            defTerm_so = rhoAir * lambda * (VPDconv * (vpd_day * Exp(lai_total * (-ln2) / 5.d0)))
            div_so = conduct_soil * (1.d0 + e20) + 1.d0
        end if

        netRad_so = (Qa(1) + Qb(1) * (solar_rad * 10.d0 ** 6.d0 / day_length)) * (1.d0 - sum( fi(:) ) )
        !SolarRad in MJ/m2/day ---> * 10^6 J/m2/day ---> /day_length converts to only daytime period ---> W/m2

        evapotra_soil = days_in_month * conduct_soil * (e20 * netRad_so + defTerm_so) / div_so / lambda * day_length
        !in J/m2/s then the "/lambda*h" converts to kg/m2/day and the days in month then coverts this to kg/m2/month

    end subroutine s_transpiration_3pgmix



    subroutine s_sizeDist_correct (n_sp, age, stems_n, biom_tree, competition_total, lai, &
        correct_bias, height_model, pars_s, pars_b, aWs, nWs, pfsPower, pfsConst, &
        dbh, basal_area, height, crown_length, crown_width, pFS, bias_scale)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! hereherehere
!    implicit none          ! FIRST statement in the declarations section
!    integer :: i
!    integer :: unit_csv
!    logical, save :: header_written = .false.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ! Diameter distributions are used to correct for bias when calculating pFS from mean dbh, and ws distributions are
        ! used to correct for bias when calculating mean dbh from mean ws. This bias is caused by Jensen's inequality and is
        ! corrected using the approach described by Duursma and Robinson (2003) FEM 186, 373-380, which uses the CV of the
        ! distributions and the exponent of the relationship between predicted and predictor variables.

        ! The default is to ignore the bias. The alternative is to correct for it by using empirically derived weibull distributions
        ! from the weibull parameters provided by the user. If the weibull distribution does not vary then just provide scale0 and shape0.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! add this back when deleting the above
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        implicit none
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ! input
        integer, intent(in) :: n_sp ! number of species
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: age
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: stems_n
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: biom_tree
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: competition_total
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: lai

        ! parameters
        integer, intent(in) :: correct_bias ! if the distribution shall be fitted
        integer, intent(in) :: height_model ! which heigh equation
        real(kind=kind(0.0d0)), dimension(18, n_sp), intent(in) :: pars_s ! parameters for bias !20251114
        real(kind=kind(0.0d0)), dimension(30, n_sp), intent(in) :: pars_b ! parameters for bias
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: aWs, nWs
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: pfsPower, pfsConst

        ! output
        real(kind=kind(0.0d0)), dimension(n_sp), intent(inout) :: dbh
        real(kind=kind(0.0d0)), dimension(n_sp), intent(inout) :: basal_area
        real(kind=kind(0.0d0)), dimension(n_sp), intent(inout) :: height
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: crown_length
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: crown_width
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: pFS
        real(kind=kind(0.0d0)), dimension(15, n_sp), intent(out) :: bias_scale


        ! Variables and parameters
        real(kind=kind(0.0d0)), dimension(n_sp) :: lai_total
        real(kind=kind(0.0d0)), dimension(n_sp) :: height_rel

        real(kind=kind(0.0d0)), dimension(n_sp) :: Hd !20251114
        real(kind=kind(0.0d0)), dimension(n_sp) :: aH, nHB, nHC
        real(kind=kind(0.0d0)), dimension(n_sp) :: aV, nVB, nVH, nVBH
        real(kind=kind(0.0d0)), dimension(n_sp) :: aK, nKB, nKH, nKC, nKrh
        real(kind=kind(0.0d0)), dimension(n_sp) :: aHL, nHLB, nHLL, nHLC, nHLrh
        real(kind=kind(0.0d0)), dimension(n_sp) :: Dscale0, DscaleB, Dscalerh, Dscalet, DscaleC
        real(kind=kind(0.0d0)), dimension(n_sp) :: Dshape0, DshapeB, Dshaperh, Dshapet, DshapeC
        real(kind=kind(0.0d0)), dimension(n_sp) :: Dlocation0, DlocationB, Dlocationrh, Dlocationt, DlocationC
        real(kind=kind(0.0d0)), dimension(n_sp) :: wsscale0, wsscaleB, wsscalerh, wsscalet, wsscaleC
        real(kind=kind(0.0d0)), dimension(n_sp) :: wsshape0, wsshapeB, wsshaperh, wsshapet, wsshapeC
        real(kind=kind(0.0d0)), dimension(n_sp) :: wslocation0, wslocationB, wslocationrh, wslocationt, wslocationC

        ! Additional variables for calculation distribution
        real(kind=kind(0.0d0)), dimension(n_sp) :: DWeibullScale, DWeibullShape, DWeibullLocation
        real(kind=kind(0.0d0)), dimension(n_sp) :: wsWeibullScale, wsWeibullShape, wsWeibullLocation
        real(kind=kind(0.0d0)), dimension(n_sp) :: Ex, Varx, CVdbhDistribution, CVwsDistribution
        real(kind=kind(0.0d0)), dimension(n_sp) :: DrelBiaspFS, DrelBiasheight, DrelBiasBasArea, DrelBiasLCL, DrelBiasCrowndiameter
        real(kind=kind(0.0d0)), dimension(n_sp) :: wsrelBias

        real(kind=kind(0.0d0)), dimension(n_sp) :: dlocation, wslocation
        real(kind=kind(0.0d0)), dimension(n_sp) :: DWeibullShape_gamma, wsWeibullShape_gamma

        include 'i_read_param_sizeDist.h'
        include 'i_read_param_sub.h'








        bias_scale(:,:) = 0.d0

        ! LAI
        lai_total = sum( lai(:) )

        ! Calculate the relative height
        ! height(:) = aH(:) * dbh(:) ** nHB(:) * competition_total(:) ** nHC(:)
        height_rel(:) = height(:) / ( sum( height(:) * stems_n(:) ) / sum( stems_n(:) ) )


        ! Check where all the locations are provided
        dlocation(:) = 1.d0
        wslocation(:) = 1.d0

        where( Dlocation0(:)==0.d0 .and. DlocationB(:)==0.d0 .and. Dlocationrh(:)==0.d0 .and. &
            Dlocationt(:)==0.d0 .and. DlocationC (:)==0.d0 ) dlocation(:) = 0.d0

        where( wslocation0(:)==0.d0 .and. wslocationB(:)==0.d0 .and. wslocationrh(:)==0.d0 .and. &
            wslocationt(:)==0.d0 .and. wslocationC (:)==0.d0 ) wslocation(:) = 0.d0

        if (correct_bias .eq. 1 ) then

            ! Calculate the DW scale -------------------
            DWeibullScale(:) = Exp( Dscale0(:) + DscaleB(:) * Log(dbh(:)) + Dscalerh(:) * Log(height_rel(:)) + &
                Dscalet(:) * Log(age(:)) + DscaleC(:) * Log(competition_total(:)))

            DWeibullShape(:) = Exp( Dshape0(:) + DshapeB(:) * Log( dbh(:) ) + Dshaperh(:) * Log(height_rel(:)) + &
                Dshapet(:) * Log(age(:)) + DshapeC(:) * Log(competition_total(:)))

            DWeibullShape_gamma(:) = f_gamma_dist(1.d0 + 1.d0 / DWeibullShape(:), n_sp)

            DWeibullLocation(:) = Exp( Dlocation0(:) + DlocationB(:) * Log(dbh(:)) + &
                    Dlocationrh(:) * Log(height_rel(:)) + Dlocationt(:) * Log(age(:)) + &
                    DlocationC(:) * Log(competition_total(:)))

            where( dlocation(:) == 0.d0 )
                DWeibullLocation(:) = NINT(dbh(:)) / 1.d0 - 1.d0 - DWeibullScale(:) * DWeibullShape_gamma(:)
            end where

            where( DWeibullLocation(:) < 0.01d0 ) DWeibullLocation(:) = 0.01d0


            Ex(:) = DWeibullLocation(:) + DWeibullScale(:) * DWeibullShape_gamma(:)
            !now convert the Ex from weibull scale to actual scale of diameter units in cm
            Varx(:) = DWeibullScale(:) ** 2.d0 * (f_gamma_dist(1.d0 + 2.d0 / DWeibullShape(:), n_sp) -  DWeibullShape_gamma ** 2.d0)
            CVdbhDistribution(:) = Varx(:) ** 0.5d0 / Ex(:)

            ! calculate the bias
            DrelBiaspFS(:) = 0.5d0 * (pfsPower(:) * (pfsPower(:) - 1.d0)) * CVdbhDistribution(:) ** 2.d0
            DrelBiasheight(:) = 0.5d0 * (nHB(:) * (nHB(:) - 1.d0)) * CVdbhDistribution(:) ** 2.d0
            DrelBiasBasArea(:) = 0.5d0 * (2.d0 * (2.d0 - 1.d0)) * CVdbhDistribution(:) ** 2.d0
            DrelBiasLCL(:) = 0.5d0 * (nHLB(:) * (nHLB(:) - 1.d0)) * CVdbhDistribution(:) ** 2.d0
            DrelBiasCrowndiameter(:) = 0.5d0 * (nKB(:) * (nKB(:) - 1.d0)) * CVdbhDistribution(:) ** 2.d0

            ! prevent unrealisticly large bias, by restricting it to within + or - 50%
            DrelBiaspFS(:) = p_min_max( DrelBiaspFS(:), -0.5d0, 0.5d0, n_sp)
            DrelBiasheight(:) = p_min_max( DrelBiasheight(:), -0.5d0, 0.5d0, n_sp)
            DrelBiasBasArea(:) = p_min_max( DrelBiasBasArea(:), -0.5d0, 0.5d0, n_sp)
            DrelBiasLCL(:) = p_min_max( DrelBiasLCL(:), -0.5d0, 0.5d0, n_sp)
            DrelBiasCrowndiameter(:) = p_min_max( DrelBiasCrowndiameter(:), -0.5d0, 0.5d0, n_sp)


            ! Calculate the biom_stem scale -------------------
            wsWeibullScale(:) = Exp( wsscale0(:) + wsscaleB(:) * Log(dbh(:)) + wsscalerh(:) * Log(height_rel(:)) + &
                wsscalet(:) * Log(age(:)) + wsscaleC(:) * Log(competition_total(:)))

            wsWeibullShape(:) = Exp( wsshape0(:) + wsshapeB(:) * Log(dbh(:)) + wsshaperh(:) * Log(height_rel(:)) + &
                wsshapet(:) * Log(age(:)) + wsshapeC(:) * Log(competition_total(:)))
            wsWeibullShape_gamma = f_gamma_dist(1.d0 + 1.d0 / wsWeibullShape(:), n_sp)

            wsWeibullLocation(:) = Exp( wslocation0(:) + wslocationB(:) * Log(dbh(:)) + &
                    wslocationrh(:) * Log(height_rel(:)) + wslocationt(:) * Log(age(:)) + &
                    wslocationC(:) * Log(competition_total(:)))

            where( wslocation(:) == 0.d0 )
                wsWeibullLocation(:) = NINT(biom_tree(:)) / 10.d0 - 1.d0 - wsWeibullScale(:) * wsWeibullShape_gamma(:)
            end where

            where( wsWeibullLocation(:) < 0.01d0 ) wsWeibullLocation(:) = 0.01d0


            Ex(:) = wsWeibullLocation(:) + wsWeibullScale(:) * wsWeibullShape_gamma
            !now convert the Ex from weibull scale to actual scale of diameter units in cm
            Varx(:) = wsWeibullScale(:) ** 2.d0 * (f_gamma_dist(1.d0 + 2.d0 / wsWeibullShape(:), n_sp) - &
                wsWeibullShape_gamma ** 2.d0)
            CVwsDistribution(:) = Varx(:) ** 0.5d0 / Ex(:)

            wsrelBias(:) = 0.5d0 * (1.d0 / nWs(:) * (1.d0 / nWs(:) - 1.d0)) * CVwsDistribution(:) ** 2.d0 !DF the nWS is replaced with 1/nWs because the equation is inverted to predict dbh from ws, instead of ws from dbh

            wsrelBias(:) = p_min_max( wsrelBias(:), -0.5d0, 0.5d0, n_sp)

        else
            DrelBiaspFS(:) = 0.d0
            DrelBiasBasArea(:) = 0.d0
            DrelBiasheight(:) = 0.d0
            DrelBiasLCL(:) = 0.d0
            DrelBiasCrowndiameter(:) = 0.d0
            wsrelBias(:) = 0.d0
        end if

        ! Correct for trees that have age 0 or are thinned (e.g. n_trees = 0)
        where( age(:) .eq. 0.d0 .or. stems_n(:) .eq. 0.d0 )
            DrelBiaspFS(:) = 0.d0
            DrelBiasBasArea(:) = 0.d0
            DrelBiasheight(:) = 0.d0
            DrelBiasLCL(:) = 0.d0
            DrelBiasCrowndiameter(:) = 0.d0
            wsrelBias(:) = 0.d0
        end where

        ! Correct for bias ------------------
        dbh(:) = (biom_tree(:) / aWs(:)) ** (1.d0 / nWs(:)) * (1.d0 + wsrelBias(:))
        basal_area(:) = ( dbh(:) ** 2.d0 / 4.d0 * Pi * stems_n(:) / 10000.d0) * (1.d0 + DrelBiasBasArea(:))

        if( height_model .eq. 1 ) then

            height(:) = ( aH(:) * dbh(:) ** nHB(:) * competition_total(:) ** nHC(:)) * (1.d0 + DrelBiasheight(:))

            crown_length(:) = ( aHL(:) * dbh(:) ** nHLB(:) * lai_total ** nHLL(:) * competition_total(:) ** nHLC(:) * &
                height_rel(:) ** nHLrh(:)) * (1.d0 + DrelBiasLCL(:))

        else if ( height_model .eq. 2 ) then

            height(:) = Hd(:) + aH(:) * exp(1.d0)**(-nHB(:)/dbh(:)) + nHC(:) * competition_total(:) * dbh(:) !20251114
            crown_length(:) = Hd(:) + aHL(:) * exp(1.d0)**(-nHLB(:)/dbh(:)) + nHLC(:) * competition_total(:) * dbh(:) !20251114

        else if ( height_model .eq. 3 ) then
            height(:) = Hd(:) + (dbh(:) ** aH(:)) / (nHB(:) + nHC(:) * (dbh(:) ** aH(:))) !20251114
            crown_length(:) = aHL(:) * height(:) !20251114

        end if






!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! hereherehere
!! --- write CSV file ---
!    open(newunit=unit_csv, file="sizeDist_output.csv", status="unknown", position="append", action="write")
!
!    if (.not. header_written) then
!        write(unit_csv, '(A)') "i,aH,nHB,nHC"
!        header_written = .true.
!    end if
!
!    do i = 1, n_sp
!        write(unit_csv, '(I4, 3(1X, E15.7))') i, aH(i), nHB(i), nHC(i)
!    end do
!
!    close(unit_csv)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!








        crown_width(:) = ( aK(:) * dbh(:) ** nKB(:) * height(:) ** nKH(:) * competition_total(:) ** nKC(:) * &
            height_rel(:) ** nKrh(:)) * (1.d0 + DrelBiasCrowndiameter(:))
        where( lai(:) .eq. 0.d0 ) crown_width(:) = 0.d0


        pFS(:) = ( pfsConst(:) * dbh(:) ** pfsPower(:)) * (1.d0 + DrelBiaspFS(:))


        ! check that the height and LCL allometric equations have not predicted that height - LCL < 0
        ! and if so reduce LCL so that height - LCL = 0 (assumes height allometry is more reliable than LCL allometry)
        where ( crown_length(:) > height(:) )
            crown_length(:) = height(:)
        end where

        ! output the matrix of biasses
        bias_scale(1,:) = DWeibullScale(:)
        bias_scale(2,:) = DWeibullShape(:)
        bias_scale(3,:) = DWeibullLocation(:)
        bias_scale(4,:) = wsWeibullScale(:)
        bias_scale(5,:) = wsWeibullShape(:)
        bias_scale(6,:) = wsWeibullLocation(:)
        bias_scale(7,:) = CVdbhDistribution(:)
        bias_scale(8,:) = CVwsDistribution(:)
        bias_scale(9,:) = wsrelBias(:)
        bias_scale(10,:) = DrelBiaspFS(:)
        bias_scale(11,:) = DrelBiasheight(:)
        bias_scale(12,:) = DrelBiasBasArea(:)
        bias_scale(13,:) = DrelBiasLCL(:)
        bias_scale(14,:) = DrelBiasCrowndiameter(:)
        bias_scale(15,:) = height_rel


    end subroutine s_sizeDist_correct

end module mod_3PG
