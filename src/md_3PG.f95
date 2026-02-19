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
        integer(kind=c_int), dimension(n_sp), intent(in) :: t_t, d_t! counter of management and defoliation interventions
        integer(kind=c_int), dimension(8), intent(in) :: settings    ! settings to indicate which equations to use

        ! Initial, forcing, parameters
        real(kind=c_double), dimension(20), intent(in) :: siteInputs
        real(kind=c_double), dimension(n_sp,7), intent(in) :: speciesInputs
        real(kind=c_double), dimension(n_man,6,n_sp), intent(in) :: managementInputs
        real(kind=c_double), dimension(n_def,9,n_sp), intent(in) :: defoliationInputs
        real(kind=c_double), dimension(n_m,9), intent(in) :: forcingInputs
        real(kind=c_double), dimension(86,n_sp), intent(in) :: pars_i
        real(kind=c_double), dimension(15,n_sp), intent(in) :: pars_b

        ! Temporary variables for self-thinning calculation
        real(kind=c_double) :: thinIntercept_eff
        real(kind=c_double) :: N_max
        real(kind=c_double) :: dbh_safe
        real(kind=c_double) :: expo
        real(kind=c_double) :: logN
        real(kind=c_double), dimension(n_sp) :: weight
        real(kind=c_double) :: weight_sum
        real(kind=c_double) :: loss_sum
        real(kind=c_double) :: scale

        real(kind=c_double) :: pp
        real(kind=c_double) :: dbh_prev_safe, dbh_ratio
        real(kind=c_double) :: modifiers
        real(kind=c_double) :: delta_term
        real(kind=c_double) :: inner
        real(kind=c_double) :: betaN_eff
        real(kind=c_double) :: inv_exp



        ! Temporary for dbh distributions
        real(kind=kind(0.0d0)), dimension(n_sp) :: dlocation
        real(kind=kind(0.0d0)), dimension(n_sp) :: DWeibullShape_gamma
        ! Temporary variables for long-term modifiers
        real(kind=8) :: f_sw_tmp, f_vpd_tmp, f_phys_tmp, vpd_mean
        ! Temporary variable when responding to defoliation
        real(kind=c_double) :: NPP_eff
        ! Temporary variable for updating age-related variables after coppice events
        real(kind=8) :: tmp_vec(1)
        integer :: jj


        ! Output array
        real(kind=c_double), dimension(n_m,n_sp,11,20), intent(inout) :: output


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
        air_pressure = 101.3d0 * Exp(-1.d0 * elevation / 8200.d0)


        ! SOIL WATER --------
        ! Assign the SWconst and SWpower parameters for this soil class
        if ( soil_class > 0.d0 ) then
            ! Standard soil types
            if (soil_class == 1) then        ! Clay !20251124
               SWconst(:) = 0.4d0
               SWpower(:) = 3.0d0
            else if (soil_class == 2) then   ! Clay Loam
               SWconst(:) = 0.5d0
               SWpower(:) = 5.0d0
            else if (soil_class == 3) then   ! Loam
               SWconst(:) = 0.55d0
               SWpower(:) = 6.0d0
            else if (soil_class == 4) then   ! Loamy sand
               SWconst(:) = 0.65d0
               SWpower(:) = 8.0d0
            else if (soil_class == 5) then   ! Sand
               SWconst(:) = 0.7d0
               SWpower(:) = 9.0d0
            else if (soil_class == 6) then   ! Sandy clay
               SWconst(:) = 0.45d0
               SWpower(:) = 4.0d0
            else if (soil_class == 7) then   ! Sandy clay loam
               SWconst(:) = 0.525d0
               SWpower(:) = 5.5d0
            else if (soil_class == 8) then   ! Sandy loam
               SWconst(:) = 0.6d0
               SWpower(:) = 7.0d0
            else if (soil_class == 9) then   ! Silt
               SWconst(:) = 0.625d0
               SWpower(:) = 7.5d0
            else if (soil_class == 10) then  ! Silty clay
               SWconst(:) = 0.425d0
               SWpower(:) = 3.5d0
            else if (soil_class == 11) then  ! Silty clay loam
               SWconst(:) = 0.475d0
               SWpower(:) = 4.5d0
            else if (soil_class == 12) then  ! Silty loam
               SWconst(:) = 0.575d0
               SWpower(:) = 6.5d0
            end if

        else if ( soil_class < 0.d0 ) then
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

        ! Check if this is the dormant period or whether the previous or following month was/is dormant
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
          dbh_prev(:) = dbh(:)
          crown_ratio(:) = 1.d0
        end where

        ! for background mortality calculations where mort_model = 2
        stems_n_total  = max(sum(stems_n(:)), 1.0d-6)
        basal_area_total = max(sum(basal_area(:)), 1.0d-6)
        dbh_total = sum(dbh(:) * stems_n(:)) / stems_n_total
        dbh_prev(:) = dbh(:)
        dbh_total_prev = dbh_total


        ! update height, crown diameter and crown length
        competition_total = sum( wood_density(ii,:) * basal_area(:) )
        crown_ratio(:) = 1.d0
        is_new(:) = (age(ii,:) >= 0.d0)
        if( any(age(ii,:) >= 0.d0) ) then
                  calculate_states = .TRUE.
                  call s_height_crown_allometry (n_sp, age(ii,:), stems_n(:), competition_total, &
                      lai(:), height_rel(:), &
                      height_model, crown_width_model, pars_i(63:82,:), &
                      dbh(:), dbh_prev(:), height(:), crown_length(:), crown_width(:), crown_ratio(:), &
                      calculate_states, is_new(:) )
        end if




       ! dbh distributions
        dlocation(:) = 1.d0
        where( Dlocation0(:)==0.d0 .and. &
                 DlocationB(:)==0.d0 .and. &
                 Dlocationrh(:)==0.d0 .and. &
                 Dlocationt(:)==0.d0 .and. &
                 DlocationC(:)==0.d0 )
        dlocation(:) = 0.d0
        end where
        DWeibullScale(:) = Exp( Dscale0(:) + DscaleB(:) * Log(dbh(:)) + Dscalerh(:) * &
                   Log(height_rel(:)) + Dscalet(:) * Log(age(ii,:)) + DscaleC(:) * Log(competition_total))
        DWeibullShape(:) = Exp( Dshape0(:) + DshapeB(:) * Log( dbh(:) ) + Dshaperh(:) * Log(height_rel(:)) + &
                                  Dshapet(:) * Log(age(ii,:)) + DshapeC(:) * Log(competition_total))
        DWeibullShape_gamma(:) = f_gamma_dist(1.d0 + 1.d0 / DWeibullShape(:), n_sp)
        DWeibullLocation(:) = Exp( Dlocation0(:) + DlocationB(:) * Log(dbh(:)) + &
                                     Dlocationrh(:) * Log(height_rel(:)) + Dlocationt(:) * Log(age(ii,:)) + &
                                     DlocationC(:) * Log(competition_total))
        where( dlocation(:) == 0.d0 )
        DWeibullLocation(:) = NINT(dbh(:)) / 1.d0 - 1.d0 - DWeibullScale(:) * DWeibullShape_gamma(:)
        end where
        where( DWeibullLocation(:) < 0.01d0 ) DWeibullLocation(:) = 0.01d0


        ! Volume and Volume increment
        volume(:) = biom_stem(:) * (1.d0 - fracBB(ii,:)) / wood_density(ii,:)
        where( aV(:) > 0 ) volume(:) = aV(:) * dbh(:) ** nVB(:) * height(:) ** nVH(:) * &
                (dbh(:) * dbh(:) * height(:)) ** nVBH(:) * stems_n(:)
        volume_cum(:) = volume(:)
        volume_old(:) = volume(:)
        volume_mai(:) = volume_cum(:) / age(ii,:)




        ! Long-term modifiers initialization
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
            ! soil nutrition modifier
            lt_fN(i) = 1.d0 - (1.d0 - fN0(i)) * (1.d0 - fertility(i)) ** fNn(i)
            if (fNn(i) == 0.d0) then
            lt_fN(i) = 1.d0
            end if
            ! Temperature (lt_fT)
            lt_fT(i) = sum(f_tmp(1:lt_mod_mths, i)) / real(lt_mod_mths, kind=8)
            fT_hist(i,1:lt_mod_mths) = lt_fT(i)
            ! PhysMod (lt_fPhys)
            ! Exclude the first month of VPD (to avoid initial zeros)
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

            ! Initialize circular buffer pointer
            hist_ptr(i) = lt_mod_mths   ! start at the last element
        end do




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

              !xxx888
              ! required to update height and crown allometry following increments in dbh
              biom_tree(:) = biom_stem(:) * 1000.d0 / stems_n(:)  ! kg/tree
              dbh(:) = ( biom_tree(:) / aWs(:)) ** (1.d0 / nWs(:))
              dbh_prev(:) = dbh(:)
              basal_area(:) = dbh(:) ** 2.d0 / 4.d0 * Pi * stems_n(:) / 10000.d0
              lai(:) =  biom_foliage(:) * SLA(ii,:) * 0.1d0
              crown_ratio(:) = 1.d0
            end where


            ! calculate partitioning parameter
            do i = 1, n_sp
                pFS(i) = ( pfsConst(i) * dbh(i) ** pfsPower(i))
            end do


            ! Test for dormancy ----------------------------------------------------------------------

            do i = 1, n_sp
            ! If this is first month after dormancy, get lai because it is required for PAR absorption.
                if( f_dormant(month, leafgrow(i), leaffall(i)) .eqv. .FALSE. ) then
                    if( f_dormant(month-1, leafgrow(i), leaffall(i)) .eqv. .TRUE. ) then
                        lai(i) =  biom_foliage_debt(i) * SLA(ii,i) * 0.1d0
                        !b_cor = .TRUE.
                    end if
                end if

                ! If this is first dormant month, set WF to 0 and move everything to the biom_foliage_debt
                if( f_dormant(month, leafgrow(i), leaffall(i)) .eqv. .TRUE. ) then
                    if( f_dormant(month-1, leafgrow(i), leaffall(i)) .eqv. .FALSE. ) then
                        biom_foliage_debt(i) = biom_foliage(i)
                        biom_foliage(i) = 0.d0
                        lai(i) =  0.d0
                        !b_cor = .TRUE.
                    end if
                end if

            end do





            ! for new cohorts calculate initial height, crown width and crown length
            competition_total = sum( wood_density(ii,:) * basal_area(:) )
            is_new(:) = (age(ii,:) .eq. 0.d0)
            if( any(age(ii,:) .eq. 0.d0) ) then
                      calculate_states = .TRUE.
                      call s_height_crown_allometry (n_sp, age(ii,:), stems_n(:), competition_total, &
                          lai(:), height_rel(:), &
                          height_model, crown_width_model, pars_i(63:82,:), &
                          dbh(:), dbh_prev(:), height(:), crown_length(:), crown_width(:), crown_ratio(:), &
                          calculate_states, is_new(:) )
            end if






            ! If any cohorts are recovering from a defoliation event, check whether they finished recovering
            ! after the previous month's using new NPP.
            do i = 1, n_sp
                   if (def_recover_t(i) > 0.0d0) then ! indicates that there has been a defoliation event
                       if (age(ii,i) >= age_last_def_event(i) + def_recover_t(i)/12.d0) then
                           def_recover_t(i) = 0.0d0
                           ! Adjust roots if def_type is 2 (coppice) and sr_ratio indicates excess roots
                           ! The recovery has finished, but if there are more roots than sr_ratio suggests, remove some of the roots to be consistent with natural root pruning to retain the shoot/root ratio
                           !if (def_type(i) == 2) then
                           !    if (sr_ratio(i) > (biom_stem(i) + biom_foliage(i)) / biom_root(i)) then
                           !        biom_loss_root_def(i) = biom_root(i) - (biom_stem(i) + biom_foliage(i)) / sr_ratio(i)
                           !        biom_root(i) = (biom_stem(i) + biom_foliage(i)) / sr_ratio(i)
                           !    end if
                           !end if
                       end if
                   end if
                   ! Coppice condition: def_type 2
                   !if (def_type(i) == 2) then
                   !    if (age(ii,i) > age_last_def_event(i) + 1.d0/12.d0) then
                   !        if (biom_foliage(i) + biom_stem(i) >= biom_foliage_adj_pre_def(i)) then
                   !            def_recover_t(i) = 0.0d0
                   !            ! Adjust roots based on sr_ratio
                   !            ! The recovery has finished, but if there are more roots than sr_ratio suggests, remove some of the roots to be consistent with natural root pruning to retain the shoot/root ratio
                   !            !if (sr_ratio(i) > (biom_stem(i) + biom_foliage(i)) / biom_root(i)) then
                   !            !    biom_loss_root_def(i) = biom_root(i) - (biom_stem(i) + biom_foliage(i)) / sr_ratio(i)
                   !            !    biom_root(i) = (biom_stem(i) + biom_foliage(i)) / sr_ratio(i)
                   !            !end if
                   !        end if
                   !    end if
                   !end if
                  ! Prune condition: def_type 1
                  !if (def_type(i) == 1) then
                  !    if (age(ii,i) >= age_last_def_event(i) + 1.d0/12.d0) then
                  !        if (biom_foliage(i) >= biom_foliage_adj_pre_def(i)) then
                  !            def_recover_t(i) = 0.0d0
                  !        end if
                  !    end if
                  !end if
                   ! Epicormic condition: def_type 3
                   !if (def_type(i) == 3) then
                   !    if (age(ii,i) >= age_last_def_event(i) + 1.d0/12.d0) then
                   !        if (biom_foliage(i) >= biom_foliage_adj_pre_def(i)) then
                   !            def_recover_t(i) = 0.0d0
                   !        end if
                   !    end if
                   !end if
            end do



          ! Add any biomass coming from stored non-structural carbohydrates if still recovering from a defoliation event
          do i = 1, n_sp

                if( def_recover_t(i) > 0.d0 ) then
                   ! if still in within the first year of a defoliation event
                   if ( age(ii,i) <= age_last_def_event(i) + 1.d0 ) then

                         if(age(ii,i) >= age_last_def_event(i) + 1.d0/12.d0 ) then

!                              if( def_type(i) == 1 .or. def_type(i) == 3 ) then !prune or epicormic response
!
!                                  if( leafgrow(i) < 1.0d-4 ) then ! evergreen species
!
!                                    if( def_recover_t(i) < 12.d0 ) then
!                                    biom_incr_foliage_def(i) = prop_carbs(i) * biom_foliage_adj_pre_def(i) / def_recover_t(i)
!                                    else
!                                    biom_incr_foliage_def(i) = prop_carbs(i) * biom_foliage_adj_pre_def(i) / 12.d0
!                                    end if
!
!                                  end if
!
!                                  if( leafgrow(i) > 1.0d-4 ) then ! deciduous species
!
!                                      if( f_dormant(month, leafgrow(i), leaffall(i)) .eqv. .FALSE.) then
!
!                                        ! calculate growing season length
!                                        if ( leafgrow(i) > leaffall(i) ) then
!                                          growing_season_length(i) = leaffall(i) - leafgrow(i) + 1
!                                        else if ( leafgrow(i) < leaffall(i) ) then
!                                          growing_season_length(i) = 12.d0 - leafgrow(i) + 1 + leaffall(i)
!                                        end if
!
!                                        if( def_recover_t(i) < 12.d0 ) then
!                                        biom_incr_foliage_def(i) = prop_carbs(i) * biom_foliage_adj_pre_def(i) / &
!                                        (def_recover_t(i) * growing_season_length(i) / 12.d0)
!                                        else
!                                        biom_incr_foliage_def(i) = prop_carbs(i) * biom_foliage_adj_pre_def(i) / &
!                                        growing_season_length(i)
!                                        end if
!
!                                      end if
!
!                                  end if
!
!                              end if



                              if( def_type(i) == 2) then !coppice response
                                  if( leafgrow(i) == 0 ) then  ! evergreen species
                                    if( def_recover_t(i) < 12.d0 ) then
                                    biom_incr_foliage_def(i) = (1.d0 - npp_fract_stem(i)) * prop_carbs(i) * &
                                    biom_foliage_adj_pre_def(i) / def_recover_t(i)
                                    biom_incr_stem_def(i) = (1.d0 / (1.d0 + pFS(i))) * prop_carbs(i) * &
                                    biom_foliage_adj_pre_def(i) / def_recover_t(i)

                                    else

                                    biom_incr_foliage_def(i) = (1.d0 - npp_fract_stem(i)) * prop_carbs(i) * &
                                    biom_foliage_adj_pre_def(i) / 12.d0
                                    biom_incr_stem_def(i) = (1.d0 / (1.d0 + pFS(i))) * prop_carbs(i) * &
                                    biom_foliage_adj_pre_def(i) / 12.d0

                                    end if
                                  end if


!                                  if( leafgrow(i) > 1.0d-4 ) then ! deciduous species
!                                     if( f_dormant(month, leafgrow(i), leaffall(i)) .eqv. .FALSE.) then  ! deciduous, but not the dormant season
!
!                                        ! calculate growing season length
!                                        if ( leafgrow(i) > leaffall(i) ) then
!                                          growing_season_length(i) = leaffall(i) - leafgrow(i) + 1
!                                        else if ( leafgrow(i) < leaffall(i) ) then
!                                          growing_season_length(i) = 12 - leafgrow(i) + 1 + leaffall(i)
!                                        end if
!
!                                        if( def_recover_t(i) < 12.d0 ) then
!                                        biom_incr_foliage_def(i) = (1.d0 - npp_fract_stem(i)) * prop_carbs(i) * &
!                                        biom_foliage_adj_pre_def(i) / (def_recover_t(i) * growing_season_length(i) / 12.d0)
!                                        biom_incr_stem_def(i) = (1.d0 / (1.d0 + pFS(i))) * prop_carbs(i) * &
!                                        biom_foliage_adj_pre_def(i) / (def_recover_t(i) * growing_season_length(i) / 12.d0)
!                                        else
!                                        biom_incr_foliage_def(i) = (1.d0 - npp_fract_stem(i)) * prop_carbs(i) * &
!                                        biom_foliage_adj_pre_def(i) / growing_season_length(i)
!                                        biom_incr_stem_def(i) = (1.d0 / (1.d0 + pFS(i))) * prop_carbs(i) * &
!                                        biom_foliage_adj_pre_def(i) / growing_season_length(i)
!                                        end if
!                                     end if
!                                  end if
                              end if
                         end if
                   end if

                else ! if not responding to defoliation, set the increments to 0

                     !biom_incr_foliage_def(i) = 0.0d0
                     !biom_incr_stem_def(i) = 0.0d0

                end if



                 ! def_recover_t > 0 and age threshold
                 if (def_recover_t(i) > 0.0d0) then
                     if (age(ii,i) >= age_last_def_event(i) + def_recover_t(i)/12.d0) then
                         def_recover_t(i) = 0.0d0
                     end if
                 end if

!                 ! Coppice condition: def_type 2, age threshold, and foliage+stem check
!                 if (def_type(i) == 2) then
!                     if (age(ii,i) > age_last_def_event(i) + 1.d0/12.d0) then
!                         if (biom_foliage(i) + biom_stem(i) >= biom_foliage_adj_pre_def(i)) then
!                             def_recover_t(i) = 0.0d0
!                         end if
!                     end if
!                 end if
!
!                 ! Prune condition: def_type 1
!                 if (def_type(i) == 1) then
!                     if (age(ii,i) >= age_last_def_event(i) + 1.d0/12.d0) then
!                         if (biom_foliage(i) >= biom_foliage_adj_pre_def(i)) then
!                             def_recover_t(i) = 0.0d0
!                         end if
!                     end if
!                 end if
!
!                 ! Epicormic condition: def_type 3
!                 if (def_type(i) == 3) then
!                     if (age(ii,i) >= age_last_def_event(i) + 1.d0/12.d0) then
!                         if (biom_foliage(i) >= biom_foliage_adj_pre_def(i)) then
!                             def_recover_t(i) = 0.0d0
!                         end if
!                     end if
!                 end if

          end do









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
                    apar(:), lai_above(:), fi(:), lambda_v(:), lambda_h(:), canopy_vol_frac(:), layer_id(:), &
                    lai_sa_ratio(:), m_apar(:))

                VPD_sp(:) = vpd_day(ii) * Exp(lai_above(:) * (-Log(2.d0)) / cVPD(:))
            end if


            ! Determine the various environmental modifiers which were not already calculated
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








            ! Monthly update of long-term modifiers
            do i = 1, n_sp
                ! Skip species not yet planted
                if (age(ii, i) < 0.d0) cycle
                ! Update circular buffer index
                hist_ptr(i) = mod(hist_ptr(i), lt_mod_mths) + 1

                ! Temperature (lt_fT)
                fT_hist(i, hist_ptr(i)) = f_tmp(ii, i)
                lt_fT(i) = sum(fT_hist(i, 1:lt_mod_mths)) / real(lt_mod_mths, kind=8)

                ! PhysMod (lt_fPhys) — exclude age effect
                fPhys_hist(i, hist_ptr(i)) = f_phys(i) / f_age(ii, i)
                lt_fPhys(i) = sum(fPhys_hist(i, 1:lt_mod_mths)) / real(lt_mod_mths, kind=8)

            end do




            ! Calculate assimilation before the water balance is done
            alpha_c(:) = alphaCx(:) * f_nutr(:) * f_tmp(ii,:) * f_frost(ii,:) * f_calpha(ii,:) * f_phys(:)
            where( lai(:) == 0.d0 ) alpha_c(:) = 0.d0
            epsilon(:) = gDM_mol * molPAR_MJ * alpha_c(:)
            GPP(:) = epsilon(:) * apar(:) / 100        ! tDM/ha (apar is MJ/m^2)
            NPP(:) = GPP(:) * y(:) + biom_incr_foliage_def(:) + biom_incr_stem_def(:)    ! assumes respiratory rate is constant
            ! the biom_incr_foliage_def and biom_incr_stem_def are reallocation of non-structural carbohydrates to NPP and therefore should not be added to GPP, which is C aquisition




            ! Water Balance ----------------------------------------------------------------------
            ! Calculate species proportions by lai
            lai_total = sum( lai(:) )
            lai_per(:) = lai(:) / lai_total
            do i = 1, n_sp
                if (lai_total == 0.d0) lai_per(i) = 0.d0
            end do

            ! Calculate conductance
            gC(:) = MaxCond(:)
            where( lai_total <= LAIgcx(:) )
                gC(:) = MinCond(:) + (MaxCond(:) - MinCond(:)) * lai_total / LAIgcx(:)
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
                prcp_interc_fract(:) = MaxIntcptn(:) * min(1.d0, lai_total / LAImaxIntcptn(:)) * LAI_per(:)
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
                ! (it will include the soil evaporation if transp_model = 2)
                f_transp_scale = 1.
            else
                f_transp_scale = evapo_transp / (transp_total + prcp_interc_total)  !scales NPP and GPP
            end if

            ! correct for actual ET
            GPP(:) = GPP(:) * f_transp_scale
            NPP(:) = NPP(:) * f_transp_scale

            ! Adjust the defoliation-related components that have already been calculated
            biom_incr_foliage_def(:) = biom_incr_foliage_def(:) * f_transp_scale
            biom_incr_stem_def(:) = biom_incr_stem_def(:) * f_transp_scale


            if ( transp_total > 0 ) then
                if(f_transp_scale < 1 ) then
                ! a different scaler is required for transpiration because all of the scaling needs
                ! to be done to the transpiration and not to the RainIntcpth, which occurs regardless of the growth
                transp_veg(:) = (evapo_transp - prcp_interc_total) / transp_total * transp_veg(:)
                evapotra_soil = (evapo_transp - prcp_interc_total) / transp_total * evapotra_soil
                end if
            end if


            ! NEED TO CROSS CHECK THIS PART, DON'T FULLY AGREE WITH IT
            if (evapo_transp /= 0.d0) then
                if (n_sp == 1) then
                    ! in case ET is zero! Also, for mixtures it is not possible to calculate WUE based on
                    ! ET because the soil evaporation cannot simply be divided between species.
                    WUE(:) = 100.d0 * NPP(:) / evapo_transp
                else
                    WUE(:) = 0.d0
                end if
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
            ! Determine biomass increments and losses
            m(:) = m0(:) + (1.d0 - m0(:)) * fertility(:)

              ! If still recovering from a defoliation event, modify the partitioning of npp
              do i = 1, n_sp
                  if ( def_recover_t(i) > 0.0d0 ) then
                      if ( def_type(i) == 1 .or. def_type(i) == 3 ) then  ! prune or epicormic, so all NPP to foliage

                          ! First, calculate usual values
                          npp_fract_root(i) = pRx(i) * pRn(i) / (pRn(i) + (pRx(i) - pRn(i)) * f_phys(i) * m(i))
                          npp_fract_stem(i) = (1.0d0 - npp_fract_root(i)) / (1.0d0 + pFS(i))
                          npp_fract_foliage(i) = 1.0d0 - npp_fract_root(i) - npp_fract_stem(i)

                          ! Second, calculate new values
                          npp_fract_root(i) = npp_fract_root(i) * (( npp_fract_root(i) + npp_fract_stem(i))- &
                          (max(prop_npp(i),npp_fract_foliage(i)) - npp_fract_foliage(i)))/( npp_fract_root(i) + npp_fract_stem(i))
                          npp_fract_stem(i) = npp_fract_stem(i) * (( npp_fract_root(i) + npp_fract_stem(i))- &
                          (max(prop_npp(i),npp_fract_foliage(i)) - npp_fract_foliage(i)))/( npp_fract_root(i) + npp_fract_stem(i))
                          npp_fract_foliage(i) = max(prop_npp(i),npp_fract_foliage(i))

                      end if

                      if ( def_type(i) == 2 ) then  ! coppice, so all NPP to foliage and stems

                          ! First, calculate usual values
                          npp_fract_root(i) = pRx(i) * pRn(i) / (pRn(i) + (pRx(i) - pRn(i)) * f_phys(i) * m(i))
                          npp_fract_stem(i) = (1.0d0 - npp_fract_root(i)) / (1.0d0 + pFS(i))
                          npp_fract_foliage(i) = 1.0d0 - npp_fract_root(i) - npp_fract_stem(i)

                          ! Second, calculate new values
                          npp_fract_foliage(i) = max(prop_npp(i),(npp_fract_foliage(i) + npp_fract_stem(i))) * &
                          npp_fract_foliage(i) / (npp_fract_foliage(i) + npp_fract_stem(i))

                          npp_fract_stem(i) = max(prop_npp(i),(npp_fract_foliage(i) + npp_fract_stem(i))) * &
                          (1-npp_fract_foliage(i) / (npp_fract_foliage(i) + npp_fract_stem(i)))

                          npp_fract_root(i) = 1.0d0 - npp_fract_foliage(i) - npp_fract_stem(i)

                      end if

                  else  ! Case when def_recover_t(i) <= 0 ! all other cases (no current defoliation)
                      npp_fract_root(i) = pRx(i) * pRn(i) / (pRn(i) + (pRx(i) - pRn(i)) * f_phys(i) * m(i))
                      npp_fract_stem(i) = (1.0d0 - npp_fract_root(i)) / (1.0d0 + pFS(i))
                      npp_fract_foliage(i) = 1.0d0 - npp_fract_root(i) - npp_fract_stem(i)
                  end if
              end do





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
                    ! distributed among the compartments
                    if( biom_foliage(i) == 0.d0 ) then
                        biom_foliage(i) = biom_foliage_debt(i)
                        biom_foliage_debt(i) = 0.d0
                    end if

                    ! Calculate biomass loss
                    biom_loss_foliage(i) = gammaF(ii, i) * biom_foliage(i)
                    biom_loss_root(i) = gammaR(i) * biom_root(i)


                    ! Calculate biomass increments (remove any non-structural carbohydrate contibutions - biom_incr_foliage_def & biom_incr_stem_def)
                    biom_incr_foliage(i) = ( NPP(i) - biom_incr_foliage_def(i) - biom_incr_stem_def(i) ) * npp_fract_foliage(i)
                    biom_incr_root(i) = ( NPP(i) - biom_incr_foliage_def(i) - biom_incr_stem_def(i) ) * npp_fract_root(i)
                    biom_incr_stem(i) = ( NPP(i) - biom_incr_foliage_def(i) - biom_incr_stem_def(i) ) * npp_fract_stem(i)


                    ! Calculate end-of-month biomass
                    biom_foliage(i) = biom_foliage(i) + biom_incr_foliage(i) - biom_loss_foliage(i) + biom_incr_foliage_def(i)
                    biom_root(i) = biom_root(i) + biom_incr_root(i) - biom_loss_root(i)
                    biom_stem(i) = biom_stem(i) + biom_incr_stem(i) + biom_incr_stem_def(i)

                end if

            end do

            ! Update dbh for volume calculations, and increment the height and crown dimensions to be consistent with the NPP
            biom_tree(:) = biom_stem(:) * 1000.d0 / stems_n(:)  ! kg/tree
            where( stems_n(:) .eq. 0.d0 ) biom_tree(:) = 0.d0

            dbh(:) = ( biom_tree(:) / aWs(:)) ** (1.d0 / nWs(:))
            basal_area(:) = dbh(:) ** 2.d0 / 4.d0 * Pi * stems_n(:) / 10000.d0
            competition_total = sum( wood_density(ii,:) * basal_area(:) )



            ! add increments to height, crown width and crown length
            is_new(:) = (age(ii,:) >= 0.d0) .and. ((dbh(:) - dbh_prev(:)) > 1.0d-5)
            if( any(is_new(:)) ) then
                      calculate_states = .FALSE.
                      call s_height_crown_allometry (n_sp, age(ii,:), stems_n(:), competition_total, &
                          lai(:), height_rel(:), &
                          height_model, crown_width_model, pars_i(63:82,:), &
                          dbh(:), dbh_prev(:), height(:), crown_length(:), crown_width(:), crown_ratio(:), &
                          calculate_states, is_new(:) )
            end if





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
            !reset mortality value
            stems_loss_manag(:) = 0.d0
            biom_loss_stem_manag(:) = 0.d0
            biom_loss_root_manag(:) = 0.d0
            biom_loss_foliage_manag(:) = 0.d0

            do i = 1, n_sp

                if( t_t(i) > 0 ) then

                    if(t_n(i) <= t_t(i)) then

                        if( age(ii,i) >= managementInputs(t_n(i),1,i) ) then

                                   ! thinning calculated using stems_n as input
                                   if ( .not. isnan(managementInputs(t_n(i),2,i)) ) then
                                        if(stems_n(i) > managementInputs(t_n(i),2,i) ) then

                                               ! thinning calculated using trees removed
                                               stems_loss_manag(i) = stems_n(i) - managementInputs(t_n(i),2,i)
                                               if ( stems_loss_manag(i) < 0.d0 ) stems_loss_manag(i) = 0.d0

                                               ! proportion of trees removed
                                               manag_remove_prop = stems_loss_manag(i) / stems_n(i)

                                               ! clamp 0–1
                                               if ( manag_remove_prop < 0.d0 ) manag_remove_prop = 0.d0
                                               if ( manag_remove_prop > 1.d0 ) manag_remove_prop = 1.d0

                                               ! compartment-specific removal fractions
                                               manag_remove_prop_compartment(1) = manag_remove_prop * managementInputs(t_n(i),3,i)  ! stem
                                               manag_remove_prop_compartment(2) = manag_remove_prop * managementInputs(t_n(i),4,i)  ! root
                                               manag_remove_prop_compartment(3) = manag_remove_prop * managementInputs(t_n(i),5,i)  ! foliage

                                          ! clamp compartments to 0–1
                                          if ( manag_remove_prop_compartment(1) < 0.d0 ) manag_remove_prop_compartment(1) = 0.d0
                                          if (manag_remove_prop_compartment(1) > 1.d0 ) manag_remove_prop_compartment(1) = 1.d0
                                          if ( manag_remove_prop_compartment(2) < 0.d0 ) manag_remove_prop_compartment(2) = 0.d0
                                          if (manag_remove_prop_compartment(2) > 1.d0 ) manag_remove_prop_compartment(2) = 1.d0
                                          if ( manag_remove_prop_compartment(3) < 0.d0 ) manag_remove_prop_compartment(3) = 0.d0
                                          if (manag_remove_prop_compartment(3) > 1.d0 ) manag_remove_prop_compartment(3) = 1.d0

                                               ! calculate biomass losses
                                               biom_loss_stem_manag(i)   = biom_stem(i)   * manag_remove_prop_compartment(1)
                                               biom_loss_root_manag(i)   = biom_root(i)   * manag_remove_prop_compartment(2)

                                               if ( f_dormant(month, leafgrow(i), leaffall(i)) ) then
                                               biom_loss_foliage_manag(i) = biom_foliage_debt(i) * manag_remove_prop_compartment(3)
                                               biom_foliage_debt(i) = biom_foliage_debt(i) - biom_loss_foliage_manag(i)
                                                 if ( biom_foliage_debt(i) < 0.d0 ) biom_foliage_debt(i) = 0.d0
                                               else
                                                 biom_loss_foliage_manag(i) = biom_foliage(i) * manag_remove_prop_compartment(3)
                                               biom_foliage(i) = biom_foliage(i) - biom_loss_foliage_manag(i)
                                                 if ( biom_foliage(i) < 0.d0 ) biom_foliage(i) = 0.d0
                                               end if

                                                   ! apply reductions
                                                   stems_n(i) = stems_n(i) - stems_loss_manag(i)
                                                   if ( stems_n(i) < 0.d0 ) stems_n(i) = 0.d0
                                                   biom_stem(i) = biom_stem(i) - biom_loss_stem_manag(i)
                                                   if ( biom_stem(i) < 0.d0 ) biom_stem(i) = 0.d0
                                                   biom_root(i) = biom_root(i) - biom_loss_root_manag(i)
                                                   if ( biom_root(i) < 0.d0 ) biom_root(i) = 0.d0
                                        end if

                                   end if

                                   ! thinning calculated using trees proportion of AGB retained
                                   if ( isnan(managementInputs(t_n(i),2,i)) ) then
                                       if( .not. isnan(managementInputs(t_n(i),6,i)) ) then

                                          ! removal proportion = 1 – retained biomass fraction
                                          manag_remove_prop = 1.d0 - managementInputs(t_n(i),6,i)

                                          ! clamp 0–1
                                          if ( manag_remove_prop < 0.d0 ) manag_remove_prop = 0.d0
                                          if ( manag_remove_prop > 1.d0 ) manag_remove_prop = 1.d0

                                          ! biomass losses
                                          biom_loss_stem_manag(i)   = biom_stem(i) * manag_remove_prop
                                          biom_loss_root_manag(i)   = biom_root(i) * manag_remove_prop

                                          if ( f_dormant(month, leafgrow(i), leaffall(i)) ) then
                                          biom_loss_foliage_manag(i) = biom_foliage_debt(i) * manag_remove_prop
                                          biom_foliage_debt(i) = biom_foliage_debt(i) - biom_loss_foliage_manag(i)
                                             if ( biom_foliage_debt(i) < 0.d0 ) biom_foliage_debt(i) = 0.d0
                                          else
                                            biom_loss_foliage_manag(i) = biom_foliage(i) * manag_remove_prop
                                          biom_foliage(i) = biom_foliage(i) - biom_loss_foliage_manag(i)
                                             if ( biom_foliage(i) < 0.d0 ) biom_foliage(i) = 0.d0
                                          end if

                                            ! prevent negative values
                                            if ( managementInputs(t_n(i),3,i) <= 0.d0 ) then
                                               stems_loss_manag(i) = stems_n(i)
                                            else
                                            stems_loss_manag(i) = stems_n(i) * (manag_remove_prop / managementInputs(t_n(i),3,i))
                                            ! clamp implied tree removal: 0 ≤ stems_loss ≤ stems_n
                                               if ( stems_loss_manag(i) < 0.d0 ) stems_loss_manag(i) = 0.d0
                                               if ( stems_loss_manag(i) > stems_n(i) ) stems_loss_manag(i) = stems_n(i)
                                            end if

                                            ! adjust stems_n and biomass
                                            stems_n(i) = stems_n(i) - stems_loss_manag(i)
                                            if ( stems_n(i) < 0.d0 ) stems_n(i) = 0.d0
                                            biom_stem(i) = biom_stem(i) - biom_loss_stem_manag(i)
                                            if ( biom_stem(i) < 0.d0 ) biom_stem(i) = 0.d0
                                            biom_root(i) = biom_root(i) - biom_loss_root_manag(i)
                                            if ( biom_root(i) < 0.d0 ) biom_root(i) = 0.d0
                                       end if
                                   end if

                            t_n(i) = t_n(i) + 1

                        end if

                    end if

                end if

            end do





            ! Defoliation --------------------------------------------------------------------------
            !reset defoliation value
            stems_loss_def(:) = 0.d0
            biom_loss_stem_def(:) = 0.d0
            biom_loss_root_def(:) = 0.d0
            biom_loss_foliage_def(:) = 0.d0
            def_type(:) = 0
            coppice_event(:) = .FALSE.

            do i = 1, n_sp

                if( d_t(i) > 0 ) then

                    if(d_n(i) <= d_t(i)) then

                        if( age(ii,i) >= defoliationInputs(d_n(i),1,i) ) then

                            ! Check whether we need to put the defoliation type back to default after first month
                            def_type(i) = int( defoliationInputs(d_n(i),2,i))
                            def_recover_t(i) = defoliationInputs(d_n(i),7,i)

                            prop_carbs(i) = defoliationInputs(d_n(i),8,i)
                            prop_npp(i) = defoliationInputs(d_n(i),9,i)

                            ! apparently it is safer to use scalars
                            stem_retained_input   = defoliationInputs(d_n(i),3,i)
                            foliage_retained_input = defoliationInputs(d_n(i),4,i)
                            root_retained_input   = defoliationInputs(d_n(i),5,i)
                            stem_input            = defoliationInputs(d_n(i),6,i)

                            ! Adjust pre-defoliation foliage mass (i.e. pre-defoliation foliage mass of trees that survived the defoliation event)
                            if( def_type(i) == 1 .or. def_type(i) == 3 ) then ! 1 = pruning, 3 = epicormic
                                biom_foliage_adj_pre_def(i) = biom_foliage(i) * root_retained_input ! depends on how many trees died as defined by root mass loss
                                !sr_ratio(i) = (biom_stem(i) + biom_foliage(i)) / biom_root(i)

                            else if (def_type(i) == 2 ) then ! also depends on how many trees died, but age needs to be adjusted as well
                                biom_foliage_adj_pre_def(i) = biom_foliage(i) * root_retained_input
                                !sr_ratio(i) = (biom_stem(i) + biom_foliage(i)) / biom_root(i)
                                coppice_event(i) = .TRUE.
                                dbh(i) = 0.1d0
                                basal_area(i) = 0.d0
                                lai(i) =  0.d0

                                ! for coppice the age and age related variables need to be updated
                                ! Overwrite current and future months for this species
                                do jj = ii, n_m
                                    age(jj,i)   = 1.d0/12.d0 + (jj - ii) / 12.d0
                                    age_m(jj,i) = age(jj,i) - 1.d0/12.d0
                                end do

                                if (ii == 1) then
                                age_m(ii,i) = age(ii,i)
                                end if

                                ! Age-dependent traits

                                ! SLA
                                tmp_vec = f_exp(1, age_m(ii,i), SLA0(i), SLA1(i), tSLA(i), 2.d0)
                                SLA(ii,i) = tmp_vec(1)

                                ! fracBB
                                tmp_vec = f_exp(1, age_m(ii,i), fracBB0(i), fracBB1(i), tBB(i), 1.d0)
                                fracBB(ii,i) = tmp_vec(1)

                                ! wood density
                                tmp_vec = f_exp(1, age_m(ii,i), rho0(i), rho1(i), tRho(i), 1.d0)
                                wood_density(ii,i) = tmp_vec(1)

                                ! gammaN
                                tmp_vec = f_exp(1, age(ii,i), gammaN0(i), gammaN1(i), tgammaN(i), ngammaN(i))
                                gammaN(ii,i) = tmp_vec(1)

                                ! gammaF
                                tmp_vec = f_exp_foliage(1, age_m(ii,i), gammaF1(i), gammaF0(i), tgammaF(i))
                                gammaF(ii,i) = tmp_vec(1)


                                ! Age modifier f_age
                                if (nAge(i) == 0.d0) then
                                    f_age(ii,i) = 1.d0
                                else
                                    f_age(ii,i) = 1.d0 / ( 1.d0 + ( ((age_m(ii,i)/MaxAge(i)) / rAge(i)) ** nAge(i) ) )
                                end if

                            else if (def_type(i) == 4) then ! 4 = stand replacing
                                def_recover_t(i) = 0.d0
                            else
                                biom_foliage_adj_pre_def(i) = 0.0d0
                            end if

                            age_last_def_event(i) = age(ii, i)

                            ! adjust biomass pools due to defoliation
                            if( f_dormant(month, leafgrow(i), leaffall(i)) .eqv. .TRUE.) then
                                ! foliage debt depends on stems that can resprout, and if dormant, needs to come from biom_foliage_debt !20250301
                                biom_loss_foliage_def(i) = biom_foliage_debt(i) * (1.d0 - root_retained_input) ! depends on how many trees died as defined by root mass loss
                                biom_foliage_debt(i) = biom_foliage_debt(i) - biom_loss_foliage_def(i) ! same as biom_foliage_debt(i) * defoliationInputs(d_n(i),3,i), where the latter is stem_retained_input
                                !biom_foliage must be 0 and remains 0
                                ! if a prune or epicormic event occurred during dormant season, then no foliage could have been removed
                                if ( def_type(i) == 1 .or. def_type(i) == 3 ) then
                                    def_recover_t(i) = 0.d0
                                end if
                            else
                                biom_loss_foliage_def(i) = biom_foliage(i) * (1.d0 - root_retained_input * &
                                foliage_retained_input) !defol_stem_mass_prop_retained
                                biom_foliage(i) = biom_foliage(i) - biom_loss_foliage_def(i)
                            end if

                            biom_loss_stem_def(i) = biom_stem(i) * (1.d0 - stem_retained_input)
                            biom_loss_root_def(i) = biom_root(i) * (1.d0 - root_retained_input)

                            biom_stem(i) = biom_stem(i) - biom_loss_stem_def(i)
                            biom_root(i) = biom_root(i) - biom_loss_root_def(i)



                            ! if root biomass declined, there was mortality, so update stems_n
                            if( root_retained_input < 1.d0 ) then

                                ! When the sum of proportion of roots retained and Sfraction is <= 1, then all N will be removed even though some biomass remains. So restrict stems_n to be at least 0.
                                ! note that this is based on stem fraction, not root or foliage fractions, which would be harder to determine as inputs

                                ! calculate reduction in tree density
                                  stems_loss_def(i) = stems_n(i) * ( 1.d0 - root_retained_input) / &
                                  stem_input

                                ! clamp implied tree removal: 0 ≤ stems_loss ≤ stems_n
                                if ( stems_loss_def(i) < 0.d0 ) stems_loss_def(i) = 0.d0
                                if ( stems_loss_def(i) > stems_n(i) ) stems_loss_def(i) = stems_n(i)

                                stems_n(i) = stems_n(i) - stems_loss_def(i)

                            end if

                            d_n(i) = d_n(i) + 1

                        end if

                    end if

                end if

            end do



            ! Mortality --------------------------------------------------------------------------

            ! Stress related ------------------
            !reset mortality value
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
                    end if
                end if
            end do



            ! Update stand structure if there was thinning, defoliation or stress-related mortality that reduced stems_n
            if ( sum(stems_loss_manag(:) + stems_loss_def(:) + stems_loss_stress(:)) > 1.0e-6 ) then
                biom_tree(:) = biom_stem(:) * 1000.d0 / stems_n(:)  ! kg/tree
                dbh(:) = ( biom_tree(:) / aWs(:)) ** (1.d0 / nWs(:))
                basal_area(:) = dbh(:) ** 2.d0 / 4.d0 * Pi * stems_n(:) / 10000.d0
            end if

            ! If there was a coppice event, update the height, crown width and crown length (dbh and basal area will already have been updated above)
                is_new(:) = coppice_event(:)
                if ( any(is_new(:)) ) then
                          competition_total = sum( wood_density(ii,:) * basal_area(:) )
                          calculate_states = .TRUE.
                          call s_height_crown_allometry (n_sp, age(ii,:), stems_n(:), competition_total, &
                              lai(:), height_rel(:), &
                              height_model, crown_width_model, pars_i(63:82,:), &
                              dbh(:), dbh_prev(:), height(:), crown_length(:), crown_width(:), crown_ratio(:), &
                              calculate_states, is_new(:) )
                end if





            ! Self-thinning / Density dependent related ------------------

            ! Initialize losses
            stems_loss_density(:)        = 0.d0
            biom_loss_stem_density(:)    = 0.d0
            biom_loss_root_density(:)    = 0.d0
            biom_loss_foliage_density(:) = 0.d0

            ! skip density-dependent mortality if any thinning/defoliation/stress mortality occurred, and also skip if there was a coppice event because the new dbh will be 0, so it will have declined
            if (sum(stems_loss_manag(:) + stems_loss_def(:) + stems_loss_stress(:)) < 1.0e-6) then
                 if (.not. any(coppice_event(:))) then

                       ! basal area proportion per cohort
                       basal_area_total = max(sum(basal_area(:)), 1.0d-12)
                       basal_area_prop(:) = basal_area(:) / basal_area_total
                       where (basal_area_prop(:) < 1.0d-6)
                           basal_area_prop(:) = 1.0d-6
                       end where
                       ! Stems per ha (per cohort)
                       stems_n_ha(:) = stems_n(:) / basal_area_prop(:)
                       where (stems_n_ha(:) < 1.0d-12)
                           stems_n_ha(:) = 1.0d-12
                       end where
                       ! Other stand-level totals
                       stems_n_total = max(sum(stems_n(:)), 1.0d-12)
                       dbh_total     = sum(dbh(:) * stems_n(:)) / stems_n_total
                       ! weighted long-term modifiers
                       lt_fN_ave    = sum(lt_fN(:)    * basal_area_prop(:))
                       lt_fT_ave    = sum(lt_fT(:)    * basal_area_prop(:))
                       lt_fPhys_ave = sum(lt_fPhys(:) * basal_area_prop(:))
                       ! maximum tree biomass per cohort
                       biom_tree_max(:) = wSx1000(:) * (1000.d0 / stems_n_ha(:))**thinPower(:)

                       if (mort_model .eq. 1) then
                           do i = 1, n_sp
                               !if (.not. f_dormant(month, leafgrow(i), leaffall(i))) then
                                   if (biom_tree_max(i) < biom_tree(i)) then
                                       stems_loss_density(i) = f_get_mortality( &
                                           stems_n_ha(i), &
                                           biom_stem(i) / basal_area_prop(i), &
                                           mS(i), wSx1000(i), thinPower(i)) * &
                                           basal_area_prop(i)
                                   end if
                                   if (stems_loss_density(i) < 0.d0) stems_loss_density(i) = 0.d0
                                   stems_loss_density(i) = min(stems_loss_density(i), stems_n(i))
                               !end if
                           end do
                       end if

                       if (mort_model .eq. 2) then
                           mort_thinn_total = 0.d0
                           ! Apply modifiers to the intercept
                           thinIntercept_eff = st_Intercept &
                               + st_fN    * log(max(lt_fN_ave,    1.0d-6)) &
                               + st_fT    * log(max(lt_fT_ave,    1.0d-6)) &
                               + st_fPhys * log(max(lt_fPhys_ave, 1.0d-6))
                           ! Stand-level self-thinning frontier
                           dbh_safe = max(dbh_total, 1.0d-6)
                           ! used weighted average nWS
                           expo = -(sum(nWs(:) * basal_area_prop(:))) / st_Power
                           logN = thinIntercept_eff + expo * log(dbh_safe)
                           N_max = exp(logN)
                           N_max = max(N_max, 0.d0)
                           ! Stand-level mortality with deadband
                           if (stems_n_total > N_max * (1.d0 + 1.d-10)) then
                               mort_thinn_total = stems_n_total - N_max
                           else
                               mort_thinn_total = 0.d0
                           end if
                           mort_thinn_total = min(mort_thinn_total, stems_n_total)
                           ! Mass-conserving allocation across cohorts
                           if (mort_thinn_total > 0.d0) then
                               ! weights proportional to basal area
                               weight(:) = basal_area(:)
                               weight_sum = sum(weight(:))
                               if (weight_sum > 0.d0) then
                                   do i = 1, n_sp
                                       stems_loss_density(i) = mort_thinn_total * weight(i) / weight_sum
                                       stems_loss_density(i) = min(stems_loss_density(i), stems_n(i))
                                   end do
                               else
                                   ! fallback: proportional to stem numbers
                                   do i = 1, n_sp
                                       stems_loss_density(i) = mort_thinn_total * stems_n(i) / stems_n_total
                                   end do
                               end if
                               ! Final renormalisation to enforce exact conservation
                               loss_sum = sum(stems_loss_density(:))
                               if (loss_sum > 0.d0) then
                                   scale = mort_thinn_total / loss_sum
                                   stems_loss_density(:) = stems_loss_density(:) * scale
                               end if
                           end if
                       end if





if (mort_model .eq. 3) then
    mort_thinn_total = 0.d0

    betaN_eff = betaN
    pp = betaB + 1.d0
    dbh_prev_safe = max(dbh_total_prev, 1.0d-6)
    dbh_ratio     = max(dbh_total / dbh_prev_safe, 1.0d-6)
    modifiers = lt_fN_ave    ** betafN * &
                lt_fT_ave    ** betafT * &
                lt_fPhys_ave ** betafPhys

    ! delta term
    delta_term = dbh_prev_safe ** pp * (1.d0 - dbh_ratio ** pp)

    ! inner argument for inversion
    inner = stems_n_total ** (1.d0 - betaN_eff) + &
            Exp(beta0) * (1.d0 - betaN_eff) / pp * delta_term * modifiers

    ! allow inner to reach zero, not artificially capped
    inner = max(inner, 0.d0)

    ! safe inversion using log-exp, with inv_exp capped for stability
    inv_exp = 1.d0 / (1.d0 - betaN_eff)
    inv_exp = max(min(inv_exp, 90.d0), -90.d0)

    ! compute mortality at stand level
    mort_thinn_total = stems_n_total - Exp(inv_exp * Log(inner))

    ! ensure mortality is physically meaningful
    mort_thinn_total = max(mort_thinn_total, 0.d0)
    mort_thinn_total = min(mort_thinn_total, stems_n_total)

! Mass-conserving allocation across cohorts
                           if (mort_thinn_total > 0.d0) then
                               ! weights proportional to basal area
                               weight(:) = basal_area(:)
                               weight_sum = sum(weight(:))
                               if (weight_sum > 0.d0) then
                                   do i = 1, n_sp
                                       stems_loss_density(i) = mort_thinn_total * weight(i) / weight_sum
                                       stems_loss_density(i) = min(stems_loss_density(i), stems_n(i))
                                   end do
                               else
                                   ! fallback: proportional to stem numbers
                                   do i = 1, n_sp
                                       stems_loss_density(i) = mort_thinn_total * stems_n(i) / stems_n_total
                                   end do
                               end if
                               ! Final renormalisation to enforce exact conservation
                               loss_sum = sum(stems_loss_density(:))
                               if (loss_sum > 0.d0) then
                                   scale = mort_thinn_total / loss_sum
                                   stems_loss_density(:) = stems_loss_density(:) * scale
                               end if
                           end if



    ! Allocate stand-level mortality across cohorts
    !if (mort_thinn_total > 0.d0) then
    !    do i = 1, n_sp
    !        if (.not. f_dormant(month, leafgrow(i), leaffall(i))) then
!
    !            if (n_sp .eq. 1) then
    !                stems_loss_density(i) = mort_thinn_total
    !            else
    !                stems_loss_density(i) = mort_thinn_total * &
    !                    Pi * dbh_total**2 / 40000.d0 / &
    !                    basal_area_total * basal_area(i) / &
    !                    max(Pi * dbh(i)**2 / 40000.d0, 1.0d-12)
    !            end if
!
    !            ! enforce cohort-level physical bounds
    !            stems_loss_density(i) = min(stems_loss_density(i), stems_n(i))
    !            stems_loss_density(i) = max(stems_loss_density(i), 0.d0)
!
    !        end if
    !    end do
    !end if
end if









                       ! Apply losses
                       do i = 1, n_sp
                           if (stems_loss_density(i) > 0.d0) then
                               biom_loss_stem_density(i)    = mS(i) * biom_stem(i)    * &
                                                              stems_loss_density(i) / max(stems_n(i), 1.0d-12)
                               biom_loss_root_density(i)    = mR(i) * biom_root(i)    * &
                                                              stems_loss_density(i) / max(stems_n(i), 1.0d-12)
                               biom_loss_foliage_density(i) = mF(i) * biom_foliage(i) * &
                                                              stems_loss_density(i) / max(stems_n(i), 1.0d-12)
                               stems_n(i)      = stems_n(i)      - stems_loss_density(i)
                               biom_stem(i)    = biom_stem(i)    - biom_loss_stem_density(i)
                               biom_root(i)    = biom_root(i)    - biom_loss_root_density(i)
                               biom_foliage(i) = biom_foliage(i) - biom_loss_foliage_density(i)
                           end if
                           if (stems_n(i) <= 0.d0) then
                               stems_n(i)      = 0.d0
                               biom_stem(i)    = 0.d0
                               biom_root(i)    = 0.d0
                               biom_foliage(i) = 0.d0
                           end if
                       end do
                 end if
            end if
            coppice_event(:) = .FALSE.



                        ! Additional calculations ------------------
            biom_tree(:) = biom_stem(:) * 1000.d0 / stems_n(:)
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! This line should be added but causes an error, it is not critical because self-thinning usually doesn't change dbh much
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !dbh(:) = ( biom_tree(:) / aWs(:)) ** (1.d0 / nWs(:))
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            basal_area(:) = dbh(:) ** 2.d0 / 4.d0 * Pi * stems_n(:) / 10000.d0

            ! lai has not been updated since the growth
            lai(:) =  biom_foliage(:) * SLA(ii,:) * 0.1d0

            ! Adjust the old volume after thinning, defoliation and mortality
            volume(:) = biom_stem(:) * (1.d0 - fracBB(ii,:)) / wood_density(ii,:)
            where( aV(:) > 0 ) volume(:) = aV(:) * dbh(:) ** nVB(:) * height(:) ** nVH(:) * &
                (dbh(:) * dbh(:) * height(:)) ** nVBH(:) * stems_n(:)
            volume_old(:) = volume(:)


            ! Used when mort_model = 2
            dbh_prev(:) = dbh(:)
            dbh_total_prev = dbh_total

            ! Efficiency
            epsilon_gpp(:) = 100 * GPP(:) / apar(:)
            epsilon_npp(:) = 100 * NPP(:) / apar(:)
            epsilon_biom_stem(:) = 100 * biom_incr_stem(:) / apar(:)

            where( apar(:) .eq. 0.d0 )
                epsilon_gpp(:) = 0.d0
                epsilon_npp(:) = 0.d0
                epsilon_biom_stem(:) = 0.d0
            end where


            ! dbh distributions
            dlocation(:) = 1.d0
            where( Dlocation0(:)==0.d0 .and. &
                     DlocationB(:)==0.d0 .and. &
                     Dlocationrh(:)==0.d0 .and. &
                     Dlocationt(:)==0.d0 .and. &
                     DlocationC(:)==0.d0 )
            dlocation(:) = 0.d0
            end where
            DWeibullScale(:) = Exp( Dscale0(:) + DscaleB(:) * Log(dbh(:)) + Dscalerh(:) * &
                       Log(height_rel(:)) + Dscalet(:) * Log(age(ii,:)) + DscaleC(:) * Log(competition_total))
            DWeibullShape(:) = Exp( Dshape0(:) + DshapeB(:) * Log( dbh(:) ) + Dshaperh(:) * Log(height_rel(:)) + &
                                      Dshapet(:) * Log(age(ii,:)) + DshapeC(:) * Log(competition_total))
            DWeibullShape_gamma(:) = f_gamma_dist(1.d0 + 1.d0 / DWeibullShape(:), n_sp)
            DWeibullLocation(:) = Exp( Dlocation0(:) + DlocationB(:) * Log(dbh(:)) + &
                                         Dlocationrh(:) * Log(height_rel(:)) + Dlocationt(:) * Log(age(ii,:)) + &
                                         DlocationC(:) * Log(competition_total))
            where( dlocation(:) == 0.d0 )
            DWeibullLocation(:) = NINT(dbh(:)) / 1.d0 - 1.d0 - DWeibullScale(:) * DWeibullShape_gamma(:)
            end where
            where( DWeibullLocation(:) < 0.01d0 ) DWeibullLocation(:) = 0.01d0




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
        !if ( leafgrow > leaffall ) then
        !    ! check which hemisphere
        !    if  ( month >= leaffall .and. month <= leafgrow ) then ! growing at winter
        !        out = .TRUE.
        !    end if
        !else if ( leafgrow < leaffall ) then
        !    if ( month < leafgrow .or. month >= leaffall ) then ! growing at summer
        !        out = .TRUE.
        !    end if
        !end if

        if (leafgrow > leaffall) then
            ! Check which hemisphere
            if (month >= leaffall) then     ! southern hemisphere
                if (month <= leafgrow) then
                    out = .TRUE.
                end if
            end if
        else if (leafgrow < leaffall) then  ! northern hemisphere
            if (month < leafgrow) then
                out = .TRUE.
            else
                if (month >= leaffall) then
                    out = .TRUE.
                end if
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

        ! Sort all height and crown heights
        Height_all = [Heightcrown(:), height(:)] ! put height and crown beginning into vector
        Height_ind = f_orderId(Height_all) ! sort the array

        ! Assign index order for further calculations
        ones(:) = -1
        ones(1:n_sp) = 1
        ones = ones(Height_ind)

    !   cummulative sum
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

        !if ( Lat >= 0.d0 .and. Lat <= 23.4d0) Then
        !    !the zenith angle only needs to be adjusted if the lat is between about -23.4 and 23.4
        !    where( dayOfYear(:) > secondxaxisintercept .or. dayOfYear(:) < firstxaxisintercept )
        !        solarangle(:) = -1.d0 * solarzenithangle(:)
        !    end where
        !end if
!
        !if (  Lat >= -23.4d0 .and. Lat < 0.d0 ) Then
        !    !the zenith angle only needs to be adjusted if the lat is between about -23.4 and 23.4
        !    where( dayOfYear(:) > firstxaxisintercept .and. dayOfYear(:) < secondxaxisintercept )
        !        solarangle(:) = -1.d0 * solarzenithangle(:)
        !    end where
        !end if

        ! Northern tropics: 0 to 23.4
        if (Lat >= 0.d0) then
            if (Lat <= 23.4d0) then
                ! Adjust solar angle for dayOfYear > secondxaxisintercept
                where(dayOfYear(:) > secondxaxisintercept)
                    solarangle(:) = -1.d0 * solarzenithangle(:)
                end where

                ! Adjust solar angle for dayOfYear < firstxaxisintercept
                where(dayOfYear(:) < firstxaxisintercept)
                    solarangle(:) = -1.d0 * solarzenithangle(:)
                end where
            end if
        end if

        ! Southern tropics: -23.4 to 0
        if (Lat >= -23.4d0) then
            if (Lat < 0.d0) then
                ! Adjust solar angle for dayOfYear > firstxaxisintercept AND < secondxaxisintercept
                where(dayOfYear(:) > firstxaxisintercept)
                    where(dayOfYear(:) < secondxaxisintercept)
                        solarangle(:) = -1.d0 * solarzenithangle(:)
                    end where
                end where
            end if
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
        !where (fullCanAge(:) > 0.d0 .and. age(:) < fullCanAge(:) )
        !    canopy_cover(:) = (age(:) + 0.01d0) / fullCanAge(:)
        !end where


        where (fullCanAge(:) > 0.d0)
        ! Further restrict to indices where age < fullCanAge
            where (age(:) < fullCanAge(:))
                canopy_cover(:) = (age(:) + 0.01d0) / fullCanAge(:)
            end where
        end where


        lightIntcptn = (1.d0 - (Exp(-k * lai / canopy_cover)))

        RADt = solar_rad * days_in_month ! MJ m-2 month-1
        apar = RADt * lightIntcptn * canopy_cover

    end subroutine s_light_3pgpjs


    subroutine s_light_3pgmix ( n_sp, height, crown_length, crown_width, lai, stems_n, solar_rad, &
        CrownShape, k, gammaAPAR, solarAngle,days_in_month, &
        apar, lai_above, fi, lambda_v, lambda_h, canopy_vol_frac, layer_id, lai_sa_ratio, m_apar)

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
        integer, dimension(n_sp), intent(in) :: CrownShape   ! crown shape of a given species; 1=cone, 2=ellipsoid, 3=half-ellipsoid, 4=rectangular
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: k
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: gammaAPAR

        real(kind=kind(0.0d0)), intent(in) :: solarAngle
        integer, intent(in) :: days_in_month

        ! output
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: apar
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: lai_above !leaf area above the given species
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: fi !the proportion of above canopy apar absorbed by each species
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: lambda_v       !Constant to partition light between species and to account for vertical canopy heterogeneity (see Equations 2 and 3 of Forrester et al., 2014, Forest Ecosystems, 1:17)
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: lambda_h         !Constant to account for horizontal canopy heterogeneity such as gaps between trees and the change in zenith angle (and shading) with latitude and season (see Equations 2 and 5 of Forrester et al., 2014, Forest Ecosystems, 1:17)
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: canopy_vol_frac !Fraction of canopy space (between lowest crown crown height to tallest height) filled by crowns
        integer, dimension(n_sp), intent(out) :: layer_id
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: lai_sa_ratio !the ratio of mean tree leaf area (m2) to crownSA (m2)

        ! Additional variables for calculation distribution
        integer :: i
        real(kind=kind(0.0d0)), dimension(n_sp) :: Heightmidcrown    !mean height of the middle of the crown (height - height to crown base)/2 + height to crown base
        real(kind=kind(0.0d0)), dimension(n_sp) :: Heightcrown ! height of the crown begining
        real(kind=kind(0.0d0)), dimension(n_sp) :: CrownSA  !mean crown surface area (m2) of a species
        real(kind=kind(0.0d0)), dimension(n_sp) :: Crownvolume   !the crown volume of a given species
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
real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: m_apar ! modifier to amplify light benefit to shorter cohorts


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


        ! If total APAR exceeds above-canopy PAR, scale down proportionally (this is possible in extreme cases after multiplying by vertical and horizontal parameters that make the sum of absorbed PAR slightly exceed the incoming radiation)
        if (sum(apar(:)) > solar_rad * days_in_month) then
            apar(:) = apar(:) * (solar_rad * days_in_month) / sum(apar(:))
        end if

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

if (n_sp > 1 ) then
      if( sum(gammaAPAR(:)) > 0.0d0) then ! no need to do this for monocultures, or for stands where all gammaAPAR are 0, because there will not be any effect
         ! avoid fi = 0.0 for shaded cohorts
         where (fi(:) < 1d-12)
             fi(:) = 1d-12
         end where
         ! average height of all cohorts, weighted by their contribution to LAI
         height_wtav_LAI = sum( height(:) * lai(:) ) / sum( max(lai(:), 1.0d-12) ) !20251114
         ! height of cohort relative to weighted average height
         height_rel_wt(:) = height(:)/height_wtav_LAI
         ! modifier to redistribute PAR not absorbed by the canopy
         !m_apar(:) = 1.d0 + sum( max(fi(:), 1d-12) ) * gammaAPAR(:) * Exp(-gammaAPAR(:) * (height_rel_wt(:) - 1.d0))
         !m_apar(:) = 1.d0 + sum( max(fi(:), 1d-12) ) * Exp(-gammaAPAR(:) * (height_rel_wt(:) - 1.d0))

         where (height_rel_wt < 1.d0)
         m_apar(:) = 1.d0 + sum( max(fi(:), 1d-12) ) * (Exp(gammaAPAR(:) * (1.d0 - height_rel_wt(:) )) - 1.d0 )
         elsewhere
         m_apar = 1.d0
         end where

         m_apar(:) = min( m_apar(:),  &
              (solar_rad * days_in_month * (1.d0 - exp(-k(:)*lai(:))))  &
              / (max(fi(:), 1d-12)*solar_rad * days_in_month) ) ! MJ m-2 month-1
         ! ensure no cohorts have their APAR reduced
         m_apar(:) = max( 1.d0, m_apar(:))
         ! only allow cohorts with relative heights < 0.5 to receive additional APAR.
         !where (height_rel_wt(:) < 0.5d0)
         !    m_apar(:) = 1.0d0
         !end where
         ! adjust the cohort APAR
         apar(:) = apar(:) * m_apar(:)
         ! ensure the total stand APAR is still less than above canopy PAR
         apar(:) = apar(:) * (solar_rad * days_in_month)/ sum( apar(:) )
      end if
else
      m_apar(:) = 1.d0
end if


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






        subroutine s_height_crown_allometry (n_sp, age, stems_n, competition_total, &
        lai, height_rel, &
        height_model, crown_width_model, pars_s, & ! removed correct_bias
        dbh, dbh_prev, height, crown_length, crown_width, crown_ratio, &
        calculate_states, is_new)

        implicit none

        ! input
        integer, intent(in) :: n_sp ! number of species
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: age
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: stems_n
        real(kind=kind(0.0d0)), intent(in) :: competition_total
        real(kind=kind(0.0d0)), dimension(n_sp), intent(in) :: lai
        logical :: calculate_states
        logical, dimension(n_sp) :: is_new

        ! parameters
        integer, intent(in) :: height_model ! which height equation
        integer, intent(in) :: crown_width_model ! which crown width equation
        integer :: i

        real(kind=kind(0.0d0)), dimension(20, n_sp), intent(in) :: pars_s ! parameters
        real(kind=kind(0.0d0)), dimension(n_sp), intent(inout) :: crown_ratio

        ! output
        real(kind=kind(0.0d0)), dimension(n_sp), intent(inout) :: dbh
        real(kind=kind(0.0d0)), dimension(n_sp), intent(inout) :: dbh_prev
        real(kind=kind(0.0d0)), dimension(n_sp), intent(inout) :: height
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: crown_length
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: crown_width

        ! Variables and parameters
        real(kind=kind(0.0d0)) :: lai_total
        real(kind=kind(0.0d0)), dimension(n_sp), intent(out) :: height_rel
        real(kind=kind(0.0d0)), dimension(n_sp) :: Hd
        real(kind=kind(0.0d0)), dimension(n_sp) :: aH, nH1, nH2, nH3, nH4
        real(kind=kind(0.0d0)), dimension(n_sp) :: aV, nVB, nVH, nVBH
        real(kind=kind(0.0d0)), dimension(n_sp) :: aK, nK1, nK2, nK3, nK4
        real(kind=kind(0.0d0)), dimension(n_sp) :: aHL, nHL1, nHL2, nHL3, nHL4

        include 'i_read_param_sub.h'





        if ( calculate_states .eqv. .TRUE. ) then
                  if( height_model .eq. 1 ) then
                       where (is_new)
                           height(:) = aH(:) * dbh(:) ** nH1(:) * competition_total ** nH2(:)
                       end where
                  else if ( height_model .eq. 2 ) then
                       where (is_new)
                           height(:) = Hd(:) + aH(:) * Exp(1.d0)**(-nH1(:)/dbh(:)) + nH2(:) * competition_total * dbh(:)
                       end where
                  else if ( height_model .eq. 3 ) then
                      do i = 1, n_sp
                          if (.not. is_new(i)) cycle
                              if (nH3(i) < 1.0e-5) then
                                  if (nH4(i) < 1.0e-5) then
                                      height(i) = Hd(i) + (dbh(i) ** aH(i)) / (nH1(i) + nH2(i) * (dbh(i) ** aH(i)))
                                  else
                                      height(i) = Hd(i) + (dbh(i) ** aH(i)) / (Exp(nH1(i) + nH3(i)*competition_total) + &
                                                   Exp(nH2(i) + nH4(i)*competition_total) * (dbh(i) ** aH(i)))
                                  end if
                              else
                                  height(i) = Hd(i) + (dbh(i) ** aH(i)) / (Exp(nH1(i) + nH3(i)*competition_total) + &
                                               Exp(nH2(i) + nH4(i)*competition_total) * (dbh(i) ** aH(i)))
                              end if
                      end do
                  end if

                  height_rel(:) = height(:) / ( sum( height(:) * stems_n(:) ) / sum( stems_n(:) ) )
                  lai_total = sum( lai(:) )

                  ! initial live-crown ratio
                  where (is_new)
                       crown_ratio(:) = ( aHL(:) * dbh(:) ** nHL1(:) * lai_total ** nHL2(:) * height_rel(:) ** nHL3(:) * &
                           competition_total ** nHL4(:))
                       crown_length(:) = crown_ratio(:) * height(:)
                  end where

                  ! initial crown width
                  if( crown_width_model .eq. 1 ) then
                      where (is_new)
                          crown_width(:) = ( aK(:) * dbh(:) ** nK1(:) * height(:) ** nK2(:) * height_rel(:) ** nK3(:) * &
                                    competition_total ** nK4(:))
                      end where
                  else if ( crown_width_model .eq. 2 ) then
                      where (is_new)
                          crown_width(:) = aK(:) * Exp(1.d0)**(-nK1(:)/dbh(:)) + nK2(:) * competition_total * dbh(:)
                      end where
                  else if ( crown_width_model .eq. 3 ) then
                      do i = 1, n_sp
                          if (.not. is_new(i)) cycle
                              if (nK3(i) < 1.0e-5) then
                                  if (nK4(i) < 1.0e-5) then
                                      crown_width(i) = (dbh(i) ** aK(i)) / (nK1(i) + nK2(i) * (dbh(i) ** aK(i)))
                                  else
                                      crown_width(i) = (dbh(i) ** aK(i)) / (Exp(nK1(i) + nK3(i)*competition_total) + &
                                                    Exp(nK2(i) + nK4(i)*competition_total) * (dbh(i) ** aK(i)))
                                  end if
                              else
                                  crown_width(i) = (dbh(i) ** aK(i)) / (Exp(nK1(i) + nK3(i)*competition_total) + &
                                                Exp(nK2(i) + nK4(i)*competition_total) * (dbh(i) ** aK(i)))
                              end if
                      end do
                  end if
        end if



        if ( calculate_states .eqv. .FALSE. ) then

                  if( height_model .eq. 1 ) then
                      where (is_new)
                           height(:) = height(:) + aH(:) * nH1(:) * (dbh(:) ** (nH1(:) - 1)) * &
                                       ( competition_total ** nH2(:) ) * ( dbh(:) - dbh_prev(:) )
                      end where
                  else if ( height_model .eq. 2 ) then
                      where (is_new)
                           height(:) = height(:) + ( aH(:) * Exp(-nH1(:)/dbh(:) ) * nH1(:)/(dbh(:)*dbh(:)) + nH2(:) * &
                                       competition_total) * ( dbh(:) - dbh_prev(:) )
                      end where
                  else if ( height_model .eq. 3 ) then
                      do i = 1, n_sp
                         if (.not. is_new(i)) cycle
                             if (nH3(i) < 1.0e-5) then
                                 if (nH4(i) < 1.0e-5) then
                                     height(i) = height(i) + (aH(i) * nH1(i) * dbh(i) ** (aH(i) - 1)) / &
                                                  ((nH1(i) + nH2(i) * dbh(i) ** aH(i)) ** 2) * (dbh(i) - dbh_prev(i))
                                 else
                                     height(i) = height(i) + (aH(i) * Exp(nH1(i) + nH3(i) * competition_total) * &
                                                  dbh(i) ** (aH(i) - 1)) / ((Exp(nH1(i) + nH3(i) * competition_total) + &
                                                  Exp(nH2(i) + nH4(i) * competition_total) * dbh(i) ** aH(i)) ** 2) * &
                                                  (dbh(i) - dbh_prev(i))
                                 end if
                             else
                                 height(i) = height(i) + (aH(i) * Exp(nH1(i) + nH3(i) * competition_total) * &
                                              dbh(i) ** (aH(i) - 1)) / ((Exp(nH1(i) + nH3(i) * competition_total) + &
                                              Exp(nH2(i) + nH4(i) * competition_total) * dbh(i) ** aH(i)) ** 2) * &
                                              (dbh(i) - dbh_prev(i))
                             end if
                      end do
                  end if

                  height_rel(:) = height(:) / ( sum( height(:) * stems_n(:) ) / sum( stems_n(:) ) )
                  lai_total = sum( lai(:) )

                  where (is_new)
                      where (dbh(:) > 0.0d0)
                          where (aHL(:) > 0.0d0)
                              crown_ratio(:) = crown_ratio(:) + aHL(:) * nHL1(:) * (dbh(:) ** (nHL1(:) - 1)) * &
                                               lai_total ** nHL2(:) * height_rel(:) ** nHL3(:) * competition_total ** nHL4(:) * &
                                               (dbh(:) - dbh_prev(:))
                              crown_length(:) = crown_ratio(:) * height(:)
                          end where
                      end where
                  end where

                  ! initial crown width
                  if( crown_width_model .eq. 1 ) then
                      where (is_new)
                          crown_width(:) = crown_width(:) + aK(:) * nK1(:) * (dbh(:) ** (nK1(:) - 1)) * &
                                      ( competition_total ** nK2(:) ) * ( dbh(:) - dbh_prev(:) )
                      end where
                  else if ( crown_width_model .eq. 2 ) then
                      where (is_new)
                          crown_width(:) = crown_width(:) + ( aK(:) * Exp(-nK1(:)/dbh(:) ) * nK1(:)/(dbh(:)*dbh(:)) + nK2(:) * &
                                      competition_total) * ( dbh(:) - dbh_prev(:) )
                      end where
                  else if ( crown_width_model .eq. 3 ) then
                      do i = 1, n_sp
                          if (.not. is_new(i)) cycle
                             if (nK3(i) < 1.0e-5) then
                                 if (nK4(i) < 1.0e-5) then
                                     crown_width(i) = crown_width(i) + (aK(i) * nK1(i) * dbh(i) ** (aK(i) - 1)) / &
                                                      ((nK1(i) + nK2(i) * dbh(i) ** aK(i)) ** 2) * (dbh(i) - dbh_prev(i))
                                 else
                                     crown_width(i) = crown_width(i) + (aK(i) * Exp(nK1(i) + nK3(i) * competition_total) * &
                                                      dbh(i) ** (aK(i) - 1)) / ((Exp(nK1(i) + nK3(i) * competition_total) + &
                                                      Exp(nK2(i) + nK4(i) * competition_total) * dbh(i) ** aK(i)) ** 2) * &
                                                      (dbh(i) - dbh_prev(i))
                                 end if
                             else
                                 crown_width(i) = crown_width(i) + (aK(i) * Exp(nK1(i) + nK3(i) * competition_total) * &
                                                  dbh(i) ** (aK(i) - 1)) / ((Exp(nK1(i) + nK3(i) * competition_total) + &
                                                  Exp(nK2(i) + nK4(i) * competition_total) * dbh(i) ** aK(i)) ** 2) * &
                                                  (dbh(i) - dbh_prev(i))
                             end if
                      end do
                  end if
        end if

    end subroutine s_height_crown_allometry



end module mod_3PG
