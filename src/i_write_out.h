! Writing the main output


! Climatic variables -------------
output(ii,:,1,1) = tmp_min(ii)
output(ii,:,1,2) = tmp_max(ii)
output(ii,:,1,3) = tmp_ave(ii)
output(ii,:,1,4) = frost_days(ii)
output(ii,:,1,5) = solar_rad(ii)
output(ii,:,1,6) = day_length(month)
output(ii,:,1,7) = prcp(ii)
output(ii,:,1,8) = vpd_day(ii)
output(ii,:,1,9) = co2(ii)
output(ii,:,1,10) = d13catm(ii)
!output(ii,:,1,11) = managementInputs(3,2,2)

! Stand variables ---------------
output(ii,:,2,1) = age(ii,:)
output(ii,:,2,2) = stems_n(:)
output(ii,:,2,3) = basal_area(:)
output(ii,:,2,4) = basal_area_prop(:)
output(ii,:,2,5) = dbh(:)
output(ii,:,2,6) = height(:)
output(ii,:,2,7) = height_rel(:) ! relative height
output(ii,:,2,8) = crown_length(:)
output(ii,:,2,9) = crown_width(:)
output(ii,:,2,10) = volume(:)
output(ii,:,2,11) = volume_mai(:)
output(ii,:,2,12) = volume_change(:)
output(ii,:,2,13) = volume_cum(:)

! Canopy variables ---------------
output(ii,:,3,1) = layer_id(:)
output(ii,:,3,2) = sla(ii,:)
output(ii,:,3,3) = lai(:)
output(ii,:,3,4) = lai_above(:)
output(ii,:,3,5) = lai_sa_ratio(:)
output(ii,:,3,6) = canopy_vol_frac(:)
output(ii,:,3,7) = canopy_cover(:)
output(ii,:,3,8) = lambda_v(:)
output(ii,:,3,9) = lambda_h(:)
output(ii,:,3,10) = aero_resist(:)
output(ii,:,3,11) = vpd_sp(:)
output(ii,:,3,12) = m_apar(:)

! Stocks variables ---------------
output(ii,:,4,1) = biom_stem(:)
output(ii,:,4,2) = biom_root(:)
output(ii,:,4,3) = biom_foliage(:)
output(ii,:,4,4) = biom_tree(:)
output(ii,:,4,5) = wood_density(ii,:)
output(ii,:,4,6) = fracBB(ii,:)
output(ii,:,4,7) = biom_loss_foliage(:)
output(ii,:,4,8) = biom_loss_root(:)
output(ii,:,4,9) = biom_incr_foliage(:)
output(ii,:,4,10) = biom_incr_root(:)
output(ii,:,4,11) = biom_incr_stem(:)

! Modifiers ---------------
output(ii,:,5,1) = f_age(ii,:)
output(ii,:,5,2) = f_vpd(:)
output(ii,:,5,3) = f_tmp(ii,:)
output(ii,:,5,4) = f_tmp_gc(ii,:)
output(ii,:,5,5) = f_frost(ii,:)
output(ii,:,5,6) = f_sw(:)
output(ii,:,5,7) = f_nutr(:)
output(ii,:,5,8) = f_calpha(ii,:)
output(ii,:,5,9) = f_cg(ii,:)
output(ii,:,5,10) = f_phys(:)
output(ii,:,5,11) = gammaF(ii,:)
output(ii,:,5,12) = f_transp_scale

! Production ---------------
output(ii,:,6,1) = gpp(:)
output(ii,:,6,2) = npp(:)
output(ii,:,6,3) = apar(:)
output(ii,:,6,4) = fi(:)
output(ii,:,6,5) = alpha_c(:)
output(ii,:,6,6) = epsilon_gpp(:)
output(ii,:,6,7) = epsilon_npp(:)
output(ii,:,6,8) = epsilon_biom_stem(:)
output(ii,:,6,9) = npp_fract_stem(:)
output(ii,:,6,10) = npp_fract_foliage(:)
output(ii,:,6,11) = npp_fract_root(:)
output(ii,:,6,12) = pFS(:)
output(ii,:,6,13) = biom_foliage_debt(:)

! Water use ---------------
output(ii,:,7,1) = conduct_canopy(:)
output(ii,:,7,2) = conduct_soil
output(ii,:,7,3) = evapotra_soil
output(ii,:,7,4) = prcp_interc(:)
output(ii,:,7,5) = prcp_interc_fract(:)
output(ii,:,7,6) = prcp_runoff
output(ii,:,7,7) = irrig_supl
output(ii,:,7,8) = wue(:)
output(ii,:,7,9) = wue_transp(:)
output(ii,:,7,10) = evapo_transp
output(ii,:,7,11) = transp_veg(:)
output(ii,:,7,12) = asw
output(ii,:,7,13) = water_runoff_polled

! Mortality ---------------
output(ii,:,8,1) = biom_tree_max(:)
output(ii,:,8,2) = gammaN(ii,:)
!output(ii,:,8,3) = stems_loss_density(:)
!output(ii,:,8,4) = stems_loss_stress(:)
output(ii,:,8,3) = mort_thinn_total
output(ii,:,8,4) = lt_fN_ave
output(ii,:,8,5) = lt_fT_ave
output(ii,:,8,6) = lt_fPhys_ave
output(ii,:,8,7) = lt_fT(:)
output(ii,:,8,8) = lt_fPhys(:)
output(ii,:,8,9) = dbh_total
output(ii,:,8,10) = stems_n_total

! Wood Delta ------------------
output(ii,:,9,1) = Gc_mol(:)
output(ii,:,9,2) = Gw_mol(:)
output(ii,:,9,3) = D13CNewPS(:)
output(ii,:,9,4) = D13CTissue(:)
output(ii,:,9,5) = InterCi(:) * 1000000.d0

! Weibull ---------------------
output(ii,:,10,1) = DWeibullScale(:)
output(ii,:,10,2) = DWeibullShape(:)
output(ii,:,10,3) = DWeibullLocation(:)


! Mortality values ---------------------
output(ii,:,11,1) = stems_loss_manag(:)
output(ii,:,11,2) = biom_loss_stem_manag(:)
output(ii,:,11,3) = biom_loss_root_manag(:)
output(ii,:,11,4) = biom_loss_foliage_manag(:)
output(ii,:,11,5) = stems_loss_stress(:)
output(ii,:,11,6) = biom_loss_stem_stress(:)
output(ii,:,11,7) = biom_loss_root_stress(:)
output(ii,:,11,8) = biom_loss_foliage_stress(:)
output(ii,:,11,9) = stems_loss_density(:)
output(ii,:,11,10) = biom_loss_stem_density(:)
output(ii,:,11,11) = biom_loss_root_density(:)
output(ii,:,11,12) = biom_loss_foliage_density(:)
output(ii,:,11,13) = stems_loss_def(:)
output(ii,:,11,14) = biom_loss_stem_def(:)
output(ii,:,11,15) = biom_loss_root_def(:)
output(ii,:,11,16) = biom_loss_foliage_def(:)
output(ii,:,11,17) = def_type(:)
!output(ii,:,11,18) = def_recover_t(:)
!output(ii,:,11,19) = def_recover_t(:)