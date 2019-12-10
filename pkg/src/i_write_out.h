! Writing the main output 


! Climatic variables -------------
output(ii,:,1,1) = tmp_min(ii)
output(ii,:,1,2) = tmp_max(ii)
output(ii,:,1,3) = tmp_ave(ii)
output(ii,:,1,4) = frost_days(ii)
output(ii,:,1,5) = solar_rad(ii)
output(ii,:,1,6) = prcp(ii)
output(ii,:,1,7) = vpd_day(ii)
output(ii,:,1,8) = co2(ii)
output(ii,:,1,9) = d13catm(ii)

! Stand variables ---------------
output(ii,:,2,1) = s_age(ii,:)
output(ii,:,2,2) = stems_n(:)
output(ii,:,2,3) = basal_area(:)
output(ii,:,2,4) = dbh(:)
output(ii,:,2,5) = height(:)
output(ii,:,2,6) = crown_length(:)
output(ii,:,2,7) = crown_width(:)

! Canopy variables ---------------
output(ii,:,3,1) = sla(ii,:)
output(ii,:,3,3) = lai(:)
output(ii,:,3,6) = lai_above(:)
output(ii,:,3,7) = lambda_v(:)
output(ii,:,3,8) = lambda_h(:)
output(ii,:,3,10) = vpd_sp(:)

! Stocks variables ---------------
output(ii,:,4,1) = biom_foliage(:)
output(ii,:,4,2) = biom_root(:)
output(ii,:,4,3) = biom_stem(:)
output(ii,:,4,6) = biom_tree(:)
output(ii,:,4,9) = gammaF(ii,:)
output(ii,:,4,10) = biom_loss_foliage(:)
output(ii,:,4,11) = biom_foliage_debt(:)


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


! Production ---------------
output(ii,:,6,1) = gpp(:)
output(ii,:,6,2) = npp_f(:)
output(ii,:,6,3) = par(:)
output(ii,:,6,4) = fi(:)
output(ii,:,6,5) = alpha_c(:)
output(ii,:,6,6) = epsilon_gpp(:)
output(ii,:,6,11) = npp_fract_root(:)
output(ii,:,6,12) = npp_fract_stem(:)
output(ii,:,6,13) = npp_fract_foliage(:)


! Mortality ---------------
output(ii,:,7,1) = biom_tree_max(:)
output(ii,:,7,2) = gammaN(ii,:)
output(ii,:,7,3) = mort_thinn(:)
output(ii,:,7,4) = mort_stress(:)

! Water use ---------------
output(ii,:,8,3) = prcp_interc_fract(:)
output(ii,:,8,4) = prcp_interc(:)
output(ii,:,8,5) = conduct_canopy(:)
output(ii,:,8,6) = conduct_soil
output(ii,:,8,7) = evapotra_soil
output(ii,:,8,8) = wue(:)
output(ii,:,8,9) = wue_transp(:)
output(ii,:,8,10) = evapo_transp
output(ii,:,8,11) = transp_veg(:)


! Wood Delta ------------------
output(ii,:,9,1) = Gc_mol(:)
output(ii,:,9,2) = Gw_mol(:)
output(ii,:,9,3) = D13CNewPS(:)
output(ii,:,9,4) = D13CTissue(:)
output(ii,:,9,5) = InterCi(:) * 1000000.d0