! Initialize variables
! Irrigation
SupIrrig = 0.d0
RunOff = 0.d0


! Climate ------------------------------
tmp_ave(:) = -9999.d0
vpd_day(:) = -9999.d0


! Modifiers ------------------------------
f_age(:,:) = -9999.d0
f_vpd(:) = -9999.d0
f_tmp(:,:) = -9999.d0
f_tmp_gc(:,:) = -9999.d0
f_frost(:,:) = -9999.d0
f_sw(:) = -9999.d0
f_nutr(:) = -9999.d0
f_calpha(:,:) = -9999.d0
f_cg(:,:) = -9999.d0
f_phys(:) = -9999.d0


! Production ------------------------------
gpp(:) = -9999.d0
npp(:) = -9999.d0
npp_f(:) = -9999.d0
par(:) = -9999.d0
alpha_c(:) = -9999.d0
epsilon_gpp(:) = -9999.d0


! Structure ------------------------------
s_age(:,:) = -9999.d0
SLA(:,:) = -9999.d0
biom_foliage_debt(:) = -0.d0

biom_incr_foliage(:) = -9999.d0
biom_incr_root(:) = -9999.d0
biom_incr_stem(:) = -9999.d0
biom_loss_foliage(:) = -9999.d0
biom_loss_root(:) = -9999.d0

! Water use ------------------------------
prcp_interc_fract(:) = -9999.d0
prcp_interc(:) = -9999.d0
conduct_canopy(:) = -9999.d0
conduct_soil = -9999.d0
transp_veg(:) = -9999.d0
evapotra_soil = -9999.d0
wue(:) = -9999.d0
wue_transp(:) = -9999.d0
evapo_transp = -9999.d0  
transp_total = -9999.d0


! Mortality ---------------------------
mort_stress(:) = -9999.d0
mort_thinn(:) = -9999.d0