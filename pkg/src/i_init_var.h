! Initialize variables

! Output ------------------------------
output(:,:,:,:) = -9999.d0

! Climatic variables -------------
tmp_ave(:) = -9999.d0
vpd_day(:) = -9999.d0

! Stand variables ---------------
age(:,:) = -9999.d0
age_m(:,:) = -9999.d0
volume(:) = 0.d0
volume_mai(:) = -9999.d0
volume_old(:) = 0.d0
volume_cum(:) = 0.d0
volume_change(:) = 0.d0

! Canopy variables ---------------
layer_id(:) = -9999
canopy_cover(:) = -9999
canopy_vol_frac(:) = -9999.d0
lai_above(:) = -9999.d0
lambda_v(:) = -9999.d0
lambda_h(:) = -9999.d0
aero_resist(:) = -9999.d0
vpd_sp(:) = -9999.d0
lai_sa_ratio(:) = -9999.d0

! Stocks variables ---------------
biom_loss_foliage(:) = 0.d0
biom_loss_root(:) = 0.d0

biom_incr_foliage(:) = 0.d0
biom_incr_root(:) = 0.d0
biom_incr_stem(:) = 0.d0

biom_foliage_debt(:) = 0.d0

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
f_transp_scale = -9999.d0

! Production ------------------------------
gpp(:) = -9999.d0
npp(:) = 0.d0
npp_f(:) = -9999.d0
par(:) = -9999.d0
fi(:) = -9999.d0
alpha_c(:) = -9999.d0
epsilon(:) = -9999.d0
epsilon_gpp(:) = -9999.d0
epsilon_npp(:) = -9999.d0
epsilon_biom_stem(:) = 0.d0
npp_fract_stem(:) = -9999.d0
npp_fract_foliage(:) = -9999.d0
npp_fract_root(:) = -9999.d0
pFS(:) = -9999.d0

! Mortality ---------------
biom_tree_max(:) = -9999.d0
mort_stress(:) = -0.d0
mort_thinn(:) = -0.d0


! Water use ---------------
conduct_canopy(:) = -9999.d0
conduct_soil = -9999.d0
evapotra_soil = -9999.d0
prcp_interc(:) = -9999.d0
prcp_interc_fract(:) = -9999.d0
prcp_runoff = 0.d0
irrig_supl = 0.d0
wue(:) = -9999.d0
wue_transp(:) = -9999.d0
evapo_transp = -9999.d0  
transp_veg(:) = -9999.d0

transp_total = -9999.d0

! Wood Delta ------------------
Gc_mol(:) = -9999.d0
Gw_mol(:) = -9999.d0
D13CNewPS(:) = -9999.d0
D13CTissue(:) = -9999.d0
InterCi(:) = -9999.d0 / 1000000.d0


! Weibull ---------------------
bias_scale(:,:) = -9999.d0


! Structure ------------------------------

SLA(:,:) = -9999.d0



