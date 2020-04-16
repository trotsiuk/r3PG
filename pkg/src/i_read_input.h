! Reading the mapin input 
! Structure
! Group
!   - site; species; climate; parameters
! Inputs are ordered by the order they are provided for the program
! Other variables are ordered by group and alphabeticaly


! Site data ----------------------------
lat         = siteInputs(1)
altitude    = int( siteInputs(2) )
soil_class  = int( siteInputs(3) )
aSW         = siteInputs(4)
asw_min     = siteInputs(5)
asw_max     = siteInputs(6)
year_i      = int( siteInputs(7) )
month_i     = int( siteInputs(8) )

! Species data -------------------------
year_p      = int( speciesInputs(:,1) )
month_p     = int( speciesInputs(:,2) )
fertility   = speciesInputs(:,3)
stems_n_i   = speciesInputs(:,4)
biom_stem_i = speciesInputs(:,5)
biom_root_i = speciesInputs(:,6)
biom_foliage_i = speciesInputs(:,7)


! Climate ------------------------------
tmp_min     = forcingInputs(:,1)
tmp_max     = forcingInputs(:,2)
tmp_ave     = forcingInputs(:,3)
prcp        = forcingInputs(:,4)
solar_rad   = forcingInputs(:,5)
frost_days  = forcingInputs(:,6)
vpd_day     = forcingInputs(:,7)
co2         = forcingInputs(:,8)
d13catm     = forcingInputs(:,9)

! Settings ----------------------------
light_model = settings(1)
transp_model = settings(2)
phys_model = settings(3)
height_model = settings(4)
correct_bias = settings(5)
calculate_d13c = settings(6)
