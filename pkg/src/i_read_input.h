! Reading the mapin input 
! Structure
! Group
!   - site; species; climate; parameters
! Inputs are ordered by the order they are provided for the program
! Other variables are ordered by group and alphabeticaly


! Site data ----------------------------
lat         = siteInputs(1)
soil_class  = int( siteInputs(2) )
aSW         = siteInputs(3)
asw_min     = siteInputs(4)
asw_max     = siteInputs(5)
year_i      = int( siteInputs(6) )
month_i     = int( siteInputs(7) )
altitude    = int( siteInputs(8) )


! Species data -------------------------
year_p      = int( speciesInputs(:,1) )
month_p     = int( speciesInputs(:,2) )
fertility   = speciesInputs(:,3)
biom_foliage_i = speciesInputs(:,4)
biom_root_i = speciesInputs(:,5)
biom_stem_i = speciesInputs(:,6)
stems_n_i   = speciesInputs(:,7)


! Climate ------------------------------
tmp_min     = forcingInputs(:,1)
tmp_max     = forcingInputs(:,2)
prcp        = forcingInputs(:,3)
solar_rad   = forcingInputs(:,4)
frost_days  = forcingInputs(:,5)
co2         = forcingInputs(:,6)
d13catm     = forcingInputs(:,7)

! Settings ----------------------------
light_model = settings(1)
transp_model = settings(2)
phys_model = settings(3)
correct_bias = settings(4)
calculate_d13c = settings(5)