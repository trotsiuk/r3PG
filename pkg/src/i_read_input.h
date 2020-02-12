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
year_p      = int( speciesInputs(:,2) )
month_p     = int( speciesInputs(:,3) )
fertility   = speciesInputs(:,4)
biom_foliage_i = speciesInputs(:,5)
biom_root_i = speciesInputs(:,6)
biom_stem_i = speciesInputs(:,7)
stems_n_i   = speciesInputs(:,8)


! Climate ------------------------------
tmp_min     = forcingInputs(:,1)
tmp_max     = forcingInputs(:,2)
prcp        = forcingInputs(:,3)
solar_rad   = forcingInputs(:,4)
frost_days  = forcingInputs(:,5)
co2         = forcingInputs(:,6)
d13catm     = forcingInputs(:,7)


! Parameters ---------------------------
pFS2        = pars_i(1,:)
pFS20       = pars_i(2,:)
aWs         = pars_i(3,:)
nWs         = pars_i(4,:)
pRx         = pars_i(5,:)
pRn         = pars_i(6,:)
gammaF1     = pars_i(7,:)
gammaF0     = pars_i(8,:)
tgammaF     = pars_i(9,:)
gammaR      = pars_i(10,:)
leafgrow    = int( pars_i(11,:) )
leaffall    = int( pars_i(12,:) )
Tmin        = pars_i(13,:)
Topt        = pars_i(14,:)
Tmax        = pars_i(15,:)
kF          = pars_i(16,:)
SWconst0    = pars_i(17,:)
SWpower0    = pars_i(18,:)
fCalpha700  = pars_i(19,:)
fCg700      = pars_i(20,:)
m0          = pars_i(21,:)
fN0         = pars_i(22,:)
fNn         = pars_i(23,:)
MaxAge      = pars_i(24,:)
nAge        = pars_i(25,:)
rAge        = pars_i(26,:)
gammaN1     = pars_i(27,:)
gammaN0     = pars_i(28,:)
tgammaN     = pars_i(29,:)
ngammaN     = pars_i(30,:)
wSx1000     = pars_i(31,:)
thinPower   = pars_i(32,:)
mF          = pars_i(33,:)
mR          = pars_i(34,:)
mS          = pars_i(35,:)
SLA0        = pars_i(36,:)
SLA1        = pars_i(37,:)
tSLA        = pars_i(38,:)
k           = pars_i(39,:)
fullCanAge  = pars_i(40,:)
MaxIntcptn  = pars_i(41,:)
LAImaxIntcptn = pars_i(42,:)
cVPD        = pars_i(43,:)
alphaCx     = pars_i(44,:)
y           = pars_i(45,:)
MinCond     = pars_i(46,:)
MaxCond     = pars_i(47,:)
LAIgcx      = pars_i(48,:)
CoeffCond   = pars_i(49,:)
BLcond      = pars_i(50,:)
RGcGW       = pars_i(51,:)
D13CTissueDif = pars_i(52,:)
aFracDiffu  = pars_i(53,:)
bFracRubi   = pars_i(54,:)
fracBB0     = pars_i(55,:)
fracBB1     = pars_i(56,:)
tBB         = pars_i(57,:)
rho0        = pars_i(58,:)
rho1        = pars_i(59,:)
tRho        = pars_i(60,:)
CrownShape  = int( pars_i(61,:) )


! Settings ----------------------------
light_model = settings(1)
transp_model = settings(2)
phys_model = settings(3)
correct_bias = settings(4)
calculate_d13c = settings(5)