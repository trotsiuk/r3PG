! Declaration file for all the variables used in the program
! Structure
! Group
!   - site; species; climate; parameters
!
! Within the group all are ordered alphabeticaly




! Climate ------------------------------
! Parameters related to climate, 
! including all modifiers
real(kind=8), dimension(n_m) :: co2             ! atmospheric CO2
real(kind=8), dimension(n_m) :: frost_days      ! number of frost days per month
real(kind=8), dimension(n_m) :: prcp            ! monthly precipitation sum
real(kind=8), dimension(n_m) :: solar_rad       ! mean daily incident solar radiation
real(kind=8), dimension(n_m) :: tmp_ave         ! mean daily temperature
real(kind=8), dimension(n_m) :: tmp_max         ! maximum daily temperature
real(kind=8), dimension(n_m) :: tmp_min         ! minimum daily temperature