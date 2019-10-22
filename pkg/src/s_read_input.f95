subroutine sub_read_input (forcingInputs, n_m)

    implicit none

    integer, intent(in) :: n_m
    real(kind=8), dimension(n_m,6), intent(in) :: forcingInputs

    include 'decl_all.h'

    ! Climate -------------------------------
    tmp_min     = forcingInputs(:,1)
    tmp_max     = forcingInputs(:,2)
    prcp        = forcingInputs(:,3)
    solar_rad   = forcingInputs(:,4)
    frost_days  = forcingInputs(:,5)
    co2         = forcingInputs(:,6)

    return

end subroutine sub_read_input