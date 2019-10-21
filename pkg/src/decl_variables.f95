subroutine decl_variables( n_m, n_sp ) bind(C, name = "decl_variables_")
    
    use, intrinsic :: iso_c_binding, only: c_double, c_int, c_bool
    implicit none

    ! Number of species and month
    integer(kind=c_int), intent(in) :: n_m
    integer(kind=c_int), intent(in) :: n_sp

    real(kind=c_double), dimension(n_m) :: tmp_ave         ! Mean daily temperature

end subroutine decl_variables