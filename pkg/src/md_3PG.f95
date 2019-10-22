module mod_3PG

    use, intrinsic :: iso_c_binding, only: c_double, c_int, c_bool
    use mod_decl_const

    implicit none
    private
    public :: s_3PG_f

contains
    
    subroutine s_3PG_f ( siteInputs, speciesInputs, forcingInputs, parameterInputs, biasInputs, &
        n_sp, n_m, output) bind(C, name = "s_3PG_f_")

        implicit none

        !********************************************************************************************
        !   Declaration

        ! Number of species and month
        integer(kind=c_int), intent(in) :: n_m
        integer(kind=c_int), intent(in) :: n_sp

        ! Initial, forcing, parameters
        real(kind=c_double), dimension(8), intent(in) :: siteInputs
        real(kind=c_double), dimension(n_sp,8), intent(in) :: speciesInputs
        real(kind=c_double), dimension(n_m,6), intent(in) :: forcingInputs
        real(kind=c_double), dimension(65,n_sp), intent(in) :: parameterInputs
        real(kind=c_double), dimension(47,n_sp), intent(in) :: biasInputs

        ! Output array
        real(kind=c_double), dimension(n_m,n_sp,11,15), intent(inout) :: output

        ! Variables, Parameters, Constants
        include 'decl_all.h'

        call sub_read_input( forcingInputs, n_m)

        
        ! Initialization
        output(:,:,:,:) = -9999._c_double
        output(:,:,1,1) = forcingInputs(:,1:2) / Pi
        output(:,2,1,2) = tmp_min(:)





    end subroutine s_3PG_f

end module mod_3PG