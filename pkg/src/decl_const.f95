! This module contains the constant parameters for the 3PGmix model
! All the parameters are ordered in the alphabetical ordered
! V.Trotsiuk [trotsiuk@fld.czu.cz]

module decl_const
    
    use, intrinsic :: iso_c_binding, only: c_double, c_int
    implicit none

    real(kind=c_double), parameter :: Pi = 3.141592654_c_double

end module decl_const