function diy_sq(q) result(value)
    real, intent(in) :: q(4)
    real :: value
    value = q(4)**2 - q(1)**2 - q(2)**2 - q(3)**2
end function diy_sq


module user
    use mcmule, only: mM, musq, pass_cut, userweight, names
    implicit none
    ! nr_q is the number of desired histograms
    integer, parameter :: n_rq = 2
    integer, parameter :: n_rbins = 90
    
    ! --- Probably Important ---
    integer, parameter :: namesLen=6
    integer, parameter :: filenamesuffixLen=10

    real, parameter :: &
        min_val(n_rq) = (/ 0., 0. /)
    real, parameter :: &
        max_val(n_rq) = (/ 1800., 900. /)
    integer :: userdim = 0
    integer :: bin_kind = 0

    contains

    subroutine fix_mu
        musq = mM**2
    end subroutine fix_mu

    function quant(q1, q2, q3, q4, q5, q6, q7)
        implicit none
        real, intent(in) :: q1(4), q2(4), q3(4), q4(4), q5(4), q6(4), q7(4)
        real :: quant(n_rq)
        real :: diy_sq

        call fix_mu
        ! Avoid IR divergence
        pass_cut = .true.
        if (q5(4) < 10) pass_cut = .false.

        names(1) = 'minv'
        quant(1) = sqrt(diy_sq(q2 + q5))
        ! quant(1) = sqrt((q2(4) + q5(4))**2 -(q2(1) + q5(1))**2 - (q2(2) + q5(2))**2 - (q2(3) + q5(3))**2)
        names(2) = 'Ee'
        quant(2) = q2(4)


    end function quant

    subroutine userevent(x, ndim)
        integer :: ndim
        real :: x(ndim)
        userweight = 1.
    end subroutine userevent

    subroutine inituser
        print*, "Loaded demo user module"
    end subroutine inituser



end module user