module user
    use mcmule
    implicit none
    ! nr_q is the number of desired histograms
    integer, parameter :: nrq = 2
    integer, parameter :: nrbins = 90
    
    ! --- Probably Important ---
    integer, parameter :: namesLen=6
    integer, parameter :: filenamesuffixLen=10
    integer :: nq=nrq
    integer :: nbins=nrbins

    real, parameter :: &
        min_val(nrq) = (/ 0., 0. /)
    real, parameter :: &
        max_val(nrq) = (/ 1800., 900. /)
    integer :: userdim = 0
    integer :: bin_kind = 0

    contains

    subroutine fix_mu
        musq = mM**2
    end subroutine fix_mu

    function quant(q1, q2, q3, q4, q5, q6, q7)
        implicit none
        real, intent(in) :: q1(4), q2(4), q3(4), q4(4), q5(4), q6(4), q7(4)
        real :: quant(nrq)

        call fix_mu
        pass_cut = .true.
        if (q5(4) < 10.) pass_cut = .false.

        names(1) = 'minv'
        ! quant(1) = sqrt(sq(q2 + q5))
        quant(1) = sqrt((q2(4) + q5(4))**2 -(q2(1) + q5(1))**2 - (q2(2) + q5(2))**2 - (q2(3) + q5(3))**2)
        names(2) = 'Ee'
        quant(2) = q2(4)


    end function quant

    subroutine userevent(x, ndim)
        use mcmule
        integer :: ndim
        real :: x(ndim)
        userweight = 1.
    end subroutine userevent

    subroutine inituser
        print*, "Loaded demo user module"
    end subroutine inituser



end module user