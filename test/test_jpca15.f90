module test_jpca15
  use stdlib_logger
  use jpca15
  use kinds, ONLY: wp => dp
  use testdrive, only : error_type, unittest_type, new_unittest, check
  implicit none
  private

  public :: collect_jpca15

contains

  !> Collect all exported unit tests
  subroutine collect_jpca15(testsuite)
    !> Collection of tests

    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    !  for logging
    character(len=100) :: log_msg
    call global_logger%configure(indent=.true., max_width=100)
    call global_logger%configure(level = NONE_LEVEL)

    testsuite = [&
                  new_unittest("diat12_der", test_diat12_der), &
                  new_unittest("diat12_ener", test_diat12_ener), &
                  new_unittest("triaaa_der", test_triaaa_der), &
                  new_unittest("triaaa_ener", test_triaaa_ener), &
                  new_unittest("jpca15_subr_e", test_jpca15_subr_e), &
                  new_unittest("jpca15_subr_der", test_jpca15_subr_der), &
                  new_unittest("jpca15_comp_pe_jiggle_Ax", test_jpca15_comp_pe_jiggle_Ax) &
    ]
  end subroutine collect_jpca15

  subroutine test_triaaa_ener(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    integer :: input, output, stat
    real(kind=wp) :: triaaa_r12 = 6.00_wp
    real(kind=wp) :: triaaa_r13 = 7.40065_wp
    real(kind=wp) :: triaaa_r23 = 1.40065_wp
    real(kind=wp) :: triaaa_ener = 1.2725131545902341E-003
    real(kind=wp), DIMENSION(3) :: der_3d
    real(kind=wp) :: ener
    real(kind=wp) :: tol = 0.01_wp

    call triaaa(triaaa_r12, triaaa_r13, triaaa_r23, ener, der_3d)
    ! print *,  "triaaa ener: ", triaaa_r12, triaaa_r13, triaaa_r23, ener, der_3d
    call check(error, ener, triaaa_ener, thr=tol)
  end subroutine test_triaaa_ener

  subroutine test_triaaa_der(error)
    !> Error handling
    implicit none
    type(error_type), allocatable, intent(out) :: error
    integer :: input, output, stat, i
    real(kind=wp) :: triaaa_r12 = 6.00_wp
    real(kind=wp) :: triaaa_r13 = 7.40065_wp
    real(kind=wp) :: triaaa_r23 = 1.40065_wp
    real(kind=wp), DIMENSION(3) :: triaaa_der_3d =  (/-0.123771E-002_wp, -0.308377E-003_wp, 0.219437E-002_wp/)
    real(kind=wp), DIMENSION(3) :: der_3d
    real(kind=wp) :: ener
    real(kind=wp) :: tol = 0.01_wp
    character(len=100) :: log_msg

    call triaaa(triaaa_r12, triaaa_r13, triaaa_r23, ener, der_3d)

    write(log_msg, '(A, F15.5, F15.5, F15.5)'), "jpca15%triaaa input", triaaa_r12, triaaa_r13, triaaa_r23
    call global_logger%log_warning(log_msg)
    write(log_msg, '(A, F15.5)'), "jpca15%triaaa.ener", ener
    call global_logger%log_warning(log_msg)
    write(log_msg, '(A, 3F15.5)'), "jpca15%triaaa.der_3d", der_3d
    call global_logger%log_warning(log_msg)
    ! print *, "triaaa der: ", triaaa_r12, triaaa_r13, triaaa_r23, ener, der_3d
    do i = 1, size(der_3d)
        call check(error, der_3d(i), triaaa_der_3d(i), thr=tol)
    end do
  end subroutine test_triaaa_der

  subroutine test_diat12_ener(error)
    type(error_type), allocatable, intent(out) :: error
    real(kind=wp) :: diat12_r = 6.0_wp
    real(kind=wp) :: diat12_ener = -8.4689982296655799E-004
    real(kind=wp) :: diat12_der = 1.2802678666105818E-003
    real(kind=wp) :: ener, der
    real(kind=wp) :: tol = 0.01_wp
    character(len=100) :: log_msg

    call diat12(diat12_r, ener, der)

    write(log_msg, '(A, F15.5)'), "jpca15%diat12 input:", diat12_r
    call global_logger%log_warning(log_msg)
    write(log_msg, '(A, F15.5)'), "jpca15%diat12.ener", ener
    call global_logger%log_warning(log_msg)
    write(log_msg, '(A, F15.5)'), "jpca15%diat12.der", der
    call global_logger%log_warning(log_msg)

    call check(error, ener, diat12_ener, thr=tol)
  end subroutine test_diat12_ener

  subroutine test_diat12_der(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    real(kind=wp) :: diat12_r = 6.0_wp
    real(kind=wp) :: diat12_ener = -8.4689982296655799E-004
    real(kind=wp) :: diat12_der = 1.2802678666105818E-003
    real(kind=wp) :: ener, der
    real(kind=wp) :: tol = 0.01_wp
    character(len=100) :: log_msg

    call diat12(diat12_r, ener, der)

    write(log_msg, '(A, F15.5)'), "jpca15%diat12 input:", diat12_r
    call global_logger%log_warning(log_msg)
    write(log_msg, '(A, F15.5)'), "jpca15%diat12.ener", ener
    call global_logger%log_warning(log_msg)
    write(log_msg, '(A, F15.5)'), "jpca15%diat12.der", der
    call global_logger%log_warning(log_msg)

    call check(error, der, diat12_der, thr=tol)
  end subroutine test_diat12_der

  subroutine test_jpca15_subr_e(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    real(kind=wp), DIMENSION(3) :: ser =  (/6.0_wp, 7.40065_wp, 1.40065_wp/)
    real(kind=wp), DIMENSION(3) :: der_3d
    real(kind=wp) :: e
    character(len=100) :: log_msg
    real(kind=wp) ::               jpca15_e = 8.4908456909716882E-003
    real(kind=wp) :: tol = 0.01_wp

    call comp_pe(ser, e, der_3d)

    write(log_msg, '(A, 3F15.5)'), "jpca15%jpca15 input:", ser
    call global_logger%log_warning(log_msg)
    write(log_msg, '(A, F15.5)'), "jpca15%jpca15.e", e
    call global_logger%log_warning(log_msg)
    write(log_msg, '(A, 3F15.5)'), "jpca15%jpca15.der_3d", der_3d
    call global_logger%log_warning(log_msg)

    call check(error, e, jpca15_e, thr=tol)
  end subroutine test_jpca15_subr_e

  subroutine test_jpca15_subr_der(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    real(kind=wp), DIMENSION(3) :: ser =  (/6.0_wp, 7.40065_wp, 1.40065_wp/)
    real(kind=wp), DIMENSION(3) :: der_3d
    real(kind=wp) :: e
    real(kind=wp), DIMENSION(3) :: jpca15_der_3d = (/-1.2377147979101314E-003, -3.0837833746203449E-004, 2.1943033475517550E-003/)
    real(kind=wp) :: tol = 0.01_wp
    integer :: i
    character(len=100) :: log_msg

    call comp_pe(ser, e, der_3d)

    write(log_msg, '(A, 3F15.5)'), "jpca15%jpca15 input:", ser
    call global_logger%log_warning(log_msg)
    write(log_msg, '(A, F15.5)'), "jpca15%jpca15.e", e
    call global_logger%log_warning(log_msg)
    write(log_msg, '(A, 3F15.5)'), "jpca15%jpca15.der", der_3d
    call global_logger%log_warning(log_msg)

    do i = 1, size(der_3d)
        call check(error, der_3d(i), jpca15_der_3d(i), thr=tol)
    end do
  end subroutine test_jpca15_subr_der

  subroutine test_jpca15_comp_pe_jiggle_Ax(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    real(kind=wp), DIMENSION(3) :: ser =  (/6.0_wp, 7.40065_wp, 1.40065_wp/)
    real(kind=wp) :: delta = 0.0001_wp
    real(kind=wp), DIMENSION(3) :: ser_delta
    real(kind=wp), DIMENSION(3) :: der_3d, der_delta_3d
    real(kind=wp) :: e, e_delta
    real(kind=wp) :: expected_e = 8.4908456909716882E-003
    real(kind=wp) :: expected_e_delta = 8.4911514317083155E-003
    real(kind=wp) :: expected_force_Ax = 3.0574074435100211E-003
    real(kind=wp) :: tol = 0.01_wp
    integer :: i
    character(len=100) :: log_msg

    ! First we compute the potential energy in the system
    call comp_pe(ser, e, der_3d)
    write(log_msg, '(A, F15.5)'), "jpca15%comp_pe delta:", delta
    call global_logger%log_warning(log_msg)
    write(log_msg, '(A, 3F15.9)'), "jpca15%comp_pe ser:", ser
    call global_logger%log_warning(log_msg)
    write(log_msg, '(A, F15.9)'), "jpca15%comp_pe e:", e
    call global_logger%log_warning(log_msg)
    write(log_msg, '(A, 3F15.9)'), "jpca15%comp_pe der_3d", der_3d
    call global_logger%log_warning(log_msg)

    ! Now we jiggle atom A in the x direction, decreasing the distance
    ! between atoms AB & AC by delta.  The distance between BC - ser(3) -
    ! remains constant
    ser_delta =  (/ser(1) - delta, ser(2) - delta, ser(3) /)
    call comp_pe(ser_delta, e_delta, der_delta_3d)
    write(log_msg, '(A, 3F15.9)'), "jpca15%comp_pe ser_delta:", ser_delta
    call global_logger%log_warning(log_msg)
    write(log_msg, '(A, F15.9)'), "jpca15%comp_pe e_delta:", e_delta
    call global_logger%log_warning(log_msg)
    write(log_msg, '(A, 3F15.9)'), "jpca15%comp_pe der_delta_3d", der_delta_3d
    call global_logger%log_warning(log_msg)

    write(log_msg, '(A, 3F15.9)'), "Force on A in x:", (e_delta - e) / delta
    call global_logger%log_warning(log_msg)
    ! print *, "jpca15: ser - ", ser
    ! print *, "jpca15:   e - ", e
    ! print *, "jpca15: der - ", der_3d
    ! I *think* these two should be equal!!!
    call check(error, (e_delta - e) / delta, expected_force_Ax, thr=tol)
    call check(error, (der_delta_3d(1) - der_3d(1)) / delta, expected_force_Ax, thr=tol)
  end subroutine test_jpca15_comp_pe_jiggle_Ax

end module test_jpca15

program tester
  use, intrinsic :: iso_fortran_env, only : error_unit
  use testdrive, only : run_testsuite
  use test_jpca15
  implicit none
  integer :: stat

  stat = 0
  call run_testsuite(collect_jpca15, error_unit, stat)

  if (stat > 0) then
    write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
    error stop
  end if

end program tester
