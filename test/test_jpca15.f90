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
                  new_unittest("jpca15_der", test_jpca15_subr_der) &
    ]
  end subroutine collect_jpca15

  subroutine test_triaaa_ener(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    integer :: input, output, stat
    real(kind=wp) :: triaaa_r12 = 0.0028739_wp
    real(kind=wp) :: triaaa_r13 = 0.0098706_wp 
    real(kind=wp) :: triaaa_r23 = 0.0064505_wp
    real(kind=wp) :: triaaa_ener = -4.3671538E-003_wp
    real(kind=wp), DIMENSION(3) :: triaaa_der_3d =  (/0.272080_wp, 0.1404722_wp, 0.2017398_wp/)
    real(kind=wp), DIMENSION(3) :: der_3d
    real(kind=wp) :: ener
    real(kind=wp) :: tol = 0.01_wp

    open(newunit=input, status="scratch")
    write(input, '(a)') "This is a valid test"
    rewind(input)

    open(newunit=output, status="scratch")
    call triaaa(triaaa_r12, triaaa_r13, triaaa_r23, ener, der_3d)
    close(input)

    rewind(output)
    !call get_line(output, line, stat)
    close(output)

    ! write(log_msg, '(A, F15.5, F15.5, F15.5)') "jpca15%triaaa input", triaaa_r12, triaaa_r13, triaaa_r23
    ! call global_logger%log_warning(log_msg)
    ! write(log_msg, '(A, F15.5)') "jpca15%triaaa.ener", ener
    ! call global_logger%log_warning(log_msg)
    ! write(log_msg, '(A, 3F15.5)') "jpca15%triaaa.der_3d", der_3d
    ! call global_logger%log_warning(log_msg)

    call check(error, ener, triaaa_ener, thr=tol)
  end subroutine test_triaaa_ener

  subroutine test_triaaa_der(error)
    !> Error handling
    implicit none
    type(error_type), allocatable, intent(out) :: error
    integer :: input, output, stat, i
    real(kind=wp) :: triaaa_r12 = 0.0028739_wp
    real(kind=wp) :: triaaa_r13 = 0.0098706_wp 
    real(kind=wp) :: triaaa_r23 = 0.0064505_wp
    real(kind=wp) :: triaaa_ener = -4.3671538E-003_wp
    real(kind=wp), DIMENSION(3) :: triaaa_der_3d =  (/0.27208_wp, 0.1404722_wp, 0.2017398_wp/)
    real(kind=wp), DIMENSION(3) :: der_3d
    real(kind=wp) :: ener
    real(kind=wp) :: tol = 0.01_wp
    character(len=100) :: log_msg
    open(newunit=input, status="scratch")
    write(input, '(a)') "This is a valid test"
    rewind(input)

    open(newunit=output, status="scratch")
    call triaaa(triaaa_r12, triaaa_r13, triaaa_r23, ener, der_3d)
    close(input)

    rewind(output)
    !call get_line(output, line, stat)
    close(output)

    write(log_msg, '(A, F15.5, F15.5, F15.5)'), "jpca15%triaaa input", triaaa_r12, triaaa_r13, triaaa_r23
    call global_logger%log_warning(log_msg)
    write(log_msg, '(A, F15.5)'), "jpca15%triaaa.ener", ener
    call global_logger%log_warning(log_msg)
    write(log_msg, '(A, 3F15.5)'), "jpca15%triaaa.der_3d", der_3d
    call global_logger%log_warning(log_msg)
    do i = 1, size(der_3d)
        call check(error, der_3d(i), triaaa_der_3d(i), thr=tol)
    end do
    !call check(error, all(der_3d == triaaa_der_3d), thr=tol)
  end subroutine test_triaaa_der

  subroutine test_diat12_ener(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    integer :: input, output, stat
    real(kind=wp) :: diat12_r = 4.9406564584124654E-3
    real(kind=wp) :: diat12_ener = 176.359788
    real(kind=wp) :: diat12_der = -35951.61226
    real(kind=wp) :: ener, der
    real(kind=wp) :: tol = 0.01_wp
    character(len=100) :: log_msg
    open(newunit=input, status="scratch")
    write(input, '(a)') "This is a valid test"
    rewind(input)

    open(newunit=output, status="scratch")
    call diat12(diat12_r, ener, der)
    close(input)

    rewind(output)
    !call get_line(output, line, stat)
    close(output)

    ! write(log_msg, '(A, F15.5)'), "jpca15%diat12 input:", diat12_r
    ! call global_logger%log_warning(log_msg)
    ! write(log_msg, '(A, F15.5)'), "jpca15%diat12.ener", ener
    ! call global_logger%log_warning(log_msg)
    ! write(log_msg, '(A, F15.5)'), "jpca15%diat12.der", der
    ! call global_logger%log_warning(log_msg)

    call check(error, ener, diat12_ener, thr=tol)
  end subroutine test_diat12_ener

  subroutine test_diat12_der(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    integer :: input, output, stat
    real(kind=wp) :: diat12_r = 4.9406564584124654E-3
    real(kind=wp) :: diat12_ener = 176.359788
    real(kind=wp) :: diat12_der = -35951.61226
    real(kind=wp) :: ener, der
    real(kind=wp) :: tol = 0.01_wp
    character(len=100) :: log_msg
    open(newunit=input, status="scratch")
    write(input, '(a)') "This is a valid test"
    rewind(input)

    open(newunit=output, status="scratch")
    call diat12(diat12_r, ener, der)
    close(input)

    rewind(output)
    !call get_line(output, line, stat)
    close(output)

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
    integer :: input, output, stat
    real(kind=wp), DIMENSION(3) :: ser =  (/0.0028739_wp, 0.0098706_wp, 0.0064505_wp/)
    real(kind=wp), DIMENSION(3) :: der_3d
    real(kind=wp) :: e
    character(len=100) :: log_msg
!    real(kind=wp), DIMENSION(3) :: jpca15_der_3d = (/-106248.97330, -9008.79442, -21091.87775/)
    real(kind=wp) ::               jpca15_e = 14331.821639646125_wp
    real(kind=wp) :: tol = 0.01_wp
    open(newunit=input, status="scratch")
    write(input, '(a)') "This is a valid test"
    rewind(input)

    open(newunit=output, status="scratch")
    call comp_pe(ser, e, der_3d)
    close(input)

    rewind(output)
    !call get_line(output, line, stat)
    close(output)

    ! write(log_msg, '(A, 3F15.5)'), "jpca15%jpca15 input:", ser
    ! call global_logger%log_warning(log_msg)
    ! write(log_msg, '(A, F15.5)'), "jpca15%jpca15.e", e
    ! call global_logger%log_warning(log_msg)
    ! write(log_msg, '(A, 3F15.5)'), "jpca15%jpca15.der_3d", der_3d
    ! call global_logger%log_warning(log_msg)

    call check(error, e, jpca15_e, thr=tol)
  end subroutine test_jpca15_subr_e

  subroutine test_jpca15_subr_der(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    integer :: input, output, stat
    real(kind=wp), DIMENSION(3) :: ser =  (/0.0028739_wp, 0.0098706_wp, 0.0064505_wp/)
    real(kind=wp), DIMENSION(3) :: der_3d
    real(kind=wp) :: e
    real(kind=wp), DIMENSION(3) :: jpca15_der_3d = (/-106248.97330438906, -9008.7944248419753, -21091.877752197026/)
    real(kind=wp) :: tol = 0.01_wp
    integer :: i
    character(len=100) :: log_msg
    open(newunit=input, status="scratch")
    write(input, '(a)') "This is a valid test"
    rewind(input)

    open(newunit=output, status="scratch")
    call comp_pe(ser, e, der_3d)
    close(input)

    rewind(output)
    !call get_line(output, line, stat)
    close(output)

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
