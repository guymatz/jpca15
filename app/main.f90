program main
  use kinds, ONLY: wp => dp
  use jpca15
  implicit none

  ! for comp_pe
  real(KIND=wp), dimension(3) :: ser, der_3d
  real(KIND=wp) :: e
  ! for diat12
  real(KIND=wp) :: r, der_1d
  real(KIND=wp) :: ener
  ! for triaaa
  real(KIND=wp) :: r12, r13, r23

  r = 6.0_wp
  call diat12(r, ener, der_1d)
  print *, "diat12 input: ", r
  print *, "diat12%ener: ", ener
  print *, "diat12%der_1d: ", der_1d
  print *, ""

  r12 = r
  r13 = 7.40065_wp
  r23 = 1.40065_wp

  call triaaa(r12, r13, r23, ener, der_3d)
  print *, "triaaa input: ", r12, r13, r23
  print *, "triaaa%ener: ", ener
  print *, "triaaa%der_3d: ", der_3d
  print *, ""

  ser = (/r12, r13, r23/)
  call comp_pe(ser, e, der_3d)
  print *, "comp_pe input: ", ser
  print *, "comp_pe%e: ", e
  print *, "comp_pe%der_3d: ", der_3d
  print *, ""

end program main
