program main
  use kinds, ONLY: wp => dp
  use jpca15
  implicit none

  ! for comp_pe
  real(KIND=wp), dimension(3) :: p1, p2, p3
  real(KIND=wp), dimension(3) :: ser, der_3d
  real(KIND=wp) :: e
  ! for diat12
  real(KIND=wp) :: r, der_1d
  real(KIND=wp) :: ener
  ! for triaaa
  real(KIND=wp) :: r12, r13, r23

  p1 = (/-6.0,     0.0, 0.0/)
  p2 = (/ 0.0,     0.0, 0.0/)
  p3 = (/ 1.40065, 0.0, 0.0/)
  r12 = norm2(p1 - p2)
  r13 = norm2(p1 - p3)
  r23 = norm2(p2 - p3)

  print *, "diat12: r12 -> ", r12
  call diat12(r12, ener, der_1d)
  print *, "diat12: ener <- ", ener
  print *, "diat12: der_1d <- ", der_1d
  print *, ""

  call triaaa(r12, r13, r23, ener, der_3d)
  print *, "triaaa: r12, r13, r23 -> ", r12, r13, r23
  print *, "triaaa: ener <- ", ener
  print *, "triaaa: der_3d <- ", der_3d
  print *, ""

  ser = (/r12, r13, r23/)
  call comp_pe(ser, e, der_3d)
  print *, "comp_pe: ser -> ", ser
  print *, "comp_pe: e <- ", e
  print *, "comp_pe: der_3d <- ", der_3d
  print *, ""

end program main
