program main
  use kinds, ONLY: wp => dp
  use jpca15
  implicit none

  ! for comp_pe
  real(KIND=wp), dimension(3) :: p1, p2, p3
  real(KIND=wp), dimension(3) :: ser = (/0.0_wp, 0.0_wp, 0.0_wp/)
  real(KIND=wp), dimension(3) :: der_3d = (/0.0_wp, 0.0_wp, 0.0_wp/)
  real(KIND=wp), dimension(3) :: ser_delta = (/0.0_wp, 0.0_wp, 0.0_wp/)
  real(KIND=wp), dimension(3) :: der_3d_delta = (/0.0_wp, 0.0_wp, 0.0_wp/)
  real(KIND=wp), dimension(3, 3) :: unit_vector
  real(KIND=wp), dimension(3, 3) :: forces_3d
  real(KIND=wp) :: e, e_delta
  real(KIND=wp) :: delta
  ! for diat12
  real(KIND=wp) :: r, der
  real(KIND=wp) :: ener
  ! for triaaa
  real(KIND=wp) :: r12, r13, r23
  real(KIND=wp), DIMENSION(3) :: F_AB, F_AC, F_BC

  delta = 0.001_wp
  p1 = (/-6.0_wp,     0.0_wp, 0.0_wp/)
  p2 = (/ 0.0_wp,     0.0_wp, 0.0_wp/)
  p3 = (/ 1.40065_wp, 0.0_wp, 0.0_wp/)
  r12 = norm2(p1 - p2)
  r13 = norm2(p1 - p3)
  r23 = norm2(p2 - p3)

  print *, "diat12:  r12 -> ", r12
  call diat12(r12, ener, der)
  print *, "diat12: ener <- ", ener
  print *, "diat12:  der <- ", der
  print *, ""

  call triaaa(r12, r13, r23, ener, der_3d)
  print *, "triaaa: r12, r13, r23 -> ", r12, r13, r23
  print *, "triaaa:           ener <- ", ener
  print *, "triaaa:         der_3d <- ", der_3d
  print *, ""

  ser = (/r12, r13, r23/)
  call comp_pe(ser, e, der_3d)
  print *, "comp_pe:    ser -> ", ser
  print *, "comp_pe:      e <- ", e
  print *, "comp_pe: der_3d <- ", der_3d
  print *, ""

  ser_delta = (/ser(1) - delta, ser(2) - delta, ser(3)/)
  call comp_pe(ser_delta, e_delta, der_3d_delta)
  print *, "comp_pe:        delta -> ",     delta
  print *, "comp_pe:    ser_delta -> ", ser_delta
  print *, "comp_pe:      e_delta <- ", e_delta
  !print *, "comp_pe: der_3d_delta <- ", der_3d_delta
  print *, ""

  ! force with finite difference using e
  print *, "Force e Ax: ", (e_delta - e) / delta
  print *, ""

  ! Now get force using der
  unit_vector(1, :) = (p1 - p2) / norm2(p1 - p2)
  unit_vector(2, :) = (p1 - p3) / norm2(p1 - p3)
  unit_vector(3, :) = (p2 - p3) / norm2(p2 - p3)
  print *, "Unit Vector AB:", unit_vector(1, :)
  print *, "Unit Vector AC:", unit_vector(2, :)
  print *, "Unit Vector BC:", unit_vector(3, :)
  F_AB = -1 * der_3d(1) * unit_vector(1, :)
  F_AC = -1 * der_3d(2) * unit_vector(2, :)
  F_BC = -1 * der_3d(3) * unit_vector(3, :)
  print *, "Force AB      :", F_AB
  print *, "Force AC      :", F_AC
  print *, "Force BC      :", F_BC
  print *, "Force sum     :", F_AB + F_AC + F_BC
  forces_3d(1, :) = F_AB + F_AC
  print *, "Force der A   :", forces_3d(1, :)
  forces_3d(2, :) = -F_AB + F_BC
  print *, "Force der B   :", forces_3d(2, :)
  forces_3d(3, :) = -F_AC - F_BC
  print *, "Force der C   :", forces_3d(3, :)
  print *, "Force sum     :", forces_3d(1, :) + forces_3d(2, :) + forces_3d(3, :)
end program main
