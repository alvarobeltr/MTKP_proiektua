program integrala

use mcf_tipos
use mcf_interpoli
use mcf_cuadratura
use funtzioak

real(kind = dp) :: x, em, a, b, T, integral1, eps, f, AA, phi, w, emaitza
integer :: i
integer, parameter :: n=100
real(kind = dp), parameter :: pi=acos(-1.0_dp)

f = 1.25_dp
T = 1.0_dp / f
a = 0.0_dp
b = 100.0_dp
eps = 10.0E-6_dp

open(unit=1, file="integrala.dat", status="replace", action="write")

AA = 1.16_dp
phi = pi/6.0_dp
x = -6.555_dp

do w=0,10



call romberg(integ, a, b, em, eps)


write(unit=1, fmt=*) w, em

end do

close(unit=1)



contains



subroutine integ(t)
real(kind = dp), intent(in) :: t
real(kind = dp) :: integ

integ = abs((AA*sin(w*t)*cos(phi))**2 * cos(2.0_dp * pi * w * t * x))

end subroutine integ

end program integrala

               
