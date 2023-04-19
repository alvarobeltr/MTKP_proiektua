program h_ekuazioa

use mcf_tipos

real(kind = dp) :: x, y, z
integer :: i, j

open(unit=1, file="h_ekuazioa.dat", status="replace", action="write")

do i=1,1000
        x = -15.0_dp + 30.0_dp * i/1000.0_dp
        do j=1,1000
                y = -15.0_dp + 30.0_dp * j/1000.0_dp
                call H(x, y, z)
                write(unit=1, fmt=*) x, y, z
        end do
end do

close(unit=1)

contains


subroutine H(x,y,z)
real(kind = dp), intent(in) :: x, y
real(kind = dp), intent(out) :: z
real(kind = dp), parameter :: a=0.07_dp, b=1.017_dp, c=15.103_dp, d=0.00656_dp, Ho=12.065_dp

z = a * (x**2 + y**2) - b * (sqrt(x**2 + c) + sqrt(y**2 + c)) - d*x*y + Ho

end subroutine H

end program h_ekuazioa
