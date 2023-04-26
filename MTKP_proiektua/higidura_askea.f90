program askea
use mcf_tipos
use funtzioa
use rk4

integer             :: i
real(kind=dp)       :: t, h
real(kind=dp), dimension(4)    :: y 
integer, parameter  :: n=100 ! h pausu kopurua 
real(kind=dp), parameter :: ta=0.0_dp,tb =100.0_dp

h=(tb-ta)/n
t    =  0.0_dp 
y(1) = 6.1_dp ! hasierako balioak
y(2) = 0.0_dp
y(3)=6.7_dp
y(4)=0.0_dp
open(unit=12,file="sollibre.dat",status="replace",action="write")
do i=1,n
 write(unit=12,fmt="(5es20.12)") t, y
 call rk4_paso_dp(t,y,f,h) 
end do
close(unit=12)
end program askea
