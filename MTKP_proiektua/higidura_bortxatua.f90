program bortxatua
use mcf_tipos
use funtzioa
use rk4

integer             :: i
real(kind=dp)       :: t, h
real(kind=dp), dimension(4)    :: y
real(kind=dp), dimension(10000):: z
integer, parameter  :: n=6000 ! h pausu kopurua
real(kind=dp), parameter :: ta=0.0_dp,tb =100.0_dp, pi=acos(-1.0_dp)

h=(tb-ta)/n
t =  0.0_dp
y(1) =6.55_dp ! hasierako balioak
y(2) = 0.2_dp
y(3)=6.55_dp
y(4)=0.0_dp
open(unit=12,file="solbortxatu.dat",status="replace",action="write")
open(unit=13,file="desfase.dat",status="replace",action="write")
z=0.0_dp
do i=1,n
 write(unit=12,fmt="(100es20.12)") t, y(:)
 z(i)=y(1)
 call rk4_paso_dp(t,y,f2,h)
end do

do i=1,50

 write(unit=13,fmt="(6es20.12)") z(i), z(i+20)

enddo
close(unit=12)
close(unit=13)

end program bortxatua
