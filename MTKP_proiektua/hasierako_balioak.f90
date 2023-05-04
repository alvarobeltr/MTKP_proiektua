program hasi
use mcf_tipos
use funtzioa
use rk4
integer             :: i,j,g
real(kind=dp)       ::p, t, h,q
real(kind=dp), dimension(4)    :: y
integer, parameter  :: n=200 ! h pausu kopurua
real(kind=dp), parameter :: ta=0.0_dp,tb =25.0_dp
h=(tb-ta)/n
open(unit=10,file="hasierako_balioen_sorta.dat",status="replace",action="write")
open(unit=15,file="x1.dat",status="replace",action="write")
open(unit=16,file="x2.dat",status="replace",action="write")
open(unit=17,file="x3.dat",status="replace",action="write")
open(unit=18,file="x4.dat",status="replace",action="write")
do g=0,n
t=0.0_dp
y(3)=-15.0_dp+30.0_dp*g/200
p=y(3)
y(2)=0.1_dp
y(4)=0.1_dp
do j=0,n
        y(1)=-15.0_dp+30.0_dp*j/200
        q=y(1)
                do i=1,n
                        write(unit=10,fmt="(6es20.12)") t, y
                        call rk4_paso_dp(t,y,f,h)
                enddo
	if ((y(1)<0.0_dp).and.(y(3)<0.0_dp)) then       
		write(unit=15,fmt="(4es20.12)")p,q, y(1), y(3)
	else if ((y(1)>0.0_dp).and.(y(3)<0)) then
                write(unit=16,fmt="(4es20.12)")p,q, y(1), y(3)
	else if ((y(3)>0.0_dp).and.(y(1)<0.0_dp)) then
                write(unit=17,fmt="(4es20.12)")p,q, y(1), y(3)
	else
                write(unit=18,fmt="(4es20.12)")p,q, y(1), y(3)
        endif
        y(3)=p
        y(2)=0.1_dp
        y(4)=0.1_dp
        t=0.0_dp
enddo
enddo
close(unit=10)
close(unit=15)
close(unit=16)
close(unit=17)
close(unit=18)

end program hasi
