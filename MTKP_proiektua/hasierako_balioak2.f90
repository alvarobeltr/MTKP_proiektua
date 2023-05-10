program denbora
use mcf_tipos
use funtzioa
use rk4
integer:: i,j,g
real(kind=dp)::p, t, h,q
real(kind=dp), dimension(4)    :: y
integer, parameter  :: n=700, m=900 ! h pausu kopurua
real(kind=dp), parameter :: ta=0.0_dp,tb =50.0_dp
h=(tb-ta)/m
open(unit=10,file="hasierako_balioen_sorta.dat",status="replace",action="write")
open(unit=15,file="y1.dat",status="replace",action="write")
open(unit=16,file="y2.dat",status="replace",action="write")
open(unit=17,file="y3.dat",status="replace",action="write")
open(unit=18,file="y4.dat",status="replace",action="write")
do g=0,n
t=0.0_dp
y(3)=-15.0_dp+30.0_dp*real(g)/real(n)
p=y(3)
y(2)=0.0_dp
y(4)=0.0_dp
do j=0,n
        y(1)=-15.0_dp+30.0_dp*real(j)/real(n)
        q=y(1)
                do i=1,m
                       
                        call rk4_paso_dp(t,y,f,h)
        if ((t>2.0_dp).and.(y(2)<0.001_dp).and.(y(4)<0.001_dp)) then
	        
	if (t<7.0_dp) then       
		write(unit=15,fmt="(100es20.12)")p,q
	else if ((t>7.0_dp).and.(t<16.0_dp)) then
                write(unit=16,fmt="(4es20.12)")p,q
	else if ((t>16.0_dp).and.(t<21)) then
                write(unit=17,fmt="(4es20.12)")p,q
	else
                write(unit=18,fmt="(4es20.12)")p,q
        endif
exit	
endif        
	
        end do
        y(3)=p
        y(2)=0.0_dp
        y(4)=0.0_dp
        t=0.0_dp

enddo
enddo
close(unit=10)
close(unit=15)
close(unit=16)
close(unit=17)
close(unit=18)

end program denbora
