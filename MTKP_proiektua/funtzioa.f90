module funtzioa 
public::f

contains 
function f(t,y) result(y4)
    use mcf_tipos
    real(kind=dp), intent(in)               :: t
real(kind=dp)::hx,hy
    real(kind=dp), dimension(:), intent(in) :: y
    real(kind=dp), dimension(size(y))       :: y4
    real(kind=dp) :: g,alpha,beta,gamma,chi
alpha=0.07_dp
beta=1.017_dp
gamma=15.103_dp
chi=0.00656_dp
g=9.8_dp
!y4(1)=x
!y4(2)=xpunto    
!y4(3)=y
!y4(4)=ypunto
!c/m=1
!hx=2.0_dp*alpha*y(1)-beta*2.0_dp*y(1)/sqrt(y(1)**2+gamma)-chi*y(3)
!hy=2.0_dp*alpha*y(3)-beta*2.0_dp*y(3)/sqrt(y(3)**2+gamma)-chi*y(1)
    y4(1) =y(2)
    y4(2)=(5.0_dp/7.0_dp)*(-y(2)-(2.0_dp*alpha*y(1)-beta*2.0_dp*y(1)/sqrt(y(1)**2+gamma)-chi*y(3))*g)
y4(3)=y(4)
y4(4)=(5.0_dp/7.0_dp)*(-y(4)-(2.0_dp*alpha*y(3)-beta*2.0_dp*y(3)/sqrt(y(3)**2+gamma)-chi*y(1))*g)

end function f
!function h(x,y)
!end function h
end module funtzioa

