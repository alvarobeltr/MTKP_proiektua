module funtzioa 
public::f,f2

contains 
function f(t,y) result(y4)
    use mcf_tipos
real(kind=dp), intent(in)               :: t
real(kind=dp), dimension(:), intent(in) :: y
real(kind=dp), dimension(size(y))       :: y4
real(kind=dp) :: g,alpha,beta,gamma,chi
alpha=0.07_dp
beta=1.017_dp
gamma=15.103_dp
chi=0.00656_dp
g=9.8_dp
y4(1)=y(2)
y4(2)=(5.0_dp/7.0_dp)*(-0.0025_dp*y(2)-(2.0_dp*alpha*y(1)-beta*y(1)/sqrt(y(1)**2+gamma)-chi*y(3))*g)
y4(3)=y(4)
y4(4)=(5.0_dp/7.0_dp)*(-0.0025_dp*y(4)-(2.0_dp*alpha*y(3)-beta*y(3)/sqrt(y(3)**2+gamma)-chi*y(1))*g)

end function f
function f2(t,y) result(y4)
    use mcf_tipos
real(kind=dp), intent(in)               :: t
real(kind=dp), dimension(:), intent(in) :: y
real(kind=dp), dimension(size(y))       :: y4
real(kind=dp) :: g,alpha,beta,gamma,chi,w,A,pi,phi
pi=acos(-1.0_dp)
alpha=0.07_dp
beta=1.017_dp
gamma=15.103_dp
chi=0.00656_dp
g=9.8_dp
w=1.25_dp
A=1.16_dp
phi=pi/6.0_dp
y4(1)=y(2)
y4(2)=(5.0_dp/7.0_dp)*(-0.0025_dp*y(2)-(2.0_dp*alpha*y(1)-beta*y(1)/sqrt(y(1)**2+gamma)-chi*y(3))*g+A*w**2*sin(w*t)*cos(phi))
y4(3)=y(4)
y4(4)=(5.0_dp/7.0_dp)*((-0.0025_dp*y(4)-(2.0_dp*alpha*y(3)-beta*y(3)/sqrt(y(3)**2+gamma)-chi*y(1))*g)+A*w**2*sin(w*t)*sin(phi))

end function f2
end module funtzioa

