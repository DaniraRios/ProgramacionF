!Con este programa podremos calcular la posicion de un proyectil en tiro parabolico, conciderando la resistencia del aire y comparalo con una situacion ideal.


module constantes
implicit none
real, parameter :: rad=(4.0*ATAN(1.0))/180 !Conversion de angulos
real, parameter :: pi=4.0*ATAN(1.0)
integer, parameter :: npts= 2000 
!Densidad del aire al nivel del mar
REAL, PARAMETER :: densidad = 1.18
Real, parameter :: cfe = 0.47
end module constantes

program tiro_resistencia
use constantes
implicit none 
real :: xi, yi, vi, theta
real :: xmaxsf, ymaxsf, timesf, xmaxcf, ymaxcf, timecf
real :: diferenciax, diferenciay
write (*,*), 'Deme las coordenadas iniciales de x & y en m'
read *, xi, yi
write (*,*), 'Deme la velocidad inicial del proyectil en m/s'
read *, vi
write (*,*), 'Deme el angulo inicial de tiro en grados'
read *, theta

call sin_friccion (xi,yi,vi,theta,xmaxsf,ymaxsf,timesf)
call con_friccion (xi,yi,vi,theta,xmaxcf,ymaxcf,timecf)


diferenciax = ((xmaxsf-xmaxcf)/xmaxcf) * 100.0
diferenciay = ((ymaxsf-ymaxcf)/ymaxcf) * 100.0


print *, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print *, "||En condiciones ideales||"
print *, "Un tiro parabolico con:"
print *, "coordenadas iniciales x=",xi,"y=",yi
print *, "velocidad inicial", vi,"m/s"
print *, "y un angulo de", theta, "radianes"
print *, "durara en el aire un tiempo de:", timesf,"s"
print *, "alcanzara una h maxima de:", ymaxsf,"m"
print *, "tendra un alcanze maximo en el eje x de:", xmaxsf,"m"
print *, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print *, "||Con resistencia del aire||"
print *, "Un tiro parabolico con:"
print *, "coordenadas iniciales x=",xi,"y=",yi
print *, "velocidad inicial", vi,"m/s"
print *, "y un angulo de", theta,"radianes"
print *, "durara en el aire un tiempo de:", timecf,"s"
print *, "alcanzara una h maxima de:", ymaxcf,"m"
print *, "tendra un alcanze maximo en el eje x de", xmaxcf,"m"
print *, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print *, "La diferencia porcentual en el eje x es:", diferenciax
print *, "La diferencia porcentual en el eje y es:", diferenciay
print *, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"


end program tiro_resistencia


subroutine sin_friccion (xi,yi,vi,theta,xmaxsf,ymaxsf,timesf)
use constantes
implicit none

real, dimension (1:npts) :: x,y,t
real :: xi, yi, vi, theta !Entran
real :: xmaxsf, ymaxsf, timesf 

integer :: i


theta=theta*rad !Convirtiendo grados a radianes

xmaxsf = xi+((vi*vi*sin(2*theta))/(9.8))
ymaxsf = yi+(((vi*vi)*(sin(theta)*sin(theta)))/(19.6))
timesf = (2*vi*sin(theta))/(9.8)

!Registramos los datos calculados
open (1, file="sin_friccion.dat")

!Calculando la posicion para cada t(i)
do i=1, npts, 1
t(i)=float(i)*0.01
x(i) = xi + (vi*cos(theta)*t(i))
y(i) = yi + (vi*sin(theta)*t(i)) - (4.9*t(i)*t(i))

!Registrando resultados
write (1,1001) x(i), y(i)
1001 format (f11.5,f11.5)
if (y(i)<0) exit
end do

CLOSE (1)
end subroutine sin_friccion



subroutine con_friccion (xi,yi,vi,theta,xmaxcf,ymaxcf,timecf)
use constantes

implicit none
integer :: i 

real, DIMENSION (0:npts) :: p,w,u,velp,velw,ap,aw 
real :: xi, yi, vi, theta !Entrada
REAL :: xmaxcf, ymaxcf, timecf !Salida
REAL :: D, area, radio,masa !D representa una constate que depende del area, 
!el coeficiente y la densidad

print *, "Deme la masa de la esfera en kg"
read *, masa


print *, "Deme el radio de la esfera en m"
read *, radio

area = pi*radio*radio



!Definiendo condiciones iniciales
p(0) = xi
w(0) = yi

velp(0) = vi*cos(theta)
velw(0) = vi*sin(theta)

D = (0.5*densidad*area*cfe)

ap(0) = -(D/masa)*velp(0)*velp(0)
aw(0) = 9.8-(D/masa)*velw(0)*velw(0)

u(0) = 0
!Abriendo archivo.dat 
open (2, file="con_friccion.dat")

!Registrando valores iniciales
write(2,1001) p(0),w(0)
1001 format (f11.5,f11.5)


!Calculando posicion para cada t(i)
do i=0, npts, 1

u(i+1) = u(i) + 0.01

velp(i+1) = velp(i)+ap(i)*u(i+1) 
velw(i+1) = velw(i)+aw(i)*u(i+1)

ap(i+1) = -(D/masa)*velp(i)*velp(i)
aw(i+1) = -9.8-(D/masa)*velp(i)*velp(i)

p(i+1) = p(i)+velp(i)*u(i+1)+(0.5*ap(i)*u(i+1)*u(i+1))
w(i+1) = w(i)+velw(i)*u(i+1)+(0.5*aw(i)*u(i+1)*u(i+1))

!Resgistrando valores 
write (2,*) p(i+1), w(i+1)
if (w(i)<0) exit
end do


close (2)
xmaxcf = p(i)
ymaxcf = MAXVAL(w)
timecf = u(i)*10.0
end subroutine con_friccion



