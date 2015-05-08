!Con este programa podremos calcular la posicion de un proyectil en tiro parabolico, conciderando la resistencia del aire y comparalo con una situacion ideal.

program tiro_resistencia

implicit none
real, parameter :: deltat = 0.01
real, parameter :: g = 9.81 !Valor de la gravedad
real, parameter :: pi=4.0*ATAN(1.0)
real, parameter :: densidad = 1.18 !Densidad del aire al nivel del mar
real, parameter :: cfe = 0.47

integer, parameter :: npts= 2000 

real :: xi, yi, vi, theta, radianes
real :: xmaxsf, ymaxsf, timesf, xmaxcf, ymaxcf, timecf
real :: diferenciax
write (*,*), 'Deme las coordenadas iniciales de x en m'
read *, xi
write (*,*), 'Deme las coordenadas iniciales de y en m'
read *, yi
write (*,*), 'Deme la velocidad inicial del proyectil en m/s'
read *, vi
write (*,*), 'Deme el angulo inicial de tiro en grados'
read *, theta

radianes = (theta*pi)/180.0

call sin_friccion (xi,yi,vi,radianes,xmaxsf,ymaxsf,timesf)
call con_friccion (xi,yi,vi,radianes,xmaxcf,ymaxcf,timecf)


diferenciax = 100.0-((xmaxcf*100.0)/xmaxsf)



print *, "#####################################################"
print *, "||En condiciones ideales||"
print *, "Un tiro parabolico con:"
print *, "coordenadas iniciales x=",xi,"y=",yi
print *, "velocidad inicial", vi,"m/s"
print *, "y un angulo de", theta, "grados"
print *, "durara en el aire un tiempo de:", timesf,"s"
print *, "alcanzara una h maxima de:", ymaxsf,"m"
print *, "tendra un alcanze maximo en el eje x de:", xmaxsf,"m"
print *, "#####################################################"
print *, "||Con resistencia del aire||"
print *, "Un tiro parabolico con:"
print *, "coordenadas iniciales x=",xi,"y=",yi
print *, "velocidad inicial", vi,"m/s"
print *, "y un angulo de", theta,"grados"
print *, "durara en el aire un tiempo de:", timecf,"s"
print *, "alcanzara una h maxima de:", ymaxcf,"m"
print *, "tendra un alcanze maximo en el eje x de", xmaxcf,"m"
print *, "#####################################################"
print *, "La diferencia porcentual en el eje x es:", diferenciax,"%"
print *, "#####################################################"


end program tiro_resistencia


subroutine sin_friccion (xi,yi,vi,radianes,xmaxsf,ymaxsf,timesf)

implicit none
real, parameter :: deltat = 0.01
real, parameter :: g = 9.81 !Valor de la gravedad
real, parameter :: pi=4.0*ATAN(1.0)
real, parameter :: densidad = 1.18 !Densidad del aire al nivel del mar
real, parameter :: cfe = 0.47

integer, parameter :: npts= 2000 

real, dimension (1:npts) :: x,y,t
real :: xi, yi, vi, radianes !Entran
real :: xmaxsf, ymaxsf, timesf !Devuelve

integer :: i


xmaxsf = xi+((vi*vi*sin(2*radianes))/(g))
ymaxsf = yi+(((vi*vi)*(sin(radianes)*sin(radianes)))/(2*g))
timesf = (2*vi*sin(radianes))/(g)

!Registramos los datos calculados
open (1, file="sin_friccion.dat")

!Calculando la posicion para cada t(i)
do i=1, npts
t(i)=float(i)*deltat
x(i) = xi + (vi*cos(radianes)*t(i))
y(i) = yi + (vi*sin(radianes)*t(i)) - ((0.5*g)*t(i)*t(i))

!Registrando resultados
write (1,*) x(i), y(i)
if (y(i)<0) exit
end do

CLOSE (1)
end subroutine sin_friccion

!#####################################################################

subroutine con_friccion (xi,yi,vi,radianes,xmaxcf,ymaxcf,timecf)

implicit none

real, parameter :: deltat = 0.01
real, parameter :: g = 9.81 !Valor de la gravedad
real, parameter :: pi=4.0*ATAN(1.0)
real, parameter :: densidad = 1.18 !Densidad del aire al nivel del mar
real, parameter :: cfe = 0.47

integer :: i 

integer, parameter :: npts= 2000 

real, dimension (0:npts) :: xx,yy,tt,velxx,velyy,axx,ayy 
real :: xi, yi, vi, radianes !Entrada
real :: xmaxcf, ymaxcf, timecf !Salida
real :: D, area, radio,masa !D representa una constate que depende del area, 
!el coeficiente y la densidad

print *, "Deme la masa de la esfera en kg"
read *, masa


print *, "Deme el radio de la esfera en m"
read *, radio

area = pi*radio*radio



!Definiendo condiciones iniciales
xx(0) = xi
yy(0) = yi

velxx(0) = vi*cos(radianes)
velyy(0) = vi*sin(radianes)

D = (0.5*densidad*area*cfe)

axx(0) = -(D/masa)*(sqrt((velxx(0)*velxx(0))+(velyy(0)*velyy(0))))*velxx(0)
ayy(0) = -g-(D/masa)*(sqrt((velxx(0)*velxx(0))+(velyy(0)*velyy(0))))*velyy(0)

tt(0) = 0
!Abriendo archivo.dat 
open (2, file="con_friccion.dat")

!Registrando valores iniciales
write(2,*) xx(0),yy(0)



!Calculando velocidad, aceleracion y posicion para cada t(i)
do i=0, npts

tt(i+1) = tt(i) + deltat

velxx(i+1) = velxx(i)+axx(i)*tt(i+1) 
velyy(i+1) = velyy(i)+ayy(i)*tt(i+1)

axx(i+1) = -(D/masa)*(sqrt((velxx(i)*velxx(i))+(velyy(i)+velyy(i))))*velxx(i)
ayy(i+1) = -g-(D/masa)*(sqrt((velxx(i)*velxx(i))+(velyy(i)+velyy(i))))*velyy(i)

xx(i+1) = xx(i)+velxx(i)*tt(i+1)+(0.5*axx(i)*tt(i+1)*tt(i+1))
yy(i+1) = yy(i)+velyy(i)*tt(i+1)+(0.5*ayy(i)*tt(i+1)*tt(i+1))

!Registrando valores 
write (2,*) xx(i+1), yy(i+1)
if (yy(i+1)<0) exit
end do

close (2)
xmaxcf = xx(i)!El alcanze en x 
ymaxcf = MAXVAL(yy) !EL alcanze en y
timecf = tt(i)*10.0 !El tiempo esta en s/10, por eso lo multiplicamos por 10

end subroutine con_friccion
