!************************************************
!This program plots projectile motion of an object.
!The program requires user input for initial velocity
!and angle of the object.The algorithm uses a time
!step of 0.01 second i.e. it calculates object's
!location in the x and y plane every 0.01 second.
!**********By: Waleed Ishaque, 2013**************
program tiro_parabolico

implicit none

real, parameter :: g = 9.81 !Valor de la gravedad
real, parameter :: pi=4.0*ATAN(1.0)
real, parameter :: deltat = 0.01

integer, parameter :: npts= 2000 

real, dimension (1:npts) :: x,y,t
real :: xi, yi, vi, radianes, theta
real :: xmax, ymax, tiempo 

integer :: i

write (*,*), 'Deme las coordenadas iniciales de x en m'
read *, xi
write (*,*), 'Deme las coordenadas iniciales de y en m'
read *, yi
write (*,*), 'Deme la velocidad inicial del proyectil en m/s'
read *, vi
write (*,*), 'Deme el angulo inicial de tiro en grados'
read *, theta

radianes = (theta*pi)/180.0


xmax = xi+((vi*vi*sin(2*radianes))/(g))
ymax = yi+(((vi*vi)*(sin(radianes)*sin(radianes)))/(2*g))
tiempo = (2*vi*sin(radianes))/(g)

!Registramos los datos calculados
open (1, file="tiro.dat")

!Calculando la posicion para cada t(i)
do i=1, npts
t(i)=float(i)*deltat
x(i) = xi + (vi*cos(radianes)*t(i))
y(i) = yi + (vi*sin(radianes)*t(i)) - ((0.5*g)*t(i)*t(i))

!Registrando resultados
write (1,*) x(i), y(i)
if (y(i)<0) exit
end do

print *, "#####################################################"
print *, "Un tiro parabolico con:"
print *, "coordenadas iniciales x=",xi,"y=",yi
print *, "velocidad inicial", vi,"m/s"
print *, "y un angulo de", theta, "grados"
print *, "durara en el aire un tiempo de:", tiempo,"s"
print *, "alcanzara una h maxima de:", ymax,"m"
print *, "tendra un alcanze maximo en el eje x de:", xmax,"m"

CLOSE (1)
end program tiro_parabolico


