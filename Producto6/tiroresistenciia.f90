!************************************************  
  !This program plots projectile motion of an object.  
  !The program requires user input for initial velocity   
  !and angle of the object.The algorithm uses a time   
  !step of 0.01 second i.e. it calculates object's  
  !location in the x and y plane every 0.01 second.  
  !**********By: Waleed Ishaque, 2013**************  
  program tiro_resistencia  
       implicit none  
       !Defining constants:  
      real, parameter :: pi = 4.0*atan(1.0) 
      real :: v, angulo, alfa, vx, vy, x0, y0, ax, ay
      real :: t, v0x, v0y, vfx, vfy, xf, yf 
      real :: m, radio, A, D
      
      real, parameter :: deltat = 0.1
      real, parameter :: cfe = 0.47
      real, parameter :: airden = 1.225
      real, parameter :: g = 9.81  
      real :: x(100),y(100)  

          integer :: i


!donde pi es pi
!donde v es la velocidad inicial compuesta por x,y  
!donde angulo es el angulo en grados
!donde alfa es el angulo en radianes
!donde t es el tiempo
!donde vx es el componente en x de la velocidad inicial
!donde vy es el componente en y de la velocidad inicial
!donde x,y son las coordenadas iniciales del objeto
!donde m es la masa de la esfera
!donde radio es el radio de la esfera
!donde a es el area de la esfera

        
       write(*,*) 'Dame la posicion inicial en x de la esfera, en metros (Real)'
 
     read *, x0

       write(*,*) 'Dame la posicion inicial en y de la esfera, en metros (Real)'
 
     read *, y0

       write(*,*) 'Dame el ángulo inicial de tiro de la esfera en grados (Real)'   

     read *, angulo   

       write(*,*) 'Dame la velocidad inicial de la esfera en m/s (Real) '   

     read *, v

       write(*,*) 'Dame la masa de la esfera en kg (Real) '   

     read *, m 
  
       write(*,*) 'Dame el radio de la esfera en m (Real) '   

     read *, radio


       !Convirtiendo los grados  a radianes   

       alfa = angulo*pi/180.0  
   
       !Calculando el area de la esfera
       
       A = pi * (radio * radio)
       
       !Calculando la constante D
       
       D = (cfe * airden * A) / 2
     
      
       ! Descomponiendo la v

       vx=(v)*(cos(alfa)) 
 
       v0x = vx

       vy=(v)*(sin(alfa)) 
    
       v0y = vy
 
!open .dat file and start writing on it using the algorithm
open(1, file='tiro.dat')
x=0
y=0
do i=0,100
 
! calculando parametros 
CALL  calculando_parametros

!escribir en el archivo tiro.dat
write(1,*) xf, yf

if (t>10) exit
end do

!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
contains 
subroutine calculando_parametros
REAL, INTENT(IN) :: D, m, v, v0x, g, v0y, deltat, x0, y0
    REAL, INTENT(OUT) :: xf, yf



ax = -(D/m) * v * v0x
ay = -g -(D/m) * v * v0y


t = (float(i) * deltat)
vfx = v0x + (ax * t)
vfy = v0y + (ay * t)

xf = x0 + (vfx * t) + ((0.5) * ax * (t*t))
yf = y0 + (vfy * t) + ((0.5) * ay * (t*t))

t = t + deltat

! llamando a la subrutina cambiar parametros, para que los componentes finales de una etapa sean los componentes iniciales de la siguiente
call cambiar_ parametros

END SUBROUTINE calculando_parametros

!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
contains 

Subroutine cambiar_parametros
    REAL, INTENT(IN) :: vfx, vfy, xf, yf 
    REAL, INTENT(OUT) :: v0x, v0y, x0, y0
vfx = v0x
vfy = v0y
xf = x0
yf = y0

END SUBROUTINE cambiar_parametros

end program tiro_resistencia
      
     


