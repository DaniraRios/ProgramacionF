!************************************************  
  !This program plots projectile motion of an object.  
  !The program requires user input for initial velocity   
  !and angle of the object.The algorithm uses a time   
  !step of 0.01 second i.e. it calculates object's  
  !location in the x and y plane every 0.01 second.  
  !**********By: Waleed Ishaque, 2013**************  
  program tiro_parabolico  
       implicit none  
       !Defining constants:  
      real, parameter :: pi = 4.0*atan(1.0) 
       real :: x0, y0, v0x, v0y, v, t, xf, yf, angulo, alfa, vfx, vfy, afx, afy, m, radio, D, A
      real, parameter :: cfe = 0.47
      real, parameter :: airden = 1.225  
       real, parameter :: g = 9.81  
      real:: x(150),y(150)  

          integer :: i 


      !donde g es la aceleracion de la gravedad, pi is "pi"   

      !v es la velocidad inicial del objeto   

       !a es el angulo de tiro, r es el mismo angulo, pero en radianes   

      !t es el tiempo   

      !x and y son cordenadas del objeto durante el tiro    

      !Seek user input   
       write(*,*) 'Dame la posicion inicial en x de la esfera, en metros (Real)'
 
     read *, x0

       write(*,*) 'Dame la posicion inicial en y de la esfera, en metros (Real)'
 
     read *, y0

       write(*,*) 'Dame el ángulo inicial de tiro de la esfera en grados (Real)'   

     read *, angulo   

       write(*,*) 'Dame la velocidad inicial de la esfera en m/s (Real) '   

     read *, v

       write(*,*) 'Dame la masa de la esfera en kilogramos (Real)'
 
     read *, m 

       write(*,*) 'Dame el radio de la esfera en metros (Real)'
 
      read *, radio  

       !Convirtiendo los grados  a radianes   

       alfa = angulo*pi/180.0  

       !Calculando el area de la esfera
       
       A = pi * (radio * radio)
       
       !Calculando la constante D
 
       D = ( cfe * airden * A) / 2



       ! Calculando las velocidades en los 2 ejes

       v0x=(v)*(cos(alfa))

       v0y=(v)*(sin(alfa))
     
       !Calculando las aceleraciones finales en el instante de tiempo
       
        t = (float (i)* 0.01)
        
        

       

       !open .dat file and start writing on it using the algorithm   

       open(1, file='tiro.dat')   

        x=0
        y=0 

       do i=1,100   

            !displacement of object in x and y direction   

           t = (float(i)*0.01)   

            x(i) = vx*t   

            y(i) = (vy*t) - (0.5*g*t*t)   

           !write output in file "proj.dat" for plotting   

            write(1,*) x(i), y(i)   

           !kill the loop when the object hits the ground   

            if (y(i)<0) exit   

       end do   

      close(1)   

      !close file 
ym = (vy**2)/(19.6)
xm = x(i)
if (vx<0) then
xm = 0
end if
!resultados en pantalla
write(*,*) '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'
write(*,*) 'Un proyectil con una velocidad inicial de',v,'m/s'
write(*,*) 'y con un ángulo de tiro de',a, 'grados'
write(*,*)  'Alcanzará una h máxima de',ym, 'metros'
write(*,*) 'Su alcanze horizontal sera de',xm, 'metros'
write(*,*) 'y durará en el aire un tiempo de',t,'segundos'

  end program tiro_parabolico 



!Subrutina para el calculo de la trayectoria
Subroutine Calculando_posicion (


!Subrutina para el calculo de la fuerza de arrastre

