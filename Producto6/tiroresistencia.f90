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
      real :: angulo, alfa, m ,radio, D, A
      real :: x0, y0, v0x, v0y, v
      real, parameter :: deltat= 0.01
      real, parameter :: cfe = 0.47
      real, parameter :: airden = 1.225  
      real, parameter :: g = 9.81  
       

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



       !Descomponinedo la velocidad inicial

       v0x=(v)*(cos(alfa))

       v0y=(v)*(sin(alfa))
     
       !Calculando las aceleraciones iniciales
       
       ax = -(D/m) * (v0x * v0x)

       ay = -g - (D/m) * (v0y * v0y)
      
       
        
       !abriendo un archivo .dat para comenzar a registrar los datos iniciales  

         OPEN (1, FILE="friccion.dat")
         WRITE (1,300) x0, y0
         1001 FORMAT (f11.5,f11.5)
   

          

  







