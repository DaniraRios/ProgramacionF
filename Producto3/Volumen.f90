Program esferic_volumen 

   Implicit None 

   Real *8 :: radius , volumen , height 

  Real *8 :: PI = 4.0 * atan(1.0) 

   Integer :: model_n = 1 

   print * , 'Enter a radius:' 
print * , 'Enter a height:'

   read * , radius 
read * , height

   volumen = PI * height * 2 *((radius-height)/3)

  print * , 'Program number =' , model_n 

  print * , 'Radius =' , radius 

   
  print * , 'Volumen =' , volumen 

 End Program esferic_volumen
