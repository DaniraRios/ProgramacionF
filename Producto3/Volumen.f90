Program Volumen_altura 

   Implicit None 

   Real *8 :: radio , vol , altura 

  Real *8 :: PI = 4.0 * atan(1.0) 

   Integer :: model_n = 2 

   print * , 'Enter a radius:'
read * , radio 
print * , 'Enter a height:'

    
read * , altura

   vol = (PI*(altura*altura))*(radio-(altura/3))

  print * , 'Program number =' , model_n 

  print * , 'Radius =' , radio 
 print * , 'Height=' , altura
   
  print * , 'Volumen =' , vol 

 End Program Volumen_altura
