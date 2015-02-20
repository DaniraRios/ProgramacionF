 ! Function . f90 : Program calls a simple function

 ! −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−

 Real *8  Function f (x,y)

   Implicit None

   Real *8 :: x, y

   f = 1.0 + sin(x*y)

 End Function f



 Program Main

  Implicit None

  Real *8 :: Xin =0.25 , Yin =2. , c , f ! declarations ( also f)

  c = f ( Xin , Yin )

 write ( * , * ) 'f(Xin, Yin) =' , c

 End Program Main 
