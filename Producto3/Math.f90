 ! Math . f90 : demo some Fortran math functions

 ! −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−

 Program Math_test ! Begin main program

  Real *8 :: i, p, x = -1.0 , y = 0, z = 2.0, w ! Declare variables x, y, z

 
   i = SQRT (x) ! Call the sine function

   p = LOG (y)  ! Call the exponential function
   w= ASIN (z)

  print * , i, p, w ! Print x, y, z

 End Program Math_test ! End main program 
