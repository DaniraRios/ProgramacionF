! Subroutine . f90 : Demonstrates the call for a simple subroutine

 ! −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−

 Subroutine g(x, y, ans1 , ans2 )

   Implicit None

   Real (8) :: x , y , ans1 , ans2 ! Declare variables

   ans1 = sin (x*y) + 1. ! Use sine intrinsic func.

   ans2 = ans1**2

 End Subroutine g

 !

 Program Main  

   Implicit None

   Real *8 :: Xin =0.25 , Yin =2.0 , Gout1 , Gout2

   call g( Xin , Yin , Gout1 , Gout2 ) ! Call the subr g

   write ( * , *) 'The answers are:' , Gout1 , Gout2

 End Program Main 
