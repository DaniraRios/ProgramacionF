PROGRAM Polar_to_Rectangular
!-----------------------------------------------------------------------
! This program accepts the polar coordinates of a point and displays
! the corresponding rectangular coordinates.  The internal subroutine 
! Convert_to_Rectangular is used to effect the conversion.  
! Variables used are:
!   RCoord, TCoord : polar coordinates of a point
!   XCoord, YCoord : rectangular coordinates of a point
!   Response       : user response to more-data question
!
! Input:  RCoord, TCoord, and Response
! Output: XCoord and YCoord
!-----------------------------------------------------------------------

  IMPLICIT NONE
  REAL :: RCoord, TCoord, XCoord, YCoord
  CHARACTER(1) :: Response

  ! Read and convert coordinates until user signals no more data
  DO
     WRITE (*, '(1X, A)', ADVANCE = "NO") &
           "Enter polar coordinates (in radians): "
     READ *, RCoord, TCoord
     CALL Convert_to_Rectangular(RCoord, TCoord, XCoord, YCoord)
     PRINT *, "Rectangular coordinates:", XCoord, YCoord  
     WRITE (*, '(/ 1X, A)', ADVANCE = "NO") &
           "More points to convert (Y or N)? "
     READ *, Response
     IF (Response /= "Y") EXIT
  END DO

CONTAINS

  !-Convert_to_Rectangular---------------------------------------------
  ! Subroutine to convert polar coordinates (R, Theta) to rectangular 
  ! coordinates (X, Y).
  !
  ! Accepts:  Polar coordinates R and Theta (in radians)
  ! Returns:  Rectangular coordinates X and Y
  !--------------------------------------------------------------------
  
  SUBROUTINE Convert_to_Rectangular(R, Theta, X, Y)
  
    REAL, INTENT(IN) :: R, Theta
    REAL, INTENT(OUT) :: X, Y
  
    X = R * COS(Theta)
    Y = R * SIN(Theta)
  
  END SUBROUTINE Convert_to_Rectangular
 
END PROGRAM Polar_to_Rectangular
