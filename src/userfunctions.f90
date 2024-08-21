!********************************************************************!
!********************************************************************!
!                                                                    !
!   mod_UserFunctions -- User-defined target functions               !
!                                                                    !
!********************************************************************!
!                                                                    !
!   Version history:                                                 !
!                    Module created: 21Aug24                 - raw54 !
!                                                                    !
!********************************************************************!
!                                                                    !
!   Known issues:                                                    !
!                    No known issues.                        - raw54 !
!                                                                    !
!********************************************************************!
!********************************************************************!
!
!>  Doxygen Section:
!
!>  @author
!>  Rob Watson
!>
!>  @brief 
!>  This module contains the user-defined target functions, which
!>  the optimisers will attempt to, well, optimise. In this case, 
!>  the optimisers are set up to minimise the problems, for ease.
!>  
!>  It's also convenient here to set up the size of the problem, the
!>  number of variables that the function to be minimised is expecting
!>  to receive
!>
!*******************************************************************
!*******************************************************************

module mod_UserFunctions

   ! Declare modules

   use precision

   ! Turn off implicit typing

   implicit none

   ! Provide a variable which determines the size of the problem

   integer(kind=WI) :: nDims = 2

   ! Set everything private

   private

   ! Turn on what should be publicly visible

   public :: nDims

   public :: quadratic

contains

   !*******************************************************************
   !*******************************************************************
   !
   !>  Function quadratic()
   !
   !>  @author
   !>  Rob Watson
   !>
   !>  @brief 
   !>  This is a simple demonstration --- a quadratic function to be
   !>  minimised
   !>  
   !>  @param[in]  x -- the input variables
   !>  @param[out] f -- the output variables
   !>
   !*******************************************************************
   !*******************************************************************

   function quadratic(x) result(f)

      implicit none

      ! Declare external variables

      real   (kind=WP), intent(in) :: x(:)
      real   (kind=WP) :: f

      ! And calculate the function
      
      f = x(1)**2 + 2*x(2)**2 - 4

      ! Return to calling program

      return

   end function quadratic

   !*******************************************************************
   !*******************************************************************

end module mod_UserFunctions

!********************************************************************!
!********************************************************************!
!                                                                    !
!   End of module mod_UserFunctions                                  !
!                                                                    !
!********************************************************************!
!********************************************************************!

