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

   integer(kind=WI), parameter :: nDims = 2

   ! Set everything private

   private

   ! Turn on what should be publicly visible

   public :: nDims

   public :: quadratic, polycos, ackley, beale, rosenbrock

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

   !*******************************************************************
   !*******************************************************************
   !
   !>  Function polycos()
   !
   !>  @author
   !>  Rob Watson
   !>
   !>  @brief 
   !>  This is a simple demonstration --- a quadratic multiplied by a 
   !>  cosine
   !>  
   !>  @param[in]  x -- the input variables
   !>  @param[out] f -- the output variables
   !>
   !*******************************************************************
   !*******************************************************************

   function polycos(x) result(f)

      implicit none

      ! Declare external variables

      real   (kind=WP), intent(in) :: x(:)
      real   (kind=WP) :: f

      ! And calculate the function
      
      f = (x(1)**2 + 2*x(2)**2 - 4) * cos(x(1)) * cos(x(2))

      ! Return to calling program

      return

   end function polycos

   !*******************************************************************
   !*******************************************************************

   !*******************************************************************
   !*******************************************************************
   !
   !>  Function Ackley()
   !
   !>  @author
   !>  Rob Watson
   !>
   !>  @brief 
   !>  This is a simple demonstration --- the Ackley function
   !>  
   !>  @param[in]  x -- the input variables
   !>  @param[out] f -- the output variables
   !>
   !*******************************************************************
   !*******************************************************************

   function ackley(x) result(f)

      implicit none

      ! Declare external variables

      real   (kind=WP), intent(in) :: x(:)
      real   (kind=WP) :: f

      ! And calculate the function
      
      f = -20.0_wp * exp(-0.2_wp * sqrt(0.5_wp * (x(1)**2 + x(2)**2))) &
                   - exp(0.5_wp * (cos(2*pi*x(1)) + cos(2*pi*x(2)))) &
                   + exp(one) + 20.0_wp

      ! Return to calling program

      return

   end function ackley

   !*******************************************************************
   !*******************************************************************


   !*******************************************************************
   !*******************************************************************
   !
   !>  Function Beale()
   !
   !>  @author
   !>  Rob Watson
   !>
   !>  @brief 
   !>  This is a simple demonstration --- the Beale function
   !>  
   !>  @param[in]  x -- the input variables
   !>  @param[out] f -- the output variables
   !>
   !*******************************************************************
   !*******************************************************************

   function beale(x) result(f)

      implicit none

      ! Declare external variables

      real   (kind=WP), intent(in) :: x(:)
      real   (kind=WP) :: f

      ! And calculate the function
      
      f = (1.5_wp - x(1) + x(1)*x(2))**2 &
        + (2.25_wp - x(1) + x(1)*x(2)*x(2))**2 &
        + (2.625_wp - x(1) + x(1)*x(2)*x(2)*x(2))**2

      ! Return to calling program

      return

   end function beale

   !*******************************************************************
   !*******************************************************************

   !*******************************************************************
   !*******************************************************************
   !
   !>  Function Rosenbrock()
   !
   !>  @author
   !>  Rob Watson
   !>
   !>  @brief 
   !>  This is a simple demonstration --- the Rosenbrock function
   !>  
   !>  @param[in]  x -- the input variables
   !>  @param[out] f -- the output variables
   !>
   !*******************************************************************
   !*******************************************************************

   function rosenbrock(x) result(f)

      implicit none

      ! Declare external variables

      real   (kind=WP), intent(in) :: x(:)
      real   (kind=WP) :: f

      ! Declare internal variables

      integer(kind=WI) :: i

      ! And calculate the function
      
      f = zero

      do i = 1, 1

         f = f + 100 * (x(i+1) - x(i)**2)**2 + (one - x(i))**2

      end do
         
      ! Return to calling program

      return

   end function rosenbrock

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

