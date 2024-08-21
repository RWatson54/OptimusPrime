!********************************************************************!
!********************************************************************!
!                                                                    !
!   mod_FunctionInterface -- Interface routines                      !
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
!>  This module contains the abstract interfaces which allow the 
!>  optimisation functions to be set separately and passed into the
!>  optimiser by function name.
!>
!>  It's expecting the optimisation function to return a single 
!>  value which can be analysed from a real-valued (or double 
!>  valued) vector.
!>
!*******************************************************************
!*******************************************************************

module mod_FunctionInterface

   ! Declare modules

   use precision

   ! Turn off implicit typing

   implicit none

   ! Declare the interface to allow procedures of this type to be passed

   abstract interface

      function optimiseFunction(z)

         ! Seem to have to use this here, rather than it being inherited

         use precision

         ! Set the argument list for this type of function

         real(kind=WP) :: optimiseFunction
         real(kind=WP), intent (in) :: z(:)

      end function optimiseFunction

   end interface

contains

   !*******************************************************************
   !*******************************************************************
   !
   !>  Function evaluateFunction()
   !
   !>  @author
   !>  Rob Watson
   !>
   !>  @brief 
   !>  This is a function which, when called with a target function
   !>  as its first argument, evaluates that function based on the 
   !>  input vector, x, and returns it as a real (or double).
   !>  
   !>  @param[in]  funcIn -- the function to be evaluated
   !>  @param[in]  x -- the real- or double- valued vector for evaluation
   !>
   !*******************************************************************
   !*******************************************************************

   function evaluateFunction(funcIn, x)

      ! Turn off implicit typing

      implicit none

      ! Declare external variables

      procedure(optimiseFunction) :: funcIn

      real   (kind=WP), intent(in) :: x(:)

      real   (kind=WP) :: evaluateFunction

      ! And evaluate the passed function, funcIn

      evaluateFunction = funcIn(x)

      ! Return to calling program

      return

   end function evaluateFunction

   !*******************************************************************
   !*******************************************************************

end module mod_FunctionInterface

!********************************************************************!
!********************************************************************!
!                                                                    !
!   End of module mod_FunctionInterface                              !
!                                                                    !
!********************************************************************!
!********************************************************************!

