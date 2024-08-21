!********************************************************************!
!********************************************************************!
!                                                                    !
!   mod_Optimisers -- A wrapper for various optimisation heuristics  !
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
!>  This module is essentially a wrapper pointing to the other 
!>  modules which contain the actual code for the optimisers (in
!>  theory, this is for clarity, so you're not scrolling through
!>  PSO when you're trying to find GA, or whatever).
!>
!*******************************************************************
!*******************************************************************

module mod_Optimisers

   ! Declare modules

   use precision
   use mod_FunctionInterface

   ! Turn off implicit typing

   implicit none

   ! Declare module variables

   ! Set everything private

   private

   ! Turn on what should be publicly visible

   public :: Optimise_SA

contains

   !*******************************************************************
   !*******************************************************************
   !
   !>  Subroutine Optimise_SA()
   !
   !>  @author
   !>  Rob Watson
   !>
   !>  @brief 
   !>  A fairly useless optimiser from a population-based viewpoint,
   !>  but might be quite good for serial testing. This is some variant
   !>  of simulated annealing. 
   !>  
   !>  @param[in]   fitnessFunc -- the function which tests the fitness
   !>  @param[in]   nDims       -- the number of input parameters
   !>  @param[out]  xOpt        -- the optimised values for return
   !>
   !*******************************************************************
   !*******************************************************************

   subroutine Optimise_SA(fitnessFunc, nDims, xOpt)

      ! Use the relevant optimiser module

      use mod_optSA

      ! Turn off implicit typing

      implicit none

      ! Declare external variables

      procedure(optimiseFunction) :: fitnessFunc

      integer(kind=WI), intent(in) :: nDims

      real   (kind=WP), intent(out) :: xOpt(nDims)

      ! Declare internal variables

      ! Unpack the options, and call the simulated annealing module

      call opt_runSA(fitnessFunc, nDims, -one, one, xOpt)

      ! Return to calling program

      return

   end subroutine Optimise_SA

   !*******************************************************************
   !*******************************************************************


end module mod_Optimisers

!********************************************************************!
!********************************************************************!
!                                                                    !
!   End of module mod_Optimisers                                     !
!                                                                    !
!********************************************************************!
!********************************************************************!

