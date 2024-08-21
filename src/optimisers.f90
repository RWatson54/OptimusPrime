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

   ! Declare a type for various optimisation settings

   type :: optimisationSettings

      ! The simulation bounds

      real   (kind=WP) :: lBound = -one
      real   (kind=WP) :: uBound =  one

      real   (kind=WP) :: lBoundI = -one
      real   (kind=WP) :: uBoundI =  one

      ! Iteration counts

      integer(kind=WI) :: nInner  = 15
      integer(kind=WI) :: nOuter  = 50
      integer(kind=WI) :: nReheat = 25

      ! Key parameters

      real   (kind=WP) :: alpha = 0.99_wp

      ! Output control

      integer(kind=WI) :: nPrint = 10


   end type optimisationSettings

   ! Declare module variables

   ! Set everything private

   private

   ! Turn on what should be publicly visible

   public :: optimisationSettings

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
   !>  Key settings in the optimisation derived type for SA are:
   !>
   !>       lBoundI, uBoundI -- the upper and lower initialisation values
   !>       lBound, uBound -- the upper and lower allowable values
   !>       nInner, nOuter -- the number of inner and outer iterations
   !>       alpha -- the geometric annealing rate parameter
   !>       nPrint -- the number of iterations after which to print
   !>
   !>  @param[in]   fitnessFunc -- the function which tests the fitness
   !>  @param[in]   nDims       -- the number of input parameters
   !>  @param[in]   settings    -- the optimisation settings derived type
   !>  @param[out]  xOpt        -- the optimised values for return
   !>
   !*******************************************************************
   !*******************************************************************

   subroutine Optimise_SA(fitnessFunc, nDims, settings, xOpt)

      ! Use the relevant optimiser module

      use mod_optSA

      ! Turn off implicit typing

      implicit none

      ! Declare external variables

      procedure(optimiseFunction) :: fitnessFunc

      integer(kind=WI), intent(in) :: nDims

      type(optimisationSettings) :: settings

      real   (kind=WP), intent(out) :: xOpt(nDims)

      ! Unpack the options, and call the simulated annealing module

      call opt_runSA(fitnessFunc, nDims, &
                     settings%lBound, settings%uBound, &
                     settings%lBoundI, settings%uBoundI, &
                     settings%nInner, &
                     settings%nOuter, &
                     settings%nReheat, &
                     settings%alpha, &
                     settings%nPrint, &
                     xOpt)

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

