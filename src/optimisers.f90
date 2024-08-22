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

      integer(kind=WI) :: nGen = 10

      ! Population sizes

      integer(kind=WI) :: nPop = 100

      ! Key parameters

      real   (kind=WP) :: alpha = 0.99_wp

      real   (kind=WP) :: pCrossover = 0.75_wp

      real   (kind=WP) :: pMutate1 = 0.075_wp
      real   (kind=WP) :: pMutate2 = 0.025_wp
      real   (kind=WP) :: pMutate3 = 0.10_wp

      integer(kind=WI) :: kTourney = 2
      integer(kind=WI) :: kCross = 1

      integer(kind=WI) :: nOffspring = 7

      ! Output control

      integer(kind=WI) :: nPrint = 10


   end type optimisationSettings

   ! Declare module variables

   ! Set everything private

   private

   ! Turn on what should be publicly visible

   public :: optimisationSettings

   public :: Optimise_SA, Optimise_GA, Optimise_ES

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


   !*******************************************************************
   !*******************************************************************
   !
   !>  Subroutine Optimise_GA()
   !
   !>  @author
   !>  Rob Watson
   !>
   !>  @brief 
   !>  Basic genetic algorithm, operating on real strings. Baseline 
   !>  k-point discrete recombination is built in to this version, and
   !>  k-way tournament selection. Two mutation operators are allowed,
   !>  drift by a small value, and total loss
   !>  
   !>  Key settings in the optimisation derived type for GA are:
   !>
   !>       lBoundI, uBoundI -- the upper and lower initialisation values
   !>       lBound, uBound -- the upper and lower allowable values
   !>       nPop -- the population size
   !>       nGen -- the number of generations
   !>       pCrossover -- the crossover probability
   !>       pMutate1, pMutate2 -- the drift and total loss probabilities
   !>       nPrint -- the number of iterations after which to print
   !>
   !>  @param[in]   fitnessFunc -- the function which tests the fitness
   !>  @param[in]   nDims       -- the number of input parameters
   !>  @param[in]   settings    -- the optimisation settings derived type
   !>  @param[out]  xOpt        -- the optimised values for return
   !>
   !*******************************************************************
   !*******************************************************************

   subroutine Optimise_GA(fitnessFunc, nDims, settings, xOpt)

      ! Use the relevant optimiser module

      use mod_optGA

      ! Turn off implicit typing

      implicit none

      ! Declare external variables

      procedure(optimiseFunction) :: fitnessFunc

      integer(kind=WI), intent(in) :: nDims

      type(optimisationSettings) :: settings

      real   (kind=WP), intent(out) :: xOpt(nDims)

      ! Unpack the options, and call the simulated annealing module

      call opt_runGA(fitnessFunc, nDims, &
                     settings%lBound, settings%uBound, &
                     settings%lBoundI, settings%uBoundI, &
                     settings%nPop, &
                     settings%nGen, &
                     settings%pCrossover, &
                     settings%pMutate1, settings%pMutate2, settings%pMutate3, &
                     settings%kTourney, settings%kCross, &
                     settings%nPrint, &
                     xOpt)

      ! Return to calling program

      return

   end subroutine Optimise_GA

   !*******************************************************************
   !*******************************************************************

   !*******************************************************************
   !*******************************************************************
   !
   !>  Subroutine Optimise_ES()
   !
   !>  @author
   !>  Rob Watson
   !>
   !>  @brief 
   !>  Basic evolutionary strategy, of the (mu+lambda) type. No 
   !>  recombination based on this. 
   !>
   !>  Key settings in the optimisation derived type for ES are:
   !>
   !>       lBoundI, uBoundI -- the upper and lower initialisation values
   !>       lBound, uBound -- the upper and lower allowable values
   !>
   !>       nPrint -- the number of iterations after which to print
   !>
   !>  @param[in]   fitnessFunc -- the function which tests the fitness
   !>  @param[in]   nDims       -- the number of input parameters
   !>  @param[in]   settings    -- the optimisation settings derived type
   !>  @param[out]  xOpt        -- the optimised values for return
   !>
   !*******************************************************************
   !*******************************************************************

   subroutine Optimise_ES(fitnessFunc, nDims, settings, xOpt)

      ! Use the relevant optimiser module

      use mod_optES

      ! Turn off implicit typing

      implicit none

      ! Declare external variables

      procedure(optimiseFunction) :: fitnessFunc

      integer(kind=WI), intent(in) :: nDims

      type(optimisationSettings) :: settings

      real   (kind=WP), intent(out) :: xOpt(nDims)

      ! Unpack the options, and call the simulated annealing module

      call opt_runES(fitnessFunc, nDims, &
                     settings%lBound, settings%uBound, &
                     settings%lBoundI, settings%uBoundI, &
                     settings%nPop, settings%nGen, settings%nOffspring, &                     
                     settings%nPrint, &
                     xOpt)

      ! Return to calling program

      return

   end subroutine Optimise_ES

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

