!********************************************************************!
!********************************************************************!
!                                                                    !
!   mod_optES -- A module for Evolutionary Strategy optimisation     !
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
!>  This module applies an Evolutionary Strategy to optimise vectors
!>  of real numbers
!>  
!>
!*******************************************************************
!*******************************************************************

module mod_optES

   ! Declare modules

   use precision
   use mod_FunctionInterface
   use mod_Stochastic
   use mod_Rnkpar
   
   ! Turn off implicit typing

   implicit none

   ! Set everything private

   private

   ! Turn on what should be publicly visible

   public :: opt_runES

contains

   !*******************************************************************
   !*******************************************************************
   !
   !>  Subroutine opt_runES()
   !
   !>  @author
   !>  Rob Watson
   !>
   !>  @brief 
   !>  Run a n evolutionary strategy optimisation to optimise a real
   !>  vector input against a supplied fitness function.
   !>  
   !>  @param[in]  fitnessFunc -- the fitness function to be optimised
   !>  @param[in]  nDims -- the number of dimensions
   !>  @param[in]  lBound -- the lower allowed bound
   !>  @param[in]  uBound -- the upper allowed bound
   !>  @param[in]  lBoundI -- the lower bound used for initialisation
   !>  @param[in]  uBoundI -- the upper bound used for initialisation
   !>  @param[in]  nPrint -- the screen output frequency
   !>  @param[out] xOpt -- the returned values
   !>
   !*******************************************************************
   !*******************************************************************

   subroutine opt_runES(fitnessFunc, nDims, &
                        lBound, uBound, lBoundI, uBoundI, &
                        nPrint, &
                        xOpt)

      ! Turn off implicit typing

      implicit none

      ! Declare external variables

      procedure(optimiseFunction) :: fitnessFunc

      integer(kind=WI) :: nDims

      real   (kind=WP) :: lBound, uBound, lBoundI, uBoundI

      real   (kind=WP) :: xOpt(nDims)

      integer(kind=WI) :: nPrint

      ! Declare internal variables

      integer(kind=WI) :: ind(3)

      ! Declare loop variables and counters
      

      ! Seed the random variables
      
      call STOCHASTIC_SeedInit(1)

      ! Work out how many offspring are required

!!$      nLambda = nOffspring * nMu
!!$
!!$      ! Allocate memory to store things
!!$
!!$      allocate(muPopulation(nDims, nMu), muVariance(nDims, nMu), muPerformance(1, nMu))
!!$      allocate(lambdaPopulation(nDims, nLambda), lambdaVariance(nDims, nLambda), lambdaPerformance(1, nLambda))
!!$
!!$      ! Create an initial mu population and variance
!!$
!!$      do iMu = 1, nMu
!!$
!!$         do iDim = 1, nDims
!!$
!!$            muPopulation(iDim, iMu) = STOCHASTIC_Uniform(lBoundI, uBoundI)
!!$            muVariance(iDim, iMu) = one
!!$
!!$         end do
!!$
!!$      end do
!!$
!!$      ! Create a lambda population by selection (and "recombination" [ = 0 ]) from the mu population
!!$
!!$      do iLambda = 1, nLambda
!!$
!!$         iMu = STOCHASTIC_Integer(1, nMu)
!!$         
!!$         lambdaPopulation(:, iLambda) = muPopulation(:,iMu)
!!$         lambdaVariance(:, iLambda) = muVariance(:, iMu)
!!$
!!$      end do
!!$
!!$      ! Mutate the variance of the lambda population according to the classic rules
!!$
!!$      do iLambda = 1, nLambda
!!$
!!$         pN = STOCHASTIC_Normal(zero, one)
!!$
!!$         do iDim = 1, nDims
!!$
!!$            pNi = STOCHASTIC_Normal(zero, one)
!!$
!!$            lambdaVariance(iDim, iLambda) = lambdaVariance(iDim, iLambda) * exp(pN - pNi)
!!$
!!$         end do
!!$
!!$      end do
!!$
!!$      ! Mutate the decision variables based on the lambda population
!!$
!!$      do iLambda = 1, nLambda
!!$
!!$         do iDim = 1, nDims
!!$
!!$            lambdaPopulation(iDim, iLambda) = lambdaPopulation(iDim, iLambda) + STOCHASTIC_Normal(zero, lambdaVariance(iDim, iLambda))
!!$
!!$         end do
!!$
!!$      end do
!!$
!!$      ! Evaluate the mu and lambda populations
!!$
!!$      do iMu = 1, nMu
!!$
!!$         muPerformance(1, iMu) = evaluateFunction(fitnessFunc, muPopulation(:, iMu))
!!$
!!$      end do
!!$
!!$      do iLambda = 1, nLambda
!!$
!!$         lambdaPerformance(1, iLambda) = evaluateFunction(fitnessFunc, lambdaPopulation(:, iLambda))
!!$
!!$      end do

      ! And choose the best of

      call rnkpar([-1.0_wp, -3.0_wp, 5.0_wp, 1.0_wp, -7.0_wp], ind, 3)

      write(6,*) ind

      ! And output the best performer

!!$      xOpt = BestIndividual(:,1)

   end subroutine opt_runES



end module mod_optES

!********************************************************************!
!********************************************************************!
!                                                                    !
!   End of module mod_optES                                          !
!                                                                    !
!********************************************************************!
!********************************************************************!





