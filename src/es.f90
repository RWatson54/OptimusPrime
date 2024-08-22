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
   !>  @param[in]  nGen -- the number of generations to run for
   !>  @param[in]  nMu -- the population size
   !>  @param[in]  nOffspring -- the offspring generated for each population member
   !>  @param[in]  nPrint -- the screen output frequency
   !>  @param[out] xOpt -- the returned values
   !>
   !*******************************************************************
   !*******************************************************************

   subroutine opt_runES(fitnessFunc, nDims, &
                        lBound, uBound, lBoundI, uBoundI, &
                        nMu, nGen, nOffspring, &
                        nPrint, &
                        xOpt)

      ! Turn off implicit typing

      implicit none

      ! Declare external variables

      procedure(optimiseFunction) :: fitnessFunc

      integer(kind=WI) :: nDims

      real   (kind=WP) :: lBound, uBound, lBoundI, uBoundI

      real   (kind=WP) :: xOpt(nDims)

      integer(kind=WI) :: nGen, nMu, nOffspring

      integer(kind=WI) :: nPrint

      ! Declare internal variables

      real   (kind=WP), allocatable :: muPopulation(:,:), muVariance(:,:), muPerformance(:,:)

      real   (kind=WP), allocatable :: lambdaPopulation(:,:), lambdaVariance(:,:), lambdaPerformance(:,:)

      real   (kind=WP), allocatable :: totalPerformance(:)

      real   (kind=WP), allocatable :: bestIndividual(:,:)

      integer(kind=WI), allocatable :: indexPerformance(:)

      real   (kind=WP) :: pN, pNi

      real   (kind=WP) :: MinErr

      integer(kind=WI) :: MinELc(1)

      ! Declare loop variables and counters

      integer(kind=WI) :: nLambda

      integer(kind=WI) :: iMu, iLambda, iDim, iGen

      ! Seed the random variables

      call STOCHASTIC_SeedInit(1)

      ! Work out how many offspring are required

      nLambda = nOffspring * nMu

      ! Allocate memory to store things

      allocate(muPopulation(nDims, nMu), muVariance(nDims, nMu), muPerformance(1, nMu))
      allocate(lambdaPopulation(nDims, nLambda), lambdaVariance(nDims, nLambda), lambdaPerformance(1, nLambda))

      allocate(totalPerformance(nLambda+nMu))

      allocate(indexPerformance(nMu))

      allocate(bestIndividual(nDims, 1))

      ! Create an initial mu population and variance

      do iMu = 1, nMu

         do iDim = 1, nDims

            muPopulation(iDim, iMu) = STOCHASTIC_Uniform(lBoundI, uBoundI)
            muVariance(iDim, iMu) = STOCHASTIC_Uniform(lBoundI, uBoundI)

         end do

      end do

      ! Evaluate this initial population

      do iMu = 1, nMu
         
         muPerformance(1, iMu) = evaluateFunction(fitnessFunc, muPopulation(:, iMu))
         
      end do

      ! Find the best performer in the initial population

      MinErr = minval(muPerformance(1,:))
      MinELc = minloc(muPerformance(1,:))
      
      bestIndividual(:,1) = muPopulation(:, MinElc(1))

      ! Report initial performance

      write(6,*) ' *** '
      write(6,*) ' *** Initial Performance: ', MinErr, sum(muPerformance) / real(nMu,WP)
      write(6,*) ' *** '

      ! Main generation loop

      do iGen = 1, nGen

         ! Create a lambda population by selection (and "recombination" [ = 0 ]) from the mu population

         do iLambda = 1, nLambda

            iMu = STOCHASTIC_Integer(1, nMu)

            lambdaPopulation(:, iLambda) = muPopulation(:,iMu)
            lambdaVariance(:, iLambda) = muVariance(:, iMu)

         end do

         ! Mutate the variance of the lambda population according to the classic rules

         do iLambda = 1, nLambda

            pN = STOCHASTIC_Normal(zero, one)

            do iDim = 1, nDims

               pNi = STOCHASTIC_Normal(zero, one)

               lambdaVariance(iDim, iLambda) = lambdaVariance(iDim, iLambda) * exp(pN - pNi)

            end do

         end do

         ! Mutate the decision variables based on the lambda population

         do iLambda = 1, nLambda

            do iDim = 1, nDims

               lambdaPopulation(iDim, iLambda) = lambdaPopulation(iDim, iLambda) + STOCHASTIC_Normal(zero, lambdaVariance(iDim, iLambda))

               lambdaPopulation(iDim, iLambda) = max(lBound, lambdaPopulation(iDim, iLambda))
               lambdaPopulation(iDim, iLambda) = min(uBound, lambdaPopulation(iDim, iLambda))

            end do

         end do

         ! Evaluate the lambda populations (mu population performance is already known)

         do iLambda = 1, nLambda

            lambdaPerformance(1, iLambda) = evaluateFunction(fitnessFunc, lambdaPopulation(:, iLambda))

         end do

         ! Assemble performance into lambda+mu

         totalPerformance = [lambdaPerformance(1,:), muPerformance(1,:)]

         ! Rank total performance from smallest to largest to get the nMu best in both populations

         call rnkpar(totalPerformance, indexPerformance, nMu)

         ! And move the best performers from both populations into a new mu generation

         do iMu = 1, nMu

            if ( indexPerformance(iMu) .le. nLambda ) then

               muPopulation(:, iMu) = lambdaPopulation(:, indexPerformance(iMu))
               muVariance(:, iMu) = lambdaVariance(:, indexPerformance(iMu))
               muPerformance(1, iMu) = lambdaPerformance(1, indexPerformance(iMu))

            else

               muPopulation(:, iMu) = muPopulation(:, indexPerformance(iMu) - nLambda)
               muVariance(:, iMu) = muVariance(:, indexPerformance(iMu) - nLambda)
               muPerformance(1, iMu) = muPerformance(1, indexPerformance(iMu) - nLambda)

            end if

         end do

         ! And locate the best performer in this generation

         MinErr = muPerformance(1,1)
         bestIndividual(:,1) = muPopulation(:,1)

         ! Report the performance of the population

         if ( mod(iGen, nPrint) .eq. 0 ) then

            write(6,*) ' *** Performance: ', iGen, MinErr, sum(muPerformance) / real(nMu,WP)

         end if

      end do

      xOpt = bestIndividual(:,1)

      ! Tidy up by deallocating memory

      deallocate(muPopulation, muVariance, muPerformance)
      deallocate(lambdaPopulation, lambdaVariance, lambdaPerformance)

      deallocate(totalPerformance)

      deallocate(indexPerformance)

      deallocate(bestIndividual)

   end subroutine opt_runES



end module mod_optES

!********************************************************************!
!********************************************************************!
!                                                                    !
!   End of module mod_optES                                          !
!                                                                    !
!********************************************************************!
!********************************************************************!





