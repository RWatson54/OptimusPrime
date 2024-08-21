!********************************************************************!
!********************************************************************!
!                                                                    !
!   mod_optGA -- A module for genetic algorithm optimisation         !
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
!>  This module does genetic algorithm stuff for vectors of real 
!>  numbers
!>  
!>
!*******************************************************************
!*******************************************************************

module mod_optGA

   ! Declare modules

   use precision
   use mod_FunctionInterface
   use mod_Stochastic

   ! Turn off implicit typing

   implicit none

   ! Set everything private

   private

   ! Turn on what should be publicly visible

   public :: opt_runGA

contains

   !*******************************************************************
   !*******************************************************************
   !
   !>  Subroutine opt_runGA()
   !
   !>  @author
   !>  Rob Watson
   !>
   !>  @brief 
   !>  Run a genetic algorithm to optimise a real vector input against 
   !>  a supplied fitness function.
   !>  
   !>  @param[in]  fitnessFunc -- the fitness function to be optimised
   !>  @param[in]  nDims -- the number of dimensions
   !>  @param[in]  lBound -- the lower allowed bound
   !>  @param[in]  uBound -- the upper allowed bound
   !>  @param[in]  lBoundI -- the lower bound used for initialisation
   !>  @param[in]  uBoundI -- the upper bound used for initialisation
   !>  @param[in]  nPop -- the population size
   !>  @param[in]  nGen -- the number of generations
   !>  @param[in]  pCrossover -- the crossover probability
   !>  @param[in]  pMutate1 -- the drift mutation probability
   !>  @param[in]  pMutate2 -- the point loss mutation probability
   !>  @param[in]  pMutate3 -- the total loss mutation probability
   !>  @param[in]  nPrint -- the screen output frequency
   !>  @param[out] xOpt -- the returned values
   !>
   !*******************************************************************
   !*******************************************************************

   subroutine opt_runGA(fitnessFunc, nDims, &
                        lBound, uBound, lBoundI, uBoundI, &
                        nPop, nGen, &
                        pCrossover, pMutate1, pMutate2, pMutate3, &
                        nPrint, &
                        xOpt)

      ! Turn off implicit typing

      implicit none

      ! Declare external variables

      procedure(optimiseFunction) :: fitnessFunc

      integer(kind=WI) :: nDims

      real   (kind=WP) :: lBound, uBound, lBoundI, uBoundI

      real   (kind=WP) :: xOpt(nDims)

      integer(kind=WI) :: nPop, nGen, nPrint

      real   (kind=WP) :: pCrossover, pMutate1, pMutate2, pMutate3

      integer(kind=WI) :: kTourney = 3

      integer(kind=WI) :: kCross = 2

      ! Declare internal variables

      real   (kind=WP), allocatable :: Population(:,:), PopulationNew(:,:)

      real   (kind=WP), allocatable :: PopulationPerformance(:,:)

      real   (kind=WP), allocatable :: BestIndividual(:,:)

      integer(kind=WI), allocatable :: Parent(:,:)

      integer(kind=WI) :: MinElc(1)

      real   (kind=WP) :: MinErr

      ! Declare loop variables and counters
      
      integer(kind=WI) :: iDim, iPop, iGen, iPar

      integer(kind=WI) :: lPar, rPar, lOff, rOff

      ! Seed the random variables
      
      call STOCHASTIC_SeedInit(1)

      ! Allocate memory to store things

      allocate(Population(nDims, nPop))
      allocate(PopulationNew(nDims, nPop))

      allocate(PopulationPerformance(1, nPop))

      allocate(BestIndividual(nDims, 1))

      allocate(Parent(2, nPop/2))

      ! Create an initial population

      do iPop = 1, nPop

         do iDim = 1, nDims

            Population(iDim, iPop) = STOCHASTIC_Uniform(lBoundI, uBoundI)

         end do

      end do

      ! Evaluate the initial population

      do iPop = 1, nPop

         PopulationPerformance(1, iPop) = evaluateFunction(fitnessFunc, Population(:, iPop))

      end do

      ! Find the best performer in the initial population

      MinErr = minval(PopulationPerformance(1,:))
      MinELc = minloc(PopulationPerformance(1,:))
      
      BestIndividual(:,1) = Population(:, MinElc(1))

      ! Report initial performance

      write(6,*) ' *** '
      write(6,*) ' *** Initial Performance: ', MinErr, sum(PopulationPerformance) / real(nPop,WP)
      write(6,*) ' *** '

      ! Begin the main generation loop

      do iGen = 1, nGen

         ! Generate the parents based on k-way tournament selection

         do iPar = 1, nPop/2

            Parent(1,iPar) = getTournamentParent(PopulationPerformance, kTourney)
            Parent(2,iPar) = getTournamentParent(PopulationPerformance, kTourney)

         end do

         ! Perform crossover (discrete, k-point crossover)

         do iPar = 1, nPop/2

            ! Two parents for each

            lPar = Parent(1, iPar)
            rPar = Parent(2, iPar)

            lOff = 2*iPar - 1
            rOff = 2*iPar

            ! And cross them over
            
            if ( STOCHASTIC_Uniform(zero,one) .lt. pCrossover ) then

               call crossover(kCross, &
                              Population(:, lPar), Population(:, rPar), &
                              PopulationNew(:, lOff), PopulationNew(:, rOff))

            end if

         end do

         ! Mutate with a small drift

         do iPop = 1, nPop

            do iDim = 1, nDims

               if ( STOCHASTIC_Uniform(zero,one) .lt. pMutate1 ) then

                  PopulationNew(iDim,iPop) = PopulationNew(iDim,iPop) + STOCHASTIC_Normal(zero,half)

                  PopulationNew(iDim,iPop) = min(PopulationNew(iDim,iPop), uBound)
                  PopulationNew(iDim,iPop) = max(PopulationNew(iDim,iPop), lBound)

               end if
              
            end do

         end do

         ! Mutate with a point loss

         do iPop = 1, nPop

            do iDim = 1, nDims

               if ( STOCHASTIC_Uniform(zero,one) .lt. pMutate2 ) then

                  PopulationNew(iDim,iPop) = STOCHASTIC_Uniform(lBound, uBound)

               end if
              
            end do

         end do

         ! Mutate with a total loss

         do iPop = 1, nPop

            if ( STOCHASTIC_Uniform(zero,one) .lt. pMutate3 ) then

               do iDim = 1, nDims

                  PopulationNew(iDim,iPop) = STOCHASTIC_Uniform(lBound, uBound)

               end do
              
            end if

         end do

         ! Practice elitism by moving the best performer somewhere into the output generation

         PopulationNew(:, STOCHASTIC_Integer(1, nPop)) = BestIndividual(:, 1)

         ! And replace the old population with the new

         Population = PopulationNew

         ! Evaluate the whole population

         do iPop = 1, nPop

            PopulationPerformance(1, iPop) = evaluateFunction(fitnessFunc, Population(:, iPop))

         end do

         ! Find the best performer in the previous generation

         MinErr = minval(PopulationPerformance(1,:))
         MinELc = minloc(PopulationPerformance(1,:))
         
         BestIndividual(:,1) = Population(:, MinElc(1))
         
         ! Report the performance of the population

         write(6,*) ' *** Performance: ', iGen, MinErr, sum(PopulationPerformance) / real(nPop,WP)

      end do

      ! And output the best performer

      xOpt = BestIndividual(:,1)


   end subroutine opt_runGA

   !*******************************************************************
   !*******************************************************************
   !
   !>  Function getTournamentParent()
   !
   !>  @author
   !>  Rob Watson
   !>
   !>  @brief 
   !>  Select a parent via k-way tournament selection for GA 
   !>  
   !>  @param[in] PopulationPerformance -- the performance of the whole pop
   !>  @param[in]  kTourney -- the number of tournament entrants
   !>  @param[out] Parent -- the parent index
   !>
   !*******************************************************************
   !*******************************************************************

   function getTournamentParent(PopulationPerformance, kTourney) result(Parent)

      implicit none

      ! Declare external variables

      real   (kind=WP), intent(in) :: PopulationPerformance(:,:)

      integer(kind=WI), intent(in) :: kTourney

      integer(kind=WI) :: Parent

      ! Declare internal variables

      integer(kind=WI) :: nPop

      integer(kind=WI) :: iT, iTourney(kTourney), minElcT(1)

      real   (kind=WP) :: fTourney(kTourney)

      ! Find the size of the population to avoid passing

      nPop = size(PopulationPerformance,2)

      ! Randomly select the members for the tournament

      do iT = 1, kTourney

         iTourney(iT) = STOCHASTIC_Integer(1, nPop)
         fTourney(iT) = PopulationPerformance(1,iTourney(iT))

      end do

      ! Run the tournament for parent

      minElcT = minloc(fTourney)

      Parent = iTourney(minElcT(1))

      ! Return to calling program

      return

   end function getTournamentParent

   !*******************************************************************
   !*******************************************************************


   !*******************************************************************
   !*******************************************************************
   !
   !>  Subroutine crossover()
   !
   !>  @author
   !>  Rob Watson
   !>
   !>  @brief 
   !>  Perform k-point discrete crossover on two real vectors
   !>  
   !>  
   !>  @param[in]  kCross -- number of crossovers
   !>  @param[in]  par1 -- Parent 1 genome
   !>  @param[in]  par2 -- Parent 1 genome
   !>  @param[out]  off1 -- Offspring 1 genome
   !>  @param[out]  off2 -- Offspring 2 genome
   !>
   !*******************************************************************
   !*******************************************************************

   subroutine crossover(kCross, par1, par2, off1, off2)

      implicit none

      ! Declare external variables

      integer(kind=WI), intent(in) :: kCross

      real   (kind=WP), intent(in) :: par1(:), par2(:)

      real   (kind=WP), intent(out) :: off1(size(par1,1)), off2(size(par2,1))

      ! Declare internal variables

      integer(kind=WI) :: nPop

      real   (kind=WP) :: bothParents(size(par1,1), 2)

      integer(kind=WI) :: CPt(kCross+2)

      integer(kind=WI) :: iC

      ! Set the size for convenience

      nPop = size(par1, 1)

      ! Set the crossover points
      
      CPt(1) = 1
      CPt(2:kCross+1) = STOCHASTIC_IntegerD(1, nPop, kCross)
      CPt(kCross+2) = nPop

      ! Put both parents into an array for switching ease

      bothParents(:,1) = par1
      bothParents(:,2) = par2

      ! Loop over, doing the crossing

      do iC = 1, kCross+1

         off1(CPt(iC):CPt(iC+1)) = bothParents(CPt(iC):CPt(iC+1),mod(iC-1,2)+1)
         off2(CPt(iC):CPt(iC+1)) = bothParents(CPt(iC):CPt(iC+1),mod(iC  ,2)+1)

      end do

      ! Return to calling program

      return

   end subroutine crossover

   !*******************************************************************
   !*******************************************************************


end module mod_optGA

!********************************************************************!
!********************************************************************!
!                                                                    !
!   End of module mod_optGA                                          !
!                                                                    !
!********************************************************************!
!********************************************************************!

