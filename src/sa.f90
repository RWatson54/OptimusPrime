!********************************************************************!
!********************************************************************!
!                                                                    !
!   mod_optSA -- A module for doing simulated annealing              !
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
!>  This module does simulated annealing for vectors of real numbers
!>
!*******************************************************************
!*******************************************************************

module mod_optSA

   ! Declare modules

   use precision
   use mod_FunctionInterface
   use mod_Stochastic

   ! Turn off implicit typing

   implicit none

   ! Set everything private

   private

   ! Turn on what should be publicly visible

   public :: opt_runSA

contains

   !*******************************************************************
   !*******************************************************************
   !
   !>  Subroutine opt_runSA()
   !
   !>  @author
   !>  Rob Watson
   !>
   !>  @brief 
   !>  Run a simulated annealing optimisation based on the supplied
   !>  parameters for a real vector input.
   !>  
   !>  @param[in]  fitnessFunc -- the fitness function to be optimised
   !>  @param[in]  nDims -- the number of dimensions
   !>  @param[in]  lBound -- the lower allowed bound
   !>  @param[in]  uBound -- the upper allowed bound
   !>  @param[out] xOpt -- the returned values
   !>
   !*******************************************************************
   !*******************************************************************

   subroutine opt_runSA(fitnessFunc, nDims, lBound, uBound, xOpt)

      ! Turn off implicit typing

      implicit none

      ! Declare external variables

      procedure(optimiseFunction) :: fitnessFunc

      integer(kind=WI) :: nDims

      real   (kind=WP) :: lBound, uBound

      real   (kind=WP) :: xOpt(nDims)

      ! Declare internal variables

      integer(kind=WI), parameter :: outerMax = 100     ! The number of outer iterations
      integer(kind=WI), parameter :: innerMax = 500     ! The number of inner iterations
      integer(kind=WI), parameter :: nOut = 50          ! The print output timer
      integer(kind=WI), parameter :: nPrint = 10000     ! The file output timer
      integer(kind=WI), parameter :: nReheat = 100      ! The reheat frequency
      
      integer(kind=WI), parameter :: nInitTest = 100    ! The number of initial tests to estimate T_0

      real   (kind=WP) :: alpha = 0.99_wp    ! The geometric annealing rate

      real   (kind=WP) :: xCurr(nDims), xBest(nDims), xI(nDims), xProp(nDims)

      real   (kind=WP) :: cCurr, cBest, cI(nInitTest), cMean, cProp

      real   (kind=WP) :: rTemp

      real   (kind=WP) :: tInit, tOff, tGrad, tCurr

      ! Declare loop variables and counters
      
      integer(kind=WI) :: iDim, iTest, jTest, iOuter, iInner
      
      integer(kind=WI) :: iBestNC, jBestNC, iTempLevel, iterNum

      ! Seed the random variables
      
      call STOCHASTIC_SeedInit(1)

      ! Make an initial guess at the variables

      do iDim = 1, nDims

         xCurr(iDim) = STOCHASTIC_Uniform(lBound, uBound)

      end do

      ! Evaluate the performance of the current guess

      cCurr = evaluateFunction(fitnessFunc, xCurr)

      ! Initialise the best guesses to the current guess

      xBest = xCurr
      cBest = cCurr

      ! Get a modest sample of the starting cost functions

      write(6,*) ' *** '      
      write(6,*) ' *** Sampling some initial data to model the temperature ...'
      write(6,*) ' *** '

      do iTest = 1, nInitTest

         ! Generate more random start points

         do iDim = 1, nDims
            
            xI(iDim) = STOCHASTIC_Uniform(lBound, uBound)
            
         end do

         ! And see if they're any good

         cI(iTest) = evaluateFunction(fitnessFunc, xI)

      end do

      ! Use this modest starting sample to get an idea of the starting temperature

      cMean = zero

      do iTest = 1, nInitTest

         do jTest = 1, nInitTest

            cMean = cMean + abs(cI(iTest) - cI(jTest))

         end do

      end do

      cMean = cMean / real(nInitTest*nInitTest, WP)

      ! Set the starting temperature

      tInit = -cMean / log(0.95_wp)

      write(6,*) ' *** Starting temperature: ', tInit

      write(6,*) ' *** '
      write(6,*) ' ***  ...done. '
      write(6,*) ' *** '
      
      ! Set the temperature switch
      
      tOff  = real(nDims, WP)
      tGrad = (one - tOff) / (zero - tInit)

      ! Initialise a "best hasn't changed for this many temperatures" counter

      iBestNC = 0
      iTempLevel = 0

      ! Begin the optimisation iteration loop

      do iOuter = 1, outerMax

         ! Moving to a new temperature level

         iTempLevel = iTempLevel + 1

         ! Initialise the " no change to best" counter for this temperature level

         jBestNC = 1

         ! Recalculate the temperature for this level using a geometric schedule

         tCurr = tInit * exp(-alpha * (iTempLevel-1))

         ! Begin the inner loop at the fixed temperature

         do iInner = 1, innerMax

            ! Propose a new configuration in the neighbourhood of the current

            call neighbouringVector(xCurr, xProp)

            ! Calculate the cost of the propose configuration

            cProp = evaluateFunction(fitnessFunc, xProp)

            !  Test if any of the conditions are met

            if (cProp .le. cCurr) then

               ! Update the current ordering

               xCurr = xProp
               cCurr = cProp

            else

               rTemp = STOCHASTIC_Uniform(zero, one)

               if (rTemp .le. exp((cCurr-cProp)/tCurr)) then

                  ! Update the current ordering
                  
                  xCurr = xProp
                  cCurr = cProp

               end if

            end if
               
            ! Update the best ordering if necessary

            if ( cProp .lt. cBest ) then

               ! Update best

               xBest = xProp
               cBest = cProp

               ! Reset the "no change to best switch" for this loop

               jBestNC = 0

            end if

            ! Report to screen and file, if necessary

            iterNum = (iOuter-1)*innerMax + iInner

            if ( mod(iterNum,nOut) .eq. 0 ) then

               ! Print some information to screen

               write(6,*) ' *** Iteration ', iterNum, ' of ', outerMax*innerMax
               write(6,*) ' ***       Best value: ', cBest, ', Current value: ', cCurr, ', Current temperature: ', tCurr

            end if

         end do

         ! If there has been no change to the best value on this inner loop, increment the counter. Otherwise, reset it

         if ( jBestNC .eq. 1 ) then

            iBestNC = iBestNC + 1

         else

            iBestNC = 0

         end if

         ! Test and see if there have been nReheat loops without a change to the best value

         if ( iBestNC .eq. nReheat ) then

            ! If there have, reheat, melting everything down and restart

            do iDim = 1, nDims

               xCurr(iDim) = STOCHASTIC_Uniform(lBound, uBound)

            end do

            ! Compute the current and best costs

            cCurr = evaluateFunction(fitnessFunc, xCurr)

            ! And reset the counters

            iTempLevel = 0
            iBestNC = 0

         end if

         !  Maintain a small chance of going back to the best

         rTemp = STOCHASTIC_Uniform(zero, one)

         if ( rTemp .lt. 0.0001_wp ) then

            xCurr = xBest
            cCurr = cBest

         end if

      end do

      ! Finally, set the returned values to the best found

      xOpt = xBest

      ! Return to calling program

      return

   end subroutine opt_runSA

   !*******************************************************************
   !*******************************************************************
   
   !*******************************************************************
   !*******************************************************************
   !
   !>  Subroutine neighbouringVector()
   !
   !>  @author
   !>  Rob Watson
   !>
   !>  @brief 
   !>  Finds a neighbouring vector for Simulated Annealing a real 
   !>  vector
   !>  
   !>  @param[in]  xIn -- the original point
   !>  @param[out] xOut -- the neighbouring point
   !>
   !*******************************************************************
   !*******************************************************************
   
   subroutine neighbouringVector(xIn, xOut)

      implicit none

      ! Declare external variables

      real   (kind=WP), intent(in) :: xIn(:)

      real   (kind=WP), intent(out) :: xOut(size(xIn,1))

      ! Declare internal variables

      integer(kind=WI) :: i

      real   (kind=WP) :: rTemp

      ! Set the output equal to the input

      xOut = xIn

      ! Loop through it, modifying as necessary

      do i = 1, size(xIn,1)

         call random_number(rTemp)

         if (rTemp .lt. 0.10_wp) then

            xOut(i) = xOut(i) + STOCHASTIC_Normal(zero, 0.10_wp)

         end if

      end do

      ! Return to calling program

      return

   end subroutine neighbouringVector

   !*******************************************************************
   !*******************************************************************


end module mod_optSA

!********************************************************************!
!********************************************************************!
!                                                                    !
!   End of module mod_optSA                                          !
!                                                                    !
!********************************************************************!
!********************************************************************!

