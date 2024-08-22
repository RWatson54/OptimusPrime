!********************************************************************!
!********************************************************************!
!                                                                    !
!   OptimusPrime -- Various optimisers for real-valued vectors       !
!                                                                    !
!********************************************************************!
!                                                                    !
!   Version history:                                                 !
!                    Program created: 21Aug24                - raw54 !
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
!>  It is often useful to be able to optimise real-valued vectors 
!>  against some target function. This program aims to provide tools
!>  to do this.
!>
!>  The user specifies their function for minimisation in a separate
!>  module (as a function), and summons the optimiser with the
!>  function name as an argument.
!>
!>  Population-based optimisation schema are implemented, and 
!>  targeted for parallel analysis of the functions, via MPI. Since
!>  a lot of the work preparing the subsequent population is done
!>  on the root process, expensive-to-evaluate functions generally
!>  parallelise better. 
!>
!>  Currently implemented optimsation schemes are:
!>       1. Simple Simulated Annealing schedule (not population-based)
!>       2. Basic Genetic Algorithm
!>       3. Basic Evolutionary Strategy
!>
!*******************************************************************
!*******************************************************************

program OptimusPrime

   ! Declare modules

   use precision
   use mpi_f08
   use mod_Welcome
   use mod_UserFunctions
   use mod_Optimisers

   ! Turn off implicit typing

   implicit none

   ! Declare program variables

   real   (kind=WP) :: xFinal(nDims)

   type(optimisationSettings) :: optSettings

   ! Say hello with a welcoming 80s-style screen

   call WL_hello()

   ! Set up the key options

   optSettings%lBound = -4.0_wp
   optSettings%uBound =  4.0_wp

   optSettings%lBoundI = -one
   optSettings%uBoundI =  one

   optSettings%nGen = 100
   optSettings%nPop = 1000

!!$   optSettings%nOuter = 50
!!$   optSettings%nReheat = 50

   optSettings%nPrint = 5
   
!!$   optSettings%alpha = 0.99_wp

   ! Run the code

   call Optimise_GA(polycos, nDims, optSettings, xFinal)

   optSettings%nPop=100
   optSettings%nOffspring=7

   call Optimise_ES(polycos, nDims, optSettings, xFinal)

   write(6,*) xFinal

   ! Say goodbye with a more modern, 2024 goodbye screen
   
   call WL_goodbye()

contains

end program OptimusPrime

!********************************************************************!
!********************************************************************!
!                                                                    !
!   End of program OptimusPrime                                      !
!                                                                    !
!********************************************************************!
!********************************************************************!

