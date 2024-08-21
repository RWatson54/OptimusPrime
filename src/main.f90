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
!>       1. Simple Simulated Annealing schedule
!>
!*******************************************************************
!*******************************************************************

program OptimusPrime

   ! Declare modules

   use precision
   use mod_UserFunctions
   use mod_Optimisers

   ! Turn off implicit typing

   implicit none

   ! Declare program variables

   real   (kind=WP) :: xFinal(nDims)

   type(optimisationSettings) :: SA_Settings

   ! Set up the key options

   SA_Settings%lBound = -4.0_wp
   SA_Settings%uBound =  4.0_wp

   SA_Settings%lBoundI = -one
   SA_Settings%uBoundI =  one

   SA_Settings%nGen = 100
!!$   SA_Settings%nOuter = 50
!!$   SA_Settings%nReheat = 50

   SA_Settings%nPrint = 5
   
!!$   SA_Settings%alpha = 0.99_wp

   ! Run the code

   write(6,*) polycos([zero,zero])
   write(6,*) polycos([zero,-3.8_wp])


   call Optimise_ES(polycos, nDims, SA_Settings, xFinal)

   write(6,*) xFinal

   

   ! Stop and exit cleanly

   stop

contains

end program OptimusPrime

!********************************************************************!
!********************************************************************!
!                                                                    !
!   End of program OptimusPrime                                      !
!                                                                    !
!********************************************************************!
!********************************************************************!

