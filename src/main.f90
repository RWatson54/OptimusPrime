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
!>       1. Basic genetic algorithm
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

   real   (kind=WP) :: xA(2)

   call Optimise_SA(quadratic, nDims, xA)

   write(6,*) xA

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

