!********************************************************************!
!********************************************************************!
!                                                                    !
!   MOD_Stochastic -- Contains random number generation routines     !
!                                                                    !
!********************************************************************!
!                                                                    !
!   Version history:                                                 !
!                    Program created: 17Jul19                - raw54 !
!                                                                    !
!********************************************************************!
!                                                                    !
!   Known issues:                                                    !
!                    No known issues.                        - raw54 !
!                                                                    !
!********************************************************************!
!********************************************************************!

module MOD_Stochastic

  use precision

contains

  !*******************************************************************
  !*******************************************************************

  subroutine STOCHASTIC_SeedInit(procIn)

    implicit none

    ! Declare variables

    integer(kind=WI)              :: procIn, i, n, clock

    integer(kind=WI), allocatable :: seed(:)

    ! Set up the random seed
    
    call random_seed(size = n)
    allocate(seed(n))

    call system_clock(count=clock)

    seed = procIn + clock + 37 * (/ (i - 1, i = 1, n) /)
    call random_seed(put = seed)

    deallocate(seed)

    ! Return to calling program

    return

  end subroutine STOCHASTIC_SeedInit

  !*******************************************************************
  !*******************************************************************

  function STOCHASTIC_Uniform(scale1, scale2) result(rnd)

    implicit none

    ! Declare variables

    real(kind=WP) :: scale1, scale2, rnd

    ! Set up the random variable

    call random_number(rnd)

    ! And scale it appropriately

    rnd = (scale2 - scale1) * rnd + scale1

    ! And return

  end function STOCHASTIC_Uniform

  !*******************************************************************
  !*******************************************************************

  function STOCHASTIC_Normal(mean, sd) result(rnd)

    implicit none

    ! Declare variables

    real(kind=WP) :: rnd, mean, sd, pi, rnd1, rnd2

    ! Get two random numbers

    rnd1 = STOCHASTIC_Uniform(0.0000001_wp,0.9999999_wp)
    rnd2 = STOCHASTIC_Uniform(0.0000001_wp,0.9999999_wp)

    ! Compute pi

    pi = 4.0 * atan(1.0)

    ! Compute the random number

    rnd = sqrt( - 2.0 * log(rnd1) ) * cos( 2.0*pi*rnd2 )

    ! And scale appropriately

    rnd = mean + sd*( rnd )

    ! And return

  end function STOCHASTIC_Normal

  !*******************************************************************
  !*******************************************************************

  function STOCHASTIC_integer(lb, ub) result(ri)

    implicit none

    ! Declare variables

    integer(kind=WI) :: lb, ub, ri

    real   (kind=WP) :: r

    ! Get a random number

    call random_number(r)

    ! Get the random integer

    ri = lb + floor((ub+1-lb)*r)

    ! And return

  end function STOCHASTIC_integer

  !*******************************************************************
  !*******************************************************************

  function STOCHASTIC_IntegerD(lb, ub, nd) result(ri)

    implicit none

    ! Declare variables

    integer(kind=WI) :: id, jd, nd, lb, ub, ri(nd)

    integer(kind=WI) :: temp1, temp2

    real   (kind=WP) :: r

    ! Loop over entire distribution

    do id = 1, nd

       call random_number(r)

       ri(id) = lb + floor((ub+1-lb)*r)

    end do

    ! Put into ascending order

    do id = 1, nd
    do jd = 1, nd - 1

       temp1 = min(ri(jd+1), ri(jd))
       temp2 = max(ri(jd+1), ri(jd))

       ri(jd  ) = temp1
       ri(jd+1) = temp2

    end do
    end do

  end function STOCHASTIC_IntegerD

  !*******************************************************************
  !*******************************************************************

  function STOCHASTIC_RouletteWheel(Error, iDim, r) result(iMem)

    implicit none

    ! Declare Variables

    real(kind=WP)    :: Error(:,:), r

    integer(kind=WI) :: iDim, iMem

    real(kind=WP)    :: MinError

    real(kind=WP)    :: Probabilities(size(Error,1)), Wheel(size(Error,1)+1), x

    integer(kind=WI) :: iPop, Length

    ! Set some sizes and values

    Length = size(Error,1)

    MinError = minval(Error(:,iDim))

    iMem = 1

    ! Calculating Probability values for each member relative to best performer

    do iPop = 1, Length

       Probabilities(iPop) = (MinError/Error(iPop,iDim))**r

    end do

    ! Normalising values so they sum to 1

    Probabilities = Probabilities/sum(Probabilities)

    ! Assigning first roulette wheel value

    Wheel(1) = zero

    ! Creating a weighted number line with built in probability values

    do iPop = 2, Length+1 

       Wheel(iPop) = Wheel(iPop-1) + Probabilities(iPop-1)

    end do

    ! Roll the roulette wheel and get your output

    x = STOCHASTIC_Uniform(epsilon(x),one)

    iMem = STOCHASTIC_BinarySearch(Wheel,x)

  end function STOCHASTIC_RouletteWheel
  
  !*******************************************************************
  !*******************************************************************
  
  function STOCHASTIC_RouletteWheelD(Error, r, ND) result(iMems)

    implicit none

    ! Declare Variables

    real(kind=WP)    :: Error(:), r

    integer(kind=WI) :: ND, iMems(ND)

    real(kind=WP)    :: MinError

    real(kind=WP)    :: Probabilities(size(Error,1)), Wheel(size(Error,1)+1), x

    integer(kind=WI) :: iPop, iSize, Length

    ! Set some sizes and values

    Length = size(Error,1)

    MinError = minval(Error(:))

    ! Calculating Probability values for each member relative to best performer

    do iPop = 1, Length

       Probabilities(iPop) = (MinError/Error(iPop))**r

    end do

    ! Normalising values so they sum to 1

    Probabilities = Probabilities/sum(Probabilities)

    ! Assigning first roulette wheel value

    Wheel(1) = zero

    ! Creating a weighted number line with built in probability values

    do iPop = 2, Length+1 

       Wheel(iPop) = Wheel(iPop-1) + Probabilities(iPop-1)

    end do

    ! Roll the roulette wheel and get your output

    do iSize = 1, ND

       iMems(iSize) = 1
       
       x = STOCHASTIC_Uniform(epsilon(x),one)

       iMems(iSize) = STOCHASTIC_BinarySearch(Wheel,x)

    end do

  end function STOCHASTIC_RouletteWheelD

  !*******************************************************************
  !*******************************************************************
  
  function STOCHASTIC_BinarySearch(a,r) result(index)

    ! Turn off implicit typing

    implicit none

    ! Declare variables

    integer(kind=WI) :: index

    integer(kind=WI) :: iLeft, iRight, iMid

    real   (kind=WP) :: a(:), r

    ! Now perform the binary search - this assumes that vector a has been ordered smallest -> largest

    iLeft  = 1
    iRight = size(a)

    do while ( iRight - iLeft .gt. 1 )

       iMid = (iLeft + iRight) / 2

       if ( a(iMid) .gt. r ) then
          iRight = iMid
       else
          iLeft = iMid
       end if

    end do

    ! Return the required index

    index = iLeft

  end function STOCHASTIC_BinarySearch

  !*****************************************************************
  !*****************************************************************

end module MOD_Stochastic

!********************************************************************!
!********************************************************************!
!                                                                    !
!   End of module MOD_stochastic                                     !
!                                                                    !
!********************************************************************!
!********************************************************************!

