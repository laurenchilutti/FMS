!***********************************************************************
!*                   GNU Lesser General Public License
!*
!* This file is part of the GFDL Flexible Modeling System (FMS).
!*
!* FMS is free software: you can redistribute it and/or modify it under
!* the terms of the GNU Lesser General Public License as published by
!* the Free Software Foundation, either version 3 of the License, or (at
!* your option) any later version.
!*
!* FMS is distributed in the hope that it will be useful, but WITHOUT
!* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
!* FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
!* for more details.
!*
!* You should have received a copy of the GNU Lesser General Public
!* License along with FMS.  If not, see <http://www.gnu.org/licenses/>.
!***********************************************************************
!> @file
!! @author Lauren Chilutti
!! @brief Test program for the mpp_transmit interface.
!! @email gfdl.climate.model.info@noaa.gov
!! @description This test program is for testing the mpp_transmit interface.

program test_mpp_transmit

  use mpp_mod, only : mpp_init, mpp_pe, mpp_npes, mpp_root_pe
  use mpp_mod, only : mpp_sync, mpp_sync_self
  use mpp_mod, only : mpp_set_stack_size, mpp_init_test_requests_allocated
  use mpp_mod, only : mpp_transmit, ALL_PES, NULL_PE
  use mpp_mod, only : mpp_error, FATAL
  use platform_mod

  implicit none

  integer                                       :: ierr
  integer                                       :: pe, npes, root

  call mpp_init(mpp_init_test_requests_allocated)
  call mpp_set_stack_size(3145746)
  pe = mpp_pe()
  npes = mpp_npes()
  root = mpp_root_pe()

  if( pe.EQ.root ) print *, '------------------> Calling test_mpp_transmit <------------------'
    call test_mpp_transmit_null_pe(npes,pe,root)
    call test_mpp_transmit_all_pes(npes,pe,root)
    call test_mpp_transmit_scalar(npes,pe,root)
    call test_mpp_transmit_2D(npes,pe,root)
    call test_mpp_transmit_3D(npes,pe,root)
    call test_mpp_transmit_4D(npes,pe,root)
  if( pe.EQ.root ) print *, '------------------> Finished test_mpp_transmit <------------------'

  call MPI_FINALIZE(ierr)

contains

  !> Test the use of NULL_PE as an argument. Only testing once for an r4_scalar.
  subroutine test_mpp_transmit_null_pe(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: i, n
    real(kind=r4_kind), dimension(1) :: a4

    n=1
    a4 = real(pe, kind=r4_kind)
    call mpp_sync()

    if (pe .EQ. 0) then
      do i = 1,npes-1
        call mpp_transmit( put_data=a4(1), plen=n, to_pe=i, get_data=a4(1), glen=n, from_pe=NULL_PE )
        call mpp_sync_self()
      end do
    else
      call mpp_transmit( put_data=a4(1), plen=n, to_pe=NULL_PE, get_data=a4(1), glen=n, from_pe=0 )
      call mpp_sync_self()
    end if
    print *, "PE ",pe, ": a4=", a4(1)
    ! a4(1) should equal 0 for all pes
    if (a4(1) .NE. 0) call mpp_error(FATAL, "Test_mpp_transmit_null_pe: transmit didn't go as expected") 
  end subroutine test_mpp_transmit_null_pe

  !> Test the use of ALL_PES as an argument.  Only testing once for an r4_scalar.
  subroutine test_mpp_transmit_all_pes(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: i, n
    real(kind=r4_kind), dimension(1) :: a4, b4, c4

    n=1
    a4 = real(pe, kind=r4_kind)
    call mpp_sync()

    call mpp_transmit( put_data=a4(1), plen=n, to_pe=ALL_PES, &
                       get_data=a4(1), glen=n, from_pe=root )
    call mpp_sync_self()
    ! a4(1) should equal 0 for all pes
    if (a4(1) .NE. 0) call mpp_error(FATAL, "Test_mpp_transmit_all_pes: transmit didn't go as expected")
  end subroutine test_mpp_transmit_all_pes

  subroutine test_mpp_transmit_scalar(npes,pe,root)
    integer, intent(in) :: npes, pe, root

    call test_mpp_transmit_r4_scalar(npes,pe,root)
    call test_mpp_transmit_r8_scalar(npes,pe,root)
    call test_mpp_transmit_i4_scalar(npes,pe,root)
    call test_mpp_transmit_i8_scalar(npes,pe,root)

  end subroutine test_mpp_transmit_scalar

  !> Test the functionality of mpp_transmit for an r4_scalar.
  subroutine test_mpp_transmit_r4_scalar(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: n
    real(kind=r4_kind), dimension(1) :: a4, b4, c4

    n=1
    a4 = real(pe, kind=r4_kind)
    call mpp_sync()

    call mpp_transmit( put_data=a4(1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b4(1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c4 = real(modulo(int(a4(1))+npes-1,npes), kind=r4_kind)
    ! b4(1) should now equal the value of a4(1)from the "from_pe"
    if (b4(1) .NE. c4(1) ) call mpp_error(FATAL, "Test_mpp_transmit_r4_scalar: transmit didn't go as expected")

  end subroutine test_mpp_transmit_r4_scalar

  !> Test the functionality of mpp_transmit for an r8_scalar.
  subroutine test_mpp_transmit_r8_scalar(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer ::n
    real(kind=r8_kind), dimension(1) :: a8, b8, c8

    n=1
    a8 = real(pe, kind=r8_kind)
    b8 = real(pe+5, kind=r8_kind)
    call mpp_sync()

    call mpp_transmit( put_data=a8(1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b8(1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c8 = real(modulo(int(a8(1))+npes-1,npes), kind=r8_kind)
    ! b8(1) should now equal the value of a8(1)from the "from_pe"
    if (b8(1) .NE. c8(1) ) call mpp_error(FATAL, "Test_mpp_transmit_r8_scalar: transmit didn't go as expected")

  end subroutine test_mpp_transmit_r8_scalar

  !> Test the functionality of mpp_transmit for an i4_scalar.
  subroutine test_mpp_transmit_i4_scalar(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: n
    real(kind=i4_kind), dimension(1) :: a4, b4, c4

    n=1
    a4 = real(pe, kind=i4_kind)
    call mpp_sync()

    call mpp_transmit( put_data=a4(1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b4(1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c4 = real(modulo(int(a4(1))+npes-1,npes), kind=i4_kind)
    ! b4(1) should now equal the value of a4(1)from the "from_pe"
    if (b4(1) .NE. c4(1) ) call mpp_error(FATAL, "Test_mpp_transmit_i4_scalar: transmit didn't go as expected")

  end subroutine test_mpp_transmit_i4_scalar

  !> Test the functionality of mpp_transmit for an i8_scalar.
  subroutine test_mpp_transmit_i8_scalar(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: i, n
    real(kind=i8_kind), dimension(1) :: a8, b8, c8

    n=1
    a8 = real(pe, kind=i8_kind)
    call mpp_sync()

    call mpp_transmit( put_data=a8(1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b8(1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c8 = real(modulo(int(a8(1))+npes-1,npes), kind=i8_kind)
    ! b8(1) should now equal the value of a8(1)from the "from_pe"
    if (b8(1) .NE. c8(1) ) call mpp_error(FATAL, "Test_mpp_transmit_i8_scalar: transmit didn't go as expected")

  end subroutine test_mpp_transmit_i8_scalar

  subroutine test_mpp_transmit_2D(npes,pe,root)
    integer, intent(in) :: npes, pe, root

    call test_mpp_transmit_r4_2D(npes,pe,root)
    call test_mpp_transmit_r8_2D(npes,pe,root)
    call test_mpp_transmit_i4_2D(npes,pe,root)
    call test_mpp_transmit_i8_2D(npes,pe,root)

  end subroutine test_mpp_transmit_2D

  !> Test the functionality of mpp_transmit for an r4_2D.
  subroutine test_mpp_transmit_r4_2D(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: i, n
    real(kind=r4_kind), dimension(1,1) :: a4, b4, c4

    n=1
    a4 = real(pe, kind=r4_kind)
    call mpp_sync()

    call mpp_transmit( put_data=a4(1,1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b4(1,1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c4 = real(modulo(int(a4(1,1))+npes-1,npes), kind=r4_kind)
    ! b4(1,1) should now equal the value of a4(1,1)from the "from_pe"
    if (b4(1,1) .NE. c4(1,1) ) call mpp_error(FATAL, "Test_mpp_transmit_r4_2D: transmit didn't go as expected")

  end subroutine test_mpp_transmit_r4_2D

  !> Test the functionality of mpp_transmit for an r8_2D.
  subroutine test_mpp_transmit_r8_2D(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: n
    real(kind=r8_kind), dimension(1,1) :: a8, b8, c8

    n=1
    a8 = real(pe, kind=r8_kind)
    call mpp_sync()

    call mpp_transmit( put_data=a8(1,1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b8(1,1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c8 = real(modulo(int(a8(1,1))+npes-1,npes), kind=r8_kind)
    ! b8(1,1) should now equal the value of a8(1,1)from the "from_pe"
    if (b8(1,1) .NE. c8(1,1) ) call mpp_error(FATAL, "Test_mpp_transmit_r8_2D: transmit didn't go as expected")

  end subroutine test_mpp_transmit_r8_2D

  !> Test the functionality of mpp_transmit for an i4_2D.
  subroutine test_mpp_transmit_i4_2D(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: n
    real(kind=i4_kind), dimension(1,1) :: a4, b4, c4

    n=1
    a4 = real(pe, kind=i4_kind)
    call mpp_sync()

    call mpp_transmit( put_data=a4(1,1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b4(1,1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c4 = real(modulo(int(a4(1,1))+npes-1,npes), kind=i4_kind)
    ! b4(1,1) should now equal the value of a4(1,1)from the "from_pe"
    if (b4(1,1) .NE. c4(1,1) ) call mpp_error(FATAL, "Test_mpp_transmit_i4_2D: transmit didn't go as expected")

  end subroutine test_mpp_transmit_i4_2D

  !> Test the functionality of mpp_transmit for an i8_2D.
  subroutine test_mpp_transmit_i8_2D(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: n
    real(kind=i8_kind), dimension(1,1) :: a8, b8, c8

    n=1
    a8 = real(pe, kind=i8_kind)
    call mpp_sync()

    call mpp_transmit( put_data=a8(1,1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b8(1,1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c8 = real(modulo(int(a8(1,1))+npes-1,npes), kind=i8_kind)
    ! b8(1,1) should now equal the value of a8(1,1)from the "from_pe"
    if (b8(1,1) .NE. c8(1,1) ) call mpp_error(FATAL, "Test_mpp_transmit_i8_2D: transmit didn't go as expected")

  end subroutine test_mpp_transmit_i8_2D

  subroutine test_mpp_transmit_3D(npes,pe,root)
    integer, intent(in) :: npes, pe, root

    call test_mpp_transmit_r4_3D(npes,pe,root)
    call test_mpp_transmit_r8_3D(npes,pe,root)
    call test_mpp_transmit_i4_3D(npes,pe,root)
    call test_mpp_transmit_i8_3D(npes,pe,root)

  end subroutine test_mpp_transmit_3D

  !> Test the functionality of mpp_transmit for an r4_3D.
  subroutine test_mpp_transmit_r4_3D(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: n
    real(kind=r4_kind), dimension(1,1,1) :: a4, b4, c4

    n=1
    a4 = real(pe, kind=r4_kind)
    call mpp_sync()

    call mpp_transmit( put_data=a4(1,1,1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b4(1,1,1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c4 = real(modulo(int(a4(1,1,1))+npes-1,npes), kind=r4_kind)
    ! b4(1,1,1) should now equal the value of a4(1,1,1)from the "from_pe"
    if (b4(1,1,1) .NE. c4(1,1,1) ) call mpp_error(FATAL, "Test_mpp_transmit_r4_3D: transmit didn't go as expected")

  end subroutine test_mpp_transmit_r4_3D

  !> Test the functionality of mpp_transmit for an r8_3D.
  subroutine test_mpp_transmit_r8_3D(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: n
    real(kind=r8_kind), dimension(1,1,1) :: a8, b8, c8

    n=1
    a8 = real(pe, kind=r8_kind)
    call mpp_sync()

    !print *, "PE ",pe, ": a8=", a8(1,1,1)," b8=", b8(1,1,1)
    call mpp_transmit( put_data=a8(1,1,1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b8(1,1,1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c8 = real(modulo(int(a8(1,1,1))+npes-1,npes), kind=r8_kind)
    ! b8(1,1,1) should now equal the value of a8(1,1,1)from the "from_pe"
    if (b8(1,1,1) .NE. c8(1,1,1) ) call mpp_error(FATAL, "Test_mpp_transmit_r8_3D: transmit didn't go as expected")


  end subroutine test_mpp_transmit_r8_3D

  !> Test the functionality of mpp_transmit for an i4_3D.
  subroutine test_mpp_transmit_i4_3D(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: n
    real(kind=i4_kind), dimension(1,1,1) :: a4, b4, c4

    n=1
    a4 = real(pe, kind=i4_kind)
    call mpp_sync()

    call mpp_transmit( put_data=a4(1,1,1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b4(1,1,1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c4 = real(modulo(int(a4(1,1,1))+npes-1,npes), kind=i4_kind)
    ! b4(1,1,1) should now equal the value of a4(1,1,1)from the "from_pe"
    if (b4(1,1,1) .NE. c4(1,1,1) ) call mpp_error(FATAL, "Test_mpp_transmit_i4_3D: transmit didn't go as expected")

  end subroutine test_mpp_transmit_i4_3D

  !> Test the functionality of mpp_transmit for an i8_3D.
  subroutine test_mpp_transmit_i8_3D(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: n
    real(kind=i8_kind), dimension(1,1,1) :: a8, b8, c8

    n=1
    a8 = real(pe, kind=i8_kind)
    call mpp_sync()

    !print *, "PE ",pe, ": a8=", a8(1,1,1)," b8=", b8(1,1,1)
    call mpp_transmit( put_data=a8(1,1,1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b8(1,1,1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c8 = real(modulo(int(a8(1,1,1))+npes-1,npes), kind=i8_kind)
    ! b8(1,1,1) should now equal the value of a8(1,1,1)from the "from_pe"
    if (b8(1,1,1) .NE. c8(1,1,1) ) call mpp_error(FATAL, "Test_mpp_transmit_i8_3D: transmit didn't go as expected")

  end subroutine test_mpp_transmit_i8_3D

  subroutine test_mpp_transmit_4D(npes,pe,root)
    integer, intent(in) :: npes, pe, root

    call test_mpp_transmit_r4_4D(npes,pe,root)
    call test_mpp_transmit_r8_4D(npes,pe,root)
    call test_mpp_transmit_i4_4D(npes,pe,root)
    call test_mpp_transmit_i8_4D(npes,pe,root)

  end subroutine test_mpp_transmit_4D

  !> Test the functionality of mpp_transmit for an r4_4D.
  subroutine test_mpp_transmit_r4_4D(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: n
    real(kind=r4_kind), dimension(1,1,1,1) :: a4, b4, c4

    n=1
    a4 = real(pe, kind=r4_kind)
    call mpp_sync()

    call mpp_transmit( put_data=a4(1,1,1,1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b4(1,1,1,1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c4 = real(modulo(int(a4(1,1,1,1))+npes-1,npes), kind=r4_kind)
    ! b4(1,1,1,1) should now equal the value of a4(1,1,1,1)from the "from_pe"
    if (b4(1,1,1,1) .NE. c4(1,1,1,1) ) call mpp_error(FATAL, "Test_mpp_transmit_r4_4D: transmit didn't go as expected")

  end subroutine test_mpp_transmit_r4_4D

  !> Test the functionality of mpp_transmit for an r8_4D.
  subroutine test_mpp_transmit_r8_4D(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: n
    real(kind=r8_kind), dimension(1,1,1,1) :: a8, b8, c8

    n=1
    a8 = real(pe, kind=r8_kind)
    call mpp_sync()

    call mpp_transmit( put_data=a8(1,1,1,1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b8(1,1,1,1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c8 = real(modulo(int(a8(1,1,1,1))+npes-1,npes), kind=r8_kind)
    ! b8(1,1,1,1) should now equal the value of a8(1,1,1,1)from the "from_pe"
    if (b8(1,1,1,1) .NE. c8(1,1,1,1) ) call mpp_error(FATAL, "Test_mpp_transmit_r8_4D: transmit didn't go as expected")

  end subroutine test_mpp_transmit_r8_4D

  !> Test the functionality of mpp_transmit for an i4_4D.
  subroutine test_mpp_transmit_i4_4D(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: n
    real(kind=i4_kind), dimension(1,1,1,1) :: a4, b4, c4

    n=1
    a4 = real(pe, kind=i4_kind)
    call mpp_sync()

    call mpp_transmit( put_data=a4(1,1,1,1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b4(1,1,1,1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c4 = real(modulo(int(a4(1,1,1,1))+npes-1,npes), kind=i4_kind)
    ! b4(1,1,1,1) should now equal the value of a4(1,1,1,1)from the "from_pe"
    if (b4(1,1,1,1) .NE. c4(1,1,1,1) ) call mpp_error(FATAL, "Test_mpp_transmit_i4_4D: transmit didn't go as expected")

  end subroutine test_mpp_transmit_i4_4D

  !> Test the functionality of mpp_transmit for an i8_4D.
  subroutine test_mpp_transmit_i8_4D(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: n
    real(kind=i8_kind), dimension(1,1,1,1) :: a8, b8, c8

    n=1
    a8 = real(pe, kind=i8_kind)
    call mpp_sync()

    call mpp_transmit( put_data=a8(1,1,1,1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b8(1,1,1,1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c8 = real(modulo(int(a8(1,1,1,1))+npes-1,npes), kind=i8_kind)
    ! b8(1,1,1,1) should now equal the value of a8(1,1,1,1)from the "from_pe"
    if (b8(1,1,1,1) .NE. c8(1,1,1,1) ) call mpp_error(FATAL, "Test_mpp_transmit_i8_4D: transmit didn't go as expected")

  end subroutine test_mpp_transmit_i8_4D

  subroutine test_mpp_transmit_5D(npes,pe,root)
    integer, intent(in) :: npes, pe, root

    call test_mpp_transmit_r4_5D(npes,pe,root)
    call test_mpp_transmit_r8_5D(npes,pe,root)
    call test_mpp_transmit_i4_5D(npes,pe,root)
    call test_mpp_transmit_i8_5D(npes,pe,root)

  end subroutine test_mpp_transmit_5D

  !> Test the functionality of mpp_transmit for an r4_5D.
  subroutine test_mpp_transmit_r4_5D(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: n
    real(kind=r4_kind), dimension(1,1,1,1,1) :: a4, b4, c4

    n=1
    a4 = real(pe, kind=r4_kind)
    call mpp_sync()

    call mpp_transmit( put_data=a4(1,1,1,1,1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b4(1,1,1,1,1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c4 = real(modulo(int(a4(1,1,1,1,1))+npes-1,npes), kind=r4_kind)
    ! b4(1,1,1,1,1) should now equal the value of a4(1,1,1,1,1)from the "from_pe"
    if (b4(1,1,1,1,1) .NE. c4(1,1,1,1,1) ) call mpp_error(FATAL, "Test_mpp_transmit_r4_5D: transmit didn't go as expected")

  end subroutine test_mpp_transmit_r4_5D

  !> Test the functionality of mpp_transmit for an r8_5D.
  subroutine test_mpp_transmit_r8_5D(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: n
    real(kind=r8_kind), dimension(1,1,1,1,1) :: a8, b8, c8

    n=1
    a8 = real(pe, kind=r8_kind)
    call mpp_sync()

    call mpp_transmit( put_data=a8(1,1,1,1,1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b8(1,1,1,1,1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c8 = real(modulo(int(a8(1,1,1,1,1))+npes-1,npes), kind=r8_kind)
    ! b8(1,1,1,1,1) should now equal the value of a8(1,1,1,1,1)from the "from_pe"
    if (b8(1,1,1,1,1) .NE. c8(1,1,1,1,1) ) call mpp_error(FATAL, "Test_mpp_transmit_r8_5D: transmit didn't go as expected")

  end subroutine test_mpp_transmit_r8_5D

  !> Test the functionality of mpp_transmit for an i4_5D.
  subroutine test_mpp_transmit_i4_5D(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: n
    real(kind=i4_kind), dimension(1,1,1,1,1) :: a4, b4, c4

    n=1
    a4 = real(pe, kind=i4_kind)
    call mpp_sync()

    call mpp_transmit( put_data=a4(1,1,1,1,1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b4(1,1,1,1,1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c4 = real(modulo(int(a4(1,1,1,1,1))+npes-1,npes), kind=i4_kind)
    ! b4(1,1,1,1,1) should now equal the value of a4(1,1,1,1,1)from the "from_pe"
    if (b4(1,1,1,1,1) .NE. c4(1,1,1,1,1) ) call mpp_error(FATAL, "Test_mpp_transmit_i4_5D: transmit didn't go as expected")

  end subroutine test_mpp_transmit_i4_5D

  !> Test the functionality of mpp_transmit for an i8_5D.
  subroutine test_mpp_transmit_i8_5D(npes,pe,root)
    integer, intent(in) :: npes, pe, root
    integer :: n
    real(kind=i8_kind), dimension(1,1,1,1,1) :: a8, b8, c8

    n=1
    a8 = real(pe, kind=i8_kind)
    call mpp_sync()

    call mpp_transmit( put_data=a8(1,1,1,1,1), plen=n, to_pe=modulo(pe+1, npes), &
                       get_data=b8(1,1,1,1,1), glen=n, from_pe=modulo(npes+pe-1, npes) )
    call mpp_sync_self()
    c8 = real(modulo(int(a8(1,1,1,1,1))+npes-1,npes), kind=i8_kind)
    ! b8(1,1,1,1,1) should now equal the value of a8(1,1,1,1,1)from the "from_pe"
    if (b8(1,1,1,1,1) .NE. c8(1,1,1,1,1) ) call mpp_error(FATAL, "Test_mpp_transmit_i8_5D: transmit didn't go as expected")

  end subroutine test_mpp_transmit_i8_5D
end program test_mpp_transmit
