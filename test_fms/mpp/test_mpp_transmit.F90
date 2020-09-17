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
  use mpp_mod, only : mpp_transmit
  use mpp_mod, only : mpp_error, FATAL
  use platform_mod

  implicit none

  integer                                       :: ierr, n
  integer                                       :: pe, npes, root

  call mpp_init(mpp_init_test_requests_allocated)
  call mpp_set_stack_size(3145746)
  pe = mpp_pe()
  npes = mpp_npes()
  root = mpp_root_pe()

  if( pe.EQ.root ) print *, '------------------> Calling test_mpp_transmit <------------------'
    call test_mpp_transmit_scalar()
    call test_mpp_transmit_2D()
    call test_mpp_transmit_3D()
    call test_mpp_transmit_4D()
  if( pe.EQ.root ) print *, '------------------> Finished test_mpp_transmit <------------------'

  call MPI_FINALIZE(ierr)

contains

  subroutine test_mpp_transmit_scalar()

  real(kind=r4_kind), dimension(1) :: a4, b4, c4
  real(kind=r8_kind), dimension(1) :: a8, b8, c8

  n=1

  a4 = real(pe, kind=r4_kind)
  a8 = real(pe, kind=r8_kind)
  b4 = real(pe+5, kind=r4_kind)
  b8 = real(pe+5, kind=r8_kind)
  !print *, "PE ",pe, ": a4=", a4(1)," b4=", b4(1)
  call mpp_sync()

  call mpp_transmit( put_data=a4(1), plen=n, to_pe=modulo(pe+npes-1,npes), &
                     get_data=b4(1), glen=n, from_pe=modulo(pe+1, npes) )
  call mpp_sync_self()
  call mpp_sync()
  !print *, "PE ",pe, ": a4=", a4(1)," b4=", b4(1)

  !print *, "PE ",pe, ": a8=", a8(1)," b8=", b8(1) 
  call mpp_transmit( put_data=a8(1), plen=n, to_pe=modulo(pe+npes-1,npes), &
                     get_data=b8(1), glen=n, from_pe=modulo(pe+1, npes) )
  !print *, "PE ",pe, ": a8=", a8(1)," b8=", b8(1)
  call mpp_sync_self()
  call mpp_sync()

  end subroutine test_mpp_transmit_scalar

  subroutine test_mpp_transmit_2D()

  real(kind=r4_kind), dimension(1,1) :: a4, b4
  real(kind=r8_kind), dimension(1,1) :: a8, b8

  n=1
  a4 = real(pe, kind=r4_kind)
  a8 = real(pe, kind=r8_kind)
  b4 = real(pe+5, kind=r4_kind)
  b8 = real(pe+5, kind=r8_kind)
  !print *, "PE ",pe, ": a4=", a4(1,1)," b4=", b4(1,1)
  call mpp_sync()

  call mpp_transmit( put_data=a4(1,1), plen=n, to_pe=modulo(pe+npes-1,npes), &
                     get_data=a4(1,1), glen=n, from_pe=modulo(pe+1, npes) )
  call mpp_sync_self()
  call mpp_sync()
  !print *, "PE ",pe, ": a4=", a4(1,1)," b4=", b4(1,1)

  !print *, "PE ",pe, ": a8=", a8(1,1)," b8=", b8(1,1)
  call mpp_transmit( put_data=a8(1,1), plen=n, to_pe=modulo(pe+npes-1,npes), &
                     get_data=b8(1,1), glen=n, from_pe=modulo(pe+1, npes) )
  !print *, "PE ",pe, ": a8=", a8(1,1)," b8=", b8(1,1)
  call mpp_sync_self()
  call mpp_sync()

  end subroutine test_mpp_transmit_2D

  subroutine test_mpp_transmit_3D()

  real(kind=r4_kind), dimension(1,1,1) :: a4, b4
  real(kind=r8_kind), dimension(1,1,1) :: a8, b8

  n=1
  a4 = real(pe, kind=r4_kind)
  a8 = real(pe, kind=r8_kind)
  b4 = real(pe+5, kind=r4_kind)
  b8 = real(pe+5, kind=r8_kind)
  !print *, "PE ",pe, ": a4=", a4(1,1,1)," b4=", b4(1,1,1)
  call mpp_sync()

  call mpp_transmit( put_data=a4(1,1,1), plen=n, to_pe=modulo(pe+npes-1,npes), &
                     get_data=b4(1,1,1), glen=n, from_pe=modulo(pe+1, npes) )
  call mpp_sync_self()
  call mpp_sync()
  !print *, "PE ",pe, ": a4=", a4(1,1,1)," b4=", b4(1,1,1)

  !print *, "PE ",pe, ": a8=", a8(1,1,1)," b8=", b8(1,1,1)
  call mpp_transmit( put_data=a8(1,1,1), plen=n, to_pe=modulo(pe+npes-1,npes), &
                     get_data=b8(1,1,1), glen=n, from_pe=modulo(pe+1, npes) )
  !print *, "PE ",pe, ": a8=", a8(1,1,1)," b8=", b8(1,1,1)

  call mpp_sync_self()
  call mpp_sync()

  end subroutine test_mpp_transmit_3D

  subroutine test_mpp_transmit_4D()

  real(kind=r4_kind), dimension(1,1,1,1) :: a4, b4
  real(kind=r8_kind), dimension(1,1,1,1) :: a8, b8

  n=1
  a4 = real(pe, kind=r4_kind)
  a8 = real(pe, kind=r8_kind)
  b4 = real(pe+5, kind=r4_kind)
  b8 = real(pe+5, kind=r8_kind)
  !print *, "PE ",pe, ": a4=", a4(1,1,1,1)," b4=", b4(1,1,1,1)
  call mpp_sync()

  call mpp_transmit( put_data=a4(1,1,1,1), plen=n, to_pe=modulo(pe+npes-1,npes), &
                     get_data=b4(1,1,1,1), glen=n, from_pe=modulo(pe+1, npes) )
  call mpp_sync_self()
  call mpp_sync()
  !print *, "PE ",pe, ": a4=", a4(1,1,1,1)," b4=", b4(1,1,1,1)

  !print *, "PE ",pe, ": a8=", a8(1,1,1,1)," b8=", b8(1,1,1,1) 
  call mpp_transmit( put_data=a8(1,1,1,1), plen=n, to_pe=modulo(pe+npes-1,npes), &
                     get_data=b8(1,1,1,1), glen=n, from_pe=modulo(pe+1, npes) )
  !print *, "PE ",pe, ": a8=", a8(1,1,1,1)," b8=", b8(1,1,1,1)
  call mpp_sync_self()
  call mpp_sync()

  end subroutine test_mpp_transmit_4D

  subroutine test_mpp_transmit_5D()

  real(kind=r4_kind), dimension(1,1,1,1,1) :: a4, b4
  real(kind=r8_kind), dimension(1,1,1,1,1) :: a8, b8

  n=1
  a4 = real(pe, kind=r4_kind)
  a8 = real(pe, kind=r8_kind)
  b4 = real(pe+5, kind=r4_kind)
  b8 = real(pe+5, kind=r8_kind)
  !print *, "PE ",pe, ": a4=", a4(1,1,1,1,1)," b4=", b4(1,1,1,1,1)
  call mpp_sync()

  call mpp_transmit( put_data=a4(1,1,1,1,1), plen=n, to_pe=modulo(pe+npes-1,npes), &
                     get_data=b4(1,1,1,1,1), glen=n, from_pe=modulo(pe+1, npes) )
  call mpp_sync_self()
  call mpp_sync()
  !print *, "PE ",pe, ": a4=", a4(1,1,1,1,1)," b4=", b4(1,1,1,1,1)

  !print *, "PE ",pe, ": a8=", a8(1,1,1,1,1)," b8=", b8(1,1,1,1,1) 
  call mpp_transmit( put_data=a8(1,1,1,1,1), plen=n, to_pe=modulo(pe+npes-1,npes), &
                     get_data=b8(1,1,1,1,1), glen=n, from_pe=modulo(pe+1, npes) )
  !print *, "PE ",pe, ": a8=", a8(1,1,1,1,1)," b8=", b8(1,1,1,1,1)
  call mpp_sync_self()
  call mpp_sync()

  end subroutine test_mpp_transmit_5D
end program test_mpp_transmit
