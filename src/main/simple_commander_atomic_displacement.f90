module simple_commander_atomic_displacement
include 'simple_lib.f08'
! pls remove/add what is needed and what is not
use simple_cmdline,            only: cmdline
use simple_parameters,         only: parameters, params_glob
use simple_commander_base,     only: commander_base
use simple_image,              only: image
use simple_nanoparticle


implicit none

public :: atomic_displacement_commander

private
#include "simple_local_flags.inc"

type, extends(commander_base) :: atomic_displacement_commander
  contains
    procedure :: execute      => exec_atomic_displacement
end type atomic_displacement_commander

contains

    subroutine exec_atomic_displacement( self, cline )
        class(atomic_displacement_commander), intent(inout) :: self
        class(cmdline),                       intent(inout) :: cline
        class(parameters), pointer    :: params_ptr => null()
        type(parameters)              :: params
!        type(cmdline)                 :: 
        real                          :: smpd
        integer                       :: box

! read volume in image class
! call vol%new(ldim, smpd)
! call vol%read(fname_vol)

! FT volume
! vol%img2ft

! 2x pad volume
! ft_vol%pad_fft
! ft back
! ft_vol%ft2img


! extract atoms with a mask - have no clue what to use to do it pls comment, my guess:
! identify_atomic_pos or identify_lattice_params ???
! find_centers
! how do I extract from a volume not pdb and with a mask??? Found routines only for extracting and masking from pdb files, but would guess something like this:
! masked_atom  = (self%theoretical_radius * XXX)
! winsz   = ceiling(masked_atom)
! npix = (2 * winsz + 1)**3 ! cubic window size (-winsz:winsz in each dim)
! next??????


! how do I assemble it back into a volume to check if all is healthy???




    end subroutine exec_atomic_displacement

end module simple_commander_atomic_displacement
