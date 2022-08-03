module simple_commander_atomic_displacement
include 'simple_lib.f08'
use simple_cmdline,            only: cmdline
use simple_parameters,         only: parameters, params_glob
use simple_commander_base,     only: commander_base
use simple_image,              only: image
use simple_commander_imgproc,  only: scale_commander
use simple_commander_atoms,    only: detect_atoms_commander, atoms_mask_commander
use simple_atoms,              only: atoms
use simple_nanoparticle
use simple_nanoparticle_utils, only: atoms_mask

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
        type(detect_atoms_commander)  :: xdetect_atms
        type(cmdline)                 :: cline_scale, cline_detect_atms
        real                          :: smpd
        integer                       :: box




    end subroutine exec_atomic_displacement

end module simple_commander_atomic_displacement
