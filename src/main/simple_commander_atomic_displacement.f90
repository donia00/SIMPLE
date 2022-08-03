module simple_commander_atomic_displacement
include 'simple_lib.f08'
! use simple_builder,          only: builder
! use simple_oris,             only: oris
! use simple_parameters,       only: parameters, params_glob
! use simple_sp_project,       only: sp_project
! use simple_image,            only: image
! use simple_binimage,         only: binimage
! use simple_qsys_env,         only: qsys_env
! use simple_stack_io,         only: stack_io
! use simple_qsys_funs
! use simple_binoris_io
! use simple_ori,               only: ori
! use simple_commander_volops,  only: reproject_commander


use simple_cmdline,            only: cmdline
use simple_parameters,         only: parameters, params_glob
use simple_commander_base,     only: commander_base
use simple_commander_imgproc,  only: scale_commander
use simple_commander_atoms,    only: detect_atoms_commander, atoms_mask_commander
use simple_commander_rec,      only: reconstruct3D_commander, reconstruct3D_commander_distr
use simple_atoms,              only: atoms
use simple_nanoparticle
use simple_nanoparticle_utils, only: atoms_mask, read_pdb2matrix, write_matrix2pdb

implicit none

public :: atomic_displacement_commander

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
    type(cmdline)                 :: cline_scale, cline_detect_atms, cline_reconstruct3D
    real                          :: smpd



end subroutine exec_atomic_displacement
