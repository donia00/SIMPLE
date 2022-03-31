module simple_nanoparticle
!$ use omp_lib
!$ use omp_lib_kinds
include 'simple_lib.f08'
use simple_image,      only: image
use simple_binimage,   only: binimage
use simple_atoms,      only: atoms
use simple_parameters, only: params_glob
use simple_nanoparticle_utils
implicit none

public :: nanoparticle
private
#include "simple_local_flags.inc"

! module global constants
real,             parameter :: CORR_THRES_SIGMA    = -3.0    ! sigma for valid_corr thresholding
integer,          parameter :: NBIN_THRESH         = 15      ! number of thresholds for binarization
integer,          parameter :: CN_THRESH_XTAL      = 5       ! cn-threshold highly crystalline NPs
integer,          parameter :: NVOX_THRESH         = 3       ! min # voxels per atom is 3
logical,          parameter :: DEBUG               = .false. ! for debugging purposes
logical,          parameter :: GENERATE_FIGS       = .false. ! for figures generation
integer,          parameter :: SOFT_EDGE           = 6
integer,          parameter :: N_DISCRET           = 1000
integer,          parameter :: CNMIN               = 3
integer,          parameter :: CNMAX               = 12
integer,          parameter :: NSTRAIN_COMPS       = 7
character(len=*), parameter :: ATOMS_STATS_FILE    = 'atoms_stats.csv'
character(len=*), parameter :: NP_STATS_FILE       = 'nanoparticle_stats.csv'
character(len=*), parameter :: CN_STATS_FILE       = 'cn_dependent_stats.csv'
character(len=*), parameter :: ATOM_VAR_CORRS_FILE = 'atom_param_corrs.txt'
character(len=*), parameter :: BIN_CLS_STATS_FILE  = 'binary_class_stats.csv'

character(len=*), parameter :: ATOM_STATS_HEAD = 'INDEX'//CSV_DELIM//'NVOX'//CSV_DELIM//'CN_STD'//CSV_DELIM//'NN_BONDL'//&
&CSV_DELIM//'CN_GEN'//CSV_DELIM//'DIAM'//CSV_DELIM//'AVG_INT'//&
&CSV_DELIM//'MAX_INT'//CSV_DELIM//'CENDIST'//CSV_DELIM//'VALID_CORR'//CSV_DELIM//'X'//&
&CSV_DELIM//'Y'//CSV_DELIM//'Z'//CSV_DELIM//'EXX_STRAIN'//CSV_DELIM//'EYY_STRAIN'//&
&CSV_DELIM//'EZZ_STRAIN'//CSV_DELIM//'EXY_STRAIN'//CSV_DELIM//'EYZ_STRAIN'//CSV_DELIM//'EXZ_STRAIN'//CSV_DELIM//'RADIAL_STRAIN'//&
&CSV_DELIM//'NVOX_CLASS'//CSV_DELIM//'NN_BONDL_CLASS'//&
&CSV_DELIM//'DIAM_CLASS'//CSV_DELIM//'MAX_INT_CLASS'//CSV_DELIM//'VALID_CORR_CLASS'//CSV_DELIM//'RADIAL_STRAIN_CLASS'

character(len=*), parameter :: ATOM_STATS_HEAD_OMIT = 'INDEX'//CSV_DELIM//'NVOX'//CSV_DELIM//'CN_STD'//CSV_DELIM//'NN_BONDL'//&
&CSV_DELIM//'CN_GEN'//CSV_DELIM//'DIAM'//CSV_DELIM//'AVG_INT'//&
&CSV_DELIM//'MAX_INT'//CSV_DELIM//'CENDIST'//CSV_DELIM//'VALID_CORR'//CSV_DELIM//'RADIAL_STRAIN'

character(len=*), parameter :: NP_STATS_HEAD = 'NATOMS'//CSV_DELIM//'DIAM'//&
&CSV_DELIM//'AVG_NVOX'//CSV_DELIM//'MED_NVOX'//CSV_DELIM//'SDEV_NVOX'//&
&CSV_DELIM//'AVG_CN_STD'//CSV_DELIM//'MED_CN_STD'//CSV_DELIM//'SDEV_CN_STD'//&
&CSV_DELIM//'AVG_NN_BONDL'//CSV_DELIM//'MED_NN_BONDL'//CSV_DELIM//'SDEV_NN_BONDL'//&
&CSV_DELIM//'AVG_CN_GEN'//CSV_DELIM//'MED_CN_GEN'//CSV_DELIM//'SDEV_CN_GEN'//&
&CSV_DELIM//'AVG_DIAM'//CSV_DELIM//'MED_DIAM'//CSV_DELIM//'SDEV_DIAM'//&
&CSV_DELIM//'AVG_AVG_INT'//CSV_DELIM//'MED_AVG_INT'//CSV_DELIM//'SDEV_AVG_INT'//&
&CSV_DELIM//'AVG_MAX_INT'//CSV_DELIM//'MED_MAX_INT'//CSV_DELIM//'SDEV_MAX_INT'//&
&CSV_DELIM//'AVG_VALID_CORR'//CSV_DELIM//'MED_VALID_CORR'//CSV_DELIM//'SDEV_VALID_CORR'//&
&CSV_DELIM//'AVG_RADIAL_STRAIN'//CSV_DELIM//'MED_RADIAL_STRAIN'//CSV_DELIM//'SDEV_RADIAL_STRAIN'//&
&CSV_DELIM//'MIN_RADIAL_STRAIN'//CSV_DELIM//'MAX_RADIAL_STRAIN'

character(len=*), parameter :: CN_STATS_HEAD = 'CN_STD'//CSV_DELIM//'NATOMS'//&
&CSV_DELIM//'AVG_NVOX'//CSV_DELIM//'MED_NVOX'//CSV_DELIM//'SDEV_NVOX'//&
&CSV_DELIM//'AVG_NN_BONDL'//CSV_DELIM//'MED_NN_BONDL'//CSV_DELIM//'SDEV_NN_BONDL'//&
&CSV_DELIM//'AVG_CN_GEN'//CSV_DELIM//'MED_CN_GEN'//CSV_DELIM//'SDEV_CN_GEN'//&
&CSV_DELIM//'AVG_DIAM'//CSV_DELIM//'MED_DIAM'//CSV_DELIM//'SDEV_DIAM'//&
&CSV_DELIM//'AVG_AVG_INT'//CSV_DELIM//'MED_AVG_INT'//CSV_DELIM//'SDEV_AVG_INT'//&
&CSV_DELIM//'AVG_MAX_INT'//CSV_DELIM//'MED_MAX_INT'//CSV_DELIM//'SDEV_MAX_INT'//&
&CSV_DELIM//'AVG_VALID_CORR'//CSV_DELIM//'MED_VALID_CORR'//CSV_DELIM//'SDEV_VALID_CORR'//&
&CSV_DELIM//'AVG_RADIAL_STRAIN'//CSV_DELIM//'MED_RADIAL_STRAIN'//CSV_DELIM//'SDEV_RADIAL_STRAIN'//&
&CSV_DELIM//'MIN_RADIAL_STRAIN'//CSV_DELIM//'MAX_RADIAL_STRAIN'

character(len=*), parameter :: BIN_CLS_STATS_HEAD = 'PARAMETER'//&
&CSV_DELIM//'AVG_1'//CSV_DELIM//'MED_1'//CSV_DELIM//'SDEV_1'//CSV_DELIM//'MIN_1'//CSV_DELIM//'MAX_1'//CSV_DELIM//'POP_1'//&
&CSV_DELIM//'AVG_2'//CSV_DELIM//'MED_2'//CSV_DELIM//'SDEV_2'//CSV_DELIM//'MIN_2'//CSV_DELIM//'MAX_2'//CSV_DELIM//'POP_2'

! container for per-atom statistics
type :: atom_stats
    ! various per-atom parameters
    integer :: cc_ind            = 0  ! index of the connected component                            INDEX
    integer :: size              = 0  ! number of voxels in connected component                     NVOX
    integer :: cn_std            = 0  ! standard coordination number                                CN_STD
    real    :: bondl             = 0. ! nearest neighbour bond lenght in A                          NN_BONDL
    real    :: cn_gen            = 0. ! generalized coordination number                             CN_GEN
    real    :: diam              = 0. ! atom diameter                                               DIAM
    real    :: avg_int           = 0. ! average grey level intensity across the connected component AVG_INT
    real    :: max_int           = 0. ! maximum            -"-                                      MAX_INT
    real    :: cendist           = 0. ! distance from the centre of mass of the nanoparticle        CENDIST
    real    :: valid_corr        = 0. ! per-atom correlation with the simulated map                 VALID_CORR
    real    :: center(3)         = 0. ! atom center                                                 X Y Z
    ! strain
    real    :: exx_strain        = 0. ! tensile strain in %                                         EXX_STRAIN
    real    :: eyy_strain        = 0. ! -"-                                                         EYY_STRAIN
    real    :: ezz_strain        = 0. ! -"-                                                         EZZ_STRAIN
    real    :: exy_strain        = 0. ! -"-                                                         EXY_STRAIN
    real    :: eyz_strain        = 0. ! -"-                                                         EYZ_STRAIN
    real    :: exz_strain        = 0. ! -"-                                                         EXZ_STRAIN
    real    :: radial_strain     = 0. ! -"-                                                         RADIAL_STRAIN
    ! binary cluster assignments
    integer :: size_cls          = 0  !                                                             NVOX_CLASS
    integer :: bondl_cls         = 0  !                                                             NN_BONDL_CLASS
    integer :: diam_cls          = 0  !                                                             DIAM_CLASS
    integer :: max_int_cls       = 0  !                                                             MAX_INT_CLASS
    integer :: valid_corr_cls    = 0  !                                                             VALID_CORR_CLASS
    integer :: radial_strain_cls = 0  !                                                             RADIAL_STRAIN_CLASS
end type atom_stats

type :: nanoparticle
    private
    type(image)           :: img, img_raw
    type(binimage)        :: img_bin, img_cc
    integer               :: ldim(3)            = 0  ! logical dimension of image
    integer               :: n_cc               = 0  ! number of atoms (connected components)                NATOMS
    integer               :: n4stats            = 0  ! number of atoms in subset used for stats calc
    real                  :: smpd               = 0. ! sampling distance
    real                  :: NPcen(3)           = 0. ! coordinates of the center of mass of the nanoparticle
    real                  :: NPdiam             = 0. ! diameter of the nanoparticle                          DIAM
    real                  :: theoretical_radius = 0. ! theoretical atom radius in A
    ! GLOBAL NP STATS
    type(stats_struct)    :: map_stats
    ! -- the rest
    type(stats_struct)    :: size_stats
    type(stats_struct)    :: cn_std_stats
    type(stats_struct)    :: bondl_stats
    type(stats_struct)    :: cn_gen_stats
    type(stats_struct)    :: diam_stats
    type(stats_struct)    :: avg_int_stats
    type(stats_struct)    :: max_int_stats
    type(stats_struct)    :: valid_corr_stats
    type(stats_struct)    :: radial_strain_stats
    ! CN-DEPENDENT STATS
    ! -- # atoms
    real                  :: natoms_cns(CNMIN:CNMAX) = 0. ! # of atoms per cn_std                            NATOMS
    ! -- the rest
    type(stats_struct)    :: size_stats_cns(CNMIN:CNMAX)
    type(stats_struct)    :: bondl_stats_cns(CNMIN:CNMAX)
    type(stats_struct)    :: cn_gen_stats_cns(CNMIN:CNMAX)
    type(stats_struct)    :: diam_stats_cns(CNMIN:CNMAX)
    type(stats_struct)    :: avg_int_stats_cns(CNMIN:CNMAX)
    type(stats_struct)    :: max_int_stats_cns(CNMIN:CNMAX)
    type(stats_struct)    :: valid_corr_stats_cns(CNMIN:CNMAX)
    type(stats_struct)    :: radial_strain_stats_cns(CNMIN:CNMAX)
    ! PER-ATOM STATISTICS
    type(atom_stats), allocatable :: atominfo(:)
    real,             allocatable :: coords4stats(:,:)
    ! BINARY CLASS STATISTICS
    type(stats_struct)    :: size_cls_stats(2)
    type(stats_struct)    :: bondl_cls_stats(2)
    type(stats_struct)    :: diam_cls_stats(2)
    type(stats_struct)    :: max_int_cls_stats(2)
    type(stats_struct)    :: valid_corr_cls_stats(2)
    type(stats_struct)    :: radial_strain_cls_stats(2)
    ! OTHER
    character(len=2)      :: element     = ' '
    character(len=4)      :: atom_name   = '    '
    character(len=STDLEN) :: npname    = '' ! fname
    character(len=STDLEN) :: fbody       = '' ! fbody
  contains
    ! constructor
    procedure          :: new => new_nanoparticle
    ! getters/setters
    procedure          :: get_ldim
    procedure          :: get_natoms
    procedure          :: set_img
    procedure          :: set_atomic_coords
    procedure          :: set_coords4stats
    procedure, private :: pack_instance4stats
    ! utils
    procedure, private :: atominfo2centers
    procedure, private :: atominfo2centers_A
    procedure, private :: center_on_atom
    procedure          :: update_ncc
    ! atomic position determination
    procedure          :: identify_lattice_params
    procedure          :: identify_atomic_pos
    procedure, private :: binarize_and_find_centers
    procedure, private :: find_centers
    procedure, private :: discard_atoms_with_low_contact_score
    procedure, private :: discard_lowly_coordinated
    procedure, private :: discard_low_valid_corr_atoms
    procedure, private :: split_atoms
    procedure, private :: validate_atoms
    ! calc stats
    procedure          :: fillin_atominfo
    procedure, private :: masscen
    procedure, private :: calc_longest_atm_dist
    ! visualization and output
    procedure, private :: simulate_atoms
    procedure, private :: write_centers_1
    procedure, private :: write_centers_2
    generic            :: write_centers => write_centers_1, write_centers_2
    procedure          :: write_individual_atoms
    procedure          :: write_csv_files
    procedure, private :: write_atominfo
    procedure, private :: write_np_stats
    procedure, private :: write_cn_stats
    ! clustering
    procedure, private :: id_corr_vars
    procedure, private :: bicluster_otsu
    procedure, private :: cluster_atom_intensity
    procedure          :: cluster_atom_maxint
    procedure          :: cluster_atom_intint
    procedure          :: cluster_bondl
    ! kill
    procedure          :: kill => kill_nanoparticle
end type nanoparticle

contains

    subroutine new_nanoparticle( self, fname, cline_smpd, element, msk )
        class(nanoparticle), intent(inout) :: self
        character(len=*),    intent(in)    :: fname
        real,                intent(in)    :: cline_smpd
        character(len=2),    intent(inout) :: element
        real, optional,      intent(in)    :: msk
        character(len=2) :: el_ucase
        integer :: nptcls
        integer :: Z ! atomic number
        real    :: smpd
        call self%kill
        self%npname    = fname
        self%fbody     = get_fbody(trim(basename(fname)), trim(fname2ext(fname)))
        self%smpd      = cline_smpd
        self%atom_name = ' '//element
        self%element   = element
        el_ucase       = upperCase(trim(adjustl(element)))
        call get_element_Z_and_radius(el_ucase, Z, self%theoretical_radius)
        if( Z == 0 ) THROW_HARD('Unknown element: '//el_ucase)
        call find_ldim_nptcls(self%npname, self%ldim, nptcls, smpd)
        call self%img%new(self%ldim, self%smpd)
        call self%img_raw%new(self%ldim, self%smpd)
        call self%img_bin%new_bimg(self%ldim, self%smpd)
        call self%img_bin%new(self%ldim, self%smpd)
        call self%img%read(fname)
        if( present(msk) ) call self%img%mask(msk, 'soft')
        call self%img_raw%copy(self%img)
        call self%img_raw%stats(self%map_stats%avg, self%map_stats%sdev, self%map_stats%maxv, self%map_stats%minv)
    end subroutine new_nanoparticle

    ! getters/setters

    subroutine get_ldim(self,ldim)
        class(nanoparticle), intent(in)  :: self
        integer,             intent(out) :: ldim(3)
        ldim = self%img%get_ldim()
    end subroutine get_ldim

    function get_natoms(self) result(n)
       class(nanoparticle), intent(inout)  :: self
       integer :: n
       call self%img_cc%get_nccs(n)
    end function get_natoms

    ! set one of the images of the nanoparticle type
    subroutine set_img( self, imgfile, which )
        class(nanoparticle), intent(inout) :: self
        character(len=*),    intent(in)    :: imgfile
        character(len=*),    intent(in)    :: which
        select case(which)
            case('img')
                call self%img%new(self%ldim, self%smpd)
                call self%img%read(imgfile)
            case('img_bin')
                call self%img_bin%new_bimg(self%ldim, self%smpd)
                call self%img_bin%read_bimg(imgfile)
            case('img_cc')
                call self%img_cc%new_bimg(self%ldim, self%smpd)
                call self%img_cc%read_bimg(imgfile)
            case DEFAULT
                THROW_HARD('Wrong input parameter img type (which); set_img')
        end select
    end subroutine set_img

    ! sets the atom positions to be the ones in the inputted PDB file.
    subroutine set_atomic_coords( self, pdb_file )
        class(nanoparticle), intent(inout) :: self
        character(len=*),    intent(in)    :: pdb_file
        type(atoms) :: a
        integer     :: i, N
        if( fname2ext(pdb_file) .ne. 'pdb' ) THROW_HARD('Inputted filename has to have pdb extension; set_atomic_coords')
        if( allocated(self%atominfo) ) deallocate(self%atominfo)
        call a%new(pdb_file)
        N = a%get_n() ! number of atoms
        allocate(self%atominfo(N))
        do i = 1, N
            self%atominfo(i)%center(:) = a%get_coord(i)/self%smpd + 1.
        enddo
        self%n_cc = N
        call a%kill
    end subroutine set_atomic_coords

    subroutine set_coords4stats( self, pdb_file )
        class(nanoparticle), intent(inout) :: self
        character(len=*),    intent(in)    :: pdb_file
        call read_pdb2matrix(pdb_file, self%coords4stats)
        self%n4stats = size(self%coords4stats, dim=2)
    end subroutine set_coords4stats

    subroutine pack_instance4stats( self, strain_array )
        class(nanoparticle), intent(inout) :: self
        real, allocatable,   intent(inout) :: strain_array(:,:)
        real,                allocatable   :: centers_A(:,:), strain_array_new(:,:)
        logical,             allocatable   :: mask(:)
        integer,             allocatable   :: imat_cc(:,:,:), imat_cc_new(:,:,:), imat_bin_new(:,:,:)
        type(atom_stats),    allocatable   :: atominfo_new(:)
        integer :: n_cc_orig, cc, cnt, nx, ny, nz
        if( .not. allocated(self%coords4stats) ) return
        centers_A = self%atominfo2centers_A()
        n_cc_orig = size(centers_A, dim=2)
        allocate(mask(n_cc_orig), source=.false.)
        call find_atoms_subset(self%coords4stats, centers_A, mask)
        ! remove atoms not in mask
        if( n_cc_orig /= self%n_cc ) THROW_HARD('incongruent # cc:s')
        ! (1) update img_cc & img_bin
        call self%img_cc%get_imat(imat_cc)
        nx = size(imat_cc, dim=1)
        ny = size(imat_cc, dim=2)
        nz = size(imat_cc, dim=3)
        allocate(imat_cc_new(nx,ny,nz), imat_bin_new(nx,ny,nz), source=0)
        cnt = 0
        do cc = 1, self%n_cc
            if( mask(cc) )then
                cnt = cnt + 1
                where(imat_cc == cc) imat_cc_new  = cnt
                where(imat_cc == cc) imat_bin_new = 1
            endif
        end do
        call self%img_cc%set_imat(imat_cc_new)
        call self%img_bin%set_imat(imat_bin_new)
        ! (2) update atominfo & strain_array
        allocate(atominfo_new(cnt), strain_array_new(cnt,NSTRAIN_COMPS))
        cnt = 0
        do cc = 1, self%n_cc
            if( mask(cc) )then
                cnt = cnt + 1
                atominfo_new(cnt)       = self%atominfo(cc)
                strain_array_new(cnt,:) = strain_array(cc,:)
            endif
        end do
        deallocate(self%atominfo, strain_array)
        allocate(self%atominfo(cnt), source=atominfo_new)
        allocate(strain_array(cnt,NSTRAIN_COMPS), source=strain_array_new)
        deallocate(centers_A, mask, imat_cc, imat_cc_new, imat_bin_new, atominfo_new)
        ! (3) update number of connected components
        self%n_cc = cnt
    end subroutine pack_instance4stats

    ! utils

    function atominfo2centers( self ) result( centers )
        class(nanoparticle), intent(in) :: self
        real, allocatable :: centers(:,:)
        integer :: sz, i
        sz = size(self%atominfo)
        allocate(centers(3,sz), source=0.)
        do i = 1, sz
            centers(:,i) = self%atominfo(i)%center(:)
        end do
    end function atominfo2centers

    function atominfo2centers_A( self ) result( centers_A )
        class(nanoparticle), intent(in) :: self
        real, allocatable :: centers_A(:,:)
        integer :: sz, i
        sz = size(self%atominfo)
        allocate(centers_A(3,sz), source=0.)
        do i = 1, sz
            centers_A(:,i) = (self%atominfo(i)%center(:) - 1.) * self%smpd
        end do
    end function atominfo2centers_A

    ! Translate the identified atomic positions so that the center of mass
    ! of the nanoparticle coincides with its closest atom
    subroutine center_on_atom( self, pdbfile_in, pdbfile_out )
        class(nanoparticle), intent(inout) :: self
        character(len=*),    intent(in)    :: pdbfile_in
        character(len=*),    intent(inout) :: pdbfile_out
        type(atoms) :: atom_centers
        real        :: m(3), vec(3), d, d_before
        integer     :: i
        call atom_centers%new(pdbfile_in)
        m(:)     = self%masscen()
        d_before = huge(d_before)
        do i = 1, self%n_cc
            d = euclid(m,self%atominfo(i)%center(:))
            if(d < d_before) then
                vec(:)   = m(:) - self%atominfo(i)%center(:)
                d_before = d
            endif
        enddo
        do i = 1, self%n_cc
            self%atominfo(i)%center(:) = self%atominfo(i)%center(:) + vec
            call atom_centers%set_coord(i,(self%atominfo(i)%center(:)-1.)*self%smpd)
        enddo
        call atom_centers%writePDB(pdbfile_out)
        call atom_centers%kill
    end subroutine center_on_atom

    subroutine update_ncc( self, img_cc )
        class(nanoparticle),      intent(inout) :: self
        type(binimage), optional, intent(inout) :: img_cc
        if( present(img_cc) )then
            call img_cc%get_nccs(self%n_cc)
        else
            call self%img_cc%get_nccs(self%n_cc)
        endif
    end subroutine update_ncc

    ! atomic position determination

    subroutine identify_lattice_params( self, a, use_auto_corr_thres )
        class(nanoparticle), intent(inout) :: self
        real,                intent(inout) :: a(3)                ! lattice parameters
        logical,             intent(in)    :: use_auto_corr_thres ! true -> use automatic corr thres
        real, allocatable :: centers_A(:,:)                       ! coordinates of the atoms in ANGSTROMS
        type(image)       :: simatms
        type(atoms)       :: atoms_obj
        ! MODEL BUILDING
        ! Phase correlation approach
        call phasecorr_one_atom(self%img, self%img, self%element)
        ! Nanoparticle binarization
        call self%binarize_and_find_centers()
        ! atom splitting by correlation map validation
        call self%split_atoms()
        ! OUTLIERS DISCARDING
        ! validation through per-atom correlation with the simulated density
        call self%simulate_atoms(atoms_obj, simatms)
        call self%validate_atoms( simatms )
        ! discard atoms with low valid_corr
        call self%discard_low_valid_corr_atoms(use_auto_corr_thres)
        ! fit lattice
        centers_A = self%atominfo2centers_A()
        call fit_lattice(self%element, centers_A, a)
        deallocate(centers_A)
        call simatms%kill
        call atoms_obj%kill
    end subroutine identify_lattice_params

    subroutine identify_atomic_pos( self, a, l_fit_lattice, use_cs_thres, use_auto_corr_thres, cs_thres )
        class(nanoparticle), intent(inout) :: self
        real,                intent(inout) :: a(3)                ! lattice parameters
        logical,             intent(in)    :: l_fit_lattice       ! fit lattice or use inputted
        logical,             intent(inout) :: use_cs_thres        ! use or not contact score thres
        logical,             intent(in)    :: use_auto_corr_thres ! true -> use automatic corr thres
        integer, optional,   intent(in)    :: cs_thres
        logical     :: use_cn_thresh, fixed_cs_thres
        type(image) :: simatms, img_cos
        type(atoms) :: atoms_obj

        print *, 'use_cs_thres ', use_cs_thres

        ! MODEL BUILDING
        ! Phase correlation approach
        call phasecorr_one_atom(self%img, self%img, self%element)
        ! Nanoparticle binarization
        call self%binarize_and_find_centers()
        ! atom splitting by correlation map validation
        call self%split_atoms()
        ! OUTLIERS DISCARDING
        ! validation through per-atom correlation with the simulated density
        call self%simulate_atoms(atoms_obj, simatms)
        call self%validate_atoms( simatms )
        ! discard atoms with low valid_corr
        call self%discard_low_valid_corr_atoms(use_auto_corr_thres)
        ! discard lowly coordinated atoms
        fixed_cs_thres = present(cs_thres)
        if( fixed_cs_thres )then
            call self%discard_atoms_with_low_contact_score(use_cn_thresh, cs_thres)
        else if( use_cs_thres )then
            call self%discard_atoms_with_low_contact_score(use_cn_thresh)
            if( use_cn_thresh ) call self%discard_lowly_coordinated(CN_THRESH_XTAL, a, l_fit_lattice)
        endif
        ! WRITE OUTPUT
        call self%img_bin%write_bimg(trim(self%fbody)//'_BIN.mrc')
        write(logfhandle,'(A)') 'output, binarized map:            '//trim(self%fbody)//'_BIN.mrc'
        call self%img_bin%grow_bins(1)
        call self%img_bin%cos_edge(SOFT_EDGE, img_cos)
        call img_cos%write(trim(self%fbody)//'_MSK.mrc')
        write(logfhandle,'(A)') 'output, envelope mask map:        '//trim(self%fbody)//'_MSK.mrc'
        call self%img_cc%write_bimg(trim(self%fbody)//'_CC.mrc')
        write(logfhandle,'(A)') 'output, connected components map: '//trim(self%fbody)//'_CC.mrc'
        call self%write_centers
        call self%simulate_atoms(atoms_obj, simatms)
        call simatms%write(trim(self%fbody)//'_SIM.mrc')
        write(logfhandle,'(A)') 'output, simulated atomic density: '//trim(self%fbody)//'_SIM.mrc'
        ! destruct
        call img_cos%kill
        call simatms%kill
        call atoms_obj%kill
    end subroutine identify_atomic_pos

    ! This subrotuine takes in input a nanoparticle and
    ! binarizes it by thresholding. The gray level histogram is split
    ! in 20 parts, which corrispond to 20 possible threshold.
    ! Among those threshold, the selected one is the for which
    ! tha correlation between the raw map and a simulated distribution
    ! obtained with that threshold reaches the maximum value.
    subroutine binarize_and_find_centers( self )
        class(nanoparticle), intent(inout) :: self
        type(binimage)       :: img_bin_t
        type(binimage)       :: img_ccs_t
        type(image)          :: pc
        type(atoms)          :: atom
        type(image)          :: simulated_distrib
        integer, allocatable :: imat_t(:,:,:)
        real,    allocatable :: x_mat(:)  ! vectorization of the volume
        real,    allocatable :: coords(:,:)
        real,    allocatable :: rmat(:,:,:)
        integer :: i, fnr
        real    :: otsu_thresh, corr, step, step_refine, max_corr, thresh, thresh_opt, lbt, rbt
        logical, parameter      :: L_BENCH = .false.
        real(timer_int_kind)    :: rt_find_ccs, rt_find_centers, rt_gen_sim, rt_real_corr, rt_tot
        integer(timer_int_kind) ::  t_find_ccs,  t_find_centers,  t_gen_sim,  t_real_corr,  t_tot
        call otsu_nano(self%img,otsu_thresh) ! find initial threshold
        write(logfhandle,'(A)') '>>> BINARIZATION'
        rmat = self%img%get_rmat()
        x_mat = pack(rmat(:self%ldim(1),:self%ldim(2),:self%ldim(3)),&
                    &rmat(:self%ldim(1),:self%ldim(2),:self%ldim(3)) >= 0.)
        allocate(imat_t(self%ldim(1), self%ldim(2), self%ldim(3)), source = 0)
        step = (maxval(x_mat)-otsu_thresh )/real(NBIN_THRESH)
        step_refine = step / 6.
        deallocate(x_mat)
        call simulated_distrib%new(self%ldim,self%smpd)
        rt_find_ccs     =  0.
        rt_find_centers =  0.
        rt_gen_sim      =  0.
        rt_real_corr    =  0.
        t_tot           =  tic()
        max_corr        = -1.
        ! discrete search
        do i = 1, NBIN_THRESH
            if( i == 1 )then
                thresh = otsu_thresh
            else
                thresh = thresh + step
            endif
            corr = t2c( thresh )
            write(logfhandle,*) 'threshold: ', thresh , 'corr: ', corr
            if( corr > max_corr )then
                max_corr  = corr
                thresh_opt = thresh
            endif
        enddo
        ! refinement
        lbt    = thresh_opt - step + step_refine
        rbt    = thresh_opt + step - step_refine
        thresh = lbt
        i      = NBIN_THRESH
        do while( thresh <= rbt )
            i = i + 1
            corr = t2c( thresh )
            write(logfhandle,*) 'threshold: ', thresh, 'corr: ', corr
            if( corr > max_corr )then
                max_corr  = corr
                thresh_opt = thresh
            endif
            thresh = thresh + step_refine
        end do
        rt_tot = toc(t_tot)
        if( L_BENCH )then
            call fopen(fnr, FILE='BINARIZE_AND_FIND_CENTERS_BENCH.txt', STATUS='REPLACE', action='WRITE')
            write(fnr,'(a)') '*** TIMINGS (s) ***'
            write(fnr,'(a,1x,f9.2)') 'find_ccs       : ', rt_find_ccs
            write(fnr,'(a,1x,f9.2)') 'find_centers   : ', rt_find_centers
            write(fnr,'(a,1x,f9.2)') 'gen_sim        : ', rt_gen_sim
            write(fnr,'(a,1x,f9.2)') 'real_corr      : ', rt_real_corr
            write(fnr,'(a,1x,f9.2)') 'total time     : ', rt_tot
            write(fnr,'(a)') ''
            write(fnr,'(a)') '*** RELATIVE TIMINGS (%) ***'
            write(fnr,'(a,1x,f9.2)') 'find_ccs       : ', (rt_find_ccs/rt_tot)     * 100.
            write(fnr,'(a,1x,f9.2)') 'find_centers   : ', (rt_find_centers/rt_tot) * 100.
            write(fnr,'(a,1x,f9.2)') 'gen_sim        : ', (rt_gen_sim/rt_tot)      * 100.
            write(fnr,'(a,1x,f9.2)') 'real_corr      : ', (rt_real_corr/rt_tot)    * 100.
            write(fnr,'(a,1x,f9.2)') 'total time     : ', rt_tot
            write(fnr,'(a,1x,f9.2)') '% accounted for: ',&
            &((rt_find_ccs+rt_find_centers+rt_gen_sim+rt_real_corr)/rt_tot)     * 100.
            call fclose(fnr)
        endif
        write(logfhandle,*) 'optimal threshold: ', thresh_opt, 'max_corr: ', max_corr
        ! Update img_bin and img_cc
        corr = t2c( thresh_opt )
        call self%img_bin%copy_bimg(img_bin_t)
        call self%img_cc%copy_bimg(img_ccs_t)
        call self%update_ncc()
        call self%find_centers()
        call img_bin_t%kill_bimg
        call img_ccs_t%kill_bimg
        ! deallocate and kill
        if(allocated(rmat))   deallocate(rmat)
        if(allocated(imat_t)) deallocate(imat_t)
        if(allocated(coords)) deallocate(coords)
        call simulated_distrib%kill
        write(logfhandle,'(A)') '>>> BINARIZATION, COMPLETED'

    contains

         ! Otsu binarization for nanoparticle maps
         ! It considers the grey level value only in the positive range.
         ! It doesn't threshold the map. It just returns the ideal threshold.
         ! This is based on the implementation of 1D otsu
         subroutine otsu_nano(img, scaled_thresh)
             use simple_math, only : otsu
             type(image),    intent(inout) :: img
             real,           intent(out)   :: scaled_thresh ! returns the threshold in the correct range
             real, pointer     :: rmat(:,:,:)
             real, allocatable :: x(:)
             call img%get_rmat_ptr(rmat)
             x = pack(rmat(:self%ldim(1),:self%ldim(2),:self%ldim(3)),&
                     &rmat(:self%ldim(1),:self%ldim(2),:self%ldim(3)) > 0.)
             call otsu(x, scaled_thresh)
         end subroutine otsu_nano

         real function t2c( thres )
             real, intent(in) :: thres
             where(rmat > thres)
                 imat_t = 1
             elsewhere
                 imat_t = 0
             endwhere
             ! Generate binary image and cc image
             call img_bin_t%new_bimg(self%ldim, self%smpd)
             call img_bin_t%set_imat(imat_t)
             t_find_ccs = tic()
             call img_ccs_t%new_bimg(self%ldim, self%smpd)
             call img_bin_t%find_ccs(img_ccs_t)
             rt_find_ccs = rt_find_ccs + toc(t_find_ccs)
             ! Find atom centers in the generated distributions
             call self%update_ncc(img_ccs_t) ! self%n_cc is needed in find_centers
             t_find_centers = tic()
             call self%find_centers(img_bin_t, img_ccs_t, coords)
             rt_find_centers = rt_find_centers + toc(t_find_centers)
             ! Generate a simulated distribution based on those center
             t_gen_sim = tic()
             call self%write_centers('centers_'//trim(int2str(i))//'_iteration', coords)
             call atom%new          ('centers_'//trim(int2str(i))//'_iteration.pdb')
             call atom%convolve(simulated_distrib, cutoff = 8.*self%smpd)
             call del_file('centers_'//trim(int2str(i))//'_iteration.pdb')
             call atom%kill
             rt_gen_sim = rt_gen_sim + toc(t_gen_sim)
             ! correlate volumes
             t_real_corr = tic()
             t2c = self%img%real_corr(simulated_distrib)
             rt_real_corr = rt_real_corr + toc(t_real_corr)
        end function t2c

    end subroutine binarize_and_find_centers

    ! Find the centers coordinates of the atoms in the particle
    ! and save it in the global variable centers.
    ! If coords is present, it saves it also in coords.
    subroutine find_centers( self, img_bin, img_cc, coords )
       class(nanoparticle),         intent(inout) :: self
       type(binimage), optional,    intent(inout) :: img_bin, img_cc
       real, optional, allocatable, intent(out)   :: coords(:,:)
       real,        pointer :: rmat_raw(:,:,:)
       integer, allocatable :: imat_cc_in(:,:,:)
       logical, allocatable :: mask(:,:,:)
       integer :: i, ii, jj, kk
       real    :: m(3), sum_mass
       ! sanity check
       if( present(img_bin) .and. .not. present(img_cc) ) THROW_HARD('img_bin and img_cc have to be both present')
       ! global variables allocation
       if( allocated(self%atominfo) ) deallocate(self%atominfo)
       allocate( self%atominfo(self%n_cc) )
       if( present(img_cc) )then
           call img_cc%get_imat(imat_cc_in)
       else
           call self%img_cc%get_imat(imat_cc_in)
       endif
       call self%img_raw%get_rmat_ptr(rmat_raw)
       allocate(mask(self%ldim(1),self%ldim(2),self%ldim(3)), source=.true.)
       !$omp parallel do default(shared) private(i,ii,jj,kk,mask,m,sum_mass) schedule(static) proc_bind(close)
       do i=1,self%n_cc
           mask     = .true.
           where( imat_cc_in /= i ) mask = .false.
           m        = 0.
           sum_mass = 0.
           do ii = 1, self%ldim(1)
               do jj = 1, self%ldim(2)
                   do kk = 1, self%ldim(3)
                       if( mask(ii,jj,kk) )then
                           m = m + real([ii,jj,kk]) * rmat_raw(ii,jj,kk)
                           sum_mass = sum_mass + rmat_raw(ii,jj,kk)
                       endif
                   enddo
               enddo
           enddo
           self%atominfo(i)%center(:) = m / sum_mass
       enddo
       !$omp end parallel do
       ! saving centers coordinates, optional
       if( present(coords) )then
           allocate(coords(3,self%n_cc))
           do i=1,self%n_cc
               coords(:,i) = self%atominfo(i)%center(:)
           end do
       endif
    end subroutine find_centers

    subroutine split_atoms( self )
        class(nanoparticle), intent(inout) :: self
        type(binimage)       :: img_split_ccs
        real,    allocatable :: x(:)
        real,    pointer     :: rmat_pc(:,:,:)
        integer, allocatable :: imat(:,:,:), imat_cc(:,:,:), imat_bin(:,:,:), imat_split_ccs(:,:,:)
        integer, parameter   :: RANK_THRESH = 4
        integer :: icc, cnt, cnt_split
        integer :: rank, m(1)
        real    :: new_centers(3,2*self%n_cc) ! will pack it afterwards if it has too many elements
        real    :: pc, radius
        write(logfhandle, '(A)') '>>> SPLITTING CONNECTED ATOMS'
        call self%img%get_rmat_ptr(rmat_pc) ! now img contains the phase correlation
        call self%img_cc%get_imat(imat_cc)  ! to pass to the subroutine split_atoms
        allocate(imat(1:self%ldim(1),1:self%ldim(2),1:self%ldim(3)),           source = imat_cc)
        allocate(imat_split_ccs(1:self%ldim(1),1:self%ldim(2),1:self%ldim(3)), source = 0)
        call img_split_ccs%new_bimg(self%ldim, self%smpd)
        call img_split_ccs%new(self%ldim, self%smpd)
        cnt       = 0
        cnt_split = 0
        do icc = 1, self%n_cc ! for each cc check if the center corresponds with the local max of the phase corr
            pc = rmat_pc(nint(self%atominfo(icc)%center(1)),nint(self%atominfo(icc)%center(2)),nint(self%atominfo(icc)%center(3)))
            ! calculate the rank
            x = pack(rmat_pc(:self%ldim(1),:self%ldim(2),:self%ldim(3)), mask=imat == icc)
            call hpsort(x)
            m(:) = minloc(abs(x - pc))
            rank = size(x) - m(1)
            deallocate(x)
            ! calculate radius
            call self%calc_longest_atm_dist(icc, radius)
            ! split
            if( rank > RANK_THRESH .or. radius > 1.5 * self%theoretical_radius )then
                where(imat == icc)
                    imat_split_ccs = 1
                end where
                cnt_split = cnt_split + 1
                call split_atom(new_centers,cnt)
            else
                cnt = cnt + 1 ! new number of centers derived from splitting
                new_centers(:,cnt) = self%atominfo(icc)%center(:)
            endif
        enddo
        write(logfhandle,*) '# atoms split: ', cnt_split
        deallocate(self%atominfo)
        self%n_cc = cnt ! update
        allocate(self%atominfo(cnt))
        ! update centers
        do icc = 1, cnt
            self%atominfo(icc)%center(:) = new_centers(:,icc)
        enddo
        call self%img_bin%get_imat(imat_bin)
        ! update binary image
        where( imat_cc > 0 )
            imat_bin = 1
        elsewhere
            imat_bin = 0
        endwhere
        ! update relevant data fields
        call img_split_ccs%set_imat(imat_split_ccs)
        call img_split_ccs%write('split_ccs.mrc')
        call img_split_ccs%kill
        call self%img_bin%set_imat(imat_bin)
        call self%img_bin%update_img_rmat()
        call self%img_bin%find_ccs(self%img_cc)
        call self%update_ncc(self%img_cc)
        call self%find_centers()
        write(logfhandle, '(A)') '>>> SPLITTING CONNECTED ATOMS, COMPLETED'

    contains

        subroutine split_atom(new_centers,cnt)
            real,    intent(inout) :: new_centers(:,:) ! updated coordinates of the centers
            integer, intent(inout) :: cnt              ! atom counter, to update the center coords
            integer :: new_center1(3), new_center2(3), new_center3(3)
            integer :: i, j, k
            logical :: found3d_cen
            logical :: mask(self%ldim(1),self%ldim(2),self%ldim(3)) ! false in the layer of connection of the atom to be split
            mask = .false. ! initialization
            ! Identify first new center
            new_center1 = maxloc(rmat_pc(:self%ldim(1),:self%ldim(2),:self%ldim(3)), mask=imat == icc)
            cnt = cnt + 1
            new_centers(:,cnt) = real(new_center1)
            do i = 1, self%ldim(1)
                do j = 1, self%ldim(2)
                    do k = 1, self%ldim(3)
                        if( imat(i,j,k) == icc )then
                            if(((real(i - new_center1(1)))**2 + (real(j - new_center1(2)))**2 + &
                            &   (real(k - new_center1(3)))**2) * self%smpd  <=  (0.9 * self%theoretical_radius)**2) then
                                mask(i,j,k) = .true.
                            endif
                        endif
                    enddo
                enddo
            enddo
            ! Second likely center.
            new_center2 = maxloc(rmat_pc(:self%ldim(1),:self%ldim(2),:self%ldim(3)), (imat == icc) .and. .not. mask)
            if( any(new_center2 > 0) )then ! if anything was found
                ! Validate second center (check if it's 2 merged atoms, or one pointy one)
                if( sum(real(new_center2 - new_center1)**2.) * self%smpd <= (0.9 * self%theoretical_radius)**2) then
                    ! the new_center2 is within the diameter of the atom position at new_center1
                    ! therefore, it is not another atom and should be removed
                    where( imat_cc == icc .and. (.not.mask) ) imat_cc = 0
                    return
                else
                    cnt = cnt + 1
                    new_centers(:,cnt) = real(new_center2)
                    ! In the case of two merged atoms, build the second atom
                    do i = 1, self%ldim(1)
                        do j = 1, self%ldim(2)
                            do k = 1, self%ldim(3)
                                if( imat(i,j,k) == icc )then
                                    if(((real(i - new_center2(1)))**2 + (real(j - new_center2(2)))**2 +&
                                    &   (real(k - new_center2(3)))**2) * self%smpd <= (0.9 * self%theoretical_radius)**2 )then
                                        mask(i,j,k) = .true.
                                    endif
                                endif
                            enddo
                        enddo
                    enddo
                endif
            endif
            ! Third likely center.
            new_center3 = maxloc(rmat_pc(:self%ldim(1),:self%ldim(2),:self%ldim(3)), (imat == icc) .and. .not. mask)
            if( any(new_center3 > 0) )then ! if anything was found
                ! Validate third center
                if(sum(real(new_center3 - new_center1)**2.) * self%smpd <= (0.9 * self%theoretical_radius)**2 .or. &
                &  sum(real(new_center3 - new_center2)**2.) * self%smpd <= (0.9 * self%theoretical_radius)**2 )then
                    ! the new_center3 is within the diameter of the atom position at new_center1 or new_center2
                    ! therefore, it is not another atom and should be removed
                    where( imat_cc == icc .and. (.not.mask) ) imat_cc = 0
                    return
                else
                    cnt = cnt + 1
                    new_centers(:,cnt) = real(new_center3)
                    found3d_cen = .false.
                    ! In the case of two merged atoms, build the second atom
                    do i = 1, self%ldim(1)
                        do j = 1, self%ldim(2)
                            do k = 1, self%ldim(3)
                                if( imat(i,j,k) == icc )then
                                    if( ((real(i - new_center3(1)))**2 + (real(j - new_center3(2)))**2 + &
                                    &    (real(k - new_center3(3)))**2) * self%smpd <= (0.9 * self%theoretical_radius)**2 )then
                                         found3d_cen = .not.mask(i,j,k)
                                         mask(i,j,k) = .true.
                                    endif
                                endif
                            enddo
                        enddo
                    enddo
                endif
            endif
            ! Set the merged cc back to 0
            where(imat_cc == icc .and. (.not.mask) ) imat_cc = 0
            call self%img_cc%set_imat(imat_cc)
        end subroutine split_atom

    end subroutine split_atoms

    subroutine validate_atoms( self, simatms )
        class(nanoparticle), intent(inout) :: self
        class(image),        intent(in)    :: simatms
        real, allocatable :: centers(:,:)           ! coordinates of the atoms in PIXELS
        real, allocatable :: pixels1(:), pixels2(:) ! pixels extracted around the center
        real    :: maxrad, corrs(self%n_cc)
        integer :: ijk(3), npix_in, npix_out1, npix_out2, i, winsz
        type(stats_struct) :: corr_stats
        maxrad  = (self%theoretical_radius * 1.5) / self%smpd ! in pixels
        winsz   = ceiling(maxrad)
        npix_in = (2 * winsz + 1)**3 ! cubic window size (-winsz:winsz in each dim)
        centers = self%atominfo2centers()
        allocate(pixels1(npix_in), pixels2(npix_in), source=0.)
        ! calculate per-atom correlations
        do i = 1, self%n_cc
            ijk = nint(centers(:,i))
            call self%img_raw%win2arr_rad(ijk(1), ijk(2), ijk(3), winsz, npix_in, maxrad, npix_out1, pixels1)
            call simatms%win2arr_rad(     ijk(1), ijk(2), ijk(3), winsz, npix_in, maxrad, npix_out2, pixels2)
            self%atominfo(i)%valid_corr = pearsn_serial(pixels1(:npix_out1),pixels2(:npix_out2))
        end do
        call calc_stats(self%atominfo(:)%valid_corr, corr_stats)
        write(logfhandle,'(A)') '>>> VALID_CORR (PER-ATOM CORRELATION WITH SIMULATED DENSITY) STATS BELOW'
        write(logfhandle,'(A,F8.4)') 'Average: ', corr_stats%avg
        write(logfhandle,'(A,F8.4)') 'Median : ', corr_stats%med
        write(logfhandle,'(A,F8.4)') 'Sigma  : ', corr_stats%sdev
        write(logfhandle,'(A,F8.4)') 'Max    : ', corr_stats%maxv
        write(logfhandle,'(A,F8.4)') 'Min    : ', corr_stats%minv
    end subroutine validate_atoms

    subroutine discard_low_valid_corr_atoms( self, use_auto_corr_thres )
        use simple_stat, only: robust_sigma_thres
        class(nanoparticle), intent(inout) :: self
        logical,             intent(in)    :: use_auto_corr_thres
        integer, allocatable :: imat_bin(:,:,:), imat_cc(:,:,:)
        integer :: cc, n_discard
        real    :: corr_thres
        write(logfhandle, '(A)') '>>> DISCARDING ATOMS WITH LOW VALID_CORR'
        call self%img_cc%get_imat(imat_cc)
        call self%img_bin%get_imat(imat_bin)
        if( use_auto_corr_thres )then
            corr_thres = min(max(robust_sigma_thres(self%atominfo(:)%valid_corr, CORR_THRES_SIGMA), 0.3), 0.7)
            write(logfhandle, *) 'Valid_corr threshold calculated: ', corr_thres
        else
            corr_thres = params_glob%corr_thres
        endif
        write(logfhandle, *) 'Valid_corr threshold applied:    ', corr_thres
        n_discard = 0
        do cc = 1, self%n_cc
            if( self%atominfo(cc)%valid_corr < corr_thres )then
                where(imat_cc == cc) imat_bin = 0
                n_discard = n_discard + 1
            endif
        end do
        call self%img_bin%set_imat(imat_bin)
        call self%img_bin%find_ccs(self%img_cc)
        call self%img_cc%get_nccs(self%n_cc)
        call self%find_centers()
        deallocate(imat_bin, imat_cc)
        write(logfhandle, *) 'Numbers of atoms discarded because of low valid_corr ', n_discard
        write(logfhandle, *) 'Total number of atoms after discarding atoms with low valid_corr ', self%n_cc
        write(logfhandle, '(A)') '>>> DISCARDING ATOMS WITH LOW VALID_CORR, COMPLETED'
    end subroutine discard_low_valid_corr_atoms

    subroutine discard_atoms_with_low_contact_score( self, use_cn_thresh, cs_thres )
        class(nanoparticle), intent(inout) :: self
        logical,             intent(inout) :: use_cn_thresh
        integer, optional,   intent(in)    :: cs_thres
        integer, allocatable :: imat_bin(:,:,:), imat_cc(:,:,:)
        real, allocatable    :: centers_A(:,:) ! coordinates of the atoms in ANGSTROMS
        integer :: cscores(self%n_cc), cc, n_discard, cthresh, new_cthresh
        type(stats_struct)   :: cscore_stats
        centers_A = self%atominfo2centers_A()
        call calc_contact_scores(self%element,centers_A,cscores)
        call calc_stats(real(cscores), cscore_stats)
        write(logfhandle,'(A)') '>>> CONTACT SCORE STATS BELOW'
        write(logfhandle,'(A,F8.4)') 'Average: ', cscore_stats%avg
        write(logfhandle,'(A,F8.4)') 'Median : ', cscore_stats%med
        write(logfhandle,'(A,F8.4)') 'Sigma  : ', cscore_stats%sdev
        write(logfhandle,'(A,F8.4)') 'Max    : ', cscore_stats%maxv
        write(logfhandle,'(A,F8.4)') 'Min    : ', cscore_stats%minv
        if( .not. present(cs_thres) )then
            cthresh = min(5,max(3,nint(cscore_stats%avg - cscore_stats%sdev)))
            write(logfhandle,'(A,I3)') 'CONTACT SCORE THRESHOLD: ', cthresh
            use_cn_thresh = .false.
            if( cthresh == 5 )then ! highly crystalline
                use_cn_thresh = .true.
                return
            endif
        else
            cthresh = cs_thres
        endif
        write(logfhandle, '(A)') '>>> DISCARDING OUTLIERS BASED ON CONTACT SCORE'
        call self%img_cc%get_imat(imat_cc)
        call self%img_bin%get_imat(imat_bin)
        ! Removing outliers from the binary image and the connected components image
        ! remove atoms with < NVOX_THRESH voxels
        do cc = 1, self%n_cc
            if( count(imat_cc == cc) < NVOX_THRESH )then
                where(imat_cc == cc) imat_bin = 0
            endif
        end do
        ! Removing outliers based on coordination number
        n_discard = 0
        call remove_lowly_contacted( cthresh )
        ! don't leave behind any atoms with cscore < cthresh - 2
        new_cthresh = cthresh - 2
        if( new_cthresh < 1 )then
            ! we're done
        else
            call remove_lowly_contacted( new_cthresh )
        endif
        deallocate(imat_bin, imat_cc, centers_A)
        write(logfhandle, *) 'Numbers of atoms discarded because of low cscore ', n_discard
        write(logfhandle, *) 'Total number of atoms after discarding outliers based on cscore  ', self%n_cc
        write(logfhandle, '(A)') '>>> DISCARDING OUTLIERS BASED ON CONTACT SCORE, COMPLETED'

        contains

            subroutine remove_lowly_contacted( cthresh )
                integer, intent(in) :: cthresh
                do cc = 1, self%n_cc
                    if( cscores(cc) < cthresh )then
                        where(imat_cc == cc) imat_bin = 0
                        n_discard = n_discard + 1
                    endif
                enddo
                call self%img_bin%set_imat(imat_bin)
                call self%img_bin%find_ccs(self%img_cc)
                ! update number of connected components
                call self%img_cc%get_nccs(self%n_cc)
                call self%find_centers()
                if( allocated(centers_A) ) deallocate(centers_A)
                centers_A = self%atominfo2centers_A()
                call calc_contact_scores(self%element,centers_A,cscores)
            end subroutine remove_lowly_contacted

    end subroutine discard_atoms_with_low_contact_score

    ! This subroutine discards outliers that resisted binarization
    ! It calculates the standard coordination number (cn) of each atom and discards
    ! the atoms with cn_std < cn_thresh
    ! It modifies the img_bin and img_cc instances deleting the identified outliers.
    subroutine discard_lowly_coordinated( self, cn_thresh, a, l_fit_lattice )
        class(nanoparticle), intent(inout) :: self
        integer,             intent(in)    :: cn_thresh     ! threshold for discarding outliers based on coordination number
        real,                intent(inout) :: a(3)          ! lattice parameter
        logical,             intent(in)    :: l_fit_lattice ! fit lattice or use inputted
        integer, allocatable :: imat_bin(:,:,:), imat_cc(:,:,:)
        real, allocatable    :: centers_A(:,:) ! coordinates of the atoms in ANGSTROMS
        real    :: cn_gen(self%n_cc)
        integer :: cn(self%n_cc), cc, n_discard, new_cn_thresh
        write(logfhandle, '(A)') '>>> DISCARDING OUTLIERS BASED ON CN'
        centers_A = self%atominfo2centers_A()
        if( l_fit_lattice ) call fit_lattice(self%element, centers_A, a) ! else use inputted lattice params
        call run_cn_analysis(self%element,centers_A,a,cn,cn_gen)
        call self%img_cc%get_imat(imat_cc)
        call self%img_bin%get_imat(imat_bin)
        ! Removing outliers from the binary image and the connected components image
        ! remove atoms with < NVOX_THRESH voxels
        do cc = 1, self%n_cc
            if( count(imat_cc == cc) < NVOX_THRESH )then
                where(imat_cc == cc) imat_bin = 0
            endif
        end do
        ! Removing outliers based on coordination number
        n_discard = 0
        call remove_lowly_coordinated( cn_thresh )
        ! don't leave behind any atoms with cn_std < cn_thresh - 2
        new_cn_thresh = cn_thresh - 2
        if( new_cn_thresh < 1 )then
            ! we're done
        else
            call remove_lowly_coordinated( new_cn_thresh )
        endif
        deallocate(imat_bin, imat_cc, centers_A)
        write(logfhandle, *) 'Numbers of atoms discarded because of low cn ', n_discard
        write(logfhandle, *) 'Total number of atoms after discarding outliers based on cn      ', self%n_cc
        write(logfhandle, '(A)') '>>> DISCARDING OUTLIERS BASED ON CN, COMPLETED'

        contains

            subroutine remove_lowly_coordinated( cn_thresh )
                integer, intent(in) :: cn_thresh
                do cc = 1, self%n_cc
                    if( cn(cc) < cn_thresh )then
                        where(imat_cc == cc) imat_bin = 0
                        n_discard = n_discard + 1
                    endif
                enddo
                call self%img_bin%set_imat(imat_bin)
                call self%img_bin%find_ccs(self%img_cc)
                ! update number of connected components
                call self%img_cc%get_nccs(self%n_cc)
                call self%find_centers()
                if( allocated(centers_A) ) deallocate(centers_A)
                centers_A = self%atominfo2centers_A()
                call run_cn_analysis(self%element,centers_A,a,self%atominfo(:)%cn_std,self%atominfo(:)%cn_gen)
            end subroutine remove_lowly_coordinated

    end subroutine discard_lowly_coordinated

    ! calc stats

    subroutine fillin_atominfo( self, a0 )
        class(nanoparticle), intent(inout) :: self
        real, optional,      intent(in)    :: a0(3) ! lattice parameters
        type(image)          :: phasecorr, simatms
        type(atoms)          :: atoms_obj
        logical, allocatable :: mask(:,:,:)
        real,    allocatable :: centers_A(:,:), tmpcens(:,:), strain_array(:,:)
        real,    pointer     :: rmat(:,:,:), rmat_corr(:,:,:)
        integer, allocatable :: imat_cc(:,:,:)
        character(len=256)   :: io_msg
        logical, allocatable :: cc_mask(:)
        real    :: tmp_diam, a(3)
        integer :: i, j, k, cc, cn, n, funit, ios
        write(logfhandle, '(A)') '>>> EXTRACTING ATOM STATISTICS'
        ! calc cn and cn_gen
        centers_A = self%atominfo2centers_A()
        if( present(a0) )then
            a = a0
        else
            call fit_lattice(self%element, centers_A, a)
        endif
        call run_cn_analysis(self%element,centers_A,a,self%atominfo(:)%cn_std,self%atominfo(:)%cn_gen)
        ! calc strain for all atoms
        allocate(strain_array(self%n_cc,NSTRAIN_COMPS), source=0.)
        call strain_analysis(self%element, centers_A, a, strain_array)
        if( allocated(self%coords4stats) ) call self%pack_instance4stats(strain_array)
        allocate(cc_mask(self%n_cc), source=.true.) ! because self%n_cc might change after pack_instance4stats
        ! validation through per-atom correlation with the simulated density
        call self%simulate_atoms(atoms_obj, simatms)
        call self%validate_atoms( simatms )
        ! calc NPdiam & NPcen
        tmpcens     = self%atominfo2centers()
        self%NPdiam = 0.
        do i = 1, self%n_cc
            tmp_diam = pixels_dist(self%atominfo(i)%center(:), tmpcens, 'max', cc_mask)
            if( tmp_diam > self%NPdiam ) self%NPdiam = tmp_diam
        enddo
        cc_mask     = .true. ! restore
        self%NPdiam = self%NPdiam * self%smpd ! in A
        write(logfhandle,*) 'nanoparticle diameter (A): ', self%NPdiam
        self%NPcen  = self%masscen()
        write(logfhandle,*) 'nanoparticle mass center: ', self%NPcen
        ! CALCULATE PER-ATOM PARAMETERS
        ! extract atominfo
        allocate(mask(1:self%ldim(1),1:self%ldim(2),1:self%ldim(3)), source = .false.)
        call phasecorr_one_atom(self%img, phasecorr, self%element)
        call phasecorr%get_rmat_ptr(rmat_corr)
        call self%img%get_rmat_ptr(rmat)
        call self%img_cc%get_imat(imat_cc)
        do cc = 1, self%n_cc
            call progress(cc, self%n_cc)
            ! index of the connected component
            self%atominfo(cc)%cc_ind = cc
            ! number of voxels in connected component
            where( imat_cc == cc ) mask = .true.
            self%atominfo(cc)%size = count(mask)
            ! distance from the centre of mass of the nanoparticle
            self%atominfo(cc)%cendist = euclid(self%atominfo(cc)%center(:), self%NPcen) * self%smpd
            ! atom diameter
            call self%calc_longest_atm_dist(cc, self%atominfo(cc)%diam)
            self%atominfo(cc)%diam = 2.*self%atominfo(cc)%diam ! radius --> diameter in A
            ! maximum grey level intensity across the connected component
            self%atominfo(cc)%max_int = maxval(rmat(1:self%ldim(1),1:self%ldim(2),1:self%ldim(3)), mask)
            ! average grey level intensity across the connected component
            self%atominfo(cc)%avg_int = sum(rmat(1:self%ldim(1),1:self%ldim(2),1:self%ldim(3)), mask)
            self%atominfo(cc)%avg_int = self%atominfo(cc)%avg_int / real(count(mask))
            ! bond length of nearest neighbour...
            self%atominfo(cc)%bondl = pixels_dist(self%atominfo(cc)%center(:), tmpcens, 'min', mask=cc_mask) ! Use all the atoms
            self%atominfo(cc)%bondl = self%atominfo(cc)%bondl * self%smpd ! convert to A
            ! set strain values
            self%atominfo(cc)%exx_strain    = strain_array(cc,1)
            self%atominfo(cc)%eyy_strain    = strain_array(cc,2)
            self%atominfo(cc)%ezz_strain    = strain_array(cc,3)
            self%atominfo(cc)%exy_strain    = strain_array(cc,4)
            self%atominfo(cc)%eyz_strain    = strain_array(cc,5)
            self%atominfo(cc)%exz_strain    = strain_array(cc,6)
            self%atominfo(cc)%radial_strain = strain_array(cc,7)
            ! reset masks
            mask    = .false.
            cc_mask = .true.
        end do
        ! CALCULATE GLOBAL NP PARAMETERS
        call calc_stats(  real(self%atominfo(:)%size),    self%size_stats, mask=self%atominfo(:)%size >= NVOX_THRESH )
        call calc_stats(  real(self%atominfo(:)%cn_std),  self%cn_std_stats        )
        call calc_stats(  self%atominfo(:)%bondl,         self%bondl_stats         )
        call calc_stats(  self%atominfo(:)%cn_gen,        self%cn_gen_stats        )
        call calc_stats(  self%atominfo(:)%diam,          self%diam_stats, mask=self%atominfo(:)%size >= NVOX_THRESH )
        call calc_zscore( self%atominfo(:)%avg_int ) ! to get comparable intensities between different particles
        call calc_zscore( self%atominfo(:)%max_int ) ! -"-
        call calc_stats(  self%atominfo(:)%avg_int,       self%avg_int_stats       )
        call calc_stats(  self%atominfo(:)%max_int,       self%max_int_stats       )
        call calc_stats(  self%atominfo(:)%valid_corr,    self%valid_corr_stats    )
        call calc_stats(  self%atominfo(:)%radial_strain, self%radial_strain_stats )
        ! CALCULATE CN-DEPENDENT STATS & WRITE CN-ATOMS
        do cn = CNMIN, CNMAX
            call calc_cn_stats( cn )
            call write_cn_atoms( cn )
        end do
        ! BINARY CLUSTERING OF RELEVANT PARAMETERS
        call fopen(funit, file=BIN_CLS_STATS_FILE, iostat=ios, status='replace', iomsg=io_msg)
        call fileiochk("simple_nanoparticle :: bicluster_otsu; ERROR when opening file "//BIN_CLS_STATS_FILE//'; '//trim(io_msg),ios)
        ! write header
        write(funit,'(a)') BIN_CLS_STATS_HEAD
        call self%bicluster_otsu('size',          funit)
        call self%bicluster_otsu('bondl',         funit)
        call self%bicluster_otsu('diam',          funit)
        call self%bicluster_otsu('max_int',       funit)
        call self%bicluster_otsu('valid_corr',    funit)
        call self%bicluster_otsu('radial_strain', funit)
        call fclose(funit)
        ! identify correlated variables with Pearson's product moment correation coefficient
        call self%id_corr_vars
        ! write pdf files with valid_corr and max_int in the B-factor field (for validation/visualisation)
        call self%write_centers('valid_corr_in_bfac_field', 'valid_corr')
        call self%write_centers('max_int_in_bfac_field',    'max_int')
        ! destruct
        deallocate(cc_mask, imat_cc, tmpcens, strain_array, centers_A)
        call phasecorr%kill
        call simatms%kill
        call atoms_obj%kill
        write(logfhandle, '(A)') '>>> EXTRACTING ATOM STATISTICS, COMPLETED'

        contains

            subroutine calc_zscore( arr )
                real, intent(inout) :: arr(:)
                arr = (arr - self%map_stats%avg) / self%map_stats%sdev
            end subroutine calc_zscore

            subroutine calc_cn_stats( cn )
                integer, intent(in)  :: cn ! calculate stats for given std cn
                integer :: cc, n, n_size, n_diam
                logical :: cn_mask(self%n_cc), size_mask(self%n_cc)
                ! Generate masks
                cn_mask   = self%atominfo(:)%cn_std == cn
                size_mask = self%atominfo(:)%size >= NVOX_THRESH .and. cn_mask
                n         = count(cn_mask)
                if( n == 0 ) return
                ! -- # atoms
                self%natoms_cns(cn) = real(n)
                if( n < 2 ) return
                ! -- the rest
                call calc_stats( real(self%atominfo(:)%size),    self%size_stats_cns(cn),          mask=size_mask )
                call calc_stats( self%atominfo(:)%bondl,         self%bondl_stats_cns(cn),         mask=cn_mask   )
                call calc_stats( self%atominfo(:)%cn_gen,        self%cn_gen_stats_cns(cn),        mask=cn_mask   )
                call calc_stats( self%atominfo(:)%diam,          self%diam_stats_cns(cn),          mask=size_mask )
                call calc_stats( self%atominfo(:)%avg_int,       self%avg_int_stats_cns(cn),       mask=cn_mask   )
                call calc_stats( self%atominfo(:)%max_int,       self%max_int_stats_cns(cn),       mask=cn_mask   )
                call calc_stats( self%atominfo(:)%valid_corr,    self%valid_corr_stats_cns(cn),    mask=cn_mask   )
                call calc_stats( self%atominfo(:)%radial_strain, self%radial_strain_stats_cns(cn), mask=cn_mask   )
            end subroutine calc_cn_stats

            subroutine write_cn_atoms( cn_std )
                integer, intent(in)  :: cn_std
                type(binimage)       :: img_atom
                integer, allocatable :: imat(:,:,:), imat_atom(:,:,:)
                logical :: cn_mask(self%n_cc)
                integer :: i
                ! make cn mask
                cn_mask = self%atominfo(:)%cn_std == cn_std
                ! make binary matrix of atoms with given cn_std
                call img_atom%copy_bimg(self%img_cc)
                allocate(imat_atom(self%ldim(1),self%ldim(2),self%ldim(3)), source = 0)
                call img_atom%get_imat(imat)
                do i = 1, self%n_cc
                    if( cn_mask(i) )then
                        where( imat == i ) imat_atom = 1
                    endif
                enddo
                call img_atom%set_imat(imat_atom)
                call img_atom%write_bimg('Atoms_with_cn_std'//trim(int2str(cn_std))//'.mrc')
                deallocate(imat,imat_atom)
                call img_atom%kill_bimg
            end subroutine write_cn_atoms

    end subroutine fillin_atominfo

    ! calc the avg of the centers coords
    function masscen( self ) result( m )
       class(nanoparticle), intent(inout) :: self
       real    :: m(3) ! mass center coords
       integer :: i
       m = 0.
       do i = 1, self%n_cc
           m = m + self%atominfo(i)%center(:)
       end do
       m = m / real(self%n_cc)
    end function masscen

    subroutine calc_longest_atm_dist( self, label, longest_dist )
       class(nanoparticle), intent(inout) :: self
       integer,             intent(in)    :: label
       real,                intent(out)   :: longest_dist
       integer, allocatable :: pos(:,:)
       integer, allocatable :: imat_cc(:,:,:)
       logical, allocatable :: mask_dist(:) ! for min and max dist calculation
       integer :: location(1)               ! location of vxls of the atom farthest from its center
       call self%img_cc%get_imat(imat_cc)
       where(imat_cc.eq.label)
           imat_cc = 1
       elsewhere
           imat_cc = 0
       endwhere
       call get_pixel_pos( imat_cc, pos ) ! pxls positions of the shell
       allocate(mask_dist(size(pos, dim=2)), source = .true.)
       if( size(pos,2) == 1 ) then ! if the connected component has size 1 (just 1 vxl)
           longest_dist  = self%smpd
           return
       else
           longest_dist  = pixels_dist(self%atominfo(label)%center(:), real(pos),'max', mask_dist, location) * self%smpd
       endif
       deallocate(imat_cc, pos, mask_dist)
    end subroutine calc_longest_atm_dist

    ! visualization and output

    subroutine simulate_atoms( self, atoms_obj, simatms, betas )
        class(nanoparticle), intent(inout) :: self
        class(atoms),        intent(inout) :: atoms_obj
        class(image),        intent(inout) :: simatms
        real, optional,      intent(in)    :: betas(self%n_cc) ! in pdb file b-factor
        logical :: betas_present
        integer :: i
        betas_present = present(betas)
        ! generate atoms object
        call atoms_obj%new(self%n_cc)
        do i = 1, self%n_cc
            call atoms_obj%set_name(     i, self%atom_name)
            call atoms_obj%set_element(  i, self%element)
            call atoms_obj%set_coord(    i, (self%atominfo(i)%center(:)-1.)*self%smpd)
            call atoms_obj%set_num(      i, i)
            call atoms_obj%set_resnum(   i, i)
            call atoms_obj%set_chain(    i, 'A')
            call atoms_obj%set_occupancy(i, 1.)
            if( betas_present )then
                call atoms_obj%set_beta( i, betas(i))
            else
                call atoms_obj%set_beta( i ,self%atominfo(i)%cn_gen) ! use generalised coordination number
            endif
        enddo
        call simatms%new(self%ldim,self%smpd)
        call atoms_obj%convolve(simatms, cutoff = 8.*self%smpd) ! con
    end subroutine simulate_atoms

    subroutine write_centers_1( self, fname, coords )
       class(nanoparticle),        intent(inout) :: self
       character(len=*), optional, intent(in)    :: fname
       real,             optional, intent(in)    :: coords(:,:)
       type(atoms) :: centers_pdb
       integer     :: cc
       if( present(coords) )then
           call centers_pdb%new(size(coords, dim = 2), dummy=.true.)
           do cc=1,size(coords, dim = 2)
               call centers_pdb%set_name(cc,self%atom_name)
               call centers_pdb%set_element(cc,self%element)
               call centers_pdb%set_coord(cc,(coords(:,cc)-1.)*self%smpd)
           enddo
       else
           call centers_pdb%new(self%n_cc, dummy=.true.)
           do cc=1,self%n_cc
               call centers_pdb%set_name(cc,self%atom_name)
               call centers_pdb%set_element(cc,self%element)
               call centers_pdb%set_coord(cc,(self%atominfo(cc)%center(:)-1.)*self%smpd)
               call centers_pdb%set_beta(cc,self%atominfo(cc)%cn_gen) ! use generalised coordination number
               call centers_pdb%set_resnum(cc,cc)
           enddo
       endif
       if( present(fname) ) then
           call centers_pdb%writepdb(fname)
       else
           call centers_pdb%writepdb(trim(self%fbody)//'_ATMS')
           write(logfhandle,*) 'output, atomic coordinates:       ', trim(self%fbody)//'_ATMS'
       endif
   end subroutine write_centers_1

   subroutine write_centers_2( self, fname, which )
      class(nanoparticle), intent(inout) :: self
      character(len=*),    intent(in)    :: fname
      character(len=*),    intent(in)    :: which ! parameter in the B-factor field of the pdb file
      type(atoms) :: centers_pdb
      integer     :: cc
      call centers_pdb%new(self%n_cc, dummy=.true.)
      do cc=1,self%n_cc
          call centers_pdb%set_name(cc,self%atom_name)
          call centers_pdb%set_element(cc,self%element)
          call centers_pdb%set_coord(cc,(self%atominfo(cc)%center(:)-1.)*self%smpd)
          select case(which)
              case('valid_corr')
                  call centers_pdb%set_beta(cc,self%atominfo(cc)%valid_corr)  ! use per-atom validation correlation
              case('max_int')
                  call centers_pdb%set_beta(cc,self%atominfo(cc)%max_int)     ! use z-score of maximum intensity
              case DEFAULT
                  call centers_pdb%set_beta(cc,self%atominfo(cc)%cn_gen)      ! use generalised coordination number
          end select
          call centers_pdb%set_resnum(cc,cc)
      enddo
     call centers_pdb%writepdb(fname)
  end subroutine write_centers_2

    subroutine write_individual_atoms( self )
        class(nanoparticle), intent(inout) :: self
        type(binimage)       :: img_atom
        integer, allocatable :: imat(:,:,:), imat_atom(:,:,:)
        integer :: i
        call img_atom%copy_bimg(self%img_cc)
        allocate(imat_atom(self%ldim(1),self%ldim(2),self%ldim(3)), source = 0)
        call img_atom%get_imat(imat)
        do i = 1, maxval(imat)
            where(imat == i)
                imat_atom = 1
            elsewhere
                imat_atom = 0
            endwhere
            call img_atom%set_imat(imat_atom)
            call img_atom%write_bimg('Atom'//trim(int2str(i))//'.mrc')
        enddo
        deallocate(imat,imat_atom)
        call img_atom%kill_bimg
    end subroutine write_individual_atoms

    subroutine write_csv_files( self )
        class(nanoparticle), intent(in) :: self
        integer            :: ios, funit, cc, cn
        character(len=256) :: io_msg
        ! NANOPARTICLE STATS
        call fopen(funit, file=NP_STATS_FILE, iostat=ios, status='replace', iomsg=io_msg)
        call fileiochk("simple_nanoparticle :: write_csv_files; ERROR when opening file "//NP_STATS_FILE//'; '//trim(io_msg),ios)
        ! write header
        write(funit,'(a)') NP_STATS_HEAD
        ! write record
        call self%write_np_stats(funit)
        call fclose(funit)
        ! CN-DEPENDENT STATS
        call fopen(funit, file=CN_STATS_FILE, iostat=ios, status='replace', iomsg=io_msg)
        call fileiochk("simple_nanoparticle :: write_csv_files; ERROR when opening file "//CN_STATS_FILE//'; '//trim(io_msg),ios)
        ! write header
        write(funit,'(a)') CN_STATS_HEAD
        ! write records
        do cn = CNMIN, CNMAX
            call self%write_cn_stats(cn, funit)
        enddo
        call fclose(funit)
        ! PER-ATOM STATS
        call fopen(funit, file=ATOMS_STATS_FILE, iostat=ios, status='replace', iomsg=io_msg)
        call fileiochk("simple_nanoparticle :: write_csv_files; ERROR when opening file "//ATOMS_STATS_FILE//'; '//trim(io_msg),ios)
        ! write header
        write(funit,'(a)') ATOM_STATS_HEAD_OMIT
        ! write records
        do cc = 1, size(self%atominfo)
            call self%write_atominfo(cc, funit, omit=.true.)
        enddo
        call fclose(funit)
    end subroutine write_csv_files

    subroutine write_atominfo( self, cc, funit, omit )
        class(nanoparticle), intent(in) :: self
        integer,             intent(in) :: cc, funit
        logical, optional,   intent(in) :: omit
        logical :: omit_here
        if( self%atominfo(cc)%size < NVOX_THRESH ) return
        omit_here = .false.
        if( present(omit) ) omit_here = omit
        601 format(F8.4,A2)
        602 format(F8.4)
        ! various per-atom parameters
        write(funit,601,advance='no') real(self%atominfo(cc)%cc_ind),           CSV_DELIM ! INDEX
        write(funit,601,advance='no') real(self%atominfo(cc)%size),             CSV_DELIM ! NVOX
        write(funit,601,advance='no') real(self%atominfo(cc)%cn_std),           CSV_DELIM ! CN_STD
        write(funit,601,advance='no') self%atominfo(cc)%bondl,                  CSV_DELIM ! NN_BONDL
        write(funit,601,advance='no') self%atominfo(cc)%cn_gen,                 CSV_DELIM ! CN_GEN
        write(funit,601,advance='no') self%atominfo(cc)%diam,                   CSV_DELIM ! DIAM
        write(funit,601,advance='no') self%atominfo(cc)%avg_int,                CSV_DELIM ! AVG_INT
        write(funit,601,advance='no') self%atominfo(cc)%max_int,                CSV_DELIM ! MAX_INT
        write(funit,601,advance='no') self%atominfo(cc)%cendist,                CSV_DELIM ! CENDIST
        write(funit,601,advance='no') self%atominfo(cc)%valid_corr,             CSV_DELIM ! VALID_CORR
        if( .not. omit_here )then
        write(funit,601,advance='no') self%atominfo(cc)%center(1),              CSV_DELIM ! X
        write(funit,601,advance='no') self%atominfo(cc)%center(2),              CSV_DELIM ! Y
        write(funit,601,advance='no') self%atominfo(cc)%center(3),              CSV_DELIM ! Z
        ! strain
        write(funit,601,advance='no') self%atominfo(cc)%exx_strain,             CSV_DELIM ! EXX_STRAIN
        write(funit,601,advance='no') self%atominfo(cc)%eyy_strain,             CSV_DELIM ! EYY_STRAIN
        write(funit,601,advance='no') self%atominfo(cc)%ezz_strain,             CSV_DELIM ! EZZ_STRAIN
        write(funit,601,advance='no') self%atominfo(cc)%exy_strain,             CSV_DELIM ! EXY_STRAIN
        write(funit,601,advance='no') self%atominfo(cc)%eyz_strain,             CSV_DELIM ! EYZ_STRAIN
        write(funit,601,advance='no') self%atominfo(cc)%exz_strain,             CSV_DELIM ! EXZ_STRAIN
        endif
        if( .not. omit_here )then
        write(funit,601,advance='no') self%atominfo(cc)%radial_strain,          CSV_DELIM ! RADIAL_STRAIN
        else
        write(funit,602)              self%atominfo(cc)%radial_strain                     ! RADIAL_STRAIN
        endif
        if( .not. omit_here )then
        ! cluster assignments
        write(funit,601,advance='no') real(self%atominfo(cc)%size_cls),         CSV_DELIM ! NVOX_CLASS
        write(funit,601,advance='no') real(self%atominfo(cc)%bondl_cls),        CSV_DELIM ! NN_BONDL_CLASS
        write(funit,601,advance='no') real(self%atominfo(cc)%diam_cls),         CSV_DELIM ! DIAM_CLASS
        write(funit,601,advance='no') real(self%atominfo(cc)%max_int_cls),      CSV_DELIM ! MAX_INT_CLASS
        write(funit,601,advance='no') real(self%atominfo(cc)%valid_corr_cls),   CSV_DELIM ! VALID_CORR_CLASS
        write(funit,602)              real(self%atominfo(cc)%radial_strain_cls)           ! RADIAL_STRAIN_CLASS
        endif
    end subroutine write_atominfo

    subroutine write_np_stats( self, funit )
        class(nanoparticle), intent(in) :: self
        integer,             intent(in) :: funit
        601 format(F8.4,A2)
        602 format(F8.4)
        ! -- # atoms
        write(funit,601,advance='no') real(self%n_cc),               CSV_DELIM ! NATOMS
        ! -- NP diameter
        write(funit,601,advance='no') self%NPdiam,                   CSV_DELIM ! DIAM
        ! -- atom size
        write(funit,601,advance='no') self%size_stats%avg,           CSV_DELIM ! AVG_NVOX
        write(funit,601,advance='no') self%size_stats%med,           CSV_DELIM ! MED_NVOX
        write(funit,601,advance='no') self%size_stats%sdev,          CSV_DELIM ! SDEV_NVOX
        ! -- standard coordination number
        write(funit,601,advance='no') self%cn_std_stats%avg,         CSV_DELIM ! AVG_CN_STD
        write(funit,601,advance='no') self%cn_std_stats%med,         CSV_DELIM ! MED_CN_STD
        write(funit,601,advance='no') self%cn_std_stats%sdev,        CSV_DELIM ! SDEV_CN_STD
        ! -- bond length
        write(funit,601,advance='no') self%bondl_stats%avg,          CSV_DELIM ! AVG_NN_BONDL
        write(funit,601,advance='no') self%bondl_stats%med,          CSV_DELIM ! MED_NN_BONDL
        write(funit,601,advance='no') self%bondl_stats%sdev,         CSV_DELIM ! SDEV_NN_BONDL
        ! -- generalized coordination number
        write(funit,601,advance='no') self%cn_gen_stats%avg,         CSV_DELIM ! AVG_CN_GEN
        write(funit,601,advance='no') self%cn_gen_stats%med,         CSV_DELIM ! MED_CN_GEN
        write(funit,601,advance='no') self%cn_gen_stats%sdev,        CSV_DELIM ! SDEV_CN_GEN
        ! -- atom diameter
        write(funit,601,advance='no') self%diam_stats%avg,           CSV_DELIM ! AVG_DIAM
        write(funit,601,advance='no') self%diam_stats%med,           CSV_DELIM ! MED_DIAM
        write(funit,601,advance='no') self%diam_stats%sdev,          CSV_DELIM ! SDEV_DIAM
        ! -- average intensity
        write(funit,601,advance='no') self%avg_int_stats%avg,        CSV_DELIM ! AVG_AVG_INT
        write(funit,601,advance='no') self%avg_int_stats%med,        CSV_DELIM ! MED_AVG_INT
        write(funit,601,advance='no') self%avg_int_stats%sdev,       CSV_DELIM ! SDEV_AVG_INT
        ! -- maximum intensity
        write(funit,601,advance='no') self%max_int_stats%avg,        CSV_DELIM ! AVG_MAX_INT
        write(funit,601,advance='no') self%max_int_stats%med,        CSV_DELIM ! MED_MAX_INT
        write(funit,601,advance='no') self%max_int_stats%sdev,       CSV_DELIM ! SDEV_MAX_INT
        ! -- maximum correlation
        write(funit,601,advance='no') self%valid_corr_stats%avg,     CSV_DELIM ! AVG_VALID_CORR
        write(funit,601,advance='no') self%valid_corr_stats%med,     CSV_DELIM ! MED_VALID_CORR
        write(funit,601,advance='no') self%valid_corr_stats%sdev,    CSV_DELIM ! SDEV_VALID_CORR
        ! -- radial strain
        write(funit,601,advance='no') self%radial_strain_stats%avg,  CSV_DELIM ! AVG_RADIAL_STRAIN
        write(funit,601,advance='no') self%radial_strain_stats%med,  CSV_DELIM ! MED_RADIAL_STRAIN
        write(funit,601,advance='no') self%radial_strain_stats%sdev, CSV_DELIM ! SDEV_RADIAL_STRAIN
        write(funit,601,advance='no') self%radial_strain_stats%minv, CSV_DELIM ! MIN_RADIAL_STRAIN
        write(funit,602)              self%radial_strain_stats%maxv            ! MAX_RADIAL_STRAIN
    end subroutine write_np_stats

    subroutine write_cn_stats( self, cn, funit )
        class(nanoparticle), intent(in) :: self
        integer,             intent(in) :: cn, funit
        601 format(F8.4,A2)
        602 format(F8.4)
        if( count(self%atominfo(:)%cn_std == cn) < 2 ) return
        ! -- coordination number
        write(funit,601,advance='no') real(cn),                              CSV_DELIM ! CN_STD
        ! -- # atoms per cn
        write(funit,601,advance='no') self%natoms_cns(cn),                   CSV_DELIM ! NATOMS
        ! -- atom size
        write(funit,601,advance='no') self%size_stats_cns(cn)%avg,           CSV_DELIM ! AVG_NVOX
        write(funit,601,advance='no') self%size_stats_cns(cn)%med,           CSV_DELIM ! MED_NVOX
        write(funit,601,advance='no') self%size_stats_cns(cn)%sdev,          CSV_DELIM ! SDEV_NVOX
        ! -- bond length
        write(funit,601,advance='no') self%bondl_stats_cns(cn)%avg,          CSV_DELIM ! AVG_NN_BONDL
        write(funit,601,advance='no') self%bondl_stats_cns(cn)%med,          CSV_DELIM ! MED_NN_BONDL
        write(funit,601,advance='no') self%bondl_stats_cns(cn)%sdev,         CSV_DELIM ! SDEV_NN_BONDL
        ! -- generalized coordination number
        write(funit,601,advance='no') self%cn_gen_stats_cns(cn)%avg,         CSV_DELIM ! AVG_CN_GEN
        write(funit,601,advance='no') self%cn_gen_stats_cns(cn)%med,         CSV_DELIM ! MED_CN_GEN
        write(funit,601,advance='no') self%cn_gen_stats_cns(cn)%sdev,        CSV_DELIM ! SDEV_CN_GEN
        ! -- atom diameter
        write(funit,601,advance='no') self%diam_stats_cns(cn)%avg,           CSV_DELIM ! AVG_DIAM
        write(funit,601,advance='no') self%diam_stats_cns(cn)%med,           CSV_DELIM ! MED_DIAM
        write(funit,601,advance='no') self%diam_stats_cns(cn)%sdev,          CSV_DELIM ! SDEV_DIAM
        ! -- average intensity
        write(funit,601,advance='no') self%avg_int_stats_cns(cn)%avg,        CSV_DELIM ! AVG_AVG_INT
        write(funit,601,advance='no') self%avg_int_stats_cns(cn)%med,        CSV_DELIM ! MED_AVG_INT
        write(funit,601,advance='no') self%avg_int_stats_cns(cn)%sdev,       CSV_DELIM ! SDEV_AVG_INT
        ! -- maximum intensity
        write(funit,601,advance='no') self%max_int_stats_cns(cn)%avg,        CSV_DELIM ! AVG_MAX_INT
        write(funit,601,advance='no') self%max_int_stats_cns(cn)%med,        CSV_DELIM ! MED_MAX_INT
        write(funit,601,advance='no') self%max_int_stats_cns(cn)%sdev,       CSV_DELIM ! SDEV_MAX_INT
        ! -- maximum correlation
        write(funit,601,advance='no') self%valid_corr_stats_cns(cn)%avg,     CSV_DELIM ! AVG_VALID_CORR
        write(funit,601,advance='no') self%valid_corr_stats_cns(cn)%med,     CSV_DELIM ! MED_VALID_CORR
        write(funit,601,advance='no') self%valid_corr_stats_cns(cn)%sdev,    CSV_DELIM ! SDEV_VALID_CORR
        ! -- radial strain
        write(funit,601,advance='no') self%radial_strain_stats_cns(cn)%avg,  CSV_DELIM ! AVG_RADIAL_STRAIN
        write(funit,601,advance='no') self%radial_strain_stats_cns(cn)%med,  CSV_DELIM ! MED_RADIAL_STRAIN
        write(funit,601,advance='no') self%radial_strain_stats_cns(cn)%sdev, CSV_DELIM ! SDEV_RADIAL_STRAIN
        write(funit,601,advance='no') self%radial_strain_stats_cns(cn)%minv, CSV_DELIM ! MIN_RADIAL_STRAIN
        write(funit,602)              self%radial_strain_stats_cns(cn)%maxv            ! MAX_RADIAL_STRAIN
    end subroutine write_cn_stats

    ! identify correlated variables with Pearson's product moment correation coefficient
    subroutine id_corr_vars( self )
        class(nanoparticle), target, intent(in) :: self
        integer, parameter :: NFLAGS = 9
        integer, parameter :: NPAIRS = (NFLAGS * (NFLAGS - 1)) / 2
        character(len=13)  :: flags(NFLAGS), flags_i(NPAIRS), flags_j(NPAIRS)
        character(len=256) :: io_msg
        real               :: vals1(self%n_cc), vals2(self%n_cc)
        real    :: corrs(NPAIRS), corrs_copy(NPAIRS), corr
        integer :: i, j, inds(NPAIRS), cnt, funit, ios
        ! variables to correlate
        flags(1)  = 'NVOX'          ! size
        flags(2)  = 'NN_BONDL'      ! bondl
        flags(3)  = 'CN_GEN'        ! cn_gen
        flags(4)  = 'DIAM'          ! diam
        flags(5)  = 'AVG_INT'       ! avg_int
        flags(6)  = 'MAX_INT'       ! max_int
        flags(7)  = 'CENDIST'    ! cendist
        flags(8)  = 'VALID_CORR'    ! valid_corr
        flags(9)  = 'RADIAL_STRAIN' ! radial_strain
        ! calculate correlations
        cnt = 0
        do i = 1, NFLAGS - 1
            call set_vals(flags(i), vals1)
            do j = i + 1, NFLAGS
                cnt          = cnt + 1
                inds(cnt)    = cnt
                flags_i(cnt) = flags(i)
                flags_j(cnt) = flags(j)
                call set_vals(flags(j), vals2)
                corrs(cnt) = pearsn_serial(vals1,vals2)
            end do
        end do
        ! sort
        corrs_copy = corrs
        call hpsort(corrs_copy, inds)
        ! write output
        call fopen(funit, file=ATOM_VAR_CORRS_FILE, iostat=ios, status='replace', iomsg=io_msg)
        call fileiochk("simple_nanoparticle :: id_corr_vars; ERROR when opening file "//ATOM_VAR_CORRS_FILE//'; '//trim(io_msg),ios)
        do i = 1, NPAIRS
            write(funit,'(A,F7.4)')'PEARSONS CORRELATION BTW '//flags_i(inds(i))//' & '//flags_j(inds(i))//' IS ', corrs(inds(i))
        end do
        call fclose(funit)

        contains

            subroutine set_vals( flag, vals )
                character(len=*), intent(in)  :: flag
                real,             intent(out) :: vals(self%n_cc)
                 select case(trim(flag))
                    case('NVOX')
                        vals = real(self%atominfo(:)%size)
                    case('NN_BONDL')
                        vals = self%atominfo(:)%bondl
                    case('CN_GEN')
                        vals = self%atominfo(:)%cn_gen
                    case('DIAM')
                        vals =  self%atominfo(:)%diam
                    case('AVG_INT')
                        vals =  self%atominfo(:)%avg_int
                    case('MAX_INT')
                        vals =  self%atominfo(:)%max_int
                    case('CENDIST')
                        vals =  self%atominfo(:)%cendist
                    case('VALID_CORR')
                        vals =  self%atominfo(:)%valid_corr
                    case('RADIAL_STRAIN')
                        vals =  self%atominfo(:)%radial_strain
                end select
            end subroutine set_vals

    end subroutine id_corr_vars

    subroutine bicluster_otsu( self, which, funit )
        class(nanoparticle), intent(inout) :: self
        character(len=*),    intent(in)    :: which
        integer,             intent(in)    :: funit
        integer :: n, bicls(self%n_cc)
        real    :: vals(self%n_cc), thresh
        logical :: cls_mask(self%n_cc), mask(self%n_cc)
        select case(which)
            case('size')
                vals = real(self%atominfo(:)%size)
                call bicluster_local(self%atominfo(:)%size >= NVOX_THRESH, which, 'NVOX')
                self%atominfo(:)%size_cls = bicls
            case('bondl')
                vals = self%atominfo(:)%bondl
                call bicluster_local(self%atominfo(:)%bondl > 0., which, 'NN_BONDL')
                self%atominfo(:)%bondl_cls = bicls
            case('diam')
                vals = self%atominfo(:)%diam
                call bicluster_local(self%atominfo(:)%diam > 0., which, 'DIAM')
                self%atominfo(:)%diam_cls = bicls
            case('max_int')
                vals = self%atominfo(:)%max_int
                mask = .true.
                call bicluster_local(mask, which, 'MAX_INT')
                self%atominfo(:)%max_int_cls = bicls
            case('valid_corr')
                vals = self%atominfo(:)%valid_corr
                mask = .true.
                call bicluster_local(mask, which, 'VALID_CORR')
                self%atominfo(:)%valid_corr_cls = bicls
            case('radial_strain')
                vals = self%atominfo(:)%radial_strain
                mask = .true.
                call bicluster_local(mask, which, 'RADIAL_STRAIN')
                self%atominfo(:)%radial_strain_cls = bicls
        case DEFAULT
            THROW_HARD('unsupported parameter for bicluster_otsu')
        end select

        contains

            subroutine bicluster_local( mask, which, param )
                logical,          intent(in) :: mask(self%n_cc)
                character(len=*), intent(in) :: which, param
                real, allocatable  :: vals4otsu(:)
                type(stats_struct) :: stats(2)
                character(len=13)  :: param_str
                integer            :: pop1, pop2
                601 format(F8.4,A2)
                602 format(F8.4)
                param_str = trim(param)
                vals4otsu = pack(vals, mask=mask)
                call otsu(vals4otsu, thresh)
                deallocate(vals4otsu)
                where( vals > thresh )
                    bicls = 1 ! 1 is large
                elsewhere
                    bicls = 2 ! 2 is small
                endwhere
                cls_mask = bicls == 1 .and. mask
                pop1     = count(cls_mask)
                call calc_stats(vals, stats(1), mask=cls_mask)
                call write_cls_atoms(1, which)
                cls_mask = bicls == 2 .and. mask
                pop2     = count(cls_mask)
                call calc_stats(vals, stats(2), mask=cls_mask)
                call write_cls_atoms(2, which)
                write(logfhandle,'(A,A21,1X,F8.4,1X,F8.4)') param_str, ' class averages 1 & 2:', stats(1)%avg, stats(2)%avg
                write(funit,'(A,A3)',advance='no') param_str,  CSV_DELIM ! PARAMETER
                write(funit,601,  advance='no') stats(1)%avg,  CSV_DELIM ! AVG_1
                write(funit,601,  advance='no') stats(1)%med,  CSV_DELIM ! MED_1
                write(funit,601,  advance='no') stats(1)%sdev, CSV_DELIM ! SDEV_1
                write(funit,601,  advance='no') stats(1)%minv, CSV_DELIM ! MIN_1
                write(funit,601,  advance='no') stats(1)%maxv, CSV_DELIM ! MAX_1
                write(funit,601,  advance='no') real(pop1),    CSV_DELIM ! POP_1
                write(funit,601,  advance='no') stats(2)%avg,  CSV_DELIM ! AVG_2
                write(funit,601,  advance='no') stats(2)%med,  CSV_DELIM ! MED_2
                write(funit,601,  advance='no') stats(2)%sdev, CSV_DELIM ! SDEV_2
                write(funit,601,  advance='no') stats(2)%minv, CSV_DELIM ! MIN_2
                write(funit,601,  advance='no') stats(2)%maxv, CSV_DELIM ! MAX_2
                write(funit,602)                real(pop2)               ! POP_2
            end subroutine bicluster_local

            subroutine write_cls_atoms( cls, which )
                integer,          intent(in) :: cls
                character(len=*), intent(in) :: which
                type(binimage)       :: img_atom
                integer, allocatable :: imat(:,:,:), imat_atom(:,:,:)
                integer :: i
                call img_atom%copy_bimg(self%img_cc)
                allocate(imat_atom(self%ldim(1),self%ldim(2),self%ldim(3)), source = 0)
                call img_atom%get_imat(imat)
                do i = 1, self%n_cc
                    if( cls_mask(i) )then
                        where( imat == i ) imat_atom = 1
                    endif
                enddo
                call img_atom%set_imat(imat_atom)
                if( cls == 1 )then
                    call img_atom%write_bimg('Atoms_'//trim(which)//'_large.mrc')
                else
                    call img_atom%write_bimg('Atoms_'//trim(which)//'_small.mrc')
                endif
                deallocate(imat,imat_atom)
                call img_atom%kill_bimg
            end subroutine write_cls_atoms

    end subroutine bicluster_otsu

    ! This subroutine clusters the atoms with respect to the maximum intensity
    ! or the integrated density (according to the values contained in feature)
    ! using kmeans algorithm with 2 classes. The initial guess fo the centers
    ! is intentionally biased. It supposes there are two distinguished classes
    ! with different avgs (proved with simulated data).
    subroutine cluster_atom_intensity( self, feature )
        use gnufor2
        use simple_nanoML, only: nanoML
        class(nanoparticle), intent(inout) :: self
        real,                intent(inout) :: feature(:)
        integer, parameter   :: MAX_IT = 50 ! maximum number of iterations for
        integer, allocatable :: imat_cc(:,:,:)
        real, pointer        :: rmat1(:,:,:), rmat2(:,:,:)
        type(image)          :: class1, class2
        type(nanoML)         :: emfit
        real    :: centers_kmeans(2) ! output of k-means
        real    :: avgs(2), vars(2), gammas(self%n_cc,2) ! output of ML
        integer :: i, cnt1, cnt2, filnum, io_stat
        feature = feature/maxval(feature)*10. ! normalise with maxval*10
        call hist(feature, 20)
        write(logfhandle,*) '****clustering wrt maximum intensity, init'
        ! Report clusters on images in dedicated directory
        call class1%new(self%ldim, self%smpd)
        call class2%new(self%ldim, self%smpd)
        call class1%get_rmat_ptr(rmat1)
        call class2%get_rmat_ptr(rmat2)
        call self%img_cc%get_imat(imat_cc)
        call fopen(filnum, file='ClusterIntensities.txt', iostat=io_stat)
        ! kmeans
        call emfit%kmeans_biased2classes(feature, centers_kmeans)
        write(filnum,*) 'centers_kmeans', centers_kmeans
        ! ML, fit
        call emfit%new(self%n_cc,2)
        call emfit%set_data(feature)
        call emfit%fit(MAX_IT,centers_kmeans)
        avgs   = emfit%get_avgs()
        vars   = emfit%get_vars()
        gammas = emfit%get_gammas()
        write(filnum,*) 'AVG/VAR 1:', avgs(1), vars(1)
        write(filnum,*) 'AVG/VAR 2:', avgs(2), vars(2)
        cnt2 = 0
        do i = 1, self%n_cc
            if( (avgs(1) - feature(i))**2. < (avgs(2) - feature(i))**2. ) then
                cnt2 = cnt2 + 1
                write(filnum,*) 'connected component #', i, 'belongs to class 1 with probability', max(gammas(i,1),gammas(i,2))
              else
                write(filnum,*) 'connected component #', i, 'belongs to class 2 with probability', max(gammas(i,1),gammas(i,2))
            endif
        enddo
        cnt1 = count((avgs(1) - feature)**2. <  (avgs(2) - feature)**2. )
        cnt2 = count((avgs(1) - feature)**2. >= (avgs(2) - feature)**2. )
        do i = 1, self%n_cc
            if( (avgs(1) - feature(i))**2. < (avgs(2) - feature(i))**2. ) then
                where( imat_cc == i ) rmat1(1:self%ldim(1),1:self%ldim(2),1:self%ldim(3)) = 1.
            else
                where( imat_cc == i ) rmat2(1:self%ldim(1),1:self%ldim(2),1:self%ldim(3)) = 1.
            endif
        enddo
        call class1%write('Class1.mrc')
        call class2%write('Class2.mrc')
        write(filnum,*)  'Class1 contains ', cnt1 ,' atoms'
        write(filnum,*)  'Class2 contains ', cnt2 ,' atoms'
        write(filnum,*)  'Total ', cnt1+cnt2, ' atoms'
        call fclose(filnum)
        call class1%kill
        call class2%kill
        deallocate(imat_cc)
        ! come back to root directory
        write(logfhandle,*) '****clustering wrt maximum intensity, completed'

    contains

        subroutine initialise_centers(data,cen1,cen2)
            real, intent(inout) :: cen1,cen2
            real, intent(inout) :: data(:)
            !>   rheapsort from numerical recepies (largest last)
            call hpsort(data)
            cen1 = sum(data(1:self%n_cc/2))/real(self%n_cc/2)
            cen2 = sum(data(self%n_cc/2+1:size(data)))/real(self%n_cc/2)
        end subroutine initialise_centers

        subroutine update_centers(cen1,cen2,converged,val1,val2)
            real,    intent(inout) :: cen1,cen2
            logical, intent(inout) :: converged
            integer, intent(inout) :: val1, val2
            integer :: i
            integer :: cnt1, cnt2
            real    :: sum1, sum2
            real :: cen1_new, cen2_new
            sum1 = 0.
            cnt1 = 0
            do i=1,self%n_cc
                if( (cen1-feature(i))**2. < (cen2-feature(i))**2. )then
                    cnt1 = cnt1 + 1 ! number of elements in cluster 1
                    sum1 = sum1 + feature(i)
                endif
            end do
            cnt2 = self%n_cc - cnt1       ! number of elements in cluster 2
            sum2 = sum(feature)- sum1
            cen1_new = sum1 / real(cnt1)
            cen2_new = sum2 / real(cnt2)
            if(abs(cen1_new - cen1) < TINY .and. abs(cen2_new - cen2) < TINY) then
                converged = .true.
            else
                converged = .false.
            endif
            ! update
            cen1 = cen1_new
            cen2 = cen2_new
            ! assign values to the centers
            if( cen1 > cen2 )then
                val1           = 1
                val2           = 0
            else
                val1           = 0
                val2           = 1
            endif
        end subroutine update_centers

    end subroutine cluster_atom_intensity

    ! Cluster the atoms wrt the maximum intensity
    ! k-means 2 classes + ML
    subroutine cluster_atom_maxint( self )
        class(nanoparticle), intent(inout) :: self
        real    :: max_intensity(self%n_cc)
        integer :: i, io_stat, filnum
        call fopen(filnum, file='MaxIntensity.csv', action='readwrite', iostat=io_stat)
        if( io_stat .ne. 0 ) then
            THROW_HARD('Unable to read file MaxIntensity.csv Did you run atoms_stats?; cluster_atom_maxint')
        endif
        read(filnum,*) ! first line is variable name
        do i = 1, self%n_cc
            read(filnum,*) max_intensity(i)
        enddo
        call fclose(filnum)
        call self%cluster_atom_intensity(max_intensity)
    end subroutine cluster_atom_maxint

    ! Cluster the atoms wrt the integrated density
    ! k-means 2 classes + ML
    subroutine cluster_atom_intint( self )
        class(nanoparticle), intent(inout) :: self
        real    :: int_intensity(self%n_cc)
        integer :: i, io_stat, filnum
        call fopen(filnum, file='IntIntensity.csv', action='readwrite', iostat=io_stat)
        if( io_stat .ne. 0 ) then
            THROW_HARD('Unable to read file IntIntensity.csv Did you run atoms_stats?; cluster_atom_intint')
        endif
        read(filnum,*) ! first line is variable name
        do i = 1, self%n_cc
            read(filnum,*) int_intensity(i)
        enddo
        call fclose(filnum)
        call self%cluster_atom_intensity(int_intensity)
    end subroutine cluster_atom_intint

    ! Cluster the atoms wrt to the interatomic distances
    subroutine cluster_bondl(self, thresh)
        class(nanoparticle), intent(inout) :: self
        real,    intent(in)  :: thresh ! threshold for class definition, user inputted
        real,    allocatable :: centroids(:)
        integer, allocatable :: labels(:), populations(:)
        real,    allocatable :: stdev_within(:), avg_dist_cog(:)
        integer              :: i, j, ncls, dim, filnum, io_stat
        real                 :: avg, stdev, cog(3)
        type(binimage)       :: img_1clss
        integer, allocatable :: imat_cc(:,:,:), imat_1clss(:,:,:)
        character(len=4)     :: str_thres
        ! Preparing for clustering
        dim = size(self%atominfo)
        allocate(labels(dim), source = 0)
        ! classify
        call hac_1d(self%atominfo(:)%bondl, thresh, labels, centroids, populations)
        ! Stats calculations
        ncls = maxval(labels)
        allocate(stdev_within(ncls), source = 0.)
        allocate(avg_dist_cog(ncls), source = 0.)
        cog = self%masscen()
        ! stdev within the same class
        ! avg dist to the center of gravity of each class
        do i = 1, ncls
            do j = 1, dim
                if(labels(j) == i) then
                   stdev_within(i) = stdev_within(i) + (self%atominfo(j)%bondl - centroids(i))**2.
                   avg_dist_cog(i) = avg_dist_cog(i) + euclid(cog,self%atominfo(j)%center(:))
                endif
            enddo
        enddo
        avg_dist_cog = (avg_dist_cog*self%smpd)/real(populations) ! in A
        where (populations>1)
            stdev_within = sqrt(stdev_within/(real(populations)-1.))
        elsewhere
            stdev_within = 0.
        endwhere
        ! avg and stdev among different classes
        avg = 0.
        do i = 1, ncls
            avg = avg + centroids(i)
        enddo
        avg = avg/real(ncls)
        stdev = 0.
        if(ncls>1) then
          do i = 1, ncls
              stdev = stdev + (centroids(i) - avg)**2
          enddo
          stdev = sqrt(stdev/(real(ncls)-1.))
        endif
        ! Output on a file
        str_thres = trim(real2str(thresh))
        call fopen(filnum, file='ClusterInterDistThresh'//str_thres//'.txt', iostat=io_stat)
        write(unit = filnum,fmt ='(a,i2,a,f6.2)') 'NR OF IDENTIFIED CLUSTERS:', ncls, ' SELECTED THRESHOLD: ',  thresh
        write(unit = filnum,fmt ='(a)') 'CLASSIFICATION '
        do i = 1, dim
          write(unit = filnum,fmt ='(a,i3,a,f6.2,a,i3)') 'Atom #: ', i, '; data (A): ', self%atominfo(i)%bondl, '; class: ', labels(i)
        enddo
        write(unit = filnum,fmt ='(a)') 'CLASS STATISTICS '
        do i = 1, ncls
          write(unit = filnum,fmt ='(a,i3,a,i3,a,f6.2,a,f6.2,a)') 'class: ', i, '; cardinality: ', populations(i), '; centroid: ', centroids(i), ' A; stdev within the class: ', stdev_within(i), ' A'
        enddo
        do i = 1, ncls
          write(unit = filnum,fmt ='(a,i3,a,f6.2,a)') 'class: ', i, '; average distance to the center of gravity: ', avg_dist_cog(i), ' A'
        enddo
        write(unit = filnum,fmt ='(a,f6.2,a,f6.2,a)') 'AVG among the classes: ', avg, ' A; STDEV among the classes: ', stdev, ' A'
        call fclose(filnum)
        call fopen(filnum, file='Dist.csv', iostat=io_stat)
        write(filnum,*) 'dist'
        do i  = 1, self%n_cc
          write(filnum,*) self%atominfo(i)%bondl
        enddo
        call fclose(filnum)
        ! Generate one figure for each class
        if(GENERATE_FIGS) then
          call img_1clss%new_bimg(self%ldim, self%smpd)
          call self%img_cc%get_imat(imat_cc)
          allocate(imat_1clss(self%ldim(1),self%ldim(2),self%ldim(3)), source = 0)
          do i = 1, ncls
              imat_1clss = 0
              do j = 1, dim
                  if(labels(j) == i) then
                      where(imat_cc == j) imat_1clss = i
                  endif
              enddo
              call img_1clss%set_imat(imat_1clss)
              call img_1clss%write_bimg('DistClass'//int2str(i)//'.mrc')
          enddo
          call img_1clss%kill_bimg
        endif
        if(allocated(stdev_within)) deallocate(stdev_within)
        deallocate(centroids, labels, populations)
    end subroutine cluster_bondl

    subroutine kill_nanoparticle(self)
        class(nanoparticle), intent(inout) :: self
        call self%img%kill()
        call self%img_raw%kill
        call self%img_bin%kill_bimg()
        call self%img_cc%kill_bimg()
        if( allocated(self%atominfo) ) deallocate(self%atominfo)
    end subroutine kill_nanoparticle

end module simple_nanoparticle
