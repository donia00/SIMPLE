! projection of 3D volumes in the Fourier domain by convolution interpolation
! to generate band-pass limited Cartesian and polar 2D Fourier transforms
module simple_projector
!$ use omp_lib
!$ use omp_lib_kinds
include 'simple_lib.f08'

use simple_kbinterpol, only: kbinterpol
use simple_image,      only: image
use simple_ori,        only: ori
use simple_ori_light,  only: ori_light
use simple_oris,       only: oris
use simple_parameters, only: params_glob
implicit none

public :: projector
private
#include "simple_local_flags.inc"

complex, parameter :: CMPLX_ZERO = cmplx(0.,0.)

type, extends(image) :: projector
    private
    procedure(interp_fcomp_fun),     pointer, public :: interp_fcomp     !< pointer to interpolation function
    procedure(fdf_interp_fcomp_fun), pointer, public :: fdf_interp_fcomp !< pointer to interpolation function for derivatives
    type(kbinterpol)      :: kbwin                   !< window function object
    integer               :: ldim_exp(3,2) = 0       !< expanded FT matrix limits
    complex, allocatable  :: cmat_exp(:,:,:)         !< expanded FT matrix
    real,    allocatable  :: polweights_mat(:,:,:)   !< polar weights matrix for the image to polar transformer
    integer, allocatable  :: polcyc1_mat(:,:,:)      !< image cyclic adresses for the image to polar transformer
    integer, allocatable  :: polcyc2_mat(:,:,:)      !< image cyclic adresses for the image to polar transformer
    logical, allocatable  :: is_in_mask(:,:,:)       !< neighbour matrix for the shape mask projector
    integer               :: wdim      = 0           !< dimension of K-B window
    integer               :: wdim_cubd = 0           !< dimension of K-B window
    integer               :: iwinsz    = 0           !< integer half-window size
    logical               :: expanded_exists=.false. !< indicates FT matrix existence
  contains
    ! CONSTRUCTORS
    procedure          :: expand_cmat
    ! SETTERS
    procedure          :: reset_expanded
    ! GETTERS
    procedure          :: is_expanded
    ! FOURIER PROJECTORS
    procedure          :: fproject
    procedure, private :: fproject_serial_1
    procedure, private :: fproject_serial_2
    generic            :: fproject_serial => fproject_serial_1, fproject_serial_2
    procedure          :: fproject_polar
    procedure          :: fdf_project_polar
    procedure          :: interp_fcomp_norm
    procedure          :: interp_fcomp_grid
    procedure          :: interp_fcomp_trilinear
    procedure          :: fdf_interp_fcomp_norm
    procedure          :: fdf_interp_fcomp_grid
    ! DESTRUCTOR
    procedure          :: kill_expanded
end type projector

interface
    pure complex function interp_fcomp_fun( self, loc )
        import :: projector
        class(projector), intent(in) :: self
        real,             intent(in) :: loc(3)
    end function

    pure subroutine fdf_interp_fcomp_fun( self, drotmat, loc, q, fcomp, dfcomp )
        import :: projector
        class(projector), intent(in)  :: self
        real(8),          intent(in)  :: drotmat(3,3,3), loc(3), q(3)
        complex,          intent(out) :: fcomp, dfcomp(3)
    end subroutine
end interface

contains

    ! CONSTRUCTOR

    !>  \brief  is a constructor of the expanded Fourier matrix
    subroutine expand_cmat( self, alpha, norm4proj )
        class(projector),  intent(inout) :: self
        real,              intent(in)    :: alpha !< oversampling factor
        logical, optional, intent(in)    :: norm4proj
        integer, allocatable :: cyck(:), cycm(:), cych(:)
        real    :: factor
        integer :: h, k, m, phys(3), logi(3), lims(2,3), ldim(3)
        logical :: l_norm4proj
        call self%kill_expanded
        ldim = self%get_ldim()
        if( .not.self%is_ft() ) THROW_HARD('volume needs to be FTed before call; expand_cmat')
        if( ldim(3) == 1      ) THROW_HARD('only for volumes; expand_cmat')
        self%kbwin         = kbinterpol(KBWINSZ, alpha)
        self%iwinsz        = ceiling(self%kbwin%get_winsz() - 0.5)
        self%wdim          = self%kbwin%get_wdim()
        self%wdim_cubd     = self%wdim**3
        lims               = transpose(self%loop_lims(3))
        self%ldim_exp(:,2) = maxval(abs(lims)) + ceiling(self%kbwin%get_winsz())
        self%ldim_exp(:,1) = -self%ldim_exp(:,2)
        ! normalize when working with 2D projections
        ! not for producing real space projections
        l_norm4proj = .false.
        if( present(norm4proj) ) l_norm4proj = norm4proj
        factor = 1.
        if( l_norm4proj ) factor = real(params_glob%box)**3 / real(params_glob%box)**2
        allocate( self%cmat_exp( self%ldim_exp(1,1):self%ldim_exp(1,2),&
                                &self%ldim_exp(2,1):self%ldim_exp(2,2),&
                                &self%ldim_exp(3,1):self%ldim_exp(3,2)),&
                                &cych(self%ldim_exp(1,1):self%ldim_exp(1,2)),&
                                &cyck(self%ldim_exp(2,1):self%ldim_exp(2,2)),&
                                &cycm(self%ldim_exp(3,1):self%ldim_exp(3,2)))
        ! pre-compute addresses in 2nd and 3rd dimension
        do h = self%ldim_exp(1,1),self%ldim_exp(1,2)
            cych(h) = h
            if( h < lims(1,1) .or. h > lims(2,1) ) cych(h) = cyci_1d( lims(:,1),h )
        enddo
        do k = self%ldim_exp(2,1),self%ldim_exp(2,2)
            cyck(k) = k
            if( k < lims(1,2) .or. k > lims(2,2) ) cyck(k) = cyci_1d( lims(:,2),k )
        enddo
        do m = self%ldim_exp(3,1),self%ldim_exp(3,2)
            cycm(m) = m
            if( m < lims(1,3) .or. m > lims(2,3) ) cycm(m) = cyci_1d( lims(:,3),m )
        enddo
        ! build expanded fourier components matrix
        self%cmat_exp = CMPLX_ZERO
        !$omp parallel do collapse(3) schedule(static) default(shared)&
        !$omp private(h,k,m,logi,phys) proc_bind(close)
        do h = self%ldim_exp(1,1),self%ldim_exp(1,2)
            do k = self%ldim_exp(2,1),self%ldim_exp(2,2)
                do m = self%ldim_exp(3,1),self%ldim_exp(3,2)
                    logi = [cych(h),cyck(k),cycm(m)]
                    phys = self%comp_addr_phys(logi)
                    self%cmat_exp(h,k,m) = factor * self%get_fcomp(logi, phys)
                enddo
            enddo
        enddo
        !$omp end parallel do
        deallocate( cych,cyck,cycm )
        ! gridding correction & interpolation
        select case(trim(params_glob%interpfun))
            case('linear')
                self%interp_fcomp => interp_fcomp_trilinear
            case DEFAULT
                ! defaults to Kaiser-Bessel
                self%interp_fcomp     => interp_fcomp_norm
                self%fdf_interp_fcomp => fdf_interp_fcomp_norm
                if( params_glob%gridding.eq.'yes')then
                    self%interp_fcomp     => interp_fcomp_grid
                    self%fdf_interp_fcomp => fdf_interp_fcomp_grid
                endif
        end select
        self%expanded_exists = .true.
    end subroutine expand_cmat

    ! SETTERS

    !>  \brief  sets the expanded Fourier matrix to zero
    subroutine reset_expanded( self )
        class(projector), intent(inout) :: self
        if( .not.self%expanded_exists )&
            &THROW_HARD('expanded Fourier matrix does not exist; reset_expanded')
        self%cmat_exp = CMPLX_ZERO
    end subroutine reset_expanded

    ! GETTERS

    logical function is_expanded( self )
        class(projector), intent(in) :: self
        is_expanded = self%expanded_exists
    end function is_expanded

    ! FOURIER PROJECTORS

    !> \brief  extracts a Fourier plane from the expanded FT matrix of a volume (self)
    subroutine fproject( self, e, fplane )
        class(projector), intent(inout) :: self
        class(ori),       intent(in)    :: e
        class(image),     intent(inout) :: fplane
        real        :: loc(3), e_rotmat(3,3)
        integer     :: h, k, sqarg, sqlp, lims(3,2), phys(3), ldim(3)
        complex(sp) :: comp
        lims   = self%loop_lims(2)
        sqlp   = (maxval(lims(:,2)))**2
        ldim   = fplane%get_ldim()
        call fplane%zero_and_flag_ft()
        e_rotmat = e%get_mat()
        !$omp parallel do collapse(2) schedule(static) default(shared)&
        !$omp private(h,k,sqarg,loc,phys,comp) proc_bind(close)
        do h=lims(1,1),lims(1,2)
            do k=lims(2,1),lims(2,2)
                sqarg = dot_product([h,k],[h,k])
                if( sqarg > sqlp ) cycle
                loc  = matmul(real([h,k,0]), e_rotmat)
                comp = self%interp_fcomp(loc)
                if (h > 0) then
                    phys(1) = h + 1
                    phys(2) = k + 1 + MERGE(ldim(2),0,k < 0)
                    phys(3) = 1
                    call fplane%set_cmat_at(phys, comp)
                else
                    phys(1) = -h + 1
                    phys(2) = -k + 1 + MERGE(ldim(2),0,-k < 0)
                    phys(3) = 1
                    call fplane%set_cmat_at(phys, conjg(comp))
                endif
            end do
        end do
        !$omp end parallel do
    end subroutine fproject

    !> \brief  extracts a Fourier plane from the expanded FT matrix of a volume (self)
    subroutine fproject_serial_1( self, e, fplane )
        class(projector),  intent(inout) :: self
        class(ori),        intent(in)    :: e
        class(image),      intent(inout) :: fplane
        real        :: loc(3), e_rotmat(3,3)
        integer     :: h, k, sqarg, sqlp, lims(3,2), phys(3), ldim(3)
        complex(sp) :: comp
        lims = self%loop_lims(2)
        sqlp = (maxval(lims(:,2)))**2
        ldim = fplane%get_ldim()
        call fplane%zero_and_flag_ft()
        e_rotmat = e%get_mat()
        do h=lims(1,1),lims(1,2)
            do k=lims(2,1),lims(2,2)
                sqarg = dot_product([h,k],[h,k])
                if( sqarg > sqlp ) cycle
                loc  = matmul(real([h,k,0]), e_rotmat)
                comp = self%interp_fcomp(loc)
                if (h > 0) then
                    phys(1) = h + 1
                    phys(2) = k + 1 + MERGE(ldim(2),0,k < 0)
                    phys(3) = 1
                    call fplane%set_cmat_at(phys, comp)
                else
                    phys(1) = -h + 1
                    phys(2) = -k + 1 + MERGE(ldim(2),0,-k < 0)
                    phys(3) = 1
                    call fplane%set_cmat_at(phys, conjg(comp))
                endif
            end do
        end do
    end subroutine fproject_serial_1

    !> \brief  extracts a Fourier plane from the expanded FT matrix of a volume (self)
    subroutine fproject_serial_2( self, ospace, iref, fplane, find_lp )
        class(projector),  intent(inout) :: self
        class(oris),       intent(in)    :: ospace
        integer,           intent(in)    :: iref
        class(image),      intent(inout) :: fplane
        integer, optional, intent(in)    :: find_lp
        real        :: loc(3), e_rotmat(3,3)
        integer     :: h, k, sqarg, sqlp, lims(3,2), phys(3), ldim(3)
        complex(sp) :: comp
        lims = self%loop_lims(2)
        sqlp = find_lp * find_lp
        ldim = fplane%get_ldim()
        call fplane%zero_and_flag_ft()
        e_rotmat = ospace%get_mat(iref)
        do h=lims(1,1),lims(1,2)
            do k=lims(2,1),lims(2,2)
                sqarg = dot_product([h,k],[h,k])
                if( sqarg > sqlp ) cycle
                loc  = matmul(real([h,k,0]), e_rotmat)
                comp = self%interp_fcomp(loc)
                if (h > 0) then
                    phys(1) = h + 1
                    phys(2) = k + 1 + MERGE(ldim(2),0,k < 0)
                    phys(3) = 1
                    call fplane%set_cmat_at(phys, comp)
                else
                    phys(1) = -h + 1
                    phys(2) = -k + 1 + MERGE(ldim(2),0,-k < 0)
                    phys(3) = 1
                    call fplane%set_cmat_at(phys, conjg(comp))
                endif
            end do
        end do
    end subroutine fproject_serial_2

    !> \brief  extracts a polar FT from a volume's expanded FT (self)
    subroutine fproject_polar( self, iref, e, pftcc, iseven, mask )
        use simple_polarft_corrcalc, only: polarft_corrcalc
        class(projector),        intent(inout) :: self    !< projector object
        integer,                 intent(in)    :: iref    !< which reference
        class(ori),              intent(in)    :: e       !< orientation
        class(polarft_corrcalc), intent(inout) :: pftcc   !< object that holds the polar image
        logical,                 intent(in)    :: iseven  !< eo flag
        logical,                 intent(in)    :: mask(:) !< interpolation mask, all .false. set to CMPLX_ZERO
        integer :: irot, k, pdim(3)
        real    :: vec(3), loc(3), e_rotmat(3,3)
        pdim = pftcc%get_pdim()
        e_rotmat = e%get_mat()
        do irot=1,pdim(1)
            do k=pdim(2),pdim(3)
                if( mask(k) )then
                    vec(:2) = pftcc%get_coord(irot,k)
                    vec(3)  = 0.
                    loc     = matmul(vec,e_rotmat)
                    call pftcc%set_ref_fcomp(iref, irot, k, self%interp_fcomp(loc), iseven)
                else
                    call pftcc%set_ref_fcomp(iref, irot, k, CMPLX_ZERO, iseven)
                endif
            end do
        end do
    end subroutine fproject_polar

    !> \brief  extracts a polar FT from a volume's expanded FT (self)
    subroutine fdf_project_polar( self, iref, euls, pftcc, iseven, mask )
        use simple_polarft_corrcalc, only: polarft_corrcalc
        class(projector),        intent(inout) :: self    !< projector object
        integer,                 intent(in)    :: iref    !< which reference
        real(dp),                intent(in)    :: euls(3) !< orientation
        class(polarft_corrcalc), intent(inout) :: pftcc   !< object that holds the polar image
        logical,                 intent(in)    :: iseven  !< eo flag
        logical,                 intent(in)    :: mask(:) !< interpolation mask, all .false. set to CMPLX_ZERO
        integer         :: irot, k, pdim(3)
        real(dp)        :: vec(3), loc(3), dR(3,3,3)
        complex         :: fcomp, dfcomp(3)
        type(ori_light) :: or
        pdim = pftcc%get_pdim()
        call or%euler2dm(euls, dR)
        do irot=1,pdim(1)
            do k=pdim(2),pdim(3)
                if( mask(k) )then
                    vec(:2) = real(pftcc%get_coord(irot,k),kind=dp)
                    vec(3)  = 0._dp
                    loc     = matmul(vec,or%euler2m(euls))
                    call self%fdf_interp_fcomp(dR, loc, vec, fcomp, dfcomp)
                    call pftcc%set_ref_fcomp( iref, irot, k, fcomp, iseven )
                    call pftcc%set_dref_fcomp( iref, irot, k, dfcomp, iseven )
                else
                    dfcomp = CMPLX_ZERO
                    call pftcc%set_ref_fcomp( iref, irot, k, CMPLX_ZERO, iseven)
                    call pftcc%set_dref_fcomp( iref, irot, k, dfcomp, iseven )
                endif
            end do
        end do
    end subroutine fdf_project_polar

    !>  \brief is to interpolate from the expanded complex matrix
    pure function interp_fcomp_norm( self, loc )result( comp )
        class(projector), intent(in) :: self
        real,             intent(in) :: loc(3)
        complex :: comp
        real    :: w(1:self%wdim,1:self%wdim,1:self%wdim)
        integer :: i, win(2,3) ! window boundary array in fortran contiguous format
        ! interpolation kernel window
        win(1,:) = nint(loc)
        win(2,:) = win(1,:) + self%iwinsz
        win(1,:) = win(1,:) - self%iwinsz
        ! interpolation kernel matrix
        w = 1.
        do i=1,self%wdim
            w(i,:,:) = w(i,:,:) * self%kbwin%apod( real(win(1,1)+i-1)-loc(1) )
            w(:,i,:) = w(:,i,:) * self%kbwin%apod( real(win(1,2)+i-1)-loc(2) )
            w(:,:,i) = w(:,:,i) * self%kbwin%apod( real(win(1,3)+i-1)-loc(3) )
        end do
        ! SUM( kernel x components )
        comp = sum( w * self%cmat_exp(win(1,1):win(2,1), win(1,2):win(2,2),win(1,3):win(2,3)) ) / sum(w)
    end function interp_fcomp_norm

    !>  \brief is to interpolate from the expanded complex matrix
    pure function interp_fcomp_grid( self, loc )result( comp )
        class(projector), intent(in) :: self
        real,             intent(in) :: loc(3)
        complex :: comp
        real    :: w(1:self%wdim,1:self%wdim,1:self%wdim)
        integer :: i, win(2,3) ! window boundary array in fortran contiguous format
        ! interpolation kernel window
        win(1,:) = nint(loc)
        win(2,:) = win(1,:) + self%iwinsz
        win(1,:) = win(1,:) - self%iwinsz
        ! interpolation kernel matrix
        w = 1.
        do i=1,self%wdim
            w(i,:,:) = w(i,:,:) * self%kbwin%apod( real(win(1,1)+i-1)-loc(1) )
            w(:,i,:) = w(:,i,:) * self%kbwin%apod( real(win(1,2)+i-1)-loc(2) )
            w(:,:,i) = w(:,:,i) * self%kbwin%apod( real(win(1,3)+i-1)-loc(3) )
        end do
        ! SUM( kernel x components )
        comp = sum( w * self%cmat_exp(win(1,1):win(2,1), win(1,2):win(2,2),win(1,3):win(2,3)) )
    end function interp_fcomp_grid

    !>  \brief is for tri-linear interpolation from the expanded complex matrix
    pure function interp_fcomp_trilinear( self, loc )result( comp )
        class(projector), intent(in) :: self
        real,             intent(in) :: loc(3)
        complex :: comp
        real    :: w(2,2,2), d(3), dp(3)
        integer :: lb(3)
        lb = floor(loc)
        d  = loc - real(lb)
        dp = 1. - d
        w(1,1,1) = product(dp)
        w(2,1,1) =  d(1) * dp(2) * dp(3)
        w(1,2,1) = dp(1) *  d(2) * dp(3)
        w(1,1,2) = dp(1) * dp(2) *  d(3)
        w(2,1,2) =  d(1) * dp(2) *  d(3)
        w(1,2,2) = dp(1) *  d(2) *  d(3)
        w(2,2,1) =  d(1) *  d(2) * dp(3)
        w(2,2,2) = product(d)
        comp = sum(w * self%cmat_exp(lb(1):lb(1)+1,lb(2):lb(2)+1,lb(3):lb(3)+1))
    end function interp_fcomp_trilinear

    !>  \brief is to compute the derivative of the interpolate from the expanded complex matrix
    !! \param loc 3-dimensional location in the volume
    !! \param q 2-dimensional location on the plane (h,k,0)
    pure subroutine fdf_interp_fcomp_norm( self, drotmat, loc, q, fcomp, dfcomps )
        class(projector), intent(in)                             :: self
        real(dp),         intent(in)                             :: drotmat(3,3,3), loc(3), q(3)
        complex(sp),      intent(out)                            :: fcomp, dfcomps(3)
        real(dp), dimension(1:self%wdim,1:self%wdim,1:self%wdim) :: w1,   w2,  w3, w
        real(dp), dimension(1:self%wdim,1:self%wdim,1:self%wdim) :: dw1, dw2, dw3
        real(dp)                                                 :: dRdangle(3), dapod_tmp(3,3)
        complex(dp)                                              :: N     !numerator
        real(dp)                                                 :: D, D2 !denominator
        integer                                                  :: i, j, win(2,3) ! window boundary array in fortran contiguous format
        win(1,:) = nint(loc)
        win(2,:) = win(1,:) + self%iwinsz
        win(1,:) = win(1,:) - self%iwinsz
        w1 = 1.0_dp ; w2 = 1.0_dp ; w3 = 1.0_dp
        do i = 1,self%wdim
            w1(i,:,:) = self%kbwin%apod_dp( real( win(1,1)+i-1,kind=dp) - loc(1) )
            w2(:,i,:) = self%kbwin%apod_dp( real( win(1,2)+i-1,kind=dp) - loc(2) )
            w3(:,:,i) = self%kbwin%apod_dp( real( win(1,3)+i-1,kind=dp) - loc(3) )
            dapod_tmp(1,i) = self%kbwin%dapod( real( win(1,1)+i-1,kind=dp) - loc(1) )
            dapod_tmp(2,i) = self%kbwin%dapod( real( win(1,2)+i-1,kind=dp) - loc(2) )
            dapod_tmp(3,i) = self%kbwin%dapod( real( win(1,3)+i-1,kind=dp) - loc(3) )
        end do
        w  = w1 * w2 * w3
        N  = sum(w * self%cmat_exp(win(1,1):win(2,1),win(1,2):win(2,2),win(1,3):win(2,3)))
        D  = sum(w)
        D2 = D*D
        fcomp = cmplx( N / D )
        do j = 1,3 ! theta, phi, psi
            dRdangle = matmul(q, drotmat(:,:,j))
            do i = 1,self%wdim
                dw1(i,:,:) = - dapod_tmp(1,i) * dRdangle(1)
                dw2(:,i,:) = - dapod_tmp(2,i) * dRdangle(2)
                dw3(:,:,i) = - dapod_tmp(3,i) * dRdangle(3)
            end do
            w = dw1  * w2 * w3  + w1 * (dw2  * w3  + w2 * dw3)
            dfcomps(j) = cmplx((sum(w * self%cmat_exp(win(1,1):win(2,1),win(1,2):win(2,2),win(1,3):win(2,3))) * D - N * sum(w)) / D2)
        enddo
    end subroutine fdf_interp_fcomp_norm

    !>  \brief is to compute the derivative of the interpolate from the expanded complex matrix
    !>  \brief no normalization by interpolation weight occurs
    !! \param loc 3-dimensional location in the volume
    !! \param q 2-dimensional location on the plane (h,k,0)
    pure subroutine fdf_interp_fcomp_grid( self, drotmat, loc, q, fcomp, dfcomps )
        class(projector), intent(in)                             :: self
        real(dp),         intent(in)                             :: drotmat(3,3,3), loc(3), q(3)
        complex(sp),      intent(out)                            :: fcomp, dfcomps(3)
        real(dp), dimension(1:self%wdim,1:self%wdim,1:self%wdim) :: w1,   w2,  w3, w
        real(dp), dimension(1:self%wdim,1:self%wdim,1:self%wdim) :: dw1, dw2, dw3
        real(dp)                                                 :: dRdangle(3), dapod_tmp(3,3)
        integer                                                  :: i, j, win(2,3) ! window boundary array in fortran contiguous format
        win(1,:) = nint(loc)
        win(2,:) = win(1,:) + self%iwinsz
        win(1,:) = win(1,:) - self%iwinsz
        w1  = 1.0_dp ; w2  = 1.0_dp ; w3  = 1.0_dp
        do i=1,self%wdim
            w1(i,:,:) = self%kbwin%apod_dp( real( win(1,1)+i-1,kind=dp) - loc(1) )
            w2(:,i,:) = self%kbwin%apod_dp( real( win(1,2)+i-1,kind=dp) - loc(2) )
            w3(:,:,i) = self%kbwin%apod_dp( real( win(1,3)+i-1,kind=dp) - loc(3) )
            dapod_tmp(1,i) = self%kbwin%dapod( real( win(1,1)+i-1,kind=dp) - loc(1) )
            dapod_tmp(2,i) = self%kbwin%dapod( real( win(1,2)+i-1,kind=dp) - loc(2) )
            dapod_tmp(3,i) = self%kbwin%dapod( real( win(1,3)+i-1,kind=dp) - loc(3) )
        end do
        w     = w1 * w2 * w3
        fcomp = cmplx(sum(w*self%cmat_exp(win(1,1):win(2,1),win(1,2):win(2,2),win(1,3):win(2,3))))
        do j = 1,3 ! theta, phi, psi
            dRdangle = matmul(q, drotmat(:,:,j))
            do i = 1,self%wdim
                dw1(i,:,:) = - dapod_tmp(1,i) * dRdangle(1)
                dw2(:,i,:) = - dapod_tmp(2,i) * dRdangle(2)
                dw3(:,:,i) = - dapod_tmp(3,i) * dRdangle(3)
            end do
            w = dw1  * w2 * w3  + w1 * (dw2  * w3  + w2 * dw3)
            dfcomps(j) = cmplx(sum(w*self%cmat_exp(win(1,1):win(2,1),win(1,2):win(2,2),win(1,3):win(2,3))))
        enddo
    end subroutine fdf_interp_fcomp_grid

    !>  \brief  is a destructor of expanded matrices (imgpolarizer AND expanded projection of)
    subroutine kill_expanded( self )
        class(projector), intent(inout) :: self !< projector instance
        if( allocated(self%cmat_exp) )deallocate(self%cmat_exp)
        self%ldim_exp        = 0
        self%expanded_exists = .false.
    end subroutine kill_expanded

end module simple_projector
