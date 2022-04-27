module simple_butterworth
!$ use omp_lib
!$ use omp_lib_kinds
include 'simple_lib.f08'
use simple_defs
use simple_image, only: image
use simple_math,  only: hyp
implicit none

contains

    ! Compute the value of the Butterworth transfer function of order n(th)
    ! at a given frequency s, with the cut-off frequency fc
    ! SOURCE :
    ! https://en.wikipedia.org/wiki/Butterworth_filter
    function butterworth(s, n, fc) result(val)
        real   , intent(in)  :: s
        integer, intent(in)  :: n
        real   , intent(in)  :: fc
        real                 :: val
        real,    parameter :: AN(9) = (/ 1., 5.1258, 13.1371, 21.8462, 25.6884, 21.8462, 13.1371, 5.1258, 1./)
        complex, parameter :: J = (0, 1) ! Complex identity: j = sqrt(-1)
        complex :: Bn, Kn                ! Normalized Butterworth polynomial, its derivative and its reciprocal
        complex :: js                    ! frequency is multiplied by the complex identity j
        integer :: k
        Bn  = (0., 0.)
        if (s/fc < 100) then
            js  = j*s/fc
            do k = 0, n
                Bn  = Bn + AN(k+1)*js**k
            end do
            Kn  = 1/Bn
            val = sqrt(real(Kn)**2 + aimag(Kn)**2)
        else
            val = epsilon(val)
        endif
    end function butterworth

    ! Compute the Butterworth kernel of the order n-th of width w
    ! with the cut-off frequency fc
    ! https://en.wikipedia.org/wiki/Butterworth_filter
    subroutine butterworth_filter(ker, n, fc)
        real,    intent(inout) :: ker(:)
        integer, intent(in)    :: n
        real   , intent(in)    :: fc
        integer :: k, l, j, half_w, ldim3, freq_val
        do freq_val = 1, size(ker)
            ker(freq_val) = butterworth(real(freq_val-1), n, fc)
        enddo        
    end subroutine butterworth_filter

    subroutine squared_diff(odd, even, diff)
        real, intent(in)    :: odd(:,:,:)
        real, intent(in)    :: even(:,:,:)
        real, intent(inout) :: diff(:,:,:)
        diff = (odd - even)**2
    end subroutine squared_diff

    ! normalized to 1 then take the squared diff
    subroutine same_energy_squared_diff(odd, even, diff)
        real, intent(in)    :: odd(:,:,:)
        real, intent(in)    :: even(:,:,:)
        real, intent(inout) :: diff(:,:,:)
        call squared_diff(odd/sum(odd), even/sum(even), diff)
    end subroutine same_energy_squared_diff

    ! from https://stats.stackexchange.com/questions/136232/definition-of-normalized-euclidean-distance#:~:text=The%20normalized%20squared%20euclidean%20distance,not%20related%20to%20Mahalanobis%20distance
    subroutine normalized_squared_diff(odd, even, diff)
        real, intent(in)    :: odd(:,:,:)
        real, intent(in)    :: even(:,:,:)
        real, intent(inout) :: diff(:,:,:)
        real                :: mean_odd, mean_even
        mean_odd  =  sum(odd)/product(shape(odd))
        mean_even = sum(even)/product(shape(even))
        call squared_diff(odd-mean_odd, even-mean_even, diff)
        diff = diff/(sum(odd-mean_odd)**2 + sum(even-mean_even)**2)
    end subroutine normalized_squared_diff

    ! optimized voxelwise uniform/nonuniform filter, using the (low-pass/butterworth)
    subroutine opt_voxel_fil(odd, even, smpd, is_uniform)
        type(image),      intent(inout) :: odd
        type(image),      intent(inout) :: even
        real,             intent(in)    :: smpd
        character(len=*), intent(in)    :: is_uniform
        type(image)          :: odd_copy, even_copy
        integer              :: k,l,m,max_lplim, box, dim3, ldim(3), find_start, find_stop, best_ind, cur_ind
        real                 :: cur_min_sum
        integer, parameter   :: CHUNKSZ=20, BW_ORDER=8, FIND_STEPSZ=2
        real,    parameter   :: LP_START = 30. ! 30 A resolution
        real,    pointer     :: rmat_odd(:,:,:)=>null(), rmat_even(:,:,:)=>null()
        real,    allocatable :: opt_mat_odd(:,:,:), opt_mat_even(:,:,:), cur_diff(:,:,:), opt_diff(:,:,:), but_fil(:)
        ldim = odd%get_ldim()
        box  = ldim(1)
        dim3 = ldim(3)
        find_stop  = calc_fourier_index(2. * smpd, box, smpd)
        find_start = calc_fourier_index(LP_START, box, smpd)
        call odd_copy%copy(odd)
        call even_copy%copy(even)
        allocate(opt_mat_odd(box,box,dim3), opt_mat_even(box,box,dim3), cur_diff(box,box,dim3), opt_diff(box,box,dim3), but_fil(box), source=0.)
        opt_diff    = huge(cur_min_sum)
        cur_min_sum  = huge(cur_min_sum)   
        best_ind     = find_start
        do cur_ind = find_start, find_stop, FIND_STEPSZ
            write(*, *) 'current Fourier index = ', cur_ind
            ! filtering odd
            call odd%copy(odd_copy)
            call odd%fft
            !call odd%lp(cur_ind)
            call butterworth_filter(but_fil, BW_ORDER, real(cur_ind))
            call odd%apply_filter(but_fil)
            call odd%ifft
            call odd%get_rmat_ptr(rmat_odd)
            call even%copy(even_copy)
            call even%get_rmat_ptr(rmat_even)
            call normalized_squared_diff(rmat_odd, rmat_even, cur_diff)
            ! filtering even using the same filter
            call even%fft
            call even%apply_filter(but_fil)
            call even%ifft
            call even%get_rmat_ptr(rmat_even)
            ! do the non-uniform, i.e. optimizing at each voxel
            if (is_uniform == 'no') then
                ! 2D vs 3D cases
                if (dim3 == 1) then
                    !$omp parallel do collapse(2) default(shared) private(k,l) schedule(dynamic,CHUNKSZ) proc_bind(close)
                    do k = 1,box
                        do l = 1,box
                            ! opt_diff     keeps the minimized cost value at each voxel of the search
                            ! opt_mat_odd  keeps the best voxel of the form B*odd
                            ! opt_mat_even keeps the best voxel of the form B*even
                            if (cur_diff(k,l,1) < opt_diff(k,l,1)) then
                                opt_mat_odd(k,l,1)  = rmat_odd(k,l,1)
                                opt_mat_even(k,l,1) = rmat_even(k,l,1)
                                opt_diff(k,l,1)     = cur_diff(k,l,1)
                            endif
                        enddo
                    enddo
                    !$omp end parallel do
                else
                    !$omp parallel do collapse(3) default(shared) private(k,l,m) schedule(dynamic,CHUNKSZ) proc_bind(close)
                    do k = 1,box
                        do l = 1,box
                            do m = 1,box
                                ! opt_diff     keeps the minimized cost value at each voxel of the search
                                ! opt_mat_odd  keeps the best voxel of the form B*odd
                                ! opt_mat_even keeps the best voxel of the form B*even
                                if (cur_diff(k,l,m) < opt_diff(k,l,m)) then
                                    opt_mat_odd(k,l,m)  = rmat_odd(k,l,m)
                                    opt_mat_even(k,l,m) = rmat_even(k,l,m)
                                    opt_diff(k,l,m)     = cur_diff(k,l,m)
                                endif
                            enddo
                        enddo
                    enddo
                    !$omp end parallel do
                endif
                cur_min_sum = sum(opt_diff)
            else
                ! keep the theta which gives the lowest cost (over all voxels)
                if (sum(cur_diff) < cur_min_sum) then
                    opt_mat_odd  = rmat_odd
                    opt_mat_even = rmat_even
                    cur_min_sum  = sum(cur_diff)
                    best_ind     = cur_ind
                endif
            endif
            write(*, *) 'min cost val = ', cur_min_sum, '; current cost = ', sum(cur_diff)
        enddo
        if (is_uniform == 'yes') then
            write(*, *) 'minimized cost at index = ', best_ind
        endif
        call odd%set_rmat(opt_mat_odd,   .false.)
        call even%set_rmat(opt_mat_even, .false.)
    end subroutine opt_voxel_fil
end module simple_butterworth
    