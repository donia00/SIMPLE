program simple_test_angres
use simple_oris, only: oris
use simple_defs
implicit none

integer    :: i
type(oris) :: os

do i=500,20000,500
    call os%new(i, is_ptcl=.false.)
    call os%spiral
    write(logfhandle,*) i, os%find_angres()
end do

       !  500    9.15350437
       !  1000   6.47799063
       !  1500   5.30222988
       !  2000   4.58571291
       !  2500   4.10317421
       !  3000   3.75123024
       !  3500   3.46825480
       !  4000   3.24581528
       !  4500   3.05811691
       !  5000   2.90361857
       !  5500   2.76879811
       !  6000   2.64885139
       !  6500   2.54815364
       !  7000   2.45272326
       !  7500   2.36931849
       !  8000   2.29413486
       !  8500   2.22672343
       !  9000   2.16610241
       !  9500   2.10529828
       ! 10000   2.05473161
       ! 10500   2.00406337
       ! 11000   1.95665383
       ! 11500   1.91362453
       ! 12000   1.87344003
       ! 12500   1.83547390
       ! 13000   1.80017865
       ! 13500   1.76786101
       ! 14000   1.73638999
       ! 14500   1.70432270
       ! 15000   1.67727935
       ! 15500   1.64898419
       ! 16000   1.62397599
       ! 16500   1.59781909
       ! 17000   1.57604551
       ! 17500   1.55221510
       ! 18000   1.52975285
       ! 18500   1.50982976
       ! 19000   1.49080479
       ! 19500   1.47121429
       ! 20000   1.45227337

end program simple_test_angres
