program test_numeric
! use netcdf
use rdee_fortran
use EasyNC

implicit none

character(*),parameter :: fname = "test.numeric.nc"

! enc_use_nc4 = .true.

call test_int4
call test_basic
call test_vea

contains
    subroutine test_int4()
        implicit none
        integer(kind=4) :: is1, ia1(2), ia1_(2), ia1_double(2,2), ia2(3,3), ia2_(3,3)
        integer(kind=4), target :: iat1(3)
        integer(kind=4), allocatable :: iaa_1d_1(:), iaa_1d_1_(:), iaa_1d_2(:), iaa_1d_2_(:)
        integer(kind=4), pointer :: iap_1d_1(:) => null(), iap_1d_1_(:) => null(), iap_1d_2(:) => null()
        
        
        print *, '>>>>>>>>>>>>>>>>>>>>>>>>>>>> enter test_int4'
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> gen data
        ! ====================== int4
        is1 = 1
        ia1 = [2,3]
        ia2 = reshape(ispan(1,9), [3,3])
        iat1 = [4,5,6]
        allocate(iaa_1d_1, source = [7,8,9, 10])
        iap_1d_1 => iat1
        allocate(iap_1d_2, source = [11,12])

        allocate(iaa_1d_2(3:5))
        iaa_1d_2 = [3,4,5]
        
        
        call easyO(fname, 'is1', is1)
        call easyO(fname, 'is1', -1)
        call easyI(fname, 'is1', is1)
        call assert(is1 .eq. -1, 'Error in easyI/O for integer-scalar!')
        
        call easyO(fname, 'ia1', ia1)
        call easyI(fname, 'ia1', ia1_)
        call assert(all(ia1 .eq. ia1_), 'Error in easyI/O for integer-array-1d')
        
        call easyO(fname, 'ia2', ia2)
        call easyI(fname, 'ia2', ia2_)
        call assert(all(ia2 .eq. ia2_), 'Error in easyI/O for integer-array-2d')
        
        call easyO(fname, 'ia1_double', ia1, [2,2], [1,1], [1,2])
        call easyO(fname, 'ia1_double', ia1, [2,2], [2,1], [1,2])
        call easyI(fname, 'ia1_double', ia1_double)
        call assert(all(ia1_double .eq. reshape([2,2,3,3], [2,2])), 'Error in easyI/O for integer sub-array')

        call easyO(fname, 'iaa_1d_1', iaa_1d_1)
        call easyOA(fname, 'iaa_1d_1', iaa_1d_1)
        call easyIA(fname, 'iaa_1d_1', iaa_1d_1_)
        call assert(all(iaa_1d_1 .eq. iaa_1d_1_), 'Error in easyI/O for allocatable integer*4')
        deallocate(iaa_1d_1_)

        call easyO(fname, 'iap_1d_1', iap_1d_1)
        call easyOP(fname, 'iap_1d_1', iap_1d_1)
        call easyIP(fname, 'iap_1d_1', iap_1d_1_)
        call easyIA(fname, 'iap_1d_1', iaa_1d_1_)
        print *, iap_1d_1
        print *, iap_1d_1_
        call assert(all(iap_1d_1 .eq. iap_1d_1_), 'Error in easyI/O for pointer integer*4')
        call assert(all(iap_1d_1 .eq. iaa_1d_1_), 'Error in easyI/O for pointer/allocatable integer*4')
        deallocate(iaa_1d_1_)
        deallocate(iap_1d_1_)

        call easyOA(fname, 'iaa_1d_2', iaa_1d_2)
        call easyIP(fname, 'iaa_1d_2', iap_1d_1_)
        print *, iaa_1d_2
        print *, iap_1d_1_
        call assert(all(iaa_1d_2 .eq. iap_1d_1_), 'Error in easyI/O for pointer/allocatable integer*4')
        deallocate(iap_1d_1_)

        call easyIP(fname, 'iaa_1d_1', iap_1d_1_, [2], [2])
        print *, iap_1d_1_
        call assert(all(iap_1d_1_ .eq. [8,9]), 'Error in subarray easyI? for pointer/allocatable integer*4')
        deallocate(iap_1d_1_)

        deallocate(iaa_1d_1)
        deallocate(iaa_1d_2)
        nullify(iap_1d_1)
        deallocate(iap_1d_2)
    end subroutine       

    subroutine test_basic
        implicit none
        integer(kind=8) :: ls1, ls1_, la_1d_1(3)
        real(kind=4) :: fs1, fs1_, fa_1d_1(3)
        real(kind=8) :: ds1, ds1_, da_1d_1(3)
        double precision :: ds2, ds2_, da_1d_2(3)

        integer(kind=8), allocatable :: la_1d_1_(:)
        real(kind=4), allocatable :: fa_1d_1_(:)
        real(kind=8), allocatable :: da_1d_1_(:)
        double precision, allocatable :: da_1d_2_(:)


        print *, '>>>>>>>>>>>>>>>>>>>>>>>>>>>> enter test_basic'

        ls1 = 1
        fs1 = 2.
        ds1 = 3.d0
        ds2 = 4.d0
        
        call easyO(fname, 'fs1', fs1)
        call easyO(fname, 'ds1', ds1)
        call easyO(fname, 'ds2', ds2)

        call easyI(fname, 'fs1', fs1_)
        call easyI(fname, 'ds1', ds1_)
        call easyI(fname, 'ds2', ds2_)

        call assert(fs1 .eq. fs1_, 'Error in easyIO for float')
        call assert(ds1 .eq. ds1_, 'Error in easyIO for double, turn 1')
        call assert(ds2 .eq. ds2_, 'Error in easyIO for double, turn 2')

        
        la_1d_1 = [1,2,3]
        fa_1d_1 = [1.,2.,3.]
        da_1d_1 = [1.d0,2.d0,3.d0]
        da_1d_2 = [1.d0,2.d0,3.d0]
        call easyO(fname, 'fa_1d_1', fa_1d_1)
        call easyO(fname, 'da_1d_1', da_1d_1)
        call easyO(fname, 'da_1d_2', da_1d_2)
        call easyIA(fname, 'fa_1d_1', fa_1d_1_)
        call easyIA(fname, 'da_1d_1', da_1d_1_)
        call easyIA(fname, 'da_1d_2', da_1d_2_)
        call assert(all(fa_1d_1 .eq. fa_1d_1_), 'Error in easyIO for real*4 array')
        call assert(all(da_1d_1 .eq. da_1d_1_), 'Error in easyIO for double array')
        call assert(all(da_1d_2 .eq. da_1d_2_), 'Error in easyIO for double array, turn 2')

        if (enc_use_nc4) then
            call easyO(fname, 'ls1', ls1)
            call easyI(fname, 'ls1', ls1_)
            call assert(ls1 .eq. ls1_, 'Error in easyIO for long')

            call easyO(fname, 'la_1d_1', la_1d_1)
            call easyIA(fname, 'la_1d_1', la_1d_1_)
            call assert(all(la_1d_1 .eq. la_1d_1_), 'Error in easyIO for long array')
        end if

        print *, 'Succeed in test_basic'

    end subroutine


    subroutine test_vea()
        implicit none
        integer :: iT

        print *, '>>>>>>>>>>>>>>>>>>>>>>>>>>>> enter test_vea'

        iT = 0
        enc_vea = 1

        call easyO(fname, 'aaq', 1)
        call easyO(fname, 'aaq', 2)
        call easyI(fname, 'aaq', iT)

        enc_vea = 0

        call assert(iT .eq. 1, 'Error in var-exist action') 

    end subroutine


end program
