Program main
    use rdee_fortran
    implicit none


    call test_ispan
    call test_fspan
    call test_remove_val
    call test_union_arr1d

Contains
    Subroutine test_ispan()
        implicit none
        
        call assert(all(ispan(1,3) .eq. [1,2,3]), 'Error in ispan')
        call assert(all(ispan(1,3,2) .eq. [1,3]), 'Error in ispan')
        call assert(all(ispan(1,4,2) .eq. [1,3]), 'Error in ispan')
        print *, 'succeed in test for ispan'

    End Subroutine

    Subroutine test_fspan()
        implicit none
       
        
        call assert(all(fspan(1.5,3.5, 5) .eq. [1.5,2., 2.5,3.,3.5]), 'Error in fspan')
        print *, 'succeed in test for fspan'

    End Subroutine

    Subroutine test_remove_val()
        implicit none

        integer :: ia_1d_1(9)
        integer, allocatable :: iaa_1d_1(:)

        character(80) :: sa_1d_1(3)
        character(80), allocatable :: saa_1d_1(:)

        ia_1d_1 = [1,2,3,4,5,6,1,2,3]
        sa_1d_1(1) = 'hello'
        sa_1d_1(2) = 'you'
        sa_1d_1(3) = 'hello'

        call remove_val(ia_1d_1, 1, iaa_1d_1)
        print *, iaa_1d_1
        call assert(all(iaa_1d_1 .eq. [2,3,4,5,6,1,2,3]), 'Error in array.remove_val')
        deallocate(iaa_1d_1)

        call remove_val(ia_1d_1, 1, iaa_1d_1, 2)
        print *, iaa_1d_1
        call assert(all(iaa_1d_1 .eq. [2,3,4,5,6,2,3]), 'Error in array.remove_val')
        deallocate(iaa_1d_1)

        call remove_val(ia_1d_1, 1, iaa_1d_1, -1)
        print *, iaa_1d_1
        call assert(all(iaa_1d_1 .eq. [2,3,4,5,6,2,3]), 'Error in array.remove_val')
        deallocate(iaa_1d_1)

        call remove_val(ia_1d_1, 1, iaa_1d_1, 1, rev_=1)
        print *, iaa_1d_1
        call assert(all(iaa_1d_1 .eq. [1,2,3,4,5,6,2,3]), 'Error in array.remove_val')
        deallocate(iaa_1d_1)


        call remove_val(sa_1d_1, 'hello', saa_1d_1, -1)
        print *, saa_1d_1
        print *, size(saa_1d_1)
        print *, saa_1d_1
        print *, lbound(saa_1d_1)
        call assert(size(saa_1d_1) .eq. 1 .and. saa_1d_1(1) .eq. 'you', 'Error in array.remove_val_string, turn 1')
        deallocate(saa_1d_1) 

        call remove_val(sa_1d_1, 'hello', saa_1d_1, rev_=1)
        print *, saa_1d_1
        call assert(size(saa_1d_1) .eq. 2 .and. saa_1d_1(2) .eq. 'you', 'Error in array.remove_val_string, turn 2')
        deallocate(saa_1d_1) 
    End Subroutine

    Subroutine test_union_arr1d()
        implicit none
        integer(kind=4), allocatable :: iaa1(:)


        iaa1 = union_arr1d([1,2], [3,4,5], [6,7])
        print *, iaa1
        call assert(all(iaa1 .eq. [1,2,3,4,5,6,7]), 'Error in rdee.array.union_arr1d for int4')
        deallocate(iaa1)
    End Subroutine

End Program

