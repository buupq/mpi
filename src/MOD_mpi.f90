module mpi_wrapper
    use mpi

    implicit none
    
    integer :: ierr,me,nprocs
    logical :: master


    contains

! start mpi
    subroutine start_mpi()
        call mpi_init(ierr)
        call mpi_comm_size(MPI_COMM_WORLD, nprocs, ierr)
        call mpi_comm_rank(MPI_COMM_WORLD, me, ierr)
        master = me.eq.0
    end subroutine start_mpi

! end mpi
    subroutine end_mpi()
        call mpi_finalize(ierr)
    end subroutine end_mpi

end module mpi_wrapper