program test_driver
    use mpi
    use mpi_wrapper,only: &
        ierr, &
        master, me, nprocs, &
        start_mpi, end_mpi

    implicit none

    integer :: i, isend, irecv
    integer :: status(MPI_STATUS_SIZE)

    ! ----- start ----- !
    call start_mpi()

    ! testing point to point blocking mpi communication
    call mpi_blocking_point_to_point()

    ! ----- end ----- !
    call end_mpi()

end program test_driver
