subroutine mpi_blocking_point_to_point()
    use mpi
    use mpi_wrapper,only: &
        ierr, &
        master, me, nprocs
    implicit none
    integer :: i, isend, irecv
    integer :: status(MPI_STATUS_SIZE)
    
    if(master) then
        ! send (i+2) to workers
        do i=1, nprocs-1
            isend = i + 2
            call MPI_Send(isend, 1, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)
        enddo

        ! receive (i+2)^2 from workers
        do i=1,nprocs-1
            call MPI_Recv(irecv, 1, MPI_INT, i, 0, MPI_COMM_WORLD, status, ierr)
            write(*,*) "MASTER: ", me, " receives: ", irecv, " from worker", i
        enddo
    else
        ! worker (me) receives (i+2) from master (0)
        call MPI_Recv(irecv, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, status, ierr)
        write(*,'(A10,I4,A10,I4)') "worker", me, " receives number: ", irecv

        ! worker (me) sends [(i+2)^2] to master (0)
        isend = irecv**2
        call MPI_Send(isend, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, ierr)
    endif
end subroutine mpi_blocking_point_to_point

!!!!!!
subroutine mpi_scatter_collective()
    use mpi
    use mpi_wrapper,only: &
        ierr, &
        master, me, nprocs

    implicit none
    
    integer,parameter :: N=10
    double precision,allocatable :: AA(:,:), A(:)
    double precision :: val
    integer :: i,j

! ------ start ------ !

    allocate(A(N))

    if (master) then
        allocate(AA(N,nprocs))
        do i=1,nprocs
            do j=1,N
                val = real((i-1)/10.0)
                AA(j,i) = val
            enddo
        enddo
    endif

    ! scatter values
    call MPI_Scatter(AA, N, MPI_DOUBLE, A, N, MPI_DOUBLE, 0, MPI_COMM_WORLD, ierr)

    ! write array A
    write(*,'(A5,I2,/,10F10.3)') "from ", me, A


end subroutine mpi_scatter_collective












