program test_driver
    
    use mpi_wrapper

    implicit none
    integer, parameter :: n = 3
    real :: A(n, n), B(n, n), C(n, n)

    call start_mpi()

    write(*,*) "Hello from rank", me, master

    call end_mpi()


end program test_driver
