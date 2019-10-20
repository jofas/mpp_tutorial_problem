program mpp_tutorial_problem
  use mpi_f08

  implicit none

  integer :: rank, c_size, i

  integer, dimension(:), allocatable :: m_chunk

  type(MPI_Status) :: stat
  type(MPI_Comm), save :: comm
  comm = mpi_comm_world

  call mpi_init()

  call mpi_comm_rank(comm, rank)
  call mpi_comm_size(comm, c_size)

  allocate(m_chunk(0:c_size-1))

  call mpi_scatter([(i, i=0, c_size ** 2 - 1)], c_size, &
    mpi_integer, m_chunk, c_size, mpi_integer, 0, comm)

  print *, "Before: ", rank, ": ", m_chunk

  call T()

  print *, "After: ", rank, ": ", m_chunk

  call T()

  print *, "Back to before: ", rank, ": ", m_chunk

  call mpi_finalize()

contains

  subroutine T()
    do i = 0, c_size - 1
      if (i == rank) continue
      call mpi_sendrecv_replace(m_chunk(i), 1, &
        mpi_integer, i, 0, i, 0, comm, stat)
    end do
  end
end
