program main
    implicit none
  
    integer :: size, i
    real(kind=8), dimension(:), allocatable :: x, b, resultJI
    real(kind=8), dimension(:,:), allocatable :: matrix
    real(kind=8) :: start_t, end_t, total_t
  
    call random_seed()
  
    open(1, file = '../files/fortran/IJ.csv', action = 'write', status='replace')
    open(2, file = '../files/fortran/JI.csv', action = 'write', status='replace')
  
    do i = 0, 10, 1
      size = 4500 * i
      allocate(x(size))  ! Aloca vetor x de tamanho size
      allocate(matrix(size, size))  ! Aloca matriz matrix de tamanho size x size
  
      call fillVector(x, size)  ! Preenche vetor x com valores aleatórios
      call fillMatrix(matrix, size)  ! Preenche matriz matrix com valores aleatórios
  
      allocate(b(size))  ! Aloca vetor b de tamanho size
      call fillResultsVector(b, size)  ! Preenche vetor b com zeros
  
      call cpu_time(start_t)  ! Inicia contagem de tempo
      call matrix_vector_product_ij(matrix, x, b, size)  ! Calcula o produto matriz-vetor na ordem IJ
      call cpu_time(end_t)  ! Encerra contagem de tempo
  
      total_t = end_t - start_t
      print *, 'It took the computer', total_t, 'to compute a', size, 'degree matrix on IJ; i =', i
      write(1, *) size, ",", total_t  ! Escreve tamanho e tempo no arquivo IJ.csv
      deallocate(b)  ! Libera memória do vetor b
  
      allocate(resultJI(size))  ! Aloca vetor resultJI de tamanho size
      call fillResultsVector(resultJI, size)  ! Preenche vetor resultJI com zeros
  
      call cpu_time(start_t)
      call matrix_vector_product_ji(matrix, x, resultJI, size)  ! Calcula o produto matriz-vetor na ordem JI
      call cpu_time(end_t)
  
      total_t = end_t - start_t
      print *, 'It took the computer', total_t, 'to compute a', size, 'degree matrix on JI; i =', i
      write(2, *) size, ",", total_t  ! Escreve tamanho e tempo no arquivo JI.csv
      deallocate(resultJI)  ! Libera memória do vetor resultJI
  
      deallocate(matrix)  ! Libera memória da matriz matrix
      deallocate(x)  ! Libera memória do vetor x
    end do
  
    close(1)  ! Fecha arquivo IJ.csv
    close(2)  ! Fecha arquivo JI.csv
  
  contains
  
    subroutine fillVector(x, size)
      implicit none
      real(kind=8), dimension(:) :: x
      integer :: size, i
  
      do i = 1, size
        x(i) = random_number()  ! Preenche vetor x com valores aleatórios entre 0 e 1
      end do
    end subroutine fillVector

    subroutine fillResultsVector(x, size)
        implicit none
        real(kind=8), dimension(:) :: x
        integer :: size, i
    
        do i = 1, size
          x(i) = 0  ! Preenche o vetor x com zeros
        end do
      end subroutine fillResultsVector
    
      subroutine fillMatrix(matrix, size)
        implicit none
        real(kind=8), dimension(:,:) :: matrix
        integer :: size, i, j
    
        do i = 1, size
          do j = 1, size
            matrix(i, j) = random_number()  ! Preenche a matriz com valores aleatórios entre 0 e 1
          end do
        end do
      end subroutine fillMatrix
    
      subroutine matrix_vector_product_ij(matrix, x, result, size)
        implicit none
        real(kind=8), dimension(:,:) :: matrix
        real(kind=8), dimension(:) :: x, result
        integer :: size, i, j
    
        do i = 1, size
          result(i) = 0  ! Inicializa o vetor de resultados com zero
          do j = 1, size
            result(i) = result(i) + matrix(i, j) * x(j)  ! Calcula o produto matriz-vetor na ordem IJ
          end do
        end do
      end subroutine matrix_vector_product_ij
    
      subroutine matrix_vector_product_ji(matrix, x, result, size)
        implicit none
        real(kind=8), dimension(:,:) :: matrix
        real(kind=8), dimension(:) :: x, result
        integer :: size, i, j
    
        do i = 1, size
          result(i) = 0  ! Inicializa o vetor de resultados com zero
          do j = 1, size
            result(i) = result(i) + matrix(j, i) * x(j)  ! Calcula o produto matriz-vetor na ordem JI
          end do
        end do
      end subroutine matrix_vector_product_ji
    
    end program main