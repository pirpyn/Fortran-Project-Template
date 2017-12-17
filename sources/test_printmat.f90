program test_printmat
  use mod_printmat
  implicit none
  real(kind=8),dimension(5,5) :: A
  real(kind=4),dimension(5) :: B
  integer(kind=8),dimension(5) :: C
  integer(kind=4),dimension(5,5) :: D

  A = -1
  B = 1e-3_4
  C = 2
  D = 42

  print*,'A ='
  call printmat(A)

  print*,'B (default) ='
  call printmat(B)

  print*,'B (row) ='
  call printmat(B,shape='r')

  print*,'B (column) ='
  call printmat(B,shape='c')

  ! Now changing the separator
  print*,'Changing the separator'
  call printmat_set('sep',' ; ')

  print*,'C (default) ='
  call printmat(C)

  print*,'C (column) ='
  call printmat(C,shape='c')

  ! Now changing the ending character
  print*,'Changing the ending'
  call printmat_set('end','<')
  print*,'D ='
  call printmat(D)

end program test_printmat
