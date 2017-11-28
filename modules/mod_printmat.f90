module mod_printmat
  ! Module pour gérer l'écriture de tableaux de un fichier
  ! Ce module consiste en une interface qui prend 2 arguments
  ! dont 1 optionnel:
  ! call printmat(A,[fid])
  ! Arguments d'entrés:
  ! ------------------------------------------------------
  ! A:     générique.  Tableaux à écrire dans le fichier.
  ! fid:   Entier.     Numéro du descripteur de fichier.  0 par défaut
  ! ------------------------------------------------------
  ! Cette interface permet d'écrire des matrices entières, réelles et complexes
  ! dans le descripteur de fichier 'fid' (0=entrée standard)
  ! une ligne est de type 'flc a(i,1) sep a(i,2) sep ... lrc'
  ! où flc (first left character), sep ( separator ), lrc (last right character )
  ! sont définis ci_dessous
  implicit none
  character(len=*),parameter,private :: flc = ''
  character(len=*),parameter,private :: lrc = ''
  character(len=*),parameter,private :: sep = ''
  interface printmat
    module procedure pmi,pmr4,pmr8,pmc4,pmc8,pmr8v
  end interface

contains

  subroutine pmi(A,fid) ! permet d'afficher une matrice entière
    integer,dimension(:,:),intent(in) :: A ! variable d'entrée
    integer :: i,j,n2,fid_loc
    integer,optional,intent(in) :: fid

    n2 = size(A,2)
    if (.not.present(fid)) then
      fid_loc = 0
    else
      fid_loc=fid
    endif

    do i=1,size(A,1)
      write(fid_loc,'(a,1x)',advance='no') flc
      do j=1,n2 -1 
        write(fid_loc,'(i0,a,1x)',advance='no') A(i,j),sep
      enddo
      write(fid_loc,'(i0,1x)',advance='no') A(i,n2)
      write(fid_loc,'(a)') lrc 
    enddo
  end subroutine

  subroutine pmr4(A,fid) ! permet d'afficher une matrice réelle simple précision
    real(kind=4),dimension(:,:),intent(in) :: A
    integer :: i,j,n2,fid_loc
    integer,optional,intent(in) :: fid

    n2 = size(A,2)
    if (.not.present(fid)) then
      fid_loc = 0
    else
      fid_loc=fid
    endif
    do i=1,size(A,1)
      write(fid_loc,'(a,1x)',advance='no') flc
      do j=1,n2 -1 
        write(fid_loc,'(es10.3,a,1x)',advance='no') A(i,j),sep
      enddo
      write(fid_loc,'(es10.3,1x)',advance='no') A(i,n2)
      write(fid_loc,'(a)') lrc 
    end do
  end subroutine

  subroutine pmr8(A,fid) ! permet d'afficher une matrice réelle double précision
    real(kind=8),dimension(:,:),intent(in) :: A
    integer :: i,j,n2,fid_loc
    integer,optional,intent(in) :: fid

    n2 = size(A,2)
    if (.not.present(fid)) then
      fid_loc = 0
    else
      fid_loc=fid
    endif

    do i=1,size(A,1)
      write(fid_loc,'(a,1x)',advance='no') flc
      do j=1,n2 -1 
        write(fid_loc,'(es10.3,a,1x)',advance='no') A(i,j),sep
      enddo
      write(fid_loc,'(es10.3,1x)',advance='no') A(i,n2)
      write(fid_loc,'(a)') lrc 
    end do
  end subroutine

  subroutine pmr8v(A,fid,shape) ! permet d'afficher une matrice réelle double précision
    real(kind=8),dimension(:),intent(in) :: A
    integer :: i,fid_loc
    integer,optional,intent(in) :: fid
    character(len=*),optional :: shape
    character(len=3) :: shape_loc

    if (.not.present(fid)) then
      fid_loc = 0
    else
      fid_loc=fid
    endif

    if (.not.present(shape)) then
      shape_loc = 'no' ! colonne
    else
      shape_loc = 'yes' ! ligne
    endif

      write(fid_loc,'(a,1x)',advance=shape_loc) flc
      do i=1,size(A) -1 
        write(fid_loc,'(es10.3,a,1x)',advance=shape_loc) A(i),sep
      enddo
      write(fid_loc,'(es10.3,1x)',advance=shape_loc) A(size(A))
      write(fid_loc,'(a)') lrc 
  end subroutine

  subroutine pmc4(A,fid) ! permet d'afficher une matrice complex simple précision
    complex(kind=4),dimension(:,:),intent(in) :: A 
    integer :: i,j,n2,fid_loc
    integer,optional,intent(in) :: fid

    n2 = size(A,2)
    if (.not.present(fid)) then
      fid_loc = 0
    else
      fid_loc=fid
    endif      

    do i=1,size(A,1)
      write(fid_loc,'(a,1x)',advance='no') flc
      do j=1,n2 -1 
        write(fid_loc,'("(",es10.3,",",es10.3,")",a,1x)',advance='no') real(A(i,j)),aimag(A(i,j)),sep
      enddo
      write(fid_loc,'("(",es10.3,",",es10.3,")")',advance='no') real(A(i,n2)),aimag(A(i,n2))
      write(fid_loc,'(a)') lrc 
    end do
  end subroutine

  subroutine pmc8(A,fid) ! permet d'afficher une matrice complexe double précision
    complex(kind=8),dimension(:,:),intent(in) :: A
    integer :: i,j,n2,fid_loc
    integer,optional,intent(in) :: fid

    n2 = size(A,2)
    if (.not.present(fid)) then
      fid_loc = 0
    else
      fid_loc=fid
    endif        

    do i=1,size(A,1)
      write(fid_loc,'(a,1x)',advance='no') flc
      do j=1,n2 -1 
        write(fid_loc,'("(",es10.3,",",es10.3,")",a,1x)',advance='no') real(A(i,j)),aimag(A(i,j)),sep
      enddo
      write(fid_loc,'("(",es10.3,",",es10.3,")")',advance='no') real(A(i,n2)),aimag(A(i,n2))
      write(fid_loc,'(a)') lrc 
    end do
  end subroutine
end module