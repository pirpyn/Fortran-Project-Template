module mod_printmat
  ! Module pour gérer l'écriture de tableaux de un fichier
  ! Ce module consiste en une interface qui prend 2 arguments
  ! dont 1 optionnel:
  ! call printmat(A,[fid])
  ! Arguments d'entrés:
  ! ------------------------------------------------------
  ! A:     générique.            Tableaux à écrire dans le fichier.
  ! fid:   Entier.    Optionnel. Numéro du descripteur de fichier.  0 par défaut
  ! shape: Caractère. Optionnel. Pour les vecteurs, choisi entre ligne ou colonne
  ! ------------------------------------------------------
  ! Cette interface permet d'écrire des matrices entières, réelles et complexes
  ! dans le descripteur de fichier 'fid' (0=entrée standard)
  ! une ligne est de type 'start_char a(i,1) sep a(i,2) sep ... end_char'
  ! où start_char (first start_char character), sep ( separator ), end_char (last end_char character )
  ! sont définis ci_dessous
  implicit none
  private
  character(len=5) :: start_char = ''
  integer :: lstart_char = 0
  character(len=5) :: end_char = ''
  integer :: lend_char = 0
  character(len=5) :: sep = ' '
  integer :: lsep = 1

  interface printmat
    module procedure printmat_i4,printmat_i8,printmat_i4v,printmat_i8v
    module procedure printmat_r4,printmat_r8,printmat_r4v,printmat_r8v
    module procedure printmat_c4,printmat_c8
    module procedure printmat_l,printmat_lv
  end interface

  public :: printmat, printmat_set
contains

  subroutine printmat_set(parameter_name,value)
    character(len=*),intent(in) :: parameter_name, value
    select case(parameter_name)
    case ('start_char','START_CHAR','start','START','left','LEFT')
      if (len(start_char).lt.len(value)) then
        print'(a,1x,i0)','Warning: printmat_set: length for ''start_char'' greater than',lstart_char
      endif
      start_char = value
      lstart_char = len(value)
    case ('sep','SEP','separator','SEPARATOR')
      if (len(sep).lt.len(value)) then
        print'(a,1x,i0)','Warning: printmat_set: length for ''sep'' greater than',lsep
      endif
      sep = value
      lsep = len(value)
    case ('end_char','END_CHAR','end','END','right','RIGHT')
      if (len(end_char).lt.len(value)) then
        print'(a,1x,i0)','Warning: printmat_set: length for ''end_char'' greater than',lend_char
      endif
      end_char = value
      lend_char = len(value)
    case default
      print'(a)','Error: printmat_set: Wrong 1st argument '''//parameter_name//''''
      stop
    end select
  end subroutine

  subroutine printmat_i4(A,fid) ! permet d'afficher une matrice entière
    integer(kind=4),dimension(:,:),intent(in) :: A ! variable d'entrée
    integer :: i,j,n2,fid_loc
    integer,optional,intent(in) :: fid

    n2 = size(A,2)
    if (.not.present(fid)) then
      fid_loc = 0
    else
      fid_loc=fid
    endif

    do i=1,size(A,1)
      write(fid_loc,'(a)',advance='no') start_char(1:lstart_char)
      do j=1,n2 -1 
        write(fid_loc,'(i0,a)',advance='no') A(i,j),sep(1:lsep)
      enddo
      write(fid_loc,'(i0,1x)',advance='no') A(i,n2)
      write(fid_loc,'(a)') end_char(1:lend_char)
    enddo
  end subroutine

  subroutine printmat_i4v(A,fid,shape) ! permet d'afficher une vecteur entier simple précision
    integer(kind=4),dimension(:),intent(in) :: A
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
      shape_loc = 'no' ! ligne 
    else
      select case(shape) 
      case ('c') ! colonne : column
        shape_loc = 'yes' ! ligne
      case ('r') ! ligne : row
        shape_loc = 'no'
      case default
        print'(a)', 'printmat: error: wrong shape argument for array of rank 1'
        stop
      end select
    endif

      write(fid_loc,'(a)',advance='no') start_char(1:lstart_char)
      do i=1,size(A) -1 
        write(fid_loc,'(i0,a)',advance=shape_loc) A(i),sep(1:lsep)
      enddo
      write(fid_loc,'(i0)',advance='no') A(size(A))
      write(fid_loc,'(a)') end_char(1:lend_char)
  end subroutine

  subroutine printmat_i8(A,fid) ! permet d'afficher une matrice entière
    integer(kind=8),dimension(:,:),intent(in) :: A ! variable d'entrée
    integer :: i,j,n2,fid_loc
    integer,optional,intent(in) :: fid

    n2 = size(A,2)
    if (.not.present(fid)) then
      fid_loc = 0
    else
      fid_loc=fid
    endif

    do i=1,size(A,1)
      write(fid_loc,'(a)',advance='no') start_char(1:lstart_char)
      do j=1,n2 -1 
        write(fid_loc,'(i0,a)',advance='no') A(i,j),sep(1:lsep)
      enddo
      write(fid_loc,'(i0,1x)',advance='no') A(i,n2)
      write(fid_loc,'(a)') end_char(1:lend_char)
    enddo
  end subroutine

  subroutine printmat_i8v(A,fid,shape) ! permet d'afficher vecteur entier double précision
    integer(kind=8),dimension(:),intent(in) :: A
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
      shape_loc = 'no' ! ligne 
    else
      select case(shape) 
      case ('c') ! colonne : column
        shape_loc = 'yes' ! ligne
      case ('r') ! ligne : row
        shape_loc = 'no'
      case default
        print'(a)', 'printmat: error: wrong shape argument for array of rank 1'
        stop
      end select
    endif

      write(fid_loc,'(a)',advance='no') start_char(1:lstart_char)
      do i=1,size(A) -1 
        write(fid_loc,'(i0,a)',advance=shape_loc) A(i),sep(1:lsep)
      enddo
      write(fid_loc,'(i0)',advance='no') A(size(A))
      write(fid_loc,'(a)') end_char(1:lend_char)
  end subroutine

  subroutine printmat_r4(A,fid) ! permet d'afficher une matrice réelle simple précision
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
      write(fid_loc,'(a)',advance='no') start_char(1:lstart_char)
      do j=1,n2 -1 
        write(fid_loc,'(es10.3,a)',advance='no') A(i,j),sep(1:lsep)
      enddo
      write(fid_loc,'(es10.3,1x)',advance='no') A(i,n2)
      write(fid_loc,'(a)') end_char(1:lend_char)
    end do
  end subroutine

  subroutine printmat_r4v(A,fid,shape) ! permet d'afficher un vecteur réel simple précision
    real(kind=4),dimension(:),intent(in) :: A
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
      shape_loc = 'no' ! ligne 
    else
      select case(shape) 
      case ('c') ! colonne : column
        shape_loc = 'yes' ! ligne
      case ('r') ! ligne : row
        shape_loc = 'no'
      case default
        print'(a)', 'printmat: error: wrong shape argument for array of rank 1'
        stop
      end select
    endif

      write(fid_loc,'(a)',advance='no') start_char(1:lstart_char)
      do i=1,size(A) -1 
        write(fid_loc,'(es10.3,a)',advance=shape_loc) A(i),sep(1:lsep)
      enddo
      write(fid_loc,'(es10.3)',advance='no') A(size(A))
      write(fid_loc,'(a)') end_char(1:lend_char)
  end subroutine

  subroutine printmat_r8(A,fid) ! permet d'afficher une matrice réelle double précision
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
      write(fid_loc,'(a)',advance='no') start_char(1:lstart_char)
      do j=1,n2 -1 
        write(fid_loc,'(es10.3,a)',advance='no') A(i,j),sep(1:lsep)
      enddo
      write(fid_loc,'(es10.3,1x)',advance='no') A(i,n2)
      write(fid_loc,'(a)') end_char(1:lend_char)
    end do
  end subroutine

  subroutine printmat_r8v(A,fid,shape) ! permet d'afficher vecteur réel double précision
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
      shape_loc = 'no' ! ligne 
    else
      select case(shape) 
      case ('c') ! colonne : column
        shape_loc = 'yes' ! ligne
      case ('r') ! ligne : row
        shape_loc = 'no'
      case default
        print'(a)', 'printmat: error: wrong shape argument for array of rank 1'
        stop
      end select
    endif

      write(fid_loc,'(a)',advance='no') start_char(1:lstart_char)
      do i=1,size(A) -1 
        write(fid_loc,'(es10.3,a)',advance=shape_loc) A(i),sep(1:lsep)
      enddo
      write(fid_loc,'(es10.3)',advance='no') A(size(A))
      write(fid_loc,'(a)') end_char(1:lend_char)
  end subroutine

  subroutine printmat_c4(A,fid) ! permet d'afficher une matrice complex simple précision
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
      write(fid_loc,'(a)',advance='no') start_char(1:lstart_char)
      do j=1,n2 -1 
        write(fid_loc,'("(",es10.3,",",es10.3,")",a)',advance='no') real(A(i,j)),aimag(A(i,j)),sep(1:lsep)
      enddo
      write(fid_loc,'("(",es10.3,",",es10.3,")")',advance='no') real(A(i,n2)),aimag(A(i,n2))
      write(fid_loc,'(a)') end_char(1:lend_char)
    end do
  end subroutine

  subroutine printmat_c8(A,fid) ! permet d'afficher une matrice complexe double précision
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
      write(fid_loc,'(a)',advance='no') start_char(1:lstart_char)
      do j=1,n2 -1 
        write(fid_loc,'("(",es10.3,",",es10.3,")",a)',advance='no') real(A(i,j)),aimag(A(i,j)),sep(1:lsep)
      enddo
      write(fid_loc,'("(",es10.3,",",es10.3,")")',advance='no') real(A(i,n2)),aimag(A(i,n2))
      write(fid_loc,'(a)') end_char(1:lend_char)
    end do
  end subroutine

  subroutine printmat_l(A,fid) ! permet d'afficher une matrice réelle double précision
    logical,dimension(:,:),intent(in) :: A
    integer :: i,j,n2,fid_loc
    integer,optional,intent(in) :: fid

    n2 = size(A,2)
    if (.not.present(fid)) then
      fid_loc = 0
    else
      fid_loc=fid
    endif

    do i=1,size(A,1)
      write(fid_loc,'(a)',advance='no') start_char(1:lstart_char)
      do j=1,n2 -1 
        write(fid_loc,'(l1,a)',advance='no') A(i,j),sep(1:lsep)
      enddo
      write(fid_loc,'(l1,1x)',advance='no') A(i,n2)
      write(fid_loc,'(a)') end_char(1:lend_char)
    end do
  end subroutine

  subroutine printmat_lv(A,fid,shape) ! permet d'afficher vecteur réel double précision
    logical,dimension(:),intent(in) :: A
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
      shape_loc = 'no' ! ligne 
    else
      select case(shape) 
      case ('c') ! colonne : column
        shape_loc = 'yes' ! ligne
      case ('r') ! ligne : row
        shape_loc = 'no'
      case default
        print'(a)', 'printmat: error: wrong shape argument for array of rank 1'
        stop
      end select
    endif

      write(fid_loc,'(a)',advance='no') start_char(1:lstart_char)
      do i=1,size(A) -1 
        write(fid_loc,'(l1,a)',advance=shape_loc) A(i),sep(1:lsep)
      enddo
      write(fid_loc,'(l1)',advance='no') A(size(A))
      write(fid_loc,'(a)') end_char(1:lend_char)
  end subroutine

end module