module mod_io
  ! Module pour gérer les lectures dans un fichier formaté ainsi : 
  ! var1 = value1 
  ! var2 = value21,value22
  ! var3=   value3
  ! var4   =value4
  ! ....
  ! Ce module consiste en une interface read_val qui prend
  ! 3 arguments : call read_val(fid, varname, value)
  ! ------------------------------------------------------
  ! Arguments d'entrés: 
  ! fid:      Entier.     Numéro du descripteur de fichier.
  ! var_name: Character.  Nom de la variable à chercher dans fid.
  ! Argument de sortie:
  ! value:    Générique.  Valeur de la variable.
  ! ------------------------------------------------------
  ! value peut être entier, réel, complexe ; kind=4,8
  ! ou aussi logical.
  ! value peut être un scalaire ou un tableau à une dimension
  ! L'important étant qu'il y ait le même nombre de termes
  ! dans le fichier à lire
  implicit none
  private
  character(len=1) :: sep = '=' ! separateur. Doit etre de taille 1
 
  ! cette interface permet de retourner la valeur independament du type 
  interface read_val ! read_val(fid,var_name,value)
    module procedure iogvr4,iogvr8,iogvra4,iogvra8
    module procedure iogvi4,iogvi8,iogvia4,iogvia8
    module procedure iogvc4,iogvc8
    module procedure iogvc
  end interface

  public :: read_val, read_val_set
contains

  subroutine read_val_set(parameter_name,value)
    character(len=*),intent(in) :: parameter_name, value
    select case(parameter_name)
    case ('sep','SEP','separator','SEPARATOR')
      if (1 .ne. len(value)) then
        print'(a,1x,i0)','Error: read_val_set: length for ''sep'' greater than 1'
        stop
      endif
      sep = value
    case default
      print'(a)','Error: printmat_set: Wrong 1st argument '''//parameter_name//''''
      stop
    end select
  end subroutine

  subroutine errmsg(fid,var_name)
    integer,intent(in) :: fid
    character(len=*),intent(in) :: var_name
    print'(a,a,a)', 'mod_io:read_val:error: Le text ''',var_name,''' n''a pas été trouvé'
    print'(4x,a,1x,i0)', 'dans le fichier dont l''unit est',fid
  end subroutine

  subroutine iogvc(fid,var_name,value) ! pour chercher une chaine de caractère
    integer,intent(in) :: fid
    character(len=*),intent(in) :: var_name
    character(len=*),intent(out) :: value
    integer :: iostat
    character(len=len(var_name)) :: mask
    character(len=1) :: car

    car = ' '
    iostat = 0
    mask = ''
    rewind(fid)
    do while ((.not.mask.eq.var_name).and.(.not.iostat.eq.-1))
      read(fid,'(a)',iostat=iostat,advance='no') mask
    enddo
    if (iostat.eq.-1) then ! fin de fichier : masque non trouvé
      call errmsg(fid,var_name)
      stop
    else 
      ! on trouvé le mask 
      ! on va vérifier que l'on a dans le fichier 'mask = value'
      do while ( ( ( car.eq.' ' ) .or. ( car.eq.char(9)) ) .and. (.not.iostat.eq.-1) )
        read(fid,'(a)',iostat=iostat,advance='no') car ! on lit le '='
      enddo
      if(.not.car.eq.sep) then
        call errmsg(fid,var_name//' = <value>')
        stop
      else
        read(fid,*,iostat=iostat) value
      endif
    endif
  end subroutine

  subroutine iogvra4(fid,var_name,value) ! pour chercher un tableau real
    integer,intent(in) :: fid
    character(len=*),intent(in) :: var_name
    real(kind=4),dimension(:), intent(out) :: value
    integer :: iostat
    character(len=len(var_name)) :: mask
    character(len=1) :: car

    car = ' '
    iostat = 0
    mask = ''
    rewind(fid)
    do while ((.not.mask.eq.var_name).and.(.not.iostat.eq.-1))
      read(fid,'(a)',iostat=iostat,advance='no') mask
    enddo
    if (iostat.eq.-1) then ! fin de fichier : masque non trouvé
      call errmsg(fid,var_name)
      stop
    else 
      ! on trouvé le mask 
      ! on va vérifier que l'on a dans le fichier 'mask = value'
      do while ( ( ( car.eq.' ' ) .or. ( car.eq.char(9)) ) .and. (.not.iostat.eq.-1) )
        read(fid,'(a)',iostat=iostat,advance='no') car ! on lit le '='
      enddo
      if(.not.car.eq.sep) then
        call errmsg(fid,var_name//' = <value>')
        stop
      else
        read(fid,*,iostat=iostat) value(:)
      endif
    endif
  end subroutine

  subroutine iogvra8(fid,var_name,value) ! pour chercher un tableau real
    character(len=*),intent(in) :: var_name
    integer,intent(in) :: fid
    real(kind=8),dimension(:), intent(out) :: value
    integer :: iostat
    character(len=len(var_name)) :: mask

    character(len=1) :: car
    car = ' '
    iostat = 0
    mask = ''
    rewind(fid)
    do while ((.not.mask.eq.var_name).and.(.not.iostat.eq.-1))
      read(fid,'(a)',iostat=iostat,advance='no') mask
    enddo
    if (iostat.eq.-1) then ! fin de fichier : masque non trouvé
      call errmsg(fid,var_name)
      stop
    else 
      ! on trouvé le mask 
      ! on va vérifier que l'on a dans le fichier 'mask = value'
      do while ( ( (car.eq.' ').or.(car.eq.char(9)) ).and.(.not.iostat.eq.-1))
        read(fid,'(a)',iostat=iostat,advance='no') car ! on lit le '='
      enddo
      if(.not.car.eq.sep) then
        call errmsg(fid,var_name//' = <value>')
        stop
      else
        read(fid,*,iostat=iostat) value(:)
      endif
    endif
  end subroutine

  subroutine iogvr4(fid,var_name,value) ! pour chercher un real
    character(len=*),intent(in) :: var_name
    integer,intent(in) :: fid
    real(kind=4),intent(out) :: value
    integer :: iostat
    character(len=len(var_name)) :: mask
    character(len=1) :: car
    car = ' '
    iostat = 0
    mask = ''
    rewind(fid)
    do while ((.not.mask.eq.var_name).and.(.not.iostat.eq.-1))
      read(fid,'(a)',iostat=iostat,advance='no') mask
    enddo
    if (iostat.eq.-1) then ! fin de fichier : masque non trouvé
      call errmsg(fid,var_name)
      stop
    else 
      ! on trouvé le mask 
      ! on va vérifier que l'on a dans le fichier 'mask = value'
      do while (((car.eq.' ').or.(car.eq.char(9))).and.(.not.iostat.eq.-1))
        read(fid,'(a)',iostat=iostat,advance='no') car ! on lit le '='
      enddo
      if(.not.car.eq.sep) then
        call errmsg(fid,var_name//' = <value>')
        stop
      else
        read(fid,*,iostat=iostat) value
      endif
    endif
  end subroutine

  subroutine iogvr8(fid,var_name,value) ! pour chercher un real
    character(len=*),intent(in) :: var_name
    integer,intent(in) :: fid
    real(kind=8),intent(out) :: value
    integer :: iostat
    character(len=len(var_name)) :: mask
    character(len=1) :: car
    car = ' '
    iostat = 0
    mask = ''
    rewind(fid)
    do while ((.not.mask.eq.var_name).and.(.not.iostat.eq.-1))
      read(fid,'(a)',iostat=iostat,advance='no') mask
    enddo
    if (iostat.eq.-1) then ! fin de fichier : masque non trouvé
      call errmsg(fid,var_name//' = <value>')
      stop
    else 
      ! on trouvé le mask 
      ! on va vérifier que l'on a dans le fichier 'mask = value'
      do while ( ( (car.eq.' ') .or. (car.eq.char(9)) ).and.(.not.iostat.eq.-1))
        read(fid,'(a)',iostat=iostat,advance='no') car ! on lit le '='
      enddo
      if(.not.car.eq.sep) then
        call errmsg(fid,var_name//' = <value>')
        stop
      else
        read(fid,*,iostat=iostat) value
      endif
    endif
  end subroutine

  subroutine iogvi4(fid,var_name,value) ! pour chercher un entier
    character(len=*),intent(in) :: var_name
    integer,intent(in) :: fid
    integer(kind=4),intent(out) :: value
    integer :: iostat
    character(len=len(var_name)) :: mask

    character(len=1) :: car
    car = ' '
    iostat = 0
    mask = ''
    rewind(fid)
    do while ((.not.mask.eq.var_name).and.(.not.iostat.eq.-1))
      read(fid,'(a)',iostat=iostat,advance='no') mask
    enddo
    if (iostat.eq.-1) then
      call errmsg(fid,var_name)
      stop
    else 
      ! on trouvé le mask 
      ! on va vérifier que l'on a dans le fichier 'mask = value'
      do while (((car.eq.' ').or.(car.eq.char(9))).and.(.not.iostat.eq.-1))
        read(fid,'(a)',iostat=iostat,advance='no') car ! on lit le '='
      enddo
      if(.not.car.eq.sep) then
        call errmsg(fid,var_name//' = <value>')
        stop
      else
        read(fid,*,iostat=iostat) value
      endif
    endif
  end subroutine

  subroutine iogvi8(fid,var_name,value) ! pour chercher un entier
    character(len=*),intent(in) :: var_name
    integer,intent(in) :: fid
    integer(kind=8),intent(out) :: value
    integer :: iostat
    character(len=len(var_name)) :: mask
    character(len=1) :: car
    car = ' '
    iostat = 0
    mask = ''
    rewind(fid)
    do while ((.not.mask.eq.var_name).and.(.not.iostat.eq.-1))
      read(fid,'(a)',iostat=iostat,advance='no') mask
    enddo
    if (iostat.eq.-1) then
      call errmsg(fid,var_name)
      stop
    else 
      ! on trouvé le mask 
      ! on va vérifier que l'on a dans le fichier 'mask = value'
      do while (((car.eq.' ').or.(car.eq.char(9))).and.(.not.iostat.eq.-1))
        read(fid,'(a)',iostat=iostat,advance='no') car ! on lit le '='
      enddo
      if(.not.car.eq.sep) then
        call errmsg(fid,var_name//' = <value>')
        stop
      else
        read(fid,*,iostat=iostat) value
      endif
    endif
  end subroutine

  subroutine iogvia4(fid,var_name,value) ! pour chercher un tableau d'entier
    character(len=*),intent(in) :: var_name
    integer,intent(in) :: fid
    integer(kind=4),dimension(:), intent(out) :: value
    integer :: iostat
    character(len=len(var_name)) :: mask

    character(len=1) :: car
    car = ' '
    iostat = 0
    mask = ''
    rewind(fid)
    do while ((.not.mask.eq.var_name).and.(.not.iostat.eq.-1))
      read(fid,'(a)',iostat=iostat,advance='no') mask
    enddo
    if (iostat.eq.-1) then ! fin de fichier : masque non trouvé
      call errmsg(fid,var_name)
      stop
    else 
      ! on trouvé le mask 
      ! on va vérifier que l'on a dans le fichier 'mask = value'
      do while (((car.eq.' ').or.(car.eq.char(9))).and.(.not.iostat.eq.-1))
        read(fid,'(a)',iostat=iostat,advance='no') car ! on lit le '='
      enddo
      if(.not.car.eq.sep) then
        call errmsg(fid,var_name//' = <value>')
        stop
      else
        read(fid,*,iostat=iostat) value(:)
      endif
    endif
  end subroutine

  subroutine iogvia8(fid,var_name,value) ! pour chercher un tableau d'entier
    character(len=*),intent(in) :: var_name
    integer,intent(in) :: fid
    integer(kind=8),dimension(:), intent(out) :: value
    integer :: iostat
    character(len=len(var_name)) :: mask

    character(len=1) :: car
    car = ' '
    iostat = 0
    mask = ''
    rewind(fid)
    do while ((.not.mask.eq.var_name).and.(.not.iostat.eq.-1))
      read(fid,'(a)',iostat=iostat,advance='no') mask
    enddo
    if (iostat.eq.-1) then ! fin de fichier : masque non trouvé
      call errmsg(fid,var_name)
      stop
    else 
      ! on trouvé le mask 
      ! on va vérifier que l'on a dans le fichier 'mask = value'
      do while (((car.eq.' ').or.(car.eq.char(9))).and.(.not.iostat.eq.-1))
        read(fid,'(a)',iostat=iostat,advance='no') car ! on lit le '='
      enddo
      if(.not.car.eq.sep) then
        call errmsg(fid,var_name//' = <value>')
        stop
      else
        read(fid,*,iostat=iostat) value(:)
      endif
    endif
  end subroutine

  subroutine iogvc4(fid,var_name,value) ! pour chercher un tableau real
    integer,intent(in) :: fid
    character(len=*),intent(in) :: var_name
    complex(kind=4), intent(out) :: value
    integer :: iostat
    character(len=len(var_name)) :: mask
    character(len=1) :: car

    car = ' '
    iostat = 0
    mask = ''
    rewind(fid)
    do while ((.not.mask.eq.var_name).and.(.not.iostat.eq.-1))
      read(fid,'(a)',iostat=iostat,advance='no') mask
    enddo
    if (iostat.eq.-1) then ! fin de fichier : masque non trouvé
      call errmsg(fid,var_name)
      stop
    else 
      ! on trouvé le mask 
      ! on va vérifier que l'on a dans le fichier 'mask = value'
      do while ( ( ( car.eq.' ' ) .or. ( car.eq.char(9)) ) .and. (.not.iostat.eq.-1) )
        read(fid,'(a)',iostat=iostat,advance='no') car ! on lit le '='
      enddo
      if(.not.car.eq.sep) then
        call errmsg(fid,var_name//' = <value>')
        stop
      else
        read(fid,*,iostat=iostat) value
      endif
    endif
  end subroutine

  subroutine iogvc8(fid,var_name,value) ! pour chercher un tableau real
    integer,intent(in) :: fid
    character(len=*),intent(in) :: var_name
    complex(kind=8), intent(out) :: value
    integer :: iostat
    character(len=len(var_name)) :: mask
    character(len=1) :: car

    car = ' '
    iostat = 0
    mask = ''
    rewind(fid)
    do while ((.not.mask.eq.var_name).and.(.not.iostat.eq.-1))
      read(fid,'(a)',iostat=iostat,advance='no') mask
    enddo
    if (iostat.eq.-1) then ! fin de fichier : masque non trouvé
      call errmsg(fid,var_name)
      stop
    else 
      ! on trouvé le mask 
      ! on va vérifier que l'on a dans le fichier 'mask = value'
      do while ( ( ( car.eq.' ' ) .or. ( car.eq.char(9)) ) .and. (.not.iostat.eq.-1) )
        read(fid,'(a)',iostat=iostat,advance='no') car ! on lit le '='
      enddo
      if(.not.car.eq.sep) then
        call errmsg(fid,var_name//' = <value>')
        stop
      else
        read(fid,*,iostat=iostat) value
      endif
    endif
  end subroutine

end module mod_io