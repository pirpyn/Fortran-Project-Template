program test_io
  use mod_io
  implicit none
  character(len=*), parameter :: filename = 'tests/mod_io.input.txt'
  real(kind=4) :: x,y
  integer(kind=4) :: i,j
  character(len=42) :: string,string2,string3
  complex :: z
  integer,parameter :: fid = 42

  open(fid,file=filename,action='read')
  call read_val(fid,'x',x)
  call read_val(fid,'i',i)
  call read_val(fid,'string',string)


  if (x.eq.3.14_4) then
    print*,'SUCCESS: x = ',x
  else
    print*,'FAILURE: x = ',x,'!=',3.14_4
  endif

  if (i.eq.-2_4) then
    print*,'SUCCESS: i = ',i
  else
    print*,'FAILURE: i = ',i,'!=',-2
  endif

  if (string.eq.'Hello World') then ! WARNING : trailing blanks no taken into account
    print*,'SUCCESS: string = '''//string//''''
  else
    print*,'FAILURE: string = '''//string//''' != ''Hello World  '''
  endif

  call read_val(fid,'y',y)
  call read_val(fid,'j',j)
  call read_val(fid,'string2',string2)


  if (y.eq.1e-9_4) then
    print*,'SUCCESS: y = ',y
  else
    print*,'FAILURE: y = ',y,'!=',1e-9_4
  endif

  if (j.eq.32_4) then
    print*,'SUCCESS: j = ',j
  else
    print*,'FAILURE: j = ',j,'!=',-2
  endif

  if (string2.eq.'Lots of blanks') then ! WARNING : trailing blanks no taken into account
    print*,'SUCCESS: string2 = '''//string2//''''
  else
    print*,'FAILURE: string2 = '''//string2//''' != ''Lots of blanks'''
  endif

  call read_val(fid,'string3',string3)

  if (string3.eq.' On severallines') then ! WARNING : trailing blanks no taken into account
    print*,'SUCCESS: string3 = '''//string3//''''
  else
    print*,'FAILURE: string3 = '''//string3//''' != ''Lots of blanks'''
  endif

  ! Changing the separator to get z
  call read_val_set('sep',':')
  call read_val(fid,'z',z)

  if (z.eq.complex(1,2)) then ! WARNING : trailing blanks no taken into account
    print*,'SUCCESS: z =',z
  else
    print*,'FAILURE: z =',z,' != ',complex(1,2)
  endif
  close(fid)
end program test_io
