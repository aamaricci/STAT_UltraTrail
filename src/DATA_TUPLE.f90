module DATA_TUPLE
  implicit none

  type tuple
     integer             :: rank
     real(8)             :: time
     character(len=3)    :: nat
     integer             :: yob
     character(len=1)    :: mf
     integer             :: rankMF
     character(len=3)    :: cat
     integer             :: rankCat     
     real(8)             :: speed
     type(tuple),pointer :: next   =>null()
   contains
     procedure,pass      :: free     => free_tuple
     procedure,pass      :: read     => read_tuple
     procedure,pass      :: show     => show_tuple
  end type tuple

  interface tuple
     module procedure :: construct_tuple_data
  end interface tuple

  !EQUALITY operator A=B  [deep copy]
  interface assignment(=)
     module procedure :: equality_tuple
  end interface assignment(=)


  public :: tuple
  public :: assignment(=)
  public :: get_time

contains


  !+------------------------------------------------------------------+
  !PURPOSE:  Free a Tuple (destructor) 
  !+------------------------------------------------------------------+
  subroutine free_tuple(self)
    class(tuple),intent(inout) :: self
    self%rank    = 0
    self%time    = 0d0
    self%nat     = 'XXX'
    self%yob     = 0
    self%mf      = ''
    self%rankMF  = 0
    self%cat     = ''
    self%rankCat = 0     
    self%speed   = 0d0
    self%next    => null()
  end subroutine free_tuple


  !+------------------------------------------------------------------+
  !PURPOSE:  Read a Tuple from a line in UNIT
  !+------------------------------------------------------------------+
  subroutine read_tuple(self,unit)
    class(tuple),intent(inout) :: self
    integer                    :: unit
    character(len=10)          :: time
    read(unit,*)       &
         self%rank,    &
         time,    &
         self%nat,     &
         self%yob,     &
         self%mf,      &
         self%rankMF,  &
         self%cat,     &
         self%rankCat, &
         self%speed
    self%time=get_time(time)
    self%next => null()
  end subroutine read_tuple


  !##################################################################
  !##################################################################
  !       CONSTRUCT TVECTOR FROM ARRAY 
  !##################################################################
  !##################################################################
  function construct_tuple_data(rank,time,nat,yob,mf,rankMF,cat,rankCat,speed) result(self)
    integer                            :: rank
    character(len=*)                   :: time
    character(len=3)                   :: nat
    integer                            :: yob
    character(len=1)                   :: mf
    integer                            :: rankMF
    character(len=3)                   :: cat
    integer                            :: rankCat     
    real(8)                            :: speed
    type(tuple),target                 :: self
    !
    call self%free()
    !
    self%rank    = rank
    self%time    = get_time(time)
    self%nat     = nat
    self%yob     = yob
    self%mf      = mf
    self%rankMF  = rankMF
    self%cat     = cat
    self%rankCat = rankCat     
    self%speed   = speed
    self%next    => null()
  end function construct_tuple_data



  !##################################################################
  !##################################################################
  !              OPERATIONS
  !##################################################################
  !##################################################################
  subroutine equality_tuple(a,b)
    type(tuple),intent(inout) :: a
    type(tuple),intent(in)    :: b
    integer                    :: i,N
    call a%free()
    a%rank    = b%rank
    a%time    = b%time
    a%nat     = b%nat
    a%yob     = b%yob
    a%mf      = b%mf
    a%rankMF  = b%rankMF
    a%cat     = b%cat
    a%rankCat = b%rankCat     
    a%speed   = b%speed
    a%next    => b%next
  end subroutine equality_tuple


  !##################################################################
  !##################################################################
  !              SHOW TVECTOR
  !##################################################################
  !##################################################################
  subroutine show_tuple(self)
    class(tuple) :: self
    !
    write(*,"(A1)",advance='no')"["
    write(*,"(I6,1X)",advance='no')self%rank
    write(*,"(F18.2,1X)",advance='no')self%time
    write(*,"(A6,1X)",advance='no')self%nat
    write(*,"(I6,1X)",advance='no')self%yob
    write(*,"(A6,1X)",advance='no')self%mf
    write(*,"(I6,1X)",advance='no')self%rankMF
    write(*,"(A6,1X)",advance='no')self%cat
    write(*,"(I6,1X)",advance='no')self%rankCat
    write(*,"(F6.2,1X)",advance='no')self%speed
    write(*,"(A1)",advance='no')"]"
    write(*,*)""
  end subroutine show_tuple



  function get_time(time) result(t)
    character(len=*) :: time
    integer          :: hpos,t
    integer          :: h,m,s
    !Get first h which can be 1 or 2 digits:
    hpos = scan(time,":")
    h = str2int(time(:hpos-1))
    m = str2int(time(hpos+1:hpos+2))
    s = str2int(time(hpos+4:hpos+5))
    t = s + m*60 + h*3600
  end function get_time

  function str2int(str) result(int)
    character(len=*) :: str
    integer          :: int
    read(str,*)  int
  end function str2int

end module DATA_TUPLE
