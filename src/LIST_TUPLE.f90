MODULE LIST_TUPLE
  USE SCIFOR
  USE DATA_TUPLE
  implicit none
  private



  type tuple_list
     integer             :: size=0
     type(tuple),pointer :: root   =>null()
   contains
     procedure,pass :: free    => free_tuple_list
     procedure,pass :: append  => append_tuple_list
     procedure,pass :: get     => get_tuple_list
     procedure,pass :: read    => read_tuple_list
     procedure,pass :: rank    => rank_tuple_list
     procedure,pass :: xrank   => xrank_tuple_list
     procedure,pass :: time    => time_tuple_list
     procedure,pass :: nat     => nat_tuple_list
     procedure,pass :: yob     => yob_tuple_list
     procedure,pass :: mf      => mf_tuple_list
     procedure,pass :: rankMF  => rankMF_tuple_list
     procedure,pass :: cat     => cat_tuple_list
     procedure,pass :: rankCat => rankCat_tuple_list
     procedure,pass :: speed   => speed_tuple_list
     procedure,pass :: pdf     => pdf_tuple_list
     procedure,pass :: show    => show_tuple_list
  end type tuple_list


  !GENERIC CONSTRUCTOR
  interface tuple_list
     module procedure :: construct_from_data
     module procedure :: construct_from_tuple
  end interface tuple_list

  !INTRINSIC FUNCTION SIZE(TUPLE_LIST)
  intrinsic :: size
  interface size
     module procedure :: size_tuple_list
  end interface size

  public :: tuple_list
  public :: size

contains



  !##################################################################
  !##################################################################
  !       LIST CONSTRUCTOR/DESTRUCTOR
  !##################################################################
  !##################################################################
  !+------------------------------------------------------------------+
  !PURPOSE:  Intrinsic constructor: given a tuple
  !+------------------------------------------------------------------+
  function construct_from_data(rank,time,nat,yob,mf,rankMF,cat,rankCat,speed) result(self)
    integer          :: rank
    character(len=*) :: time
    character(len=3) :: nat
    integer          :: yob
    character(len=1) :: mf
    integer          :: rankMF
    character(len=3) :: cat
    integer          :: rankCat     
    real(8)          :: speed
    type(tuple_list) :: self
    type(tuple)      :: t
    call self%free()
    allocate(self%root)
    t = tuple(rank,time,nat,yob,mf,rankMF,cat,rankCat,speed)
    call self%append(t)
  end function construct_from_data

  !+------------------------------------------------------------------+
  !PURPOSE:  Intrinsic constructor: given a list of keys+operators
  !+------------------------------------------------------------------+
  function construct_from_tuple(t) result(self)
    type(tuple)      :: t
    type(tuple_list) :: self
    call self%free()
    allocate(self%root)
    call self%append(t)
  end function construct_from_tuple




  !+------------------------------------------------------------------+
  !PURPOSE:  Free an operators_list (destructor) 
  !+------------------------------------------------------------------+
  recursive subroutine free_tuple_list(self)
    class(tuple_list),intent(inout) :: self
    type(tuple),pointer             :: p,c
    if(.not.associated(self%root))return
    do
       p => self%root
       c => p%next
       if(.not.associated(c))exit  !empty list
       p%next => c%next
       call c%free()
       c%next => null()
       deallocate(c)
    enddo
    self%size=0
    self%root=>null()
    p=>null()
    c=>null()
  end subroutine free_tuple_list





  !##################################################################
  !##################################################################
  !       APPEND/READ & OPERATORS 
  !##################################################################
  !##################################################################
  !+------------------------------------------------------------------+
  !PURPOSE:  Append == Put a sparse matrix as operator in the operators_list
  !+------------------------------------------------------------------+
  subroutine append_tuple_list(self,t)
    class(tuple_list),intent(inout) :: self
    type(tuple),intent(in)              :: t
    type(tuple),pointer                 :: p,c
    logical                             :: iupdate
    !
    if(.not.associated(self%root))allocate(self%root)
    !
    iupdate = .false.
    p => self%root
    c => p%next
    do                            !traverse the list until QN is found
       if(.not.associated(c))exit
       if (c%rank == t%rank) then
          iupdate = .true.
          exit
       endif
       p => c
       c => c%next
    end do
    !
    if(iupdate)then                !KEY exists: update operator
       c%rank    = t%rank
       c%time    = t%time
       c%nat     = t%nat
       c%yob     = t%yob
       c%mf      = t%mf
       c%rankMF  = t%rankMF
       c%cat     = t%cat
       c%rankCat = t%rankCat     
       c%speed   = t%speed
    else                        !KEY does not exist: create a new element
       allocate(p%next)
       p%next%rank    = t%rank
       p%next%time    = t%time
       p%next%nat     = t%nat
       p%next%yob     = t%yob
       p%next%mf      = t%mf
       p%next%rankMF  = t%rankMF
       p%next%cat     = t%cat
       p%next%rankCat = t%rankCat     
       p%next%speed   = t%speed
       if(.not.associated(c))then !end of the list special case (c=>c%next)
          p%next%next  => null()
       else
          p%next%next  => c      !the %next of the new node come to current
       end if
       self%size = self%size+1
    endif
    p=>null()
    c=>null()
  end subroutine append_tuple_list



  !+------------------------------------------------------------------+
  !PURPOSE: Return tuple from the tuple_list
  !+------------------------------------------------------------------+
  function get_tuple_list(self,index) result(t)
    class(tuple_list)   :: self
    integer,optional    :: index
    type(tuple)         :: t
    integer             :: index_
    type(tuple),pointer :: c
    logical             :: ifound
    !
    index_=self%size;if(present(index))index_=index
    if(index_>self%size)stop "get_tuple_list: index !in [1,self.size]"
    if(index_<0)index_=self%size+index_+1
    !
    ifound=.false.
    c => self%root%next
    loop:do                            !traverse the list until QN is found
       if(.not.associated(c))exit
       if(c%rank == index_)then
          ifound=.true.
          exit
       endif
       c => c%next
    end do loop
    if(.not.ifound)stop "get_tuple_list error: not found"
    !
    t = c
    !
    c=>null()
  end function get_tuple_list





  subroutine read_tuple_list(self,file)
    class(tuple_list),intent(inout) :: self
    character(len=*),intent(in)     :: file
    type(tuple)                     :: t
    integer                         :: i,L
    L = file_length(file)
    open(457,file=trim(file))
    do i=1,L
       call t%read(457)
       call self%append(t)
    enddo
    close(457)
  end subroutine read_tuple_list





  function rank_tuple_list(self) result(data)
    class(tuple_list),intent(inout)  :: self
    integer,dimension(:),allocatable :: data
    real(8),dimension(:),allocatable :: yout
    type(tuple)                      :: t
    integer                          :: i
    if(allocated(data))deallocate(data)
    allocate(data(size(self)))
    do i=1,size(self)
       t = self%get(index=i)
       data(i) = t%rank
    enddo
    call t%free()
  end function rank_tuple_list


  function xrank_tuple_list(self,N) result(data)
    class(tuple_list),intent(inout)  :: self
    integer,intent(in),optional      :: N
    real(8),dimension(:),allocatable :: data,data_
    type(tuple)                      :: t
    integer                          :: i
    if(allocated(data))deallocate(data)
    allocate(data(size(self)))
    do i=1,size(self)
       t = self%get(index=i)
       data(i) = dble(t%rank)/size(self)
    enddo
    call t%free()
    if(present(N))then
       allocate(data_,source=data)
       deallocate(data);allocate(data(N))
       call poly_spline(&
            linspace(0d0,1d0,size(data_)),data_,&
            linspace(0d0,1d0,N),data,10)
    end if
  end function xrank_tuple_list


  function time_tuple_list(self,N) result(data)
    class(tuple_list),intent(inout)  :: self
    integer,intent(in),optional      :: N
    real(8),dimension(:),allocatable :: data,data_
    type(tuple)                      :: t
    integer                          :: i
    if(allocated(data))deallocate(data)
    allocate(data(size(self)))
    do i=1,size(self)
       t = self%get(index=i)
       data(i) = t%time
    enddo
    call t%free()
    if(present(N))then
       allocate(data_,source=data)
       deallocate(data);allocate(data(N))
       call poly_spline(&
            linspace(0d0,1d0,size(data_)),data_,&
            linspace(0d0,1d0,N),data,10)
    end if
  end function time_tuple_list



  function nat_tuple_list(self) result(data)
    class(tuple_list),intent(inout) :: self
    character(len=3),dimension(:),allocatable :: data
    type(tuple)                     :: t
    integer                         :: i
    if(allocated(data))deallocate(data)
    allocate(data(size(self)))
    do i=1,size(self)
       t = self%get(index=i)
       data(i) = t%nat
    enddo
    call t%free()
  end function nat_tuple_list



  function yob_tuple_list(self) result(data)
    class(tuple_list),intent(inout)  :: self
    integer,dimension(:),allocatable :: data
    type(tuple)                      :: t
    integer                          :: i
    if(allocated(data))deallocate(data)
    allocate(data(size(self)))
    do i=1,size(self)
       t = self%get(index=i)
       data(i) = t%yob
    enddo
    call t%free()
  end function yob_tuple_list

  function mf_tuple_list(self) result(data)
    class(tuple_list),intent(inout) :: self
    character(len=1),dimension(:),allocatable :: data
    type(tuple)                     :: t
    integer                         :: i
    if(allocated(data))deallocate(data)
    allocate(data(size(self)))
    do i=1,size(self)
       t = self%get(index=i)
       data(i) = t%mf
    enddo
    call t%free()
  end function mf_tuple_list


  function rankMF_tuple_list(self) result(data)
    class(tuple_list),intent(inout) :: self
    integer,dimension(:),allocatable :: data
    type(tuple)                     :: t
    integer                         :: i
    if(allocated(data))deallocate(data)
    allocate(data(size(self)))
    do i=1,size(self)
       t = self%get(index=i)
       data(i) = t%rankmf
    enddo
    call t%free()
  end function rankmf_tuple_list

  function cat_tuple_list(self) result(data)
    class(tuple_list),intent(inout) :: self
    character(len=3),dimension(:),allocatable :: data
    type(tuple)                     :: t
    integer                         :: i
    if(allocated(data))deallocate(data)
    allocate(data(size(self)))
    do i=1,size(self)
       t = self%get(index=i)
       data(i) = t%cat
    enddo
    call t%free()
  end function cat_tuple_list

  function rankCAT_tuple_list(self) result(data)
    class(tuple_list),intent(inout) :: self
    integer,dimension(:),allocatable :: data
    type(tuple)                     :: t
    integer                         :: i
    if(allocated(data))deallocate(data)
    allocate(data(size(self)))
    do i=1,size(self)
       t = self%get(index=i)
       data(i) = t%rankCAT
    enddo
    call t%free()
  end function rankCAT_tuple_list


  function speed_tuple_list(self) result(data)
    class(tuple_list),intent(inout) :: self
    real(8),dimension(:),allocatable :: data
    type(tuple)                     :: t
    integer                         :: i
    if(allocated(data))deallocate(data)
    allocate(data(size(self)))
    do i=1,size(self)
       t = self%get(index=i)
       data(i) = t%speed
    enddo
    call t%free()
  end function speed_tuple_list




  function pdf_tuple_list(self,N,type) result(pdf)
    class(tuple_list),intent(inout)      :: self
    integer,intent(in)                   :: N
    character(len=*),intent(in),optional :: type
    character(len=10)                    :: type_
    type(pdf_kernel)                     :: pdf
    real(8),dimension(:),allocatable     :: data
    real(8)                              :: a,b,sigma
    type_='time';if(present(type))type_=type
    select case(str(to_lower(type_)))
    case ("t","time")
       data = self%time()
    case ("s","speed")
       data = self%speed()
    case default
       stop "pdf_tuple_list error: type not in [time,speed]"
    end select
    a = 0d0 ; b = 1.5*data(size(data))
    call pdf_allocate(pdf,N)
    call pdf_set_range(pdf,a,b)
    call pdf_sigma(pdf,data,sigma)
    call pdf_push_sigma(pdf,sigma)
    call pdf_accumulate(pdf,data)
  end function pdf_tuple_list



  !##################################################################
  !##################################################################
  !              ENUMERATOR & ITERATORS
  !##################################################################
  !##################################################################
  !+------------------------------------------------------------------+
  !PURPOSE:  Returns the size of given operators_list
  !+------------------------------------------------------------------+
  function size_tuple_list(self) result(size)
    class(tuple_list),intent(in) :: self
    integer                      :: size
    size = self%size
  end function size_tuple_list








  !##################################################################
  !##################################################################
  !               SHOW 
  !##################################################################
  !##################################################################
  !+------------------------------------------------------------------+
  !PURPOSE:  Pretty print an operators_list
  !+------------------------------------------------------------------+
  recursive subroutine show_tuple_list(self)
    class(tuple_list),intent(inout) :: self
    integer                         :: i,count=0
    type(tuple),pointer             :: c
    !
    write(*,"(A6,I12)")"Size :",self%size
    write(*,"(A18)")"------------------"
    c => self%root%next
    do
       if(.not.associated(c))exit
       count=count+1
       call c%show()
       c => c%next
    end do
    c=>null()
  end subroutine show_tuple_list




END MODULE LIST_TUPLE
