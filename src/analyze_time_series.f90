program analysis
  USE SCIFOR
  USE DATA_TUPLE
  USE LIST_TUPLE
  implicit none

  integer                                   :: i
  character(len=12 )                        :: time
  integer                                   :: t0,t,irace
  integer                                   :: pos,flen,list_length,iunit,ounit,lunit
  character(len=100)                        :: ifile,ofile
  character(len=256)                        :: event
  type(pdf_kernel) :: pdf
  real(8)                                   :: a,b,sigma
  real(8),dimension(:),allocatable          :: xdata,ydata,sdata
  type(tuple)                               :: td
  type(tuple_list),dimension(:),allocatable :: tlist


  open(free_unit(lunit),file='list.race')
  list_length=file_length('list.race')

  allocate(tlist(list_length))
  
  do irace=1,list_length
     read(lunit,*)event
     ifile="input_"//str(event)//".csv"
     !>read data from file:
     call tlist(irace)%read(str(ifile))

     td=tlist(irace)%get(1)
     t0=td%time

     xdata = tlist(irace)%xrank()
     ydata = tlist(irace)%time()
     sdata = tlist(irace)%speed()

     ofile="output_"//str(event)//".dat"
     open(free_unit(ounit),file=str(ofile))
     do i=1,size(tlist(irace))
        write(ounit,*)xdata(i),ydata(i)-t0,ydata(i),sdata(i)
     enddo
     close(ounit)


     
     
     pdf = tlist(irace)%pdf(500,'time')
     call pdf_print(pdf,"PDF_"//str(ofile))
  enddo








end program analysis
