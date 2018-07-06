module func
  implicit none
  real(8),parameter :: pi=4.d0*atan(1.d0)
contains
  function delta(Pbar,omg,haba)
    real(8),intent(in) :: Pbar,omg,haba
    real(8) :: delta
    delta = exp(-(Pbar-omg)**2/(2.0d0*haba**2))/(sqrt(2.0d0*pi*haba**2))
  end function delta
end module func

program main
  use func
  implicit none
  integer,parameter :: m=50
  integer :: i,j,n
  integer :: irr
  real(8) :: x
  real(8) :: max,min,dw,Pbar
  real(8) :: av
  real(8) :: sum_w,sig
  real(8),allocatable :: DATA(:)
  open(10,file='data.dat',status='old')
  open(11,file='hist.dat')

  n = 0
  sig = 0.d0
  do
     read(10,*,iostat=irr) 
     if (irr < 0) exit
     n = n+1
  end do
  rewind(10)

  allocate(DATA(n))
  do i = 1, n
     read(10,*) DATA(i)
  end do
  
  max = maxval(DATA)
  min = minval(DATA)
  dw  = (max-min)/dble(m)
  av  = sum(DATA)/dble(n)
  
  do i = 0, m
     sum_w = 0.d0
     Pbar = min+dw*dble(i)
     do j = 1, n
        sum_w = sum_w+delta(Pbar,DATA(j),2.d0*dw)
     end do
     write(*,'(2f20.8)') Pbar, sum_w
     write(11,'(2f20.8)') Pbar, sum_w
  end do
  
  do i = 1, n
     sig = sig+abs(av-DATA(i))/dble(n)
  end do
  print *, 'sigma'
  write(*,*) sig
  
  
end program main
