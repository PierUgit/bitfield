#define DEBUG

#ifndef DEBUG
#define _PURE_ pure
#else
#define _PURE_
#endif

!***********************************************************************************************
! Author: Pierre Hugonnet
! https://github.com/PierUgit
! License: GPL v3 
!***********************************************************************************************
! Implementation of a 1-bit logical array, stored in a default integer array
! Not cpu-efficient at all, but memory efficient
!
! Limitations:
! - 1D only
! - size limited to a default integer
!
! Example: 
! 
! type(bitfield_t) :: b
!
! call b%allocate(size)
! call b%allocate(lb,ub)
!     integer :: size, lb, ub
!
! call b%deallocate()
!
! n  = b%getsize()
! lb = b%getlb()
! ub = b%getub() 
! call b%setlb(lb)
! call b%setub(ub)
!     integer :: n, lb, ub
!
! c = b                         ! efficient
!     type(bitfield_t) :: b, c
! 
! call b%set(bool)              ! efficient if bool is a scalar
! call b%set(pos,bool)          ! not efficient
! call b%set(from,to,inc,bool)  ! efficient if bool is a scalar and |inc|==1
!     logical :: bool[(:)]
!     integer :: pos, from, top, inc
!     *Note:* b must always be allocated beforehand
!
! b = bool                      ! efficient
!     type(bitfield_t) :: b
!     logical :: bool
! b = bool                      ! not efficient
!     type(bitfield_t) :: b
!     logical, allocatable :: bool(:)
!     Note: allocation on assignement can occur
!
! call b%get(pos,bool)          ! not efficient
!     logical :: bool           
! call b%get(bool)              ! not efficient
! call b%get(from,to,inc,bool)  ! not efficient
!     logical :: bool(:)
!     integer :: pos, frompos, topos
!     *Note:* bool(:) must be allocated beforehand
!
! bool = b%fget(pos)            ! not efficient
!     logical :: bool
! bool = b%fget()               ! not efficient
! bool = b%fget(from,to,inc)    ! not efficient
!     logical :: bool(:)
!     integer :: pos, from, top, inc
!     *Note:* bool(:) must be allocated beforehand
!
! bool = b                      ! not efficient
!     type(bitfield_t) :: b
!     logical, allocatable :: bool(:)
!     *Note:* works only for an allocatable LHS; allocation on assignement can occur
!
! call b%extract(from,to,inc,c)     ! efficient if inc==1
! c = b%fextract(from,to,inc)       ! efficient if inc==1
!     integer :: from, to, inc
!     type(bitfield_t) :: c
!
! call b%replace(from,to,inc,c)     ! efficient if inc==1
!     integer :: from, to, inc
!     type(bitfield_t) :: c
!
! n = b%count()                 ! efficient
! n = b%count(from,top,inc)     ! efficient if |inc|==1
!     integer :: from, to, inc
!
! bool = b%all()                ! efficient
! bool = b%all(from,to,inc)     ! efficient if |inc|==1
!     integer :: from, to, inc
!
! bool = b%any()                ! efficient
! bool = b%any(from,to,inc)     ! efficient if |inc|==1
!     integer :: from, to, inc
!
! call b%not()                  ! efficient
! call b%not(from,to,inc)       ! efficient if |inc|==1
! c = .not.b                    ! efficient
!     type(bitfield_t) :: b, c
!
! c = b1 .and.  b2              ! efficient
! c = b1 .or.   b2              ! efficient
! c = b1 .eqv.  b2              ! efficient
! c = b1 .neqv. b2              ! efficient
!     type(bitfield_t) :: b1, b2, c
! 
! bool = ( b1 == b2 )           ! efficient
! bool = ( b1 /= b2 )           ! efficient
!     type(bitfield_t) :: b1, b2
!     logical :: bool
!
!***********************************************************************************************
module bitfield
!use iso_fortran_env
implicit none

   private

   public :: bitfield_t, bitfield_check
   public :: assignment(=), operator(==), operator(/=)
   public :: operator(.not.), operator(.and.), operator(.or.)
   public :: operator(.eqv.), operator(.neqv.)
   public :: BITFIELD_GROWONLY, BITFIELD_GROWSHRINK

   integer, parameter :: ik = selected_int_kind(r=18)
   integer, parameter :: l = bit_size(0_ik)
   integer, parameter :: l2l = nint(log(real(l))/log(2.0))
   integer, parameter :: ll = 64*l
   integer, parameter :: minbatch = 10
   integer(ik), parameter :: zeros = 0
   integer(ik), parameter :: ones = not(zeros)
   integer, parameter :: BITFIELD_GROWONLY = 0
   integer, parameter :: BITFIELD_GROWSHRINK = 1

   type :: bitfield_t
      private
      integer(ik), allocatable :: a(:)
      integer :: n = -1
      integer :: lb = 1
      integer :: ub = 0
      integer :: jmax = -1
      integer :: strat = BITFIELD_GROWONLY
   contains
      private
      procedure, public :: allocate => b_allocate
      procedure, public :: deallocate => b_deallocate
      procedure, public :: allocated => b_allocated
      
      procedure, public :: resize => b_resize
      procedure, public :: recap => b_recap
      procedure, public :: set_dynamic_capacity => b_set_dynamic_capacity
      
      procedure :: append_b => b_append_b
      procedure :: append_l0 => b_append_l0
      procedure :: append_l1 => b_append_l1
      generic, public :: append => append_b, append_l0, append_l1
      
      procedure, public :: drop => b_drop
   
      procedure, public :: getsize => b_getsize
      procedure, public :: getcapacity => b_getcapacity
      procedure, public :: getlb => b_getlb
      procedure, public :: getub => b_getub
      procedure, public :: setlb => b_setlb
      procedure, public :: setub => b_setub
   
      procedure :: set0 => b_set0
      procedure :: setall0 => b_setall0
      procedure :: setrange0 => b_setrange0
      procedure :: setall1 => b_setall1
      procedure :: setrange1 => b_setrange1
      generic, public:: set => set0, setall0, setall1, setrange0, setrange1
      
      procedure :: get0 => b_get0
      procedure :: getall => b_getall
      procedure :: getrange => b_getrange
      generic, public :: get => get0, getall, getrange
   
      procedure :: fget0 => b_fget0
      procedure :: fgetall => b_fgetall
      procedure :: fgetrange => b_fgetrange
      generic, public :: fget => fget0, fgetall, fgetrange
   
      procedure :: countall => b_countall
      procedure :: countrange => b_countrange
      generic, public :: count => countall, countrange
   
      procedure :: allall => b_allall
      procedure :: allrange => b_allrange
      generic, public  :: all => allall, allrange
      procedure :: anyall => b_anyall
      procedure :: anyrange => b_anyrange
      generic, public :: any => anyall, anyrange
   
      procedure, public :: extract => b_extract
      procedure, public :: fextract => b_fextract
      procedure, public :: replace => b_replace   
      
      procedure :: notall => b_notall
      procedure :: notrange => b_notrange
      generic, public :: not => notall, notrange
   end type

   interface assignment(=)
      module procedure assign_b2b, assign_l2b_0, assign_l2b_1, assign_b2l
   end interface

   interface operator(.not.)
      module procedure b_fnotall
   end interface
   interface operator(.and.)
      module procedure b_and
   end interface
   interface operator(.or.)
      module procedure b_or
   end interface
   interface operator(.eqv.)
      module procedure b_eqv
   end interface
   interface operator(.neqv.)
      module procedure b_neqv
   end interface
   interface operator(==)
      module procedure b_equal
   end interface
   interface operator(/=)
      module procedure b_notequal
   end interface

contains

   logical function bitfield_check() result(stat)
   
      integer :: ii
      
      stat = all( btest( ones, [(ii,ii=0,l-1)] ) )
      stat = stat .and. shiftr(101,1) == 50 .and. shiftl(101,1) == 202
   end function
   


   integer _PURE_ function b_getsize(this)
      class(bitfield_t), intent(in) :: this
      
      b_getsize = this%n
   end function 
   
   integer _PURE_ function b_getcapacity(this)
      class(bitfield_t), intent(in) :: this
      
      b_getcapacity = size( this%a ) * l
   end function 
   
   integer _PURE_ function b_getlb(this)
      class(bitfield_t), intent(in) :: this
      
      b_getlb = this%lb
   end function 

   integer _PURE_ function b_getub(this)
      class(bitfield_t), intent(in) :: this
      
      b_getub = this%ub
   end function 

   _PURE_ subroutine b_setlb(this,lb)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: lb
      
      if (this%n > 0) then
         this%lb = lb
         this%ub = lb + this%n -1
      end if
   end subroutine 

   _PURE_ subroutine b_setub(this,ub)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: ub
      
      if (this%n > 0) then
         this%lb = ub - this%n + 1
         this%ub = ub
      end if
   end subroutine 



   _PURE_ subroutine b_allocate(this,n,lb,ub,mold,source,capacity)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: n, lb, ub, capacity
      type(bitfield_t), intent(in) :: mold, source
      optional :: n, lb, ub, mold, source, capacity
      
      integer :: lb___, ub___, si___
            
      if (allocated(this%a)) error stop "b_allocate: bitfield is already allocated"
      
      if (present(n).or.present(ub)) then
         if (present(n)) then
            lb___ = 1
            ub___ = n
         else
            lb___ = lb
            ub___ = ub
         end if
         si___ = 1
      else if (present(mold)) then
         lb___ = mold%lb
         ub___ = mold%ub
      else if (present(source)) then
         lb___ = source%lb
         ub___ = source%ub
      end if
      call allocate_core( this, lb___, ub___ )
      if (present(capacity)) call b_recap( this, capacity )
      if (present(source)) this%a(0:source%jmax) = source%a(0:source%jmax)
      
   end subroutine 

   _PURE_ subroutine allocate_core(this,lb,ub)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: lb, ub
            
      if (ub >= lb) then
         this%n = ub - lb + 1 
         this%lb = lb
         this%ub = ub
         allocate( this%a(0:(this%n-1)/l) )
         this%jmax = ubound( this%a, 1 )
      else
         this%n = 0 
         allocate( this%a(0:0) )
      end if
      
   end subroutine 

   _PURE_ subroutine b_deallocate(this)
      class(bitfield_t), intent(inout) :: this
            
      if (.not.b_allocated(this)) error stop "b_deallocate: bitfield is not allocated"
      
      deallocate( this%a )
      this%n = -1
      this%lb = 1
      this%ub = 0
      this%jmax = -1
      this%strat = BITFIELD_GROWONLY
   end subroutine 
   
   _PURE_ logical function b_allocated(this)
      class(bitfield_t), intent(in) :: this
            
      b_allocated = allocated( this%a )
   end function 
   
   
   
   _PURE_ subroutine b_set_dynamic_capacity(this,strat)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: strat
      
      this%strat = strat
   end subroutine
   
   _PURE_ subroutine b_resize(this,lb,ub,keep)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: lb, ub
      logical, intent(in) :: keep
      optional :: keep
      
      integer :: n, si, newcap

      if (.not.b_allocated(this)) error stop "b_resize: bitfield is not allocated"

      n = ub - lb + 1
      if (n > size(this%a) * l) then
         newcap = 2 * size(this%a) * l
         do while (n > newcap)
            newcap = 2*newcap
         end do
         call b_recap( this, newcap, keep )
      else if (3*n <= size(this%a) * l .and. this%strat == BITFIELD_GROWSHRINK) then
         newcap = size(this%a) * l / 2
         do while (3*n <= newcap)
            newcap = newcap / 2
         end do
         call b_recap( this, newcap, keep )
      end if
      this%n = n
      this%lb = lb
      this%ub = ub
      this%jmax = (this%n-1) / l   
   end subroutine
   
   _PURE_ subroutine b_recap(this,capacity,keep)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: capacity
      logical, intent(in) :: keep
      optional :: capacity, keep
      
      integer :: newcap
      logical :: keep___
      integer(ik), allocatable :: atmp(:)
      
      if (.not.b_allocated(this)) error stop "b_resize: bitfield is not allocated"

      keep___ = .true. ; if (present(keep)) keep___ = keep

      newcap = this%n
      if (present(capacity)) newcap = max( capacity, newcap )
      newcap = ((newcap-1)/l+1) * l
      if (newcap /= size(this%a)*l) then
         allocate( atmp(0:(newcap-1)/l) )
         if (keep___) atmp(0:this%jmax) = this%a(0:this%jmax)
         call move_alloc( atmp, this%a )
      end if
   end subroutine
      
         
            
   _PURE_ subroutine b_append_b(this,that)
      class(bitfield_t), intent(inout) :: this
      type(bitfield_t), intent(in) :: that
      
      integer :: ub

#ifdef DEBUG   
      if (.not.b_allocated(this)) error stop "b_append_b: bitfield is not allocated"
#endif
      ub = this%ub
      call b_resize( this, (this%lb), this%ub+that%n, .true. )
      call b_replace( this, ub+1, this%n, 1, that )
   end subroutine
         
   _PURE_ subroutine b_append_l0(this,v)
      class(bitfield_t), intent(inout) :: this
      logical, intent(in) :: v
            
#ifdef DEBUG   
      if (.not.b_allocated(this)) error stop "b_append_l0: bitfield is not allocated"
#endif
      call b_resize( this, (this%lb), this%ub+1, .true. )
      call b_set0( this, this%n, v )
   end subroutine
   
   _PURE_ subroutine b_append_l1(this,v)
      class(bitfield_t), intent(inout) :: this
      logical, intent(in) :: v(:)
                  
      integer :: ub

#ifdef DEBUG   
      if (.not.b_allocated(this)) error stop "b_append_l0: bitfield is not allocated"
#endif
      ub = this%ub
      call b_resize( this, (this%lb), this%ub+size(v), .true. )
      call b_setrange1( this, ub+1, this%n, 1, v )
   end subroutine

   _PURE_ subroutine b_drop(this,k)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: k
      optional :: k
      
#ifdef DEBUG   
      if (.not.b_allocated(this)) error stop "b_append_l0: bitfield is not allocated"
#endif
      if (present(k)) then
         call b_resize( this, (this%lb), max(this%ub-k,0), .true. )
      else
         call b_resize( this, (this%lb), max(this%ub-1,0), .true. )
      end if
                  
   end subroutine
      
   
   
   _PURE_ subroutine assign_b2b(this,that)
      class(bitfield_t), intent(inout) :: this
      type(bitfield_t), intent(inout) :: that
      
      if (b_allocated(this) .and. this%getsize() /= that%getsize()) &
         call b_deallocate(this)
      if (.not.b_allocated(this)) &
         call allocate_core( this, that%getlb(), that%getub() )
      this%a(:) = that%a(0:that%jmax)
   end subroutine 
   
   _PURE_ subroutine assign_l2b_0(this,v)
      class(bitfield_t), intent(inout) :: this
      logical, intent(in) :: v
      
      call b_setall0(this,v)
   end subroutine 
   
   _PURE_ subroutine assign_l2b_1(this,v)
      class(bitfield_t), intent(inout) :: this
      logical, allocatable, intent(in) :: v(:)
      
      if (b_allocated(this) .and. this%getsize() /= size(v)) call b_deallocate(this)
      if (.not.b_allocated(this)) call allocate_core(this,lbound(v,1),ubound(v,1))
      call b_setall1(this,v)
   end subroutine 

   _PURE_ subroutine assign_b2l(v,this)
      logical, allocatable, intent(out) :: v(:)
      type(bitfield_t), intent(in) :: this
      
      if (allocated(v) .and. this%getsize() /= size(v)) deallocate(v)
      if (.not.allocated(v)) allocate( v(this%getlb():this%getub()) )
      call b_getall(this,v)
   end subroutine 

   

   _PURE_ subroutine b_set0(this,i,v)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: i
      logical, intent(in) :: v
      
      integer :: ii, j
      
      ! no runtime check, as it would hurt the performances for a single bit set
      call indeces(this,i,j,ii)
      if (v) then
         this%a(j) = ibset(this%a(j),ii)
      else
         this%a(j) = ibclr(this%a(j),ii)
      end if
   end subroutine 

   _PURE_ subroutine b_setall0(this,v)
      class(bitfield_t), intent(inout) :: this
      logical, intent(in) :: v
      
      if (.not.b_allocated(this)) error stop "b_setall0: bitfield is not allocated"
      this%a(:) = merge(ones,zeros,v)
   end subroutine 

   _PURE_ recursive subroutine b_setrange0(this,istart,istop,inc,v)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: istart, istop, inc
      logical, intent(in) :: v
      
      integer(ik) :: a
      integer :: iistart, iistop, jstart, jstop, i, j, k
      integer :: iir(l), iirs
      
      if (inc < 0) then
         call b_setrange0(this,istop+mod(istart-istop,-inc),istart,-inc,v)
         return
      end if
      
      if (.not.b_allocated(this)) error stop "b_setrange0: bitfield is not allocated"
      if (istart < this%lb .or. istart > this%ub .or. istop < this%lb .or. istop > this%ub) &
         error stop "b_setrange0(): out of bound indeces" 
      if (istop < istart) return
      
      if (inc == 1) then
         a = merge(ones,zeros,v)
         call indeces(this,istart,jstart,iistart)
         call indeces(this,istop ,jstop ,iistop)
         if (jstart == jstop) then
            call mvbits( a, 0, iistop-iistart+1, this%a(jstart), iistart )
         else
            call mvbits( a, 0, l-iistart, this%a(jstart), iistart )
            this%a(jstart+1:jstop-1) = a
            call mvbits(a,0,iistop+1,this%a(jstop),0)
         endif
      else if (inc <= l/minbatch) then
         call indeces(this,istart,jstart,iistart)
         call indeces(this,istop ,jstop ,iistop)
         j = jstart
         iirs = 0
         do
            call getiirs(jstart,jstop,iistart,iistop,inc,j,iir,iirs)  
            a = this%a(j)
            if (v) then ; do k = 1, iirs ; a = ibset(a,iir(k)) ; end do
                   else ; do k = 1, iirs ; a = ibclr(a,iir(k)) ; end do
            end if
            this%a(j) = a
            if (j == jstop) exit
         end do
      else
         do i = istart, istop, inc
            call b_set0(this,i,v)
         end do
      end if
   end subroutine 
   
   _PURE_ subroutine b_setall1(this,v)
      class(bitfield_t), intent(inout) :: this
      logical, intent(in) :: v(:)
      
      call b_setrange1(this,this%lb,this%ub,1,v)
   end subroutine 

   _PURE_ subroutine b_setrange1(this,istart,istop,inc,v)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: istart, istop, inc
      logical, intent(in) :: v(:)
      
      integer :: k, j, i, iistart, iistop, jstart, jstop, iv
      integer :: iir(l), iirs
      integer(ik) :: a
      
      if (.not.b_allocated(this)) error stop "b_setrange1: bitfield is not allocated"
      if (this%n == 0 ) then
         if ((istop-istart)*inc >=0) error stop "b_setrange1(): out of bound indeces" 
         return
      else
         if (istart < this%lb .or. istart > this%ub .or. istop < this%lb .or. istop > this%ub) &
            error stop "b_setrange1(): out of bound indeces" 
      end if
      if ( (istop-istart)/inc+1 /= size(v) ) error stop "b_setrange1(): the shapes differ" 
      
      iv = 0
      do i = istart, istop, inc
         iv = iv+1
         call b_set0( this, i, v(iv) )
      end do
   end subroutine 

   

   _PURE_ subroutine b_get0(this,i,v)
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: i
      logical, intent(out) :: v
      
      integer :: j, ii
      
      call indeces(this,i,j,ii)
      v = btest(this%a(j),ii)
   end subroutine 
   
   _PURE_ subroutine b_getall(this,v)
      class(bitfield_t), intent(in) :: this
      logical, intent(out) :: v(:)
      
      if (this%getsize() /= size(v)) error stop "b_getall(): the sizes differ" 
      call b_getrange(this,this%lb,this%ub,1,v)
   end subroutine 
   
   _PURE_ subroutine b_getrange(this,istart,istop,inc,v)
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      logical, intent(out) :: v(:)
      
      integer :: i1, i2, j, iistart, iistop, jstart, jstop, i, iv
      integer :: iir(l), iirs
      
      if (sign(1,istop-istart)*sign(1,inc) < 0) return
      if (istart < this%lb .or. istart > this%ub .or. istop < this%lb .or. istop > this%ub) &
         error stop "b_getrange1(): out of bound indeces" 

      if (0 < inc .and. inc <= l/minbatch) then
         call indeces( this, istart, jstart, iistart)
         call indeces( this, istop , jstop , iistop)
         j = jstart
         i1 = 1
         iirs = 0
         do
            call getiirs(jstart,jstop,iistart,iistop,inc,j,iir,iirs)
            i2 = i1+iirs-1
            v(i1:i2) = btest(this%a(j),iir(1:iirs))
            if (j == jstop) exit
            i1 = i2+1
         end do
      else if (0 < -inc .and. -inc <= l/minbatch) then
         call indeces( this, istart,                       jstart, iistart )
         call indeces( this, istop+mod(istart-istop,-inc), jstop,  iistop  )
         j = jstop
         i1 = size(v)
         iirs = 0
         do
            call getiirs(jstop,jstart,iistop,iistart,-inc,j,iir,iirs)
            i2 = i1-iirs+1
            v(i1:i2:-1) = btest(this%a(j),iir(1:iirs))
            if (j == jstart) exit
            i1 = i2-1
         end do
      else
         iv = 0
         do i = istart, istop, inc
            iv = iv+1
            call b_get0(this,i,v(iv))
         end do
      end if
   end subroutine 
         
   _PURE_ function b_fget0(this,i) result(v)
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: i
      logical :: v
      
      call b_get0(this,i,v)
   end function 

   _PURE_ function b_fgetall(this) result(v)
      class(bitfield_t), intent(in) :: this
      logical, allocatable:: v(:)
      
      allocate( v(this%getlb():this%getub()) )
      call b_getall(this,v)
   end function 

   _PURE_ function b_fgetrange(this,istart,istop,inc) result(v)
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      logical, allocatable :: v(:)
      
      integer :: n
      
      n = abs((istop-istart)/inc+1)
      allocate( v(n) )
      call b_getrange(this,istart,istop,inc,v)   
   end function


   
   _PURE_ subroutine b_replace(this,istart,istop,inc,that)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: istart, istop, inc
      type(bitfield_t), intent(in) :: that
      
      integer :: k, i, iistart, iistop, jstart, jstop, j, jsource, iisource, isource
      integer :: iir(l), iirs
      
      if (that%getsize() <= 0) return
      if (istart < this%lb .or. istart > this%ub .or. istop < this%lb .or. istop > this%ub) &
         error stop "b_replace(): out of bound bounds" 
      
      call indeces(this,istart,jstart,iistart)
      call indeces(this,istop,jstop ,iistop)
      if (inc == 1) then
         if (jstart == jstop) then
            call mvbits(that%a(0),0,iistop-iistart+1,this%a(jstart),iistart)
         else
            jsource = -1
            do j = jstart, jstop
               if (jsource >= 0) &
                  call mvbits(that%a(jsource),l-iistart,iistart,this%a(j),0)
               jsource = jsource + 1
               if (jsource <= ubound(that%a,1)) &
                  call mvbits(that%a(jsource),0,l-iistart,this%a(j),iistart)
            end do
         end if
      else
         isource = 0
         do i = istart, istop, inc
            isource = isource + 1
            call b_set0( this, i, b_fget0(that,isource) )
         end do      
      end if
   end subroutine 



   _PURE_ subroutine b_extract(this,istart,istop,inc,that)
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      type(bitfield_t), intent(inout) :: that
      
      integer :: k, i, iistart, iistop, jstart, jstop, j, jdest, iidest, n, idest
      integer :: iir(l), iirs
      logical :: v(l)
      
      if (istart < this%lb .or. istart > this%ub .or. istop  < this%lb .or. istop  > this%ub) &
         error stop "b_extract(): out of bound indeces" 
      
      if (b_allocated(that)) call b_deallocate( that )
         
      n = (istop-istart)/inc + 1
      if (n <= 0) then
         call allocate_core(that,1,0)
         return
      end if
      
      call allocate_core(that,1,n)
      call indeces(this,istart,jstart,iistart)
      call indeces(this,istop ,jstop ,iistop)
      if (inc == 1) then
         if (jstart == jstop) then
            call mvbits(this%a(jstart),iistart,iistop-iistart+1,that%a(0),0)
         else
            jdest = -1
            do j = jstart, jstop
               if (jdest >= 0) &
                  call mvbits(this%a(j),0,iistart,that%a(jdest),l-iistart)
               jdest = jdest + 1 ; 
               if (jdest <= ubound(that%a,1)) &
                  call mvbits(this%a(j),iistart,l-iistart,that%a(jdest),0)
            end do
         end if
      else
         idest = 0
         do i = istart, istop, inc
            idest = idest + 1
            call b_set0( that, idest, b_fget0(this,i) )
         end do
      end if
   end subroutine 
   
   _PURE_ function b_fextract(this,istart,istop,inc) result(that)
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      type(bitfield_t) :: that
      
      call b_extract(this,istart,istop,inc,that)
   end function
      
   

   _PURE_ logical function b_allall(this)
      class(bitfield_t), intent(in) :: this
      
      b_allall = b_allrange(this,this%lb,this%ub,1)
   end function 

   _PURE_ recursive logical function b_allrange(this,istart,istop,inc) result(v)
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      
      integer :: kstart, kstop
      type(bitfield_t) :: bb
   
      if (inc < 0) then
         v = b_allrange(this,istop+mod(istart-istop,-inc),istart,-inc)
      else
         v = .true.
         kstart = istart
         do while (kstart <= istop)
            kstop = min( kstart + (ll-1)*inc, istop )
            call b_extract( this, kstart, kstop, inc, bb )
            call set_end( bb )
            v = v .and. all( bb%a == ones )
            if (.not.v) return
            kstart = kstop + inc
         end do         
      end if      

   end function 



   _PURE_ logical function b_anyall(this)
      class(bitfield_t), intent(in) :: this
      
      b_anyall = b_anyrange(this,this%lb,this%ub,1)
   end function 

   _PURE_ recursive logical function b_anyrange(this,istart,istop,inc) result(v)
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      
      integer :: kstart, kstop
      type(bitfield_t) :: bb
   
      if (inc < 0) then
         v = b_anyrange(this,istop+mod(istart-istop,-inc),istart,-inc)
      else
         v = .false.
         kstart = istart
         do while (kstart <= istop)
            kstop = min( kstart + (ll-1)*inc, istop )
            call b_extract( this, kstart, kstop, inc, bb )
            call clear_end( bb )
            v = v .or. any( bb%a /= zeros )
            if (v) return
            kstart = kstop + inc
         end do         
      end if      

   end function 



   _PURE_ integer function b_countall(this) result(v)
      class(bitfield_t), intent(in) :: this
      
      v = b_countrange(this,this%lb,this%ub,1)
   end function 

   _PURE_ recursive integer function b_countrange(this,istart,istop,inc) result(v)
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      
      integer :: kstart, kstop
      type(bitfield_t) :: bb
   
      if (inc < 0) then
         v = b_countrange(this,istop+mod(istart-istop,-inc),istart,-inc)
      else
         v = 0
         kstart = istart
         do while (kstart <= istop)
            kstop = min( kstart + (ll-1)*inc, istop )
            call b_extract( this, kstart, kstop, inc, bb )
            call clear_end( bb )
            v = v + sum( popcnt( bb%a ) )
            kstart = kstop + inc
         end do         
      end if      

   end function 
   
   
   _PURE_ subroutine b_notall(this)
      class(bitfield_t), intent(inout) :: this
            
      this%a(:) = not( this%a )
   end subroutine
   
   _PURE_ recursive subroutine b_notrange(this,istart,istop,inc)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: istart, istop, inc
      
      integer :: kstart, kstop
      type(bitfield_t) :: bb
   
      if (inc < 0) then
         call b_notrange(this,istop+mod(istart-istop,-inc),istart,-inc)
      else
         kstart = istart
         do while (kstart <= istop)
            kstop = min( kstart + (ll-1)*inc, istop )
            call b_extract( this, kstart, kstop, inc, bb )
            bb%a(:) = not( bb%a )
            call b_replace( this, kstart, kstop, inc, bb )
            kstart = kstop + inc
         end do         
      end if      

   end subroutine 

   _PURE_ function b_fnotall(this) result(b)
      type(bitfield_t), intent(in) :: this
      type(bitfield_t) :: b
            
      call allocate_core(b,1,this%n)
      b%a(:) = not( this%a )
   end function
   
   _PURE_ function b_and(this,that) result(b)
      type(bitfield_t), intent(in) :: this, that
      type(bitfield_t) :: b
            
      call allocate_core(b,1,this%n)
      b%a(:) = iand( this%a, that%a )
   end function
   
   _PURE_ function b_or(this,that) result(b)
      type(bitfield_t), intent(in) :: this, that
      type(bitfield_t) :: b
            
      call allocate_core(b,1,this%n)
      b%a(:) = ior( this%a, that%a )
   end function
   
   _PURE_ function b_eqv(this,that) result(b)
      type(bitfield_t), intent(in) :: this, that
      type(bitfield_t) :: b
            
      call allocate_core(b,1,this%n)
      b%a(:) = ieor( this%a, that%a )
      b%a(:) = not(b%a)
   end function

   _PURE_ function b_neqv(this,that) result(b)
      type(bitfield_t), intent(in) :: this, that
      type(bitfield_t) :: b
            
      call allocate_core(b,1,this%n)
      b%a(:) = ieor( this%a, that%a )
   end function
   
   _PURE_ logicalfunction b_equal(this,that) result(v)
      type(bitfield_t), intent(in) :: this, that
            
      integer :: j, j1, ii1, j2, ii2
      integer(ik) :: a1, a2
      
      call indeces( this, this%ub, j1, ii1 )
      call indeces( that, that%ub, j2, ii2 )
      a1 = zeros ; a2 = zeros
      call mvbits( this%a(j1), 0, ii1+1, a1, 0 )
      call mvbits( this%a(j2), 0, ii2+1, a2, 0 )
      v = a1 == a2
      if (.not.v) return
      do j = 0, this%jmax - 1
         v = v .and. this%a(j) == that%a(j)
         if (.not.v) return
      end do
   end function

   _PURE_ logicalfunction b_notequal(this,that) result(v)
      type(bitfield_t), intent(in) :: this, that
            
      integer :: j, j1, ii1, j2, ii2
      integer(ik) :: a1, a2
      
      call indeces( this, this%ub, j1, ii1 )
      call indeces( that, that%ub, j2, ii2 )
      a1 = zeros ; a2 = zeros
      call mvbits( this%a(j1), 0, ii1+1, a1, 0 )
      call mvbits( this%a(j2), 0, ii2+1, a2, 0 )
      v = a1 /= a2
      if (v) return
      do j = 0, this%jmax - 1
         v = v .or. this%a(j) /= that%a(j)
         if (v) return
      end do
   end function
   
   
   
   _PURE_ subroutine indeces(this,i,j,ii)
      type(bitfield_t), intent(in) :: this
      integer, intent(in) :: i
      integer, intent(out) :: j, ii
      
      ii = i - this%lb
      !j = ii/l ; ii = ii - j*l
      j = shiftr(ii,l2l); ii = ii - shiftl(j,l2l)
   end subroutine
   
   _PURE_ subroutine clear_end(this)
      type(bitfield_t), intent(inout) :: this
      
      integer :: ii, iii, j
      
      call indeces(this,this%ub,j,ii)
      do iii = ii+1, l-1
         this%a(j) = ibclr(this%a(j),iii)
      end do
   end subroutine   
   
   _PURE_ subroutine set_end(this)
      type(bitfield_t), intent(inout) :: this
      
      integer :: ii, iii, j
      
      call indeces(this,this%ub,j,ii)
      do iii = ii+1, l-1
         this%a(j) = ibset(this%a(j),iii)
      end do
   end subroutine   
   
   _PURE_ subroutine getiirs(jstart,jstop,iistart,iistop,inc,j,iir,iirs)
      integer, intent(in) :: jstart, jstop, iistart, iistop, inc
      integer, intent(inout) :: j
      integer, intent(inout) :: iir(l), iirs
      
      integer :: ii, delta
      
      if (j > jstart .and. j < jstop) then
         ! more frequent case for large bitsets, the whole chunk is updated
         delta = iirs*inc - l
         if (delta >= 0) then
            iir(1:iirs) = iir(1:iirs) + delta
            if (iir(iirs) > l-1) iirs = iirs-1
         else
            iirs = iirs+1
            iir(1:iirs) = iir(1:iirs) + delta
         end if
         j = j+1
      else if (iirs == 0 .and. jstart == jstop) then
         ! the bitfield is made of a single chunk 
         ii = iistart
         do
            iirs = iirs+1 ; iir(iirs) = ii
            ii = ii + inc ; if (ii > iistop) exit
         end do
      else if (iirs == 0 .and. j == jstart) then
         ! More than one chunk, the first chunk is set (can be incomplete)
         ii = iistart
         do
            iirs = iirs+1 ; iir(iirs) = ii
            ii = ii + inc ; if (ii > l-1) exit
         end do
      else if (j == jstart .and. j < jstop) then
         ! More than two chunks, the second chunk is set (is complete)
         ii = iir(iirs) + inc - l
         iirs = 0
         do
            iirs = iirs+1 ; iir(iirs) = ii
            ii = ii + inc ; if (ii > l-1) exit
         end do
         if (iirs < l) iir(iirs+1) = iir(iirs) + inc
         j = j+1
      else if (iirs > 0 .and. j == jstop) then
         ! More than one chunk, the last chunk is set (can be incomplete)
         ii = iir(iirs) + inc - l
         iirs = 0
         do
            iirs = iirs+1 ; iir(iirs) = ii
            ii = ii + inc ; if (ii > iistop) exit
         end do
      else if (j == jstop) then   
         ! special case, call directly on the last chunk 
         ! inc==1 in this specific case
         iirs = 0
         ii = iistop
         do
            iirs = iirs+1 ; iir(iirs) = ii
            ii = ii - 1 ; if (ii < 0) exit
         end do
      end if
   end subroutine

end module