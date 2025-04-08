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
module bitfield
use iso_fortran_env
use iso_c_binding
implicit none

   private

   public :: bitfield_t, bitfield_check, bitfield_size
   public :: assignment(=), operator(==), operator(/=)
   public :: operator(.not.), operator(.and.), operator(.or.)
   public :: operator(.eqv.), operator(.neqv.)
   public :: BITFIELD_GROWONLY, BITFIELD_GROWSHRINK

   integer, parameter :: k0 = kind(0)
   integer, parameter :: ik = selected_int_kind(r=18)
   integer, parameter :: sk = c_size_t, bitfield_size = sk
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
      integer(sk) :: n = -1
      integer(sk) :: lb = 1
      integer(sk) :: ub = 0
      integer(sk) :: jmax = -1
      integer :: strat = BITFIELD_GROWONLY
   contains
      private
      procedure :: allocate1_k0 => b_allocate1
      procedure :: allocate1_sk => b_allocate1_sk
      procedure :: allocate2_k0 => b_allocate2
      procedure :: allocate2_sk => b_allocate2_sk
      procedure :: allocate3    => b_allocate3
      generic, public :: allocate => allocate1_k0, allocate1_sk, &
                                     allocate2_k0, allocate2_sk, &
                                     allocate3
      procedure, public :: deallocate => b_deallocate
      procedure, public :: allocated => b_allocated
      
      procedure :: resize_k0 => b_resize
      procedure :: resize_sk => b_resize_sk
      generic, public :: resize => resize_k0, resize_sk
      procedure :: recap1_k0 => b_recap1
      procedure :: recap1_sk => b_recap1_sk
      generic, public :: recap => recap1_k0, recap1_sk
      procedure, public :: set_dynamic_capacity => b_set_dynamic_capacity
      
      procedure :: append_b => b_append_b
      procedure :: append_l0 => b_append_l0
      procedure :: append_l1 => b_append_l1
      generic, public :: append => append_b, append_l0, append_l1
      
      procedure :: drop0 => b_drop0
      procedure :: drop_k0 => b_drop
      procedure :: drop_sk => b_drop_sk
      generic, public :: drop => drop_k0, drop_sk
   
      procedure, public :: getsize => b_getsize
      procedure, public :: getcapacity => b_getcapacity
      procedure, public :: getlb => b_getlb
      procedure, public :: getub => b_getub
      procedure, public :: setlb => b_setlb
      procedure, public :: setub => b_setub
   
      procedure :: set0 => b_set0
      procedure :: set0_sk => b_set0_sk
      procedure :: setall0 => b_setall0
      procedure :: setrange0 => b_setrange0
      procedure :: setrange0_sk => b_setrange0_sk
      procedure :: setall1 => b_setall1
      procedure :: setrange1 => b_setrange1
      procedure :: setrange1_sk => b_setrange1_sk
      generic, public:: set => set0, setall0, setall1, setrange0, setrange1, &
                               set0_sk, setrange0_sk, setrange1_sk
      
      procedure :: get0 => b_get0
      procedure :: get0_sk => b_get0_sk
      procedure :: getall => b_getall
      procedure :: getrange => b_getrange
      procedure :: getrange_sk => b_getrange_sk
      generic, public :: get => get0, getall, getrange, get0_sk, getrange_sk
   
      procedure :: fget0 => b_fget0
      procedure :: fget0_sk => b_fget0_sk
      procedure :: fgetall => b_fgetall
      procedure :: fgetrange => b_fgetrange
      procedure :: fgetrange_sk => b_fgetrange_sk
      generic, public :: fget => fget0, fgetall, fgetrange, fget0_sk, fgetrange_sk
   
      procedure :: countall => b_countall
      procedure :: countrange => b_countrange
      procedure :: countrange_sk => b_countrange_sk
      generic, public :: count => countall, countrange, countrange_sk
   
      procedure :: allall => b_allall
      procedure :: allrange => b_allrange
      procedure :: allrange_sk => b_allrange_sk
      generic, public  :: all => allall, allrange, allrange_sk
      procedure :: anyall => b_anyall
      procedure :: anyrange => b_anyrange
      procedure :: anyrange_sk => b_anyrange_sk
      generic, public :: any => anyall, anyrange, anyrange_sk
   
      procedure :: extract_k0 => b_extract
      procedure :: extract_sk => b_extract_sk
      generic, public :: extract => extract_k0, extract_sk
      procedure :: fextract_k0 => b_fextract
      procedure :: fextract_sk => b_fextract_sk
      generic, public :: fextract => fextract_k0, fextract_sk
      procedure :: replace_k0 => b_replace   
      procedure :: replace_sk => b_replace_sk 
      generic, public :: replace => replace_k0, replace_sk
      
      procedure :: notall => b_notall
      procedure :: notrange_k0 => b_notrange
      procedure :: notrange_sk => b_notrange_sk
      generic, public :: not => notall, notrange_k0, notrange_sk
   end type
   
   type kwe_t
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

   !********************************************************************************************
   logical function bitfield_check() result(stat)
   !********************************************************************************************
      integer :: ii
      
      stat = all( btest( ones, [(ii,ii=0,l-1)] ) )
      stat = stat .and. shiftr(101,1) == 50 .and. shiftl(101,1) == 202
   end function
   


   !********************************************************************************************
   integer(sk) _PURE_ function b_getsize(this)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      
      b_getsize = this%n
   end function 
   
   !********************************************************************************************
   integer(sk) _PURE_ function b_getcapacity(this)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      
      b_getcapacity = size( this%a, kind=sk ) * l
   end function 
   
   !********************************************************************************************
   integer(sk) _PURE_ function b_getlb(this)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      
      b_getlb = this%lb
   end function 

   !********************************************************************************************
   integer(sk) _PURE_ function b_getub(this)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      
      b_getub = this%ub
   end function 

   !********************************************************************************************
   _PURE_ subroutine b_setlb_sk(this,lb)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer(sk), intent(in) :: lb
      
      if (this%n > 0) then
         this%lb = lb
         this%ub = lb + this%n -1
      end if
   end subroutine 

   !********************************************************************************************
   _PURE_ subroutine b_setlb(this,lb)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: lb
      
      call b_setlb_sk( this, int(lb,kind=sk) )
   end subroutine 

   !********************************************************************************************
   _PURE_ subroutine b_setub_sk(this,ub)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer(sk), intent(in) :: ub
      
      if (this%n > 0) then
         this%lb = ub - this%n + 1
         this%ub = ub
      end if
   end subroutine 
   
   !********************************************************************************************
   _PURE_ subroutine b_setub(this,ub)
   !**************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: ub
      
      call b_setub_sk( this, int(ub,kind=sk) )
   end subroutine 



   !**************************************************************************************
   _PURE_ subroutine b_allocate1_sk(this,n,kwe,capacity)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer(sk), intent(in) :: n, capacity
      type(kwe_t) :: kwe
      optional :: kwe, capacity
                  
      if (allocated(this%a)) error stop "b_allocate: bitfield is already allocated"
      
      call allocate_core( this, 1_sk, n )
      if (present(capacity)) call b_recap1_sk( this, kwe, capacity, .false. )      
   end subroutine 

   !********************************************************************************************
   _PURE_ subroutine b_allocate1(this,n,kwe,capacity)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: n, capacity
      type(kwe_t) :: kwe
      optional :: kwe, capacity
                  
      integer(sk), allocatable :: capacity___
      
      if (present(capacity)) capacity___ = capacity
      call b_allocate1_sk( this, int(n,kind=sk), kwe, capacity___ )
   end subroutine 

   !**************************************************************************************
   _PURE_ subroutine b_allocate2_sk(this,lb,ub,kwe,capacity)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer(sk), intent(in) :: lb, ub, capacity
      type(kwe_t) :: kwe
      optional :: kwe, capacity
                  
      if (allocated(this%a)) error stop "b_allocate: bitfield is already allocated"
      
      call allocate_core( this, lb, ub )
      if (present(capacity)) call b_recap1_sk( this, kwe, capacity, .false. )      
   end subroutine 

   !********************************************************************************************
   _PURE_ subroutine b_allocate2(this,lb,ub,kwe,capacity)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: lb, ub, capacity
      type(kwe_t) :: kwe
      optional :: kwe, capacity
                  
      integer(sk), allocatable :: capacity___
      
      if (present(capacity)) capacity___ = capacity
      call b_allocate2_sk( this, int(lb,kind=sk), int(ub,kind=sk), kwe, capacity___ )
   end subroutine 

   !********************************************************************************************
   _PURE_ subroutine b_allocate3(this,kwe,mold,source,capacity)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      type(kwe_t) :: kwe
      integer(sk), intent(in) :: capacity
      type(bitfield_t), intent(in) :: mold, source
      optional :: kwe, mold, source, capacity
      
      integer(sk) :: lb___, ub___
            
      if (allocated(this%a)) error stop "b_allocate: bitfield is already allocated"
      
      if (present(mold)) then
         lb___ = mold%lb
         ub___ = mold%ub
      else if (present(source)) then
         lb___ = source%lb
         ub___ = source%ub
      end if
      call allocate_core( this, lb___, ub___ )
      if (present(capacity)) call b_recap1_sk( this, kwe, capacity, .false. )
      if (present(source)) this%a(0:source%jmax) = source%a(0:source%jmax)
      
   end subroutine 

   !********************************************************************************************
   _PURE_ subroutine allocate_core(this,lb,ub)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer(sk), intent(in) :: lb, ub
            
      if (ub >= lb) then
         this%n = ub - lb + 1 
         this%lb = lb
         this%ub = ub
         allocate( this%a(0:(this%n-1)/l) )
         this%jmax = ubound( this%a, dim=1, kind=sk )
      else
         this%n = 0 
         allocate( this%a(0:0) )
      end if
      
   end subroutine 

   !********************************************************************************************
   _PURE_ subroutine b_deallocate(this)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
            
      call check_alloc( this, "b_deallocate" )
      
      deallocate( this%a )
      this%n = -1
      this%lb = 1
      this%ub = 0
      this%jmax = -1
      this%strat = BITFIELD_GROWONLY
   end subroutine 
   
   !********************************************************************************************
   _PURE_ logical function b_allocated(this)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
            
      b_allocated = allocated( this%a )
   end function 
   
   
   
   !********************************************************************************************
   _PURE_ subroutine b_set_dynamic_capacity(this,strat)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: strat
      
      this%strat = strat
   end subroutine
   
   !********************************************************************************************
   _PURE_ subroutine b_resize_sk(this,lb,ub,kwe,keep)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer(sk), intent(in) :: lb, ub
      type(kwe_t) :: kwe
      logical, intent(in) :: keep
      optional :: kwe, keep
      
      integer(sk) :: n, newcap

      call check_alloc( this, "b_resize_sk" )

      n = ub - lb + 1
      if (n > size(this%a,kind=sk) * l) then
         newcap = 2 * size(this%a,kind=sk) * l
         do while (n > newcap)
            newcap = 2*newcap
         end do
         call b_recap1_sk( this, kwe, newcap, keep )
      else if (3*n <= size(this%a,kind=sk) * l .and. this%strat == BITFIELD_GROWSHRINK) then
         newcap = size(this%a,kind=sk) * l / 2
         do while (3*n <= newcap)
            newcap = newcap / 2
         end do
         call b_recap1_sk( this, kwe, newcap, keep )
      end if
      this%n = n
      this%lb = lb
      this%ub = ub
      this%jmax = (this%n-1) / l   
   end subroutine
   
   !********************************************************************************************
   _PURE_ subroutine b_resize(this,lb,ub,kwe,keep)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: lb, ub
      type(kwe_t) :: kwe
      logical, intent(in) :: keep
      optional :: kwe, keep
      
      call b_resize_sk( this, int(lb,kind=sk), int(ub,kind=sk), kwe, keep )  
   end subroutine

   !********************************************************************************************
   _PURE_ subroutine b_recap1_sk(this,kwe,capacity,keep)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      type(kwe_t) :: kwe
      integer(sk), intent(in) :: capacity
      logical, intent(in) :: keep
      optional :: kwe, capacity, keep

      integer(sk) :: capacity___
      logical :: keep___

      capacity___ = 0; if (present(capacity)) capacity___ = capacity
      keep = .true.; if (present(keep)) keep___ = keep

      call b_recap_core( this, capacity___, keep___ )
   end subroutine
   
   !********************************************************************************************
   _PURE_ subroutine b_recap1(this,kwe,capacity,keep)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      type(kwe_t) :: kwe
      integer, intent(in) :: capacity
      logical, intent(in) :: keep
      optional :: capacity, kwe, keep
      
      integer(sk), allocatable :: capacity___
      logical, allocatable :: keep___

      if (present(capacity)) capacity___ = capacity
      if (present(keep)) keep___ = keep

      call b_recap1_sk( this, kwe, capacity___, keep___ )
   end subroutine
  
   !********************************************************************************************
   _PURE_ subroutine b_recap_core(this,capacity,keep)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer(sk), intent(in) :: capacity
      logical, intent(in) :: keep
      
      integer(sk) :: newcap
      integer(ik), allocatable :: atmp(:)
      
      call check_alloc( this, "b_recap_core" )

      newcap = max( capacity, this%n, l )
      newcap = ((newcap-1)/l+1) * l
      if (newcap /= size(this%a,kind=sk)*l) then
         allocate( atmp(0:(newcap-1)/l) )
         if (keep___) atmp(0:this%jmax) = this%a(0:this%jmax)
         call move_alloc( atmp, this%a )
      end if
   end subroutine
         
            

   !********************************************************************************************
   _PURE_ subroutine b_append_b(this,that)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      type(bitfield_t), intent(in) :: that
      
      integer(sk) :: ub

#ifdef DEBUG   
      call check_alloc( this, "b_append_b" )
      call check_alloc( that, "b_append_b" )
#endif
      ub = this%ub
      call b_resize_sk( this, this%lb, this%ub+that%n, .true. )
      call b_replace_sk( this, ub+1, this%n, 1_sk, that )
   end subroutine
         
   _PURE_ subroutine b_append_l0(this,v)
      class(bitfield_t), intent(inout) :: this
      logical, intent(in) :: v
            
#ifdef DEBUG   
      call check_alloc( this, "b_append_l0" )
#endif
      call b_resize_sk( this, this%lb, this%ub+1, .true. )
      call b_set0_sk( this, this%n, v )
   end subroutine
   
   !********************************************************************************************
   _PURE_ subroutine b_append_l1(this,v)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      logical, intent(in) :: v(:)
                  
      integer(sk) :: ub

#ifdef DEBUG   
      call check_alloc( this, "b_append_l1" )
#endif
      ub = this%ub
      call b_resize_sk( this, this%lb, this%ub+size(v,kind=sk), .true. )
      call b_setrange1_sk( this, ub+1, this%n, 1_sk, v )
   end subroutine

   !********************************************************************************************
    _PURE_ subroutine b_drop0(this)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      
#ifdef DEBUG   
      call check_alloc( this, "b_drop0" )
#endif
      call b_resize_sk( this, this%lb, max(this%ub-1,0), .true. )                  
   end subroutine

   !********************************************************************************************
  _PURE_ subroutine b_drop_sk(this,k)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer(sk), intent(in) :: k
      
#ifdef DEBUG   
      call check_alloc( this, "b_drop_sk" )
#endif
      call b_resize_sk( this, this%lb, max(this%ub-k,0), .true. )
   end subroutine
      
   !********************************************************************************************
   _PURE_ subroutine b_drop(this,k)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: k
      
#ifdef DEBUG   
      call check_alloc( this, "b_drop" )
#endif
      call b_resize_sk( this, this%lb, max(this%ub-k,0), .true. )
                  
   end subroutine
 

   
   !********************************************************************************************
   _PURE_ subroutine assign_b2b(this,that)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      type(bitfield_t), intent(inout) :: that
      
      if (b_allocated(this) .and. this%n /= that%n) &
         call b_deallocate(this)
      if (.not.b_allocated(this)) &
         call allocate_core( this, that%lb, that%ub )
      this%a(:) = that%a(0:that%jmax)
   end subroutine 
   
   !********************************************************************************************
   _PURE_ subroutine assign_l2b_0(this,v)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      logical, intent(in) :: v
      
      call b_setall0(this,v)
   end subroutine 
   
   !********************************************************************************************
   _PURE_ subroutine assign_l2b_1(this,v)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      logical, allocatable, intent(in) :: v(:)
      
      if (b_allocated(this) .and. this%getsize() /= size(v,kind=sk)) &
         call b_deallocate(this)
      if (.not.b_allocated(this)) &
         call allocate_core(this,lbound(v,dim=1,kind=sk),ubound(v,dim=1,kind=sk))
      call b_setall1(this,v)
   end subroutine 

   !********************************************************************************************
   _PURE_ subroutine assign_b2l(v,this)
   !********************************************************************************************
      logical, allocatable, intent(out) :: v(:)
      type(bitfield_t), intent(in) :: this
      
      if (allocated(v) .and. this%getsize() /= size(v,kind=sk)) deallocate(v)
      if (.not.allocated(v)) allocate( v(this%lb:this%ub) )
      call b_getall(this,v)
   end subroutine 

   

   !********************************************************************************************
   _PURE_ subroutine b_set0_sk(this,i,v)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer(sk), intent(in) :: i
      logical, intent(in) :: v
      
      integer :: ii
      integer(sk) :: j
      
#ifdef DEBUG   
      call check_alloc( this, "b_set0_sk" )
      call check_1index( this, i, "b_set0_sk" )
#endif
      call indeces(this,i,j,ii)
      if (v) then
         this%a(j) = ibset(this%a(j),ii)
      else
         this%a(j) = ibclr(this%a(j),ii)
      end if
   end subroutine 

   !********************************************************************************************
   _PURE_ subroutine b_set0(this,i,v)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: i
      logical, intent(in) :: v
      
      call b_set0_sk( this, int(i,kind=sk), v )
   end subroutine 

   !********************************************************************************************
   _PURE_ subroutine b_setall0(this,v)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      logical, intent(in) :: v
      
#ifdef DEBUG   
      call check_alloc( this, "b_setall0" )
#endif
      this%a(:) = merge(ones,zeros,v)
   end subroutine 

   !********************************************************************************************
   _PURE_ recursive subroutine b_setrange0_sk(this,istart,istop,inc,v)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer(sk), intent(in) :: istart, istop, inc
      logical, intent(in) :: v
      
      integer(ik) :: a
      integer :: iistart, iistop, k
      integer(sk) :: jstart, jstop, j, i
      integer :: iir(l), iirs
      
      if (inc < 0) then
         call b_setrange0_sk(this,istop+mod(istart-istop,-inc),istart,-inc,v)
         return
      end if
      
#ifdef DEBUG   
      call check_alloc( this, "b_setrange0_sk" )
#endif
      if (this%n == 0 .or. istop < istart) return
#ifdef DEBUG   
      call check_3index( this, istart, istop, inc, "b_setrange0_sk" )
#endif
      
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
            call b_set0_sk(this,i,v)
         end do
      end if
   end subroutine 

   !********************************************************************************************
   _PURE_ subroutine b_setrange0(this,istart,istop,inc,v)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: istart, istop, inc
      logical, intent(in) :: v
      
      call b_setrange0_sk( this, int(istart,kind=sk), int(istop,kind=sk) &
                         , int(inc,kind=sk),v)
   end subroutine 
   
   !********************************************************************************************
   _PURE_ subroutine b_setall1(this,v)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      logical, intent(in) :: v(:)
      
      call b_setrange1_sk( this, this%lb, this%ub, 1_sk, v )
   end subroutine 

   _PURE_ subroutine b_setrange1_sk(this,istart,istop,inc,v)
      class(bitfield_t), intent(inout) :: this
      integer(sk), intent(in) :: istart, istop, inc
      logical, intent(in) :: v(:)
      
      integer :: iistart, iistop
      integer(sk) :: j, i, jstart, jstop, iv
      integer :: iir(l), iirs
      integer(ik) :: a
      
#ifdef DEBUG   
      call check_alloc( this, "b_setrange1_sk" )
#endif
      if (this%n == 0) return
      if (sign(1_sk,istop-istart)*sign(1_sk,inc) < 0) return
#ifdef DEBUG   
      call check_4index( this, istart, istop, inc, size(v,kind=sk), "b_setrange1_sk" )
#endif
      
      iv = 0
      do i = istart, istop, inc
         iv = iv+1
         call b_set0_sk( this, i, v(iv) )
      end do
   end subroutine 

   !********************************************************************************************
   _PURE_ subroutine b_setrange1(this,istart,istop,inc,v)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: istart, istop, inc
      logical, intent(in) :: v(:)
      
      call b_setrange1_sk( this, int(istart,kind=sk), int(istop,kind=sk) &
                         , int(inc,kind=sk), v )
   end subroutine 

   

   !********************************************************************************************
   _PURE_ subroutine b_get0_sk(this,i,v)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      integer(sk), intent(in) :: i
      logical, intent(out) :: v
      
      integer :: ii
      integer(sk) :: j
      
#ifdef DEBUG   
      call check_alloc( this, "b_get0_sk" )
      call check_1index( this, i, "b_get0_sk" )
#endif
      call indeces(this,i,j,ii)
      v = btest(this%a(j),ii)
   end subroutine 
   
   !********************************************************************************************
   _PURE_ subroutine b_get0(this,i,v)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: i
      logical, intent(out) :: v
      
      call b_get0_sk( this, int(i,kind=sk), v )
   end subroutine 

   !********************************************************************************************
   _PURE_ subroutine b_getall(this,v)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      logical, intent(out) :: v(:)
      
      call b_getrange_sk( this, this%lb, this%ub, 1_sk, v )
   end subroutine 
   
   !********************************************************************************************
   _PURE_ subroutine b_getrange_sk(this,istart,istop,inc,v)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      integer(sk), intent(in) :: istart, istop, inc
      logical, intent(out) :: v(:)
      
      integer :: iistart, iistop
      integer(sk) :: i, i1, i2, iv, j, jstart, jstop
      integer :: iir(l), iirs
      
#ifdef DEBUG
      call check_alloc( this, "b_getrange_sk" )
#endif
      if (this%n == 0) return
      if (sign(1_sk,istop-istart)*sign(1_sk,inc) < 0) return
#ifdef DEBUG   
      call check_4index( this, istart, istop, inc, size(v,kind=sk), "b_getrange_sk" ) 
#endif

      if (0 < inc .and. inc <= l/minbatch) then
         call indeces( this, istart, jstart, iistart)
         call indeces( this, istop , jstop , iistop )
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
         call indeces( this, istart,                         jstart, iistart )
         call indeces( this, istop + mod(istart-istop,-inc), jstop,  iistop  )
         j = jstop
         i1 = size(v,kind=sk)
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
            call b_get0_sk(this,i,v(iv))
         end do
      end if
   end subroutine 
         
   !********************************************************************************************
   _PURE_ subroutine b_getrange(this,istart,istop,inc,v)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      logical, intent(out) :: v(:)
      
      call b_getrange_sk( this, int(istart,kind=sk), int(istop,kind=sk) &
                        , int(inc,kind=sk), v )
   end subroutine 

   !********************************************************************************************
   _PURE_ function b_fget0_sk(this,i) result(v)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      integer(sk), intent(in) :: i
      logical :: v
      
      call b_get0_sk(this,i,v)
   end function 

   !********************************************************************************************
   _PURE_ function b_fget0(this,i) result(v)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: i
      logical :: v
      
      call b_get0(this,i,v)
   end function 

   !********************************************************************************************
   _PURE_ function b_fgetall(this) result(v)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      logical, allocatable:: v(:)
      
      allocate( v(this%lb:this%ub) )
      call b_getall(this,v)
   end function 

   !********************************************************************************************
   _PURE_ function b_fgetrange_sk(this,istart,istop,inc) result(v)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      integer(sk), intent(in) :: istart, istop, inc
      logical, allocatable :: v(:)
      
      integer(sk) :: n
      
      n = abs((istop-istart)/inc+1)
      allocate( v(n) )
      call b_getrange_sk( this, istart, istop, inc, v )   
   end function

   !********************************************************************************************
   _PURE_ function b_fgetrange(this,istart,istop,inc) result(v)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      logical, allocatable :: v(:)
      
      integer :: n
      
      n = abs((istop-istart)/inc+1)
      allocate( v(n) )
      call b_getrange(this,istart,istop,inc,v)   
   end function


   
   !********************************************************************************************
   _PURE_ subroutine b_replace_sk(this,istart,istop,inc,that)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer(sk), intent(in) :: istart, istop, inc
      type(bitfield_t), intent(in) :: that
      
      integer :: iistart, iistop, iisource
      integer(sk) :: i, j, jstart, jstop, jsource, isource
      integer :: iir(l), iirs
      
#ifdef DEBUG
      call check_alloc( this, "b_replace_sk" )
#endif
      if (this%n <= 0) return
      if (sign(1_sk,istop-istart)*sign(1_sk,inc) < 0) return
#ifdef DEBUG
      call check_4index( this, istart, istop, inc, that%n, "b_replace_sk" )
#endif
      
      call indeces(this,istart,jstart,iistart)
      call indeces(this,istop ,jstop ,iistop)
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
            call b_set0_sk( this, i, b_fget0_sk(that,isource) )
         end do      
      end if
   end subroutine 

   !********************************************************************************************
   _PURE_ subroutine b_replace(this,istart,istop,inc,that)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: istart, istop, inc
      type(bitfield_t), intent(in) :: that
      
      call b_replace_sk( this, int(istart,kind=sk), int(istop,kind=sk) &
                       , int(inc,kind=sk), that )
   end subroutine 



   !********************************************************************************************
   _PURE_ subroutine b_extract_sk(this,istart,istop,inc,that)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      integer(sk), intent(in) :: istart, istop, inc
      type(bitfield_t), intent(inout) :: that
      
      integer :: iistart, iistop, iidest
      integer(sk) :: i, jstart, jstop, j, jdest, n, idest
      integer :: iir(l), iirs
      logical :: v(l)
      
#ifdef DEBUG
      call check_alloc( this, "b_extract_sk" )
#endif
      if (this%n <= 0) return
      if (sign(1_sk,istop-istart)*sign(1_sk,inc) < 0) return
#ifdef DEBUG
      call check_3index( this, istart, istop, inc, "b_extract_sk" )
#endif
      if (b_allocated(that)) call b_deallocate( that )
         
      n = (istop-istart)/inc + 1
      if (n <= 0) then
         call allocate_core(that,1_sk,0_sk)
         return
      end if
      
      call allocate_core(that,1_sk,n)
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
            call b_set0_sk( that, idest, b_fget0_sk(this,i) )
         end do
      end if
   end subroutine 

   !********************************************************************************************
   _PURE_ subroutine b_extract(this,istart,istop,inc,that)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      type(bitfield_t), intent(inout) :: that
      
      call b_extract_sk( this, int(istart,kind=sk), int(istop,kind=sk) &
                       , int(inc,kind=sk), that )
   end subroutine 

   _PURE_ function b_fextract_sk(this,istart,istop,inc) result(that)
      class(bitfield_t), intent(in) :: this
      integer(sk), intent(in) :: istart, istop, inc
      type(bitfield_t) :: that
      
      call b_extract_sk(this,istart,istop,inc,that)
   end function
      
   !********************************************************************************************
   _PURE_ function b_fextract(this,istart,istop,inc) result(that)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      type(bitfield_t) :: that
      
      call b_extract(this,istart,istop,inc,that)
   end function
   

   !********************************************************************************************
   _PURE_ logical function b_allall(this)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      
      b_allall = b_allrange_sk( this, this%lb, this%ub, 1_sk )
   end function 

   !********************************************************************************************
   _PURE_ recursive logical function b_allrange_sk(this,istart,istop,inc) result(v)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      integer(sk), intent(in) :: istart, istop, inc
      
      integer(sk) :: kstart, kstop
      type(bitfield_t) :: bb
   
      if (inc < 0) then
         v = b_allrange_sk(this,istop+mod(istart-istop,-inc),istart,-inc)
      else
         v = .true.
#ifdef DEBUG   
         call check_alloc( this, "b_allrange_sk" )
#endif
         if (this%n == 0 ) return
         if ((istop-istart)*inc < 0) return
#ifdef DEBUG   
         call check_3index( this, istart, istop, inc, "b_allrange_sk" )
#endif
         kstart = istart
         do while (kstart <= istop)
            kstop = min( kstart + (ll-1)*inc, istop )
            call b_extract_sk( this, kstart, kstop, inc, bb )
            call set_end( bb )
            v = v .and. all( bb%a == ones )
            if (.not.v) return
            kstart = kstop + inc
         end do         
      end if      

   end function 

   !********************************************************************************************
   _PURE_ recursive logical function b_allrange(this,istart,istop,inc) result(v)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      
      v = b_allrange_sk( this, int(istart,kind=sk), int(istop,kind=sk), int(inc,kind=sk) )
   end function 


   !********************************************************************************************
   _PURE_ logical function b_anyall(this)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      
      b_anyall = b_anyrange_sk( this, this%lb, this%ub, 1_sk )
   end function 

   !********************************************************************************************
   _PURE_ recursive logical function b_anyrange_sk(this,istart,istop,inc) result(v)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      integer(sk), intent(in) :: istart, istop, inc
      
      integer(sk) :: kstart, kstop
      type(bitfield_t) :: bb
   
      if (inc < 0) then
         v = b_anyrange_sk(this,istop+mod(istart-istop,-inc),istart,-inc)
      else
         v = .false.
#ifdef DEBUG   
         call check_alloc( this, "b_anyrange_sk" )
#endif
         if (this%n == 0 ) return
         if ((istop-istart)*inc < 0) return
#ifdef DEBUG   
         call check_3index( this, istart, istop, inc, "b_anyrange_sk" )
#endif
         kstart = istart
         do while (kstart <= istop)
            kstop = min( kstart + (ll-1)*inc, istop )
            call b_extract_sk( this, kstart, kstop, inc, bb )
            call clear_end( bb )
            v = v .or. any( bb%a /= zeros )
            if (v) return
            kstart = kstop + inc
         end do         
      end if      

   end function 

   !********************************************************************************************
   _PURE_ recursive logical function b_anyrange(this,istart,istop,inc) result(v)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      
      v = b_anyrange_sk( this, int(istart,kind=sk), int(istop,kind=sk), int(inc,kind=sk) )
   end function 



   !********************************************************************************************
   _PURE_ integer function b_countall(this) result(v)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      
      v = b_countrange_sk( this, this%lb, this%ub, 1_sk )  
   end function 

   !********************************************************************************************
   _PURE_ recursive integer function b_countrange_sk(this,istart,istop,inc) result(v)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      integer(sk), intent(in) :: istart, istop, inc
      
      integer(sk) :: kstart, kstop
      type(bitfield_t) :: bb
   
      if (inc < 0) then
         v = b_countrange_sk(this,istop+mod(istart-istop,-inc),istart,-inc)
      else
         v = 0
#ifdef DEBUG   
         call check_alloc( this, "b_countrange_sk" )
#endif
         if (this%n == 0 ) return
         if ((istop-istart)*inc < 0) return
#ifdef DEBUG   
         call check_3index( this, istart, istop, inc, "b_countrange_sk" )
#endif
         kstart = istart
         do while (kstart <= istop)
            kstop = min( kstart + (ll-1)*inc, istop )
            call b_extract_sk( this, kstart, kstop, inc, bb )
            call clear_end( bb )
            v = v + sum( popcnt( bb%a ) )
            kstart = kstop + inc
         end do         
      end if      

   end function 
   
   !********************************************************************************************
   _PURE_ recursive integer function b_countrange(this,istart,istop,inc) result(v)
   !********************************************************************************************
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      
      v = b_countrange_sk( this, int(istart,kind=sk), int(istop,kind=sk), int(inc,kind=sk) )
   end function 


   
   !********************************************************************************************
   _PURE_ subroutine b_notall(this)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
            
      this%a(:) = not( this%a )
   end subroutine
   
   !********************************************************************************************
   _PURE_ recursive subroutine b_notrange_sk(this,istart,istop,inc)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer(sk), intent(in) :: istart, istop, inc
      
      integer(sk) :: kstart, kstop
      type(bitfield_t) :: bb
   
      if (inc < 0) then
         call b_notrange_sk(this,istop+mod(istart-istop,-inc),istart,-inc)
      else
#ifdef DEBUG   
         call check_alloc( this, "b_notrange_sk" )
#endif
         if (this%n == 0 ) return
         if ((istop-istart)*inc < 0) return
#ifdef DEBUG   
         call check_3index( this, istart, istop, inc, "b_notrange_sk" )
#endif
         kstart = istart
         do while (kstart <= istop)
            kstop = min( kstart + (ll-1)*inc, istop )
            call b_extract_sk( this, kstart, kstop, inc, bb )
            bb%a(:) = not( bb%a )
            call b_replace_sk( this, kstart, kstop, inc, bb )
            kstart = kstop + inc
         end do         
      end if      

   end subroutine 

   !********************************************************************************************
   _PURE_ recursive subroutine b_notrange(this,istart,istop,inc)
   !********************************************************************************************
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: istart, istop, inc
      
      call b_notrange_sk( this, int(istart,kind=sk), int(istop,kind=sk), int(inc,kind=sk) )  
   end subroutine 

   !********************************************************************************************
   _PURE_ function b_fnotall(this) result(b)
   !********************************************************************************************
      type(bitfield_t), intent(in) :: this
      type(bitfield_t) :: b
            
      call allocate_core(b,1_sk,this%n)
      b%a(:) = not( this%a )
   end function
   
   !********************************************************************************************
   _PURE_ function b_and(this,that) result(b)
   !********************************************************************************************
      type(bitfield_t), intent(in) :: this, that
      type(bitfield_t) :: b
            
      call allocate_core(b,1_sk,this%n)
      b%a(:) = iand( this%a, that%a )
   end function
   
   !********************************************************************************************
   _PURE_ function b_or(this,that) result(b)
   !********************************************************************************************
      type(bitfield_t), intent(in) :: this, that
      type(bitfield_t) :: b
            
      call allocate_core(b,1_sk,this%n)
      b%a(:) = ior( this%a, that%a )
   end function
   
   !********************************************************************************************
   _PURE_ function b_eqv(this,that) result(b)
   !********************************************************************************************
      type(bitfield_t), intent(in) :: this, that
      type(bitfield_t) :: b
            
      call allocate_core(b,1_sk,this%n)
      b%a(:) = ieor( this%a, that%a )
      b%a(:) = not(b%a)
   end function

   !********************************************************************************************
   _PURE_ function b_neqv(this,that) result(b)
   !********************************************************************************************
      type(bitfield_t), intent(in) :: this, that
      type(bitfield_t) :: b
            
      call allocate_core(b,1_sk,this%n)
      b%a(:) = ieor( this%a, that%a )
   end function
   
   !********************************************************************************************
   _PURE_ logicalfunction b_equal(this,that) result(v)
   !********************************************************************************************
      type(bitfield_t), intent(in) :: this, that
            
      integer :: ii1, ii2
      integer(sk) :: j, j1, j2
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

   !********************************************************************************************
   _PURE_ logicalfunction b_notequal(this,that) result(v)
   !********************************************************************************************
      type(bitfield_t), intent(in) :: this, that
            
      integer :: ii1, ii2
      integer(sk) :: j, j1, j2
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
   
   
   
   !********************************************************************************************
   _PURE_ subroutine indeces(this,i,j,ii)
   !********************************************************************************************
      type(bitfield_t), intent(in) :: this
      integer(sk), intent(in) :: i
      integer(sk), intent(out) :: j
      integer, intent(out) :: ii
      
      integer(sk) :: ii___
      
      ii___ = i - this%lb
      !j = ii/l ; ii = ii - j*l
      j = shiftr(ii___,l2l); ii = ii___ - shiftl(j,l2l)
   end subroutine
   
   !********************************************************************************************
   _PURE_ subroutine clear_end(this)
   !********************************************************************************************
      type(bitfield_t), intent(inout) :: this
      
      integer :: ii, iii
      integer(sk) :: j
      
      call indeces(this,this%ub,j,ii)
      do iii = ii+1, l-1
         this%a(j) = ibclr(this%a(j),iii)
      end do
   end subroutine   
   
   !********************************************************************************************
   _PURE_ subroutine set_end(this)
   !********************************************************************************************
      type(bitfield_t), intent(inout) :: this
      
      integer :: ii, iii
      integer(sk) :: j
      
      call indeces(this,this%ub,j,ii)
      do iii = ii+1, l-1
         this%a(j) = ibset(this%a(j),iii)
      end do
   end subroutine   
   
   !********************************************************************************************
   _PURE_ subroutine getiirs(jstart,jstop,iistart,iistop,inc,j,iir,iirs)
   !********************************************************************************************
      integer(sk), intent(in) :: jstart, jstop, inc
      integer, intent(in) :: iistart, iistop
      integer(sk), intent(inout) :: j
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
   
   !********************************************************************************************
   _PURE_ subroutine check_alloc( this, name )
   !********************************************************************************************
      type(bitfield_t), intent(in) :: this
      character(*), intent(in) :: name
      
      if (.not.b_allocated( this )) then
         print*, "*** In " // name // "():"
         error stop "bitfield not allocated"
      end if
   end subroutine

   !********************************************************************************************
   _PURE_ subroutine check_1index( this, i, name )
   !********************************************************************************************
      type(bitfield_t), intent(in) :: this
      integer(sk), intent(in) :: i
      character(*), intent(in) :: name
      
      if (i < this%lb .or. i > this%ub) then
         print*, "*** In " // name // "(): "
         error stop "out of bound index"
      end if
   end subroutine

   !********************************************************************************************
   _PURE_ subroutine check_3index(this,istart,istop,inc,name)
   !********************************************************************************************
      type(bitfield_t), intent(in) :: this
      integer(sk), intent(in) :: istart, istop, inc
      character(*), intent(in) :: name
         
      call check_1index( this, istart, name )
      call check_1index( this, istop,  name )
      if (inc == 0) then
         print*, "*** In " // name // "(): "
         error stop "inc is equal to 0"
      end if
   end subroutine

   !********************************************************************************************
   _PURE_ subroutine check_4index(this,istart,istop,inc,s,name)
   !********************************************************************************************
      type(bitfield_t), intent(in) :: this
      integer(sk), intent(in) :: istart, istop, inc, s
      character(*), intent(in) :: name
         
      call check_3index( this, istart, istop, inc, name )
      if ( (istop-istart)/inc+1 /= s ) then
         print*, "*** In " // name // "(): "
         error stop "the sizes differ" 
      end if
   end subroutine
         
end module
