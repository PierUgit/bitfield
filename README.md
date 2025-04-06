# bitfield v0.2.0

Implementation of a resizable 1-bit logical array, stored in an integer array under 
the hood. Not cpu-efficient at all, but memory efficient.

Limitations:
- 1D only
- size currently limited to a default integer

Beta version

`type(bitfield_t) :: b` holds the resizable array of bits

## basic manipulations

```
logical function bitfield_check()
```
Returns `.true.` if the `integer` type behaves as expected (e.g. that all bits are $0$ when the integer value is $0$).

```
subroutine b%allocate( n [, capacity] )
subroutine b%allocate( lb, ub [, capacity] )
subroutine b%allocate( mold [, capacity] )
subroutine b%allocate( source [, capacity] )
    integer, intent(in) :: n, lb, ub, capacity
    type(bitfield_t) :: mold, source
```
Allocates an array of `n bits (the lower bound is $1$ by default), or of `(ub-lb+1) bits,
or of the same shape as `mold` or as `source`. 
- If `source` is coded, the content of `source` is copied to `b`.
- If `capacity` is coded, `capacity` bits are actually reserved, allowing further 
  increase of the size without having to reallocate and copy the data. If `capacity` is
  smaller than the needed size, it is ignored.

```
subroutine b%deallocate()
```
Deallocates the array of bits. 

```
logical function b%allocated()
```
Returns .true. iif `b`is allocated

```
integer function b%getsize()
integer function b%getcapa()
integer function b%getlb()
integer function b%getub() 
```
Return respectively the size, the capacity, the lower bound, and the upper bound,
of the array.

```
subroutine b%setlb(lb)
subroutine b%setub(ub)
    integer :: lb, ub
```
Reset respectively the lower bound and the upper bound of the array.
- The size is unchanged!


## dynamic size features

```
subroutine b%set_dynamic_capacity( strategy )
    integer, intent(in) :: strategy ! BITFIELD_GROWONLY, BITFIELD_GROWSHRINK
```
The capacity of the array can automatically increase if needed (this is the default
behavior). Optionaly, it can also decrease.
- `strategy=BITFIELD_GROWONLY` (default) : the capacity can only increase
- `strategy=BITFIELD_GROWSHRINK` : the capacity can also decrease

```
subroutine b%resize( lb, ub [, keep])
    integer, intent(in) :: lb, ub
    logical, intent(in) :: keep
```
Dynamically resizes the array by giving new lower and upper bounds. No reallocation is 
needed if the new size is smaller or equal to the current capacity.
- if `keep` is `.true.` (which is the default), the content of the array is preserved in 
  any case (otherwise it can be lost in case of reallocation).
  
```
subroutine b%recap( [capacity], [keep] )
    integer, intent(in) :: capacity
    logical, intent(in) :: keep
```
Sets a new capacity. This will most of time trigger a reallocation.
- if the requested capacity is smaller than the current size, OR if it is not coded, 
  it is internally to the current size
- if `keep` is `.true.` (which is the default), the content of the array is preserved in 
  any case (otherwise it can be lost in case of reallocation).

```
subroutine b%append( c )
subroutine b%append( bool0 )
subroutine b%append( bool1 )
    type(bitfield_t), intent(in) :: c
    logical, intent(in) :: bool0
    logical, intent(in) :: bool1(:)
```
Appends to bit array `b`: the bit array `c`, or the scalar `bool0`, or the array 
`bool1(:)`. The size and capacity of `b` are modified as needed.

```
subroutine b%drop( [k] )
    integer :: k
```
Drop the `k` last elements of the bit array. If `k` is not coded, the last element is 
dropped. The size and capacity of `b` are modified as needed.
 
## defined intrinsic assignements

```
type(bitfield_t) :: b, c

c = b                         ! efficient
```
`c` is a copy of `b`, except the capacity that can differ.

```
type(bitfield_t) :: b
logical :: bool

b = bool0                     ! efficient
```
`b` must be allocated beforehand

```
type(bitfield_t) :: b
logical, allocatable :: bool(:)

b = bool                      ! not efficient
```
`b` gets the shape (including the bounds) and values of `bool(:)`.
- if `b` was allocated, it is first deallocated
- this assignment won't work if `bool(:)` is not an allocatable array
- this assignment is not CPU efficient

```
type(bitfield_t) :: b
logical, allocatable :: bool(:)

bool = b                      ! not efficient
```
`bool(:)` gets the shape (including the bounds) and values of `b`
- if `bool(:)` was allocated, it is first deallocated
- this assignment won't work if `bool(:)` is not an allocatable array
- this assignment is not CPU efficient


## bit manipulations

In all the procedures below, the increment `inc` can be negative
```
subroutine b%set( pos, bool )          ! not efficient
    integer, intent(in) :: pos
    logical, intent(in) :: bool
```
Sets the bit at index `pos` to the value of `bool`

```
subroutine b%set(bool)              ! efficient
subroutine b%set(from,to,inc,bool)  ! efficient
    logical, intent(in) :: bool
    integer :: from, to, inc
```
Sets the whole bit array, or the bits at indeces `from:to:inc`, to the value of `bool`

```
subroutine b%set(bool)              ! efficient
subroutine b%set(from,to,inc,bool)  ! efficient
    logical, intent(in) :: bool(:)
    integer, intent(in) :: from, to, inc
```
Sets the whole bit array, or the bits at indeces `from:to:inc`, to the values of `bool(:)`
- the sizes must conform

```
subroutine b%get(pos,bool)          ! not efficient
logical function b%fget(pos)        ! not efficient
    integer, intent(in) :: pos
    logical, intent(out) :: bool        
```
Gets the value of the bit at index `pos` (either in `bool` or in the function result)

```
subroutine b% get(bool)              ! not efficient
subroutine b% get(from,to,inc,bool)  ! not efficient
function   b%fget()                  ! not efficient
function   b%fget(from,to,inc)       ! not efficient
    integer, intent(in) :: from, to, inc
    logical, intent(out) :: bool(:)
    logical :: fget(:)
```
Gets the values of the whole bit array, or the bits at indeces `from:to:inc`, either in
the argument `bool` or in a function result.
- bool(:) must be allocated beforehand, and the sizes must conform

```
subroutine b% extract(from,to,inc,c)     ! efficient if inc==1
function   b%fextract(from,to,inc)       ! efficient if inc==1
    integer, intent(in) :: from, to, inc
    type(bitfield_t), intent(out) :: c
    type(bitfield_t) :: fextract
```
Extracts the bits at indeces `from:to:inc` to a new bit array
- if `c` is allocated beforehand, it is first deallocated

```
subroutine b%replace(from,to,inc,c)     ! efficient if inc==1
    integer, intent(in) :: from, to, inc
    type(bitfield_t), intent(in) :: c
```
Replaces the bits of `b` at indeces `from:to:inc` from the bits of the `c`.
- the sizes must conform


## logical operations

```
integer function b%count()                 ! efficient
integer function b%count(from,top,inc)     ! efficient if |inc|==1
    integer, intent(in) :: from, to, inc
```
Counts the number of bits equal to ".true." in the whole array, or at indeces `from:to:inc`

```
logical function b%all()                ! efficient
logical function b%all(from,to,inc)     ! efficient if |inc|==1
    integer, intent(in) :: from, to, inc
```
Is `.true.` iif all the bits of the array, or all the bits at indeces `from:to:inc`, are `.true.`

```
logical function b%any()                ! efficient
logical function b%any(from,to,inc)     ! efficient if |inc|==1
    integer, intent(in) :: from, to, inc
```
Is `.true.` iif any bit of the array, or any bits at indeces `from:to:inc`, is `.true.`

```
subroutine b%not()                  ! efficient
subroutine b%not(from,to,inc)       ! efficient if |inc|==1
```
Negates all the bits of the array, or the bits at indeces `from:to:inc`


## overloaded operators

```
type(bitfield_t) :: b, c

c = .not.b                    ! efficient
```
Is equivalent to `c = b; call c%not()`

```
type(bitfield_t) :: b1, b2, c

c = b1 .and.  b2              ! efficient
c = b1 .or.   b2              ! efficient
c = b1 .eqv.  b2              ! efficient
c = b1 .neqv. b2              ! efficient
```
bitwise `and`, `or`, `eqv`, `neqv`

```
type(bitfield_t) :: b1, b2
logical :: bool

bool = ( b1 == b2 )           ! efficient
bool = ( b1 /= b2 )           ! efficient
```
Comparison operators
