# bitfield v0.5.2

Beta version

Implementation of a 1-bit logical pool, stored in an integer array under 
the hood. Not cpu-efficient at all, but memory efficient. Main features:
- Dynamically resizable array
- Many operations can be applied on array sections, with a stride.
- built-in multithreading

## Limitations

1D only


## API

`type(bitfield_t) :: b` holds a resizable array of bits.

An array of bits is characterised by a size (number of bits). The lower bound is 1 by default,
but it can also be any arbitrary integer value.

The interfaces of the procedures below are written for both the default integer and the
`integer(kind=bitfield_size_t)` (which is the same as `integer(kind=c_size_t)`). In the (unlikely on current machines) case where `bitfield_size_t` is the default integer kind, 
the code must be compiled with the `NO_DEFAULT_INT` macro to avoid ambiguous interfaces
and compilation errors.

In the descriptions below one uses `integer, parameter :: sk = bitfield_size_t`.

Many of the procedures below can operate on array sections with some stride `istart:istop:inc`.
- All operations with strides different from 1 or -1 are not efficient
- Some operations are efficient with strides equal 1 or -1
- Some operations are efficient only with strides equal to 1

**Thread-safety**
- The procedures are thread-safe, but operating on the same bitfield from different threads is **not thread-safe**.
- However, some of the inefficient operations can be optionally run on several threads,
  by coding the optional parameter `nth=<value>` (if `0` is coded, the number of
  threads is the default one).

**All optional arguments MUST be coded with a keyword (`keyword=value`).**

### basic manipulations

```
logical function bitfield_check()
```
Returns `.true.` if the `integer` type behaves as expected (e.g. that all bits are $0$ when the integer value is $0$).

```
subroutine b%allocate( n [, source] [, capacity] )
subroutine b%allocate( lb, ub [, source] [, capacity] )
    integer[(sk)], intent(in) :: n, lb, ub, capacity
    logical, intent(in) :: source
subroutine b%allocate( [mold] [, capacity] )
subroutine b%allocate( [source] [, capacity] )
    integer[(sk)], intent(in) :: capacity
    type(bitfield_t) :: mold, source
```
Allocates a (hidden) pool of `n`, or of `(ub-lb+1)` bits, or of the same shape 
as `mold` or as `source`. 
- If `source` is coded, the content of `source` is copied to `b`.
- If `capacity` is coded, `capacity` bits are actually reserved, allowing further 
  increase of the size without having to reallocate and copy the bits. If `capacity` is
  smaller than the needed size, it is ignored.

```
subroutine b%deallocate()
```
Deallocates the pool of bits. 

```
logical function b%allocated()
```
Returns .true. iif `b` has an allocated pool of bits

```
integer(sk) function b%getsize()
integer(sk) function b%getcapa()
integer(sk) function b%getlb()
integer(sk) function b%getub() 
```
Return respectively the size, the capacity, the lower bound, and the upper bound,
of the array.

```
subroutine b%setlb(lb)
subroutine b%setub(ub)
    integer[(sk)] :: lb, ub
```
Reset respectively the lower bound and the upper bound of the array.
- The size is unchanged!


### dynamic size features

```
subroutine b%set_dynamic_capacity( strategy )
    integer, intent(in) :: strategy ! BITFIELD_GROWONLY, BITFIELD_GROWSHRINK
```
The capacity of the array can automatically increase if needed (this is the default
behavior). Optionaly, it can also decrease.
- `strategy=BITFIELD_GROWONLY` (default) : the capacity can only increase
- `strategy=BITFIELD_GROWSHRINK` : the capacity can also decrease

```
subroutine b%resize( n [, keep])
    integer[(sk)], intent(in) :: n
    logical, intent(in) :: keep
```
Dynamically resizes the pool of bits by giving new lower and upper bounds. 
No reallocation is needed if the new size is smaller or equal to the current capacity.
- the lower bound is preserved (unless `n=0`, in which case the lower bound is reset to 1)
- if `keep` is `.true.` (which is the default), the content of the array is preserved in 
  any case (otherwise it can be lost in case of reallocation).
  
```
subroutine b%recap( [capacity], [keep] )
    integer[(sk)], intent(in) :: capacity
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
    integer[(sk)] :: k
```
Drop the `k` last elements of the bit array. If `k` is not coded, the last element is 
dropped. The size and capacity of `b` are modified as needed.
 
### defined intrinsic assignements

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
`b%allocated()` must be equal to `.true.` beforehand

```
type(bitfield_t) :: b
logical, allocatable :: bool(:)

b = bool     ! (1)     not efficient
bool = b     ! (2)     not efficient
```
(1) The pool of bits in `b` gets the shape (including the bounds) and values of `bool(:)`.
- if `b%allocated()` is `.true.` beforehand and if the sizes differ, then 
  the pool of bits is first deallocated  

(2) `bool(:)` gets the shape (including the bounds) and values of the pool of bits in `b`
- if `bool(:)` is allocated beforehand and if the sizes differ, then it is first 
  deallocated
  
(1) & (2)
- these assignments won't work if `bool(:)` is not an allocatable array
- these assignments are not CPU efficient


### bit manipulations

In all the procedures below, the increment `inc` can be negative
```
subroutine b%set( i, bool )          ! not efficient
    integer[(sk)], intent(in) :: i
    logical, intent(in) :: bool
```
Sets the bit at index `i` to the value of `bool`

```
subroutine b%set(bool)                              ! efficient
subroutine b%set(istart,istop,inc,bool,[nth])  ! efficient if |inc|==1
    logical, intent(in) :: bool
    integer[(sk)], intent(in) :: istart, istop, inc
    integer, intent(in) :: nth
```
Sets the whole bit array, or the bits at indeces `istart:istop:inc`, to the value of `bool`

```
subroutine b%set(bool,[nth])                   ! not efficient
subroutine b%set(istart,istop,inc,bool,[nth])  ! not efficient
    logical, intent(in) :: bool(:)
    integer[(sk)], intent(in) :: istart, istop, inc
    integer, intent(in) :: nth
```
Sets the whole bit array, or the bits at indeces `istart:istop:inc`, to the values of `bool(:)`
- the sizes must conform

```
subroutine b%get(i,bool)          ! not efficient
logical function b%fget(i)        ! not efficient
    integer[(sk)], intent(in) :: i
    logical, intent(out) :: bool        
```
Gets the value of the bit at index `i` (either in `bool` or in the function result)

```
subroutine b% get(bool,[nth])                   ! not efficient
subroutine b% get(istart,istop,inc,bool,[nth])  ! not efficient
function   b%fget([nth])                        ! not efficient
function   b%fget(istart,istop,inc,[nth])       ! not efficient
    integer[(sk)], intent(in) :: istart, istop, inc
    logical, intent(out) :: bool(:)
    logical :: fget(:)
    integer, intent(in) :: nth
```
Gets the values of the whole bit array, or the bits at indeces `istart:istop:inc`, either in
the argument `bool` or in a function result.
- bool(:) must be allocated beforehand, and the sizes must conform

```
subroutine b% extract(istart,istop,inc,c,[nth])     ! efficient if inc==1
function   b%fextract(istart,istop,inc,[nth])       ! efficient if inc==1
    integer[(sk)], intent(in) :: istart, istop, inc
    type(bitfield_t), intent(out) :: c
    type(bitfield_t) :: fextract
    integer, intent(in) :: nth
```
Extracts the bits at indeces `istart:istop:inc` to a new bit array
- if `c` is allocated beforehand, it is first deallocated

```
subroutine b%replace(istart,istop,inc,c,[nth])     ! efficient if inc==1
    integer[(sk)], intent(in) :: istart, istop, inc
    type(bitfield_t), intent(in) :: c
    integer, intent(in) :: nth
```
Replaces the bits of `b` at indeces `istart:istop:inc` istart the bits of the `c`.
- the sizes must conform


### logical operations

```
integer function b%count()                                ! efficient
integer function b%count(istart,istop,inc,[nth])     ! efficient if |inc|==1
    integer[(sk)], intent(in) :: istart, istop, inc
    integer, intent(in) :: nth
```
Counts the number of bits equal to ".true." in the whole array, or at indeces `istart:istop:inc`

```
logical function b%all()                                ! efficient
logical function b%all(istart,istop,inc,[nth])     ! efficient if |inc|==1
    integer[(sk)], intent(in) :: istart, istop, inc
    integer, intent(in) :: nth
```
Is `.true.` iif all the bits of the array, or all the bits at indeces `istart:istop:inc`, are `.true.`

```
logical function b%any()                                ! efficient
logical function b%any(istart,istop,inc,[nth])     ! efficient if |inc|==1
    integer[(sk)], intent(in) :: istart, istop, inc
    integer, intent(in) :: nth
```
Is `.true.` iif any bit of the array, or any bit at indeces `istart:istop:inc`, is `.true.`

```
subroutine b%not()                                  ! efficient
subroutine b%not(istart,istop,inc,[nth])       ! efficient if |inc|==1
    integer[(sk)], intent(in) :: istart, istop, inc
    integer, intent(in) :: nth
```
Negates all the bits of the array, or the bits at indeces `istart:istop:inc`


### overloaded operators

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


### user defined I/Os

```
write( unit, '(DT)' ) b     ! not efficient
read ( unit, '(DT)' ) b     ! not efficient
    type(bitfield_t) :: b
    integer :: unit   ! can also be a string in case of internal read/write
```
Overloaded **formatted** I/Os. Writes or reads `"0"` or `"1"` characters, standing for the 
`.false.` or `.true.` values of the bitfield.
- In the `read` version, `b` can be unallocated, or allocated with a size that is not 
  large enough: it will be allocated and/or resized as needed. If it has to be allocated, 
  the lower bound is always $1$, otherwise the original lower bound is retained.

```
write( unit ) b     ! efficient
read ( unit ) b     ! efficient
    type(bitfield_t) :: b
    integer :: unit
```
Overloaded **unformatted** I/Os. 
- In the `read` version, `b` can be unallocated, or allocated with a size that is not 
  large enough: it will be allocated and/or resized as needed. If it has to be allocated, 
  the lower bound is read from the file, otherwise the existing lower bound is retained.
