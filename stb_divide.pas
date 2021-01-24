unit stb_divide;

// stb_divide.h - v0.93 - public domain - Sean Barrett, Feb 2010
// Three kinds of divide/modulus of signed integers.
//
// HISTORY
//
//   v0.93  2020-02-02  Write useful exit() value from main()
//   v0.92  2019-02-25  Fix warning
//   v0.91  2010-02-27  Fix euclidean division by INT_MIN for non-truncating C
//                      Check result with 64-bit math to catch such cases
//   v0.90  2010-02-24  First public release
//
// USAGE
//
// In *ONE* source file, put:
//
//    #define STB_DIVIDE_IMPLEMENTATION
//    // #define C_INTEGER_DIVISION_TRUNCATES  // see Note 1
//    // #define C_INTEGER_DIVISION_FLOORS     // see Note 2
//    #include "stb_divide.h"
//
// Other source files should just include stb_divide.h
//
// Note 1: On platforms/compilers that you know signed C division
// truncates, you can #define C_INTEGER_DIVISION_TRUNCATES.
//
// Note 2: On platforms/compilers that you know signed C division
// floors (rounds to negative infinity), you can #define
// C_INTEGER_DIVISION_FLOORS.
//
// You can #define STB_DIVIDE_TEST in which case the implementation
// will generate a main() and compiling the result will create a
// program that tests the implementation. Run it with no arguments
// and any output indicates an error; run it with any argument and
// it will also print the test results. Define STB_DIVIDE_TEST_64
// to a 64-bit integer type to avoid overflows in the result-checking
// which give false negatives.
//
// ABOUT
//
// This file provides three different consistent divide/mod pairs
// implemented on top of arbitrary C/C++ division, including correct
// handling of overflow of intermediate calculations:
//
//     trunc:   a/b truncates to 0,           a%b has same sign as a
//     floor:   a/b truncates to -inf,        a%b has same sign as b
//     eucl:    a/b truncates to sign(b)*inf, a%b is non-negative
//
// Not necessarily optimal; I tried to keep it generally efficient,
// but there may be better ways.
//
// Briefly, for those who are not familiar with the problem, we note
// the reason these divides exist and are interesting:
//
//     'trunc' is easy to implement in hardware (strip the signs,
//          compute, reapply the signs), thus is commonly defined
//          by many languages (including C99)
//
//     'floor' is simple to define and better behaved than trunc;
//          for example it divides integers into fixed-size buckets
//          without an extra-wide bucket at 0, and for a fixed
//          divisor N there are only |N| possible moduli.
//
//     'eucl' guarantees fixed-sized buckets *and* a non-negative
//          modulus and defines division to be whatever is needed
//          to achieve that result.
//
// See "The Euclidean definition of the functions div and mod"
// by Raymond Boute (1992), or "Division and Modulus for Computer
// Scientists" by Daan Leijen (2001)
//
// We assume of the built-in C division:
//     (a) modulus is the remainder for the corresponding division
//     (b) a/b truncates if a and b are the same sign
//
// Property (a) requires (a/b)*b + (a%b)==a, and is required by C.
// Property (b) seems to be true of all hardware but is *not* satisfied
// by the euclidean division operator we define, so it's possibly not
// always true. If any such platform turns up, we can add more cases.
// (Possibly only stb_div_trunc currently relies on property (b).)
//
// LICENSE
//
//   See end of file for license information.

{$MODE FPC}
{$MODESWITCH DEFAULTPARAMETERS}
{$MODESWITCH OUT}
{$MODESWITCH RESULT}

interface

type
  TStbDivideInt = PtrInt;

// v1 is for value to be divided, v2 is for value to divide by
function stb_div_trunc(v1, v2: TStbDivideInt): TStbDivideInt;
function stb_div_floor(v1, v2: TStbDivideInt): TStbDivideInt;
function stb_div_eucl (v1, v2: TStbDivideInt): TStbDivideInt;
function stb_mod_trunc(v1, v2: TStbDivideInt): TStbDivideInt;
function stb_mod_floor(v1, v2: TStbDivideInt): TStbDivideInt;
function stb_mod_eucl (v1, v2: TStbDivideInt): TStbDivideInt;

{$IF Defined(STB_DIVIDE_TEST)}function stb_divide_run_test: PtrInt;{$ENDIF}

implementation

const
  INT_MIN = Low(TStbDivideInt);

// the following macros are designed to allow testing
// other platforms by simulating them
{$IF not Defined(STB_DIVIDE_TEST_FLOOR)}
function stb__div(a, b: TStbDivideInt): TStbDivideInt; inline;
begin
  Exit(a div b);
end;

function stb__mod(a, b: TStbDivideInt): TStbDivideInt; inline;
begin
  Exit(a mod b);
end;
{$ELSE}
// implement floor-style divide on trunc platform
function stb__div(v1, v2: TStbDivideInt): TStbDivideInt; inline;
var
  q, r: TStbDivideInt;
begin
  q := v1 div v2;
  r := v1 mod v2;
  if ((r > 0) && (v2 < 0)) or ((r < 0) and (v2 > 0)) then begin
    Exit(q-1);
  end else
    Exit(q);
end;

function stb__mod(v1, v2: TStbDivideInt): TStbDivideInt; inline;
var
  r: TStbDivideInt;
begin
  r := v1 mod v2;
  if ((r > 0) and (v2 < 0)) or ((r < 0) and (v2 > 0)) then begin
    Exit(r + v2);
  end else
    Exit(r);
end;
{$ENDIF}

function stb_div_trunc(v1, v2: TStbDivideInt): TStbDivideInt;
begin
{$IF Defined(C_INTEGER_DIVISION_TRUNCATES)}
  Exit(v1 div v2);
{$ELSE}
  if (v1 >= 0) and (v2 <= 0) then
    Exit(- stb__div(-v1, v2)); // both negative to avoid overflow
  if (v1 <= 0) and (v2 >= 0) then begin
    if v1 <> INT_MIN then begin
      Exit(- stb__div(v1, -v2));    // both negative to avoid overflow
    end else
      Exit(- stb__div(v1 + v2, -v2) - 1); // push v1 away from wrap point
  end else
    Exit(v1 div v2);            // same sign, so expect truncation
{$ENDIF}
end;

function stb_div_floor(v1, v2: TStbDivideInt): TStbDivideInt;
begin
{$IF Defined(C_INTEGER_DIVISION_FLOORS)}
  Exit(v1 div v2);
{$ELSE}
  if (v1 >= 0) and (v2 < 0) then begin
    if (-v1) + v2 + 1 < 0 then begin // check if increasing v1's magnitude overflows
      Exit(- stb__div(-v1 + v2 + 1, v2)); // nope, so just compute it
    end else begin
      if ((-v1) mod v2) <> 0 then begin
        Exit(- stb__div(-v1, v2) - 1);
      end else
        Exit(- stb__div(-v1, v2));
    end;
  end;
  if (v1 < 0) and (v2 >= 0) then begin
    if v1 <> INT_MIN then begin
      if v1 - v2 + 1 < 0 then begin // check if increasing v1's magnitude overflows
        Exit(- stb__div(v1 - v2 + 1, - v2)); // nope, so just compute it
      end else begin
        if stb__mod(v1,-v2) <> 0 then begin
          Exit(- stb__div(- v1, v2) - 1);
        end else
          Exit(- stb__div(- v1, v2));
      end;
     end else begin // it must be possible to compute -(v1+v2) without overflowing
       if stb__mod(-(v1+v2),v2) <> 0 then begin
         Exit(- stb__div(- (v1 + v2), v2) - 2);
       end else
         Exit(- stb__div(- (v1 + v2), v2) - 1);
     end;
  end else
    Exit(v1 div v2);           // same sign, so expect truncation
{$ENDIF}
end;

function stb_div_eucl (v1, v2: TStbDivideInt): TStbDivideInt;
var
  q, r: TStbDivideInt;
begin
{$IF Defined(C_INTEGER_DIVISION_TRUNCATES)}
  q := v1 div v2;
  r := v1 mod v2;
{$ELSE}
  // handle every quadrant separately, since we can't rely on q and r flor
  if v1 >= 0 then begin
    if v2 >= 0 then begin
      Exit(stb__div(v1, v2));
    end else if v2 <> INT_MIN then begin
      q := - stb__div(v1, - v2);
      r :=   stb__mod(v1, - v2);
    end else begin
      q := 0;
      r := v1;
    end;
  end else if v1 <> INT_MIN then begin
    if v2 >= 0 then begin
      q := - stb__div(- v1, v2);
      r := - stb__mod(- v1, v2);
    end else if v2 <> INT_MIN then begin
      q :=   stb__div(- v1, - v2);
      r := - stb__mod(- v1, - v2);
    end else begin // if v2 is INT_MIN, then we can't use -v2, but we can't divide by v2
      q := 1;
      r := v1 - q * v2;
    end;
  end else begin // if v1 is INT_MIN, we have to move away from overflow place
    if v2 >= 0 then begin
      q := - stb__div(- (v1 + v2), v2) - 1;
      r := - stb__mod(- (v1 + v2), v2);
    end else begin
      q :=   stb__div(- (v1 - v2), - v2) + 1;
      r := - stb__mod(- (v1 - v2), - v2);
    end;
  end;
{$ENDIF}
  if r >= 0 then begin
    Exit(q);
  end else begin
    if v2 > 0 then begin
      Exit(q - 1);
    end else
      Exit(q + 1);
  end;
end;

function stb_mod_trunc(v1, v2: TStbDivideInt): TStbDivideInt;
var
  r: TStbDivideInt;
begin
{$IF Defined(C_INTEGER_DIVISION_FLOORS)}
  Exit(v1 mod v2);
{$ELSE}
  if v1 >= 0 then begin // modulus result should always be positive
    r := stb__mod(v1, v2);
    if r >= 0 then begin
      Exit(r);
    end else begin
      if v2 > 0 then begin
        Exit(r + v2);
      end else
        Exit(r - v2);
    end;
  end else begin    // modulus result should always be negative
    r := stb__mod(v1, v2);
    if r <= 0 then begin
      Exit(r);
    end else begin
      if v2 > 0 then begin
        Exit(r - v2);
      end else
        Exit(r + v2);
    end;
  end;
{$ENDIF}
end;

function stb_mod_floor(v1, v2: TStbDivideInt): TStbDivideInt;
var
  r: TStbDivideInt;
begin
{$IF Defined(C_INTEGER_DIVISION_FLOORS)}
  Exit(v1 mod v2);
{$ELSE}
  if v2 >= 0 then begin // result should always be positive
    r := stb__mod(v1, v2);
    if r >= 0 then begin
      Exit(r);
    end else
      Exit(r + v2);
  end else begin // result should always be negative
    r := stb__mod(v1, v2);
    if r <= 0 then begin
      Exit(r);
    end else
      Exit(r + v2);
  end;
{$ENDIF}
end;

function stb_mod_eucl (v1, v2: TStbDivideInt): TStbDivideInt;
var
  r: TStbDivideInt;
begin
  r := stb__mod(v1,v2);

  if r >= 0 then begin
    Exit(r);
  end else begin
    if v2 > 0 then begin // abs()
      Exit(r + v2);
    end else
      Exit(r - v2);
  end;
end;

{$IF Defined(STB_DIVIDE_TEST)}
const
  INT_MAX = High(TStbDivideInt);

var
  show: Boolean = False;
  err: LongInt = 0;

procedure stbdiv_check(q, r, a, b: LongInt; _type: PAnsiChar; dir: LongInt);
type
  STB_DIVIDE_TEST_64 = Int64;
var
  q64, r64, a64, b64: STB_DIVIDE_TEST_64;
begin
  if ((dir > 0) and (r < 0)) or ((dir < 0) and (r > 0)) then begin
    Writeln(stderr, 'FAILED: ', _type, '(', a, ',', b, ') remainder ', r, ' in wrong direction');
    Inc(err);
  end else begin
    if b <> INT_MIN then begin // can't compute abs(), but if b==INT_MIN all remainders are valid
      if (r <= -abs(b)) or (r >= abs(b)) then begin
        Writeln(stderr, 'FAILED: ', _type, '(', a, ',', b, ') remainder ', r, ' out of range');
        Inc(err);
      end;
    end;
  end;
{$IF Defined(STB_DIVIDE_TEST_64)}
  begin
    q64 := q;
    r64 := r;
    a64 := a;
    b64 := b;
    if q64 * b64 + r64 <> a64 then begin
      Writeln(stderr, 'FAILED: ', _type, '(', a, ',', b, ') remainder ', r, ' doesn''t match quotient ', q);
      Inc(err);
    end;
  end;
{$ELSE}
  if q * b + r <> a then begin
    Writeln(stderr, 'FAILED: ', _type, '(', a, ',', b, ') remainder ', r, ' doesn''t match quotient ', q);
    Inc(err);
  end;
{$ENDIF}
end;

procedure test(a, b: TStbDivideInt);
var
  q, r: TStbDivideInt;
begin
  if show then Writeln('(', a, ',', b, ') |  ');
  q := stb_div_trunc(a, b); r := stb_mod_trunc(a, b);
  if show then Writeln('(', a, ',' , b, ')  ');
  stbdiv_check(q, r, a, b, 'trunc', a);
  q := stb_div_floor(a, b); r := stb_mod_floor(a, b);
  if show then Writeln('(', q, ',', r, ')  ');
  stbdiv_check(q, r, a, b, 'floor', b);
  q := stb_div_eucl (a, b);
  r := stb_mod_eucl (a, b);
  if show then Writeln('(', q, ',', r, ')');
  stbdiv_check(q, r, a, b, 'euclidean', 1);
end;

procedure testh(a, b: TStbDivideInt);
var
  q, r: TStbDivideInt;
begin
  if show then Writeln('(', a, ',', b, ') |');
  q := stb_div_trunc(a, b);
  r := stb_mod_trunc(a, b);
  stbdiv_check(q, r, a, b, 'trunc', a);
  if show then Writeln('             (', q, ',', r, ')');
  q := stb_div_floor(a, b);
  r := stb_mod_floor(a, b);
  stbdiv_check(q, r, a, b, 'floor', b);
  if show then Writeln('   (', q, ',', r, ')');
  q := stb_div_eucl (a, b);
  r := stb_mod_eucl (a, b);
  stbdiv_check(q, r, a, b, 'euclidean', 1);
  if show then Writeln('   (', q, ',', r, ')', q,r);
end;

function stb_divide_run_test: PtrInt;
begin
  err := 0;
  show := False;

  if ParamCount > 0 then
    show := True;

  test(8,3);
  test(8,-3);
  test(-8,3);
  test(-8,-3);
  test(1,2);
  test(1,-2);
  test(-1,2);
  test(-1,-2);
  test(8,4);
  test(8,-4);
  test(-8,4);
  test(-8,-4);

  test(INT_MAX,1);
  test(INT_MIN,1);
  test(INT_MIN+1,1);
  test(INT_MAX,-1);
  //test(INT_MIN,-1); // this traps in MSVC, so we leave it untested
  test(INT_MIN+1,-1);
  test(INT_MIN,-2);
  test(INT_MIN+1,2);
  test(INT_MIN+1,-2);
  test(INT_MAX,2);
  test(INT_MAX,-2);
  test(INT_MIN+1,2);
  test(INT_MIN+1,-2);
  test(INT_MIN,2);
  test(INT_MIN,-2);
  test(INT_MIN,7);
  test(INT_MIN,-7);
  test(INT_MIN+1,4);
  test(INT_MIN+1,-4);

  testh(-7, INT_MIN);
  testh(-1, INT_MIN);
  testh(1, INT_MIN);
  testh(7, INT_MIN);

  testh(INT_MAX-1, INT_MIN);
  testh(INT_MAX,   INT_MIN);
  testh(INT_MIN,   INT_MIN);
  testh(INT_MIN+1, INT_MIN);

  testh(INT_MAX-1, INT_MAX);
  testh(INT_MAX  , INT_MAX);
  testh(INT_MIN  , INT_MAX);
  testh(INT_MIN+1, INT_MAX);

  Exit(err);
end;
{$ENDIF}

end.

{
------------------------------------------------------------------------------
This software is available under 2 licenses -- choose whichever you prefer.
------------------------------------------------------------------------------
ALTERNATIVE A - MIT License
Copyright (c) 2017 Sean Barrett
Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
------------------------------------------------------------------------------
ALTERNATIVE B - Public Domain (www.unlicense.org)
This is free and unencumbered software released into the public domain.
Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
software, either in source code form or as a compiled binary, for any purpose,
commercial or non-commercial, and by any means.
In jurisdictions that recognize copyright laws, the author or authors of this
software dedicate any and all copyright interest in the software to the public
domain. We make this dedication for the benefit of the public at large and to
the detriment of our heirs and successors. We intend this dedication to be an
overt act of relinquishment in perpetuity of all present and future rights to
this software under copyright law.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
------------------------------------------------------------------------------
}
