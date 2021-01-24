unit stb_sprintf;

// stb_sprintf - v1.09 - public domain snprintf() implementation
// originally by Jeff Roberts / RAD Game Tools, 2015/10/20
// http://github.com/nothings/stb
//
// allowed types:  sc uidBboXx p AaGgEef n
// lengths      :  hh h ll j z t I64 I32 I
//
// Contributors:
//    Fabian "ryg" Giesen (reformatting)
//    Doj (pascal port)
//
// Contributors (bugfixes):
//    github:d26435
//    github:trex78
//    github:account-login
//    Jari Komppa (SI suffixes)
//    Rohit Nirmal
//    Marcin Wojdyr
//    Leonard Ritter
//    Stefano Zanotti
//    Adam Allison
//    Arvid Gerstmann
//    Markus Kolb
//
// LICENSE:
//
//   See end of file for license information.

{
Single file sprintf replacement.

Originally written by Jeff Roberts at RAD Game Tools - 2015/10/20.
Hereby placed in public domain.

This is a full sprintf replacement that supports everything that
the C runtime sprintfs support, including float/double, 64-bit integers,
hex floats, field parameters (%*.*d stuff), length reads backs, etc.

Why would you need this if sprintf already exists?  Well, first off,
it's *much* faster (see below). It's also much smaller than the CRT
versions code-space-wise. We've also added some simple improvements
that are super handy (commas in thousands, callbacks at buffer full,
for example). Finally, the format strings for MSVC and GCC differ
for 64-bit integers (among other small things), so this lets you use
the same format strings in cross platform code.

It uses the standard single file trick of being both the header file
and the source itself. If you just include it normally, you just get
the header file function definitions. To get the code, you include
it from a C or C++ file and define STB_SPRINTF_IMPLEMENTATION first.

It only uses va_args macros from the C runtime to do it's work. It
does cast doubles to S64s and shifts and divides U64s, which does
drag in CRT code on most platforms.

It compiles to roughly 8K with float support, and 4K without.
As a comparison, when using MSVC static libs, calling sprintf drags
in 16K.

API:
====
int stbsp_sprintf( char * buf, char const * fmt, ... )
int stbsp_snprintf( char * buf, int count, char const * fmt, ... )
  Convert an arg list into a buffer.  stbsp_snprintf always returns
  a zero-terminated string (unlike regular snprintf).

int stbsp_vsprintf( char * buf, char const * fmt, va_list va )
int stbsp_vsnprintf( char * buf, int count, char const * fmt, va_list va )
  Convert a va_list arg list into a buffer.  stbsp_vsnprintf always returns
  a zero-terminated string (unlike regular snprintf).

int stbsp_vsprintfcb( STBSP_SPRINTFCB * callback, void * user, char * buf, char const * fmt, va_list va )
    typedef char * STBSP_SPRINTFCB( char const * buf, void * user, int len );
  Convert into a buffer, calling back every STB_SPRINTF_MIN chars.
  Your callback can then copy the chars out, print them or whatever.
  This function is actually the workhorse for everything else.
  The buffer you pass in must hold at least STB_SPRINTF_MIN characters.
    // you return the next buffer to use or 0 to stop converting

void stbsp_set_separators( char comma, char period )
  Set the comma and period characters to use.

FLOATS/DOUBLES:
===============
This code uses a internal float->ascii conversion method that uses
doubles with error correction (double-doubles, for ~105 bits of
precision).  This conversion is round-trip perfect - that is, an atof
of the values output here will give you the bit-exact double back.

One difference is that our insignificant digits will be different than
with MSVC or GCC (but they don't match each other either).  We also
don't attempt to find the minimum length matching float (pre-MSVC15
doesn't either).

If you don't need float or doubles at all, define STB_SPRINTF_NOFLOAT
and you'll save 4K of code space.

64-BIT INTS:
============
This library also supports 64-bit integers and you can use MSVC style or
GCC style indicators (%I64d or %lld).  It supports the C99 specifiers
for size_t and ptr_diff_t (%jd %zd) as well.

EXTRAS:
=======
Like some GCCs, for integers and floats, you can use a ' (single quote)
specifier and commas will be inserted on the thousands: "%'d" on 12345
would print 12,345.

For integers and floats, you can use a "$" specifier and the number
will be converted to float and then divided to get kilo, mega, giga or
tera and then printed, so "%$d" 1000 is "1.0 k", "%$.2d" 2536000 is
"2.53 M", etc. For byte values, use two $:s, like "%$$d" to turn
2536000 to "2.42 Mi". If you prefer JEDEC suffixes to SI ones, use three
$:s: "%$$$d" -> "2.42 M". To remove the space between the number and the
suffix, add "_" specifier: "%_$d" -> "2.53M".

In addition to octal and hexadecimal conversions, you can print
integers in binary: "%b" for 256 would print 100.

PERFORMANCE vs MSVC 2008 32-/64-bit (GCC is even slower than MSVC):
===================================================================
"%d" across all 32-bit ints (4.8x/4.0x faster than 32-/64-bit MSVC)
"%24d" across all 32-bit ints (4.5x/4.2x faster)
"%x" across all 32-bit ints (4.5x/3.8x faster)
"%08x" across all 32-bit ints (4.3x/3.8x faster)
"%f" across e-10 to e+10 floats (7.3x/6.0x faster)
"%e" across e-10 to e+10 floats (8.1x/6.0x faster)
"%g" across e-10 to e+10 floats (10.0x/7.1x faster)
"%f" for values near e-300 (7.9x/6.5x faster)
"%f" for values near e+300 (10.0x/9.1x faster)
"%e" for values near e-300 (10.1x/7.0x faster)
"%e" for values near e+300 (9.2x/6.0x faster)
"%.320f" for values near e-300 (12.6x/11.2x faster)
"%a" for random values (8.6x/4.3x faster)
"%I64d" for 64-bits with 32-bit values (4.8x/3.4x faster)
"%I64d" for 64-bits > 32-bit values (4.9x/5.5x faster)
"%s%s%s" for 64 char strings (7.1x/7.3x faster)
"...512 char string..." ( 35.0x/32.5x faster!)
}

{$MODE OBJFPC}
{$MODESWITCH DEFAULTPARAMETERS}
{$MODESWITCH OUT}
{$MODESWITCH RESULT}

interface

{$IF not Declared(STB_SPRINTF_MIN)}
const
  STB_SPRINTF_MIN = 512; // how many characters per callback
{$ENDIF}

type
  STBSP_SPRINTFCB = function(buf: PAnsiChar; user: Pointer; len: SizeUInt): PAnsiChar;

function stbsp_vsprintf(buf: PAnsiChar; fmt: PAnsiChar; const va: array of Const): PtrInt;
function stbsp_vsnprintf(buf: PAnsiChar; count: PtrInt; fmt: PAnsiChar; const va: array of Const): PtrInt;
function stbsp_sprintf(buf: PAnsiChar; fmt: PAnsiChar; const va: array of Const): PtrInt;
function stbsp_snprintf(buf: PAnsiChar; count: PtrInt; fmt: PAnsiChar; const va: array of Const): PtrInt;

function stbsp_vsprintfcb(callback: STBSP_SPRINTFCB; user: Pointer; buf: PAnsiChar; fmt: PAnsiChar; const va: array of Const): PtrInt;
procedure stbsp_set_separators(comma: AnsiChar; period: AnsiChar);

implementation

{$IF not Defined(STB_SPRINTF_NOUNALIGNED)} // define this before inclusion to force stbsp_sprintf to always use aligned accesses
{$DEFINE STBSP__UNALIGNED}
{$ENDIF}

{$IF not Defined(STB_SPRINTF_NOFLOAT)}
// internal float utility functions
function stbsp__real_to_str(start: PPAnsiChar; len: PUInt32; _out: PAnsiChar; decimal_pos: PInt32; value: Double; frac_digits: UInt32): Int32; forward;
function stbsp__real_to_parts(bits: PInt64; expo: PInt32; value: Double): Int32; forward;

const
  STBSP__SPECIAL = $7000;
{$ENDIF}

var
  stbsp__period: AnsiChar = '.';
  stbsp__comma: AnsiChar = ',';

type
Tstbsp__digitpair = record
  temp: UInt16; // force next field to be 2-byte aligned
  pair: array[0 .. 201 - 1] of AnsiChar;
end;
const
stbsp__digitpair: Tstbsp__digitpair = (
  temp: 0;
  pair: ('0','0','0','1','0','2','0','3','0','4',
         '0','5','0','6','0','7','0','8','0','9',
         '1','0','1','1','1','2','1','3','1','4',
         '1','5','1','6','1','7','1','8','1','9',
         '2','0','2','1','2','2','2','3','2','4',
         '2','5','2','6','2','7','2','8','2','9',
         '3','0','3','1','3','2','3','3','3','4',
         '3','5','3','6','3','7','3','8','3','9',
         '4','0','4','1','4','2','4','3','4','4',
         '4','5','4','6','4','7','4','8','4','9',
         '5','0','5','1','5','2','5','3','5','4',
         '5','5','5','6','5','7','5','8','5','9',
         '6','0','6','1','6','2','6','3','6','4',
         '6','5','6','6','6','7','6','8','6','9',
         '7','0','7','1','7','2','7','3','7','4',
         '7','5','7','6','7','7','7','8','7','9',
         '8','0','8','1','8','2','8','3','8','4',
         '8','5','8','6','8','7','8','8','8','9',
         '9','0','9','1','9','2','9','3','9','4',
         '9','5','9','6','9','7','9','8','9','9',
         #0);
);

procedure stbsp_set_separators(comma: AnsiChar; period: AnsiChar);
begin
  stbsp__period := period;
  stbsp__comma := comma;
end;

const
  STBSP__LEFTJUST = 1;
  STBSP__LEADINGPLUS = 2;
  STBSP__LEADINGSPACE = 4;
  STBSP__LEADING_0X = 8;
  STBSP__LEADINGZERO = 16;
  STBSP__INTMAX = 32;
  STBSP__TRIPLET_COMMA = 64;
  STBSP__NEGATIVE = 128;
  STBSP__METRIC_SUFFIX = 256;
  STBSP__HALFWIDTH = 512;
  STBSP__METRIC_NOSPACE = 1024;
  STBSP__METRIC_1024 = 2048;
  STBSP__METRIC_JEDEC = 4096;

procedure stbsp__lead_sign(fl: UInt32; sign: PAnsiChar);
begin
  sign[0] := #0;
  if (fl and STBSP__NEGATIVE) <> 0 then begin
    sign[0] := #1;
    sign[1] := '-';
  end else if (fl and STBSP__LEADINGSPACE) <> 0 then begin
    sign[0] := #1;
    sign[1] := ' ';
  end else if (fl and STBSP__LEADINGPLUS) <> 0 then begin
    sign[0] := #1;
    sign[1] := '+';
  end;
end;

function stbsp_vsprintfcb(callback: STBSP_SPRINTFCB; user: Pointer; buf: PAnsiChar; fmt: PAnsiChar; const va: array of Const): PtrInt;
label
  scandd, endfmt, schk1, schk2, flags_done, ld, lchk, scopy, doafloat,
  radixnum, flt_lead, doexpfromg, dofloatfromg, done;
const
  hex: array[0 .. 18 - 1] of AnsiChar = (
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'a', 'b', 'c', 'd', 'e', 'f', 'x', 'p'
  );
  hexu: array[0 .. 18 - 1] of AnsiChar = (
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'A', 'B', 'C', 'D', 'E', 'F', 'X', 'P'
  );
  STBSP__NUMSZ = 512; // big enough for e308 (with commas) or e-307
var
  va_index: PtrInt;
  bf, f: PAnsiChar;
  tlen: PtrInt;
  fw, pr, tz: Int32;
  fl: UInt32;
  v, c: UInt32;
  num: array[0 .. STBSP__NUMSZ - 1] of AnsiChar;
  lead: array[0 .. 8 - 1] of AnsiChar;
  tail: array[0 .. 8 - 1] of AnsiChar;
  s, h: PAnsiChar;
  l, n, cs: UInt32;
  n64: UInt64;
{$IF not Defined(STB_SPRINTF_NOFLOAT)}
  fv: Double;
{$ENDIF}
  dp: Int32;
  sn: PAnsiChar;
  d: PPtrInt;
  divisor: Double;
  i: Int32;
  idx: Byte;
  i64: Int64;
  o: PAnsiChar;
  string_of_one_char: array[0 .. 1] of AnsiChar; // TODO check it used once

      // replacements for va_arg
      function va_uint32: UInt32;
      begin
        with va[va_index] do begin
          Inc(va_index);
          case VType of
            vtInteger: Exit(VInteger);
            vtInt64: Exit(VInt64^);
            else Exit(0);
          end;
        end;
      end;
      function va_int32: UInt32;
      begin
        with va[va_index] do begin
          Inc(va_index);
          case VType of
            vtInteger: Exit(VInteger);
            vtInt64: Exit(VInt64^);
            else Exit(0);
          end;
        end;
      end;
      function va_uint64: UInt64;
      begin
        with va[va_index] do begin
          Inc(va_index);
          case VType of
            vtInteger: Exit(VInteger);
            vtInt64: Exit(VInt64^);
            else Exit(0);
          end;
        end;
      end;
      function va_int64: UInt64;
      begin
        with va[va_index] do begin
          Inc(va_index);
          case VType of
            vtInteger: Exit(VInteger);
            vtInt64: Exit(VInt64^);
            else Exit(0);
          end;
        end;
      end;
      function va_pansichar: PAnsiChar;
      begin
        with va[va_index] do begin
          Inc(va_index);
          case VType of
            vtChar: begin
                string_of_one_char[0] := VChar;
                string_of_one_char[1] := #0;
                Exit(@string_of_one_char[0]);
              end;
            vtWideChar: begin
                string_of_one_char[0] := AnsiChar(Ord(VWideChar));
                string_of_one_char[1] := #0;
                Exit(@string_of_one_char[0]);
              end;
            vtString: Exit(@(VString^)[1]);
            vtPChar: Exit(VPChar);
            vtAnsiString: Exit(@AnsiString(VAnsiString)[1]);
            else Exit('null'); // 'null' or ''?
          end;
        end;
      end;
      function va_int: PtrInt;
      begin
        with va[va_index] do begin
          Inc(va_index);
          case VType of
            vtInteger: Exit(VInteger);
            vtChar: Exit(Ord(VChar));
            vtWideChar: Exit(Ord(VWideChar));
            vtInt64: Exit(VInt64^);
            else Exit(0);
          end;
        end;
      end;
      function va_char: AnsiChar;
      begin
        with va[va_index] do begin
          Inc(va_index);
          case VType of
            vtInteger: Exit(AnsiChar(VInteger));
            vtChar: Exit(VChar);
            vtWideChar: Exit(AnsiChar(Ord(VWideChar)));
            vtInt64: Exit(AnsiChar(VInt64^));
            else Exit('?');
          end;
        end;
      end;
      function va_pointer: Pointer;
      begin
        with va[va_index] do begin
          Inc(va_index);
          case VType of
            vtPointer: Exit(VPointer);
            else Exit(nil);
          end;
        end;
      end;
      function va_double: Double;
      begin
        with va[va_index] do begin
          Inc(va_index);
          case VType of
            vtInteger: Exit(VInteger);
            vtExtended: Exit(VExtended^);
            else Exit(0.0);
          end;
        end;
      end;

      // macros for the callback buffer stuff
      function stbsp__chk_cb_bufL(bytes: PtrInt): Boolean;
      var
        len: PtrInt;
      begin
        len := bf - buf;
        if len + bytes >= STB_SPRINTF_MIN then begin
          Inc(tlen, len);
          buf := callback(buf, user, len);
          bf := buf;
          if bf = nil then
            Exit(True);
        end;
        Exit(False);
      end;
      function stbsp__chk_cb_buf(bytes: PtrInt): Boolean;
      begin
        if callback <> nil then
          Exit(stbsp__chk_cb_bufL(bytes));
        Exit(False);
      end;
      function stbsp__flush_cb: Boolean;
      begin
        Exit(stbsp__chk_cb_bufL(STB_SPRINTF_MIN - 1));
      end; // flush if there is even one byte in the buffer
      procedure stbsp__cb_buf_clamp(var cl: Int32; v: Int32);
      var
        lg: PtrInt;
      begin
        cl := v;
        if callback <> nil then begin
          lg := STB_SPRINTF_MIN - PtrInt(bf - buf);
          if cl > lg then
             cl := lg;
        end;
      end;
begin
  va_index := 0;
   tlen := 0;

   bf := buf;
   f := fmt;
   while True do begin
      // fast copy everything up to the next % (or end of string)
      while True do begin
         while (UIntPtr(f) and 3) <> 0 do begin
         schk1:
            if f[0] = '%' then
               goto scandd;
         schk2:
            if f[0] = #0 then
               goto endfmt;
            if stbsp__chk_cb_buf(1) then
              goto done;
            bf^ := f[0];
            Inc(bf);
            Inc(f);
         end;
         while True do begin
            // Check if the next 4 bytes contain %(0x25) or end of string.
            // Using the 'hasless' trick:
            // https://graphics.stanford.edu/~seander/bithacks.html#HasLessInWord
            v := PUInt32(f)^;
            c := (not v) and $80808080;
            if (((v xor $25252525) - $01010101) and c) > 0 then
              goto schk1;
            if ((v - $01010101) and c) <> 0 then
              goto schk2;
            if callback <> nil then begin
              if (STB_SPRINTF_MIN - PtrInt(bf - buf)) < 4 then
                goto schk1;
            end;
            {$IF Defined(STB_SPRINTF_NOUNALIGNED)}
                if (PPtrUInt(bf)^ and 3) > 0 then begin
                  bf[0] := f[0];
                  bf[1] := f[1];
                  bf[2] := f[2];
                  bf[3] := f[3];
                end else
            {$ENDIF} begin
                PUInt32(bf)^ := v;
            end;
            Inc(bf, 4);
            Inc(f, 4);
         end;
      end;
   scandd:

      Inc(f);

      // ok, we have a percent, read the modifiers first
      fw := 0;
      pr := -1;
      fl := 0;
      tz := 0;

      // flags
      while True do begin
         case f[0] of
         // if we have left justify
         '-': begin
           fl := fl or STBSP__LEFTJUST;
           Inc(f);
           continue;
         end;
         // if we have leading plus
         '+': begin
           fl := fl or STBSP__LEADINGPLUS;
           Inc(f);
           continue;
         end;
         // if we have leading space
         ' ': begin
           fl := fl or STBSP__LEADINGSPACE;
           Inc(f);
           continue;
         end;
         // if we have leading 0x
         '#': begin
           fl := fl or STBSP__LEADING_0X;
           Inc(f);
           continue;
         end;
         // if we have thousand commas
         '''': begin
           fl := fl or STBSP__TRIPLET_COMMA;
           Inc(f);
           continue;
         end;
         // if we have kilo marker (none->kilo->kibi->jedec)
         '$': begin
           if (fl and STBSP__METRIC_SUFFIX) > 0 then begin
             if (fl and STBSP__METRIC_1024) > 0 then begin
               fl := fl or STBSP__METRIC_JEDEC;
             end else begin
               fl := fl or STBSP__METRIC_1024;
             end;
           end else begin
             fl := fl or STBSP__METRIC_SUFFIX;
           end;
           Inc(f);
           continue;
         end;
         // if we don't want space between metric suffix and number
         '_': begin
           fl := fl or STBSP__METRIC_NOSPACE;
           Inc(f);
           continue;
         end;
         // if we have leading zero
         '0': begin
           fl := fl or STBSP__LEADINGZERO;
           Inc(f);
           goto flags_done;
         end;
         else goto flags_done;
         end;
      end;
   flags_done:

      // get the field width
      if f[0] = '*' then begin
        fw := va_uint32;
        Inc(f);
      end else begin
        while (f[0] >= '0') and (f[0] <= '9') do begin
          fw := fw * 10 + (Ord(f[0]) - Ord('0'));
          Inc(f);
        end;
      end;
      // get the precision
      if f[0] = '.' then begin
        Inc(f);
        if f[0] = '*' then begin
          pr := va_uint32;
          Inc(f);
        end else begin
          pr := 0;
          while (f[0] >= '0') and (f[0] <= '9') do begin
            pr := pr * 10 + (Ord(f[0]) - Ord('0'));
            Inc(f);
          end;
        end;
      end;

      // handle integer size overrides
      case f[0] of
      // are we halfwidth?
      'h': begin
        fl := fl or STBSP__HALFWIDTH;
        Inc(f);
        if f[0] = 'h' then
          Inc(f); // QUARTERWIDTH
      end;
      // are we 64-bit (unix style)
      'l': begin
        {$IF SizeOf(PtrInt) = 8} // sizeof(long) replacement
          fl := fl or STBSP__INTMAX;
        {$ENDIF}
        Inc(f);
        if f[0] = 'l' then begin
          fl := fl or STBSP__INTMAX;
          Inc(f);
        end;
      end;
      // are we 64-bit on intmax? (c99)
      'j': begin
        {$IF SizeOf(SizeUInt) = 8}
          fl := fl or STBSP__INTMAX;
        {$ENDIF}
        Inc(f);
      end;
      // are we 64-bit on size_t or ptrdiff_t? (c99)
      'z': begin
        {$IF SizeOf(PtrInt) = 8}
          fl := fl or STBSP__INTMAX;
        {$ENDIF}
        Inc(f);
      end;
      't': begin
        {$IF SizeOf(PtrInt) = 8} // sizeof(ptrdiff_t) replacement
          fl := fl or STBSP__INTMAX;
        {$ENDIF}
        Inc(f);
      end;
      // are we 64-bit (msft style)
      'I':
         if (f[1] = '6') and (f[2] = '4') then begin
            fl := fl or STBSP__INTMAX;
            Inc(f, 3);
         end else if (f[1] = '3') and (f[2] = '2') then begin
            Inc(f, 3);
         end else begin
           {$IF SizeOf(Pointer) = 8}
             fl := fl or STBSP__INTMAX;
           {$ENDIF}
           Inc(f);
         end;
      end;

      // handle each replacement
      case f[0] of
      's': begin
         // get the string
         s := va_pansichar;
         if s = nil then
           s := 'null';
         // get the length
         sn := s;
         while True do begin
           if (PtrUInt(sn) and 3) = 0 then
             break;
         lchk:
           if sn[0] = #0 then
             goto ld;
           Inc(sn);
         end;
         n := $ffffffff;
         if pr >= 0 then begin
           n := UInt32(sn - s);
           if n >= UInt32(pr) then
             goto ld;
           n := UInt32(pr - n) shr 2;
         end;
         while n <> 0 do begin
           v := PUInt32(sn)^;
           if ((v - $01010101) and (not v) and PtrUInt($80808080)) <> 0 then
             goto lchk;
           Inc(sn, 4);
           Dec(n);
         end;
         goto lchk;
      ld:

         l := UInt32(sn - s);
         // clamp to precision
         if l > UInt32(pr) then
           l := pr;
         lead[0] := AnsiChar(0);
         tail[0] := AnsiChar(0);
         pr := 0;
         dp := 0;
         cs := 0;
         // copy the string in
         goto scopy;
      end;

      'c': begin // char
         // get the character
         s := @num[0] + STBSP__NUMSZ - 1;
         s^ := va_char;
         l := 1;
         lead[0] := AnsiChar(0);
         tail[0] := AnsiChar(0);
         pr := 0;
         dp := 0;
         cs := 0;
         goto scopy;
      end;

      'n': // weird write-bytes specifier
      begin
         d := va_pointer;
         d^ := tlen + PtrInt(bf - buf);
      end;

{$IF Defined(STB_SPRINTF_NOFLOAT)}
      'A',              // float
      'a',              // hex float
      'G',              // float
      'g',              // float
      'E',              // float
      'e',              // float
      'f': begin        // float
         va_double;
         s := 'No float';
         l := 8;
         lead[0] := 0;
         tail[0] := 0;
         pr := 0;
         dp := 0;
         cs := 0;
         goto scopy;
      end;
{$ELSE}
      'A', // hex float
      'a': begin // hex float
        if f[0] = 'A' then begin
          h := hexu;
        end else
          h := hex;
        fv := va_double;
        if pr = -1 then
          pr := 6; // default is 6
        // read the double into a string
        if stbsp__real_to_parts(PInt64(@n64), @dp, fv) <> 0 then
          fl := fl or STBSP__NEGATIVE;

        s := num + 64;

        stbsp__lead_sign(fl, @lead[0]);

        if dp = -1023 then begin
          if n64 <> 0 then begin
            dp := -1022;
          end else
            dp := 0;
        end else
          n64 := n64 or (UInt64(1) shl 52);
        n64 := n64 shl (64 - 56);
        if pr < 15 then
          Inc(n64, ((UInt64(8) shl UInt64(56)) shr (pr * 4)));
// add leading chars

{$IF Defined(STB_SPRINTF_MSVC_MODE)}
         s[0] := '0';
         s[1] := 'x';
         Inc(s, 2);
{$ELSE}
         lead[1 + Ord(lead[0])] := '0';
         lead[2 + Ord(lead[0])] := 'x';
         Inc(Byte(lead[0]), 2);
{$ENDIF}
         s^ := h[(n64 shr 60) and 15];
         Inc(s);
         n64 := n64 shl 4;
         if pr <> 0 then begin
           s^ := stbsp__period;
           Inc(s);
         end;
         sn := s;

         // print the bits
         n := pr;
         if n > 13 then
            n := 13;
         if pr > Int32(n) then
            tz := pr - n;
         pr := 0;
         while n > 0 do begin
           Dec(n);
           s^ := h[(n64 shr 60) and 15];
           Inc(s);
           n64 := n64 shl 4;
         end;

         // print the expo
         tail[1] := h[17];
         if dp < 0 then begin
           tail[2] := '-';
           dp := -dp;
         end else
           tail[2] := '+';
         if dp >= 1000 then begin
           n := 6;
         end else if dp >= 100 then begin
           n := 5;
         end else if dp >= 10 then begin
           n := 4;
         end else
           n := 3;
         tail[0] := AnsiChar(n);
         while True do begin
           tail[n] := AnsiChar(Ord('0') + (dp mod 10));
           if n <= 3 then
             break;
           Dec(n);
           dp := dp div 10;
         end;

         dp := PtrInt(s - sn);
         l := PtrInt(s - (num + 64));
         s := num + 64;
         cs := 1 + (3 shl 24);
         goto scopy;
      end;

      'G', // float
      'g': begin // float
        if f[0] = 'G' then begin
          h := @hexu[0];
        end else
          h := @hex[0];
        fv := va_double;
        if pr = -1 then begin
          pr := 6;
        end else if pr = 0 then
          pr := 1; // default is 6
        // read the double into a string
        if stbsp__real_to_str(@sn, @l, num, @dp, fv, (pr - 1) or $80000000) <> 0 then
          fl := fl or STBSP__NEGATIVE;

        // clamp the precision and delete extra zeros after clamp
        n := pr;
        if l > UInt32(pr) then
          l := pr;
        while (l > 1) and (pr <> 0) and (sn[l - 1] = '0') do begin
          Dec(pr);
          Dec(l);
        end;

        // should we use %e
        if (dp <= -4) or (dp > Int32(n)) then begin
          if pr > Int32(l) then begin
            pr := l - 1;
          end else if pr <> 0 then
            Dec(pr); // when using %e, there is one digit before the decimal
          goto doexpfromg;
        end;
        // this is the insane action to get the pr to match %g semantics for %f
        if (dp > 0) then begin
          if dp < Int32(l) then begin
            pr := l - dp;
          end else
            pr := 0;
        end else begin
          if pr > Int32(l) then begin
            pr := -dp + Int32(l);
          end else
            pr := -dp + pr;
        end;
        goto dofloatfromg;
      end;

      'E', // float
      'e': // float
      begin
        if f[0] = 'E' then begin
          h := @hexu[0];
        end else
          h := @hex[0];
        fv := va_double;
        if pr = -1 then
          pr := 6; // default is 6
        // read the double into a string
        if stbsp__real_to_str(@sn, @l, num, @dp, fv, pr or $80000000) <> 0 then
          fl := fl or STBSP__NEGATIVE;
      doexpfromg:
        tail[0] := AnsiChar(0);
        stbsp__lead_sign(fl, @lead[0]);
        if dp = STBSP__SPECIAL then begin
          s := PAnsiChar(sn);
          cs := 0;
          pr := 0;
          goto scopy;
        end;
        s := num + 64;
        // handle leading chars
        s^ := sn[0];
        Inc(s);

        if pr <> 0 then begin
          s^ := stbsp__period;
          Inc(s);
        end;

        // handle after decimal
        if (l - 1) > UInt32(pr) then
          l := pr + 1;
        for n := 1 to l - 1 do begin
          s^ := sn[n];
          Inc(s);
        end;
        // trailing zeros
        tz := pr - (l - 1);
        pr := 0;
        // dump expo
        tail[1] := h[$e];
        Dec(dp, 1);
        if dp < 0 then begin
          tail[2] := '-';
          dp := -dp;
        end else
          tail[2] := '+';
{$IF Defined(STB_SPRINTF_MSVC_MODE)}
        n := 5;
{$ELSE}
        if dp >= 100 then begin
          n := 5;
        end else
          n := 4;
{$ENDIF}
        tail[0] := AnsiChar(n);
        while True do begin
          tail[n] := AnsiChar(Ord('0') + dp mod 10);
          if n <= 3 then
            break;
          Dec(n);
          dp := dp div 10;
        end;
        cs := 1 + (3 shl 24); // how many tens
        goto flt_lead;
      end;

      'f': begin // float
        fv := va_double;
      doafloat:
         // do kilos
         if (fl and STBSP__METRIC_SUFFIX) <> 0 then begin
            divisor := 1000.0;
            if (fl and STBSP__METRIC_1024) <> 0 then
               divisor := 1024.0;
            while fl < $4000000 do begin
               if (fv < divisor) and (fv > -divisor) then
                 break;
               fv := fv / divisor;
               Inc(fl, $1000000);
            end;
         end;
         if pr = -1 then
            pr := 6; // default is 6
         // read the double into a string
         if stbsp__real_to_str(@sn, @l, num, @dp, fv, pr) <> 0 then
            fl := fl or STBSP__NEGATIVE;
      dofloatfromg:
         tail[0] := #0;
         stbsp__lead_sign(fl, @lead[0]);
         if dp = STBSP__SPECIAL then begin
           s := PAnsiChar(sn);
           cs := 0;
           pr := 0;
           goto scopy;
         end;
         s := num + 64;

         // handle the three decimal varieties
         if dp <= 0 then begin
           // handle 0.000*000xxxx
           s^ := '0';
           Inc(s);
           if pr <> 0 then begin
             s^ := stbsp__period;
             Inc(s);
           end;
           n := -dp;
           if Int32(n) > pr then
             n := pr;
           i := n;
           while i <> 0 do begin
             if (PtrUInt(s) and 3) = 0 then
               break;
             s^ := '0';
             Inc(s);
             Dec(i);
           end;
           while i >= 4 do begin
             PUInt32(s)^ := $30303030;
             Inc(s, 4);
             Dec(i, 4);
           end;
           while i <> 0 do begin
             s^ := '0';
             Inc(s);
             Dec(i);
           end;
           if Int32(l + n) > pr then
             l := pr - n;
           i := l;
           while i <> 0 do begin
             s^ := sn^;
             Inc(sn);
             Inc(s);
             Dec(i);
           end;
           tz := pr - (n + l);
           cs := 1 + (3 shl 24); // how many tens did we write (for commas below)
         end else begin
           if (fl and STBSP__TRIPLET_COMMA) <> 0 then begin
             cs := (600 - UInt32(dp)) mod 3;
           end else
             cs := 0;
           if UInt32(dp) >= l then begin
              // handle xxxx000*000.0
              n := 0;
              while True do begin
                Inc(cs);
                if ((fl and STBSP__TRIPLET_COMMA) <> 0) and (cs = 4) then begin
                  cs := 0;
                  s^ := stbsp__comma;
                  Inc(s);
                end else begin
                  s^ := sn[n];
                  Inc(s);
                  Inc(n);
                  if n >= l then
                    break;
                end;
              end;
              if n < UInt32(dp) then begin
                 n := dp - n;
                 if (fl and STBSP__TRIPLET_COMMA) = 0 then begin
                    while n <> 0 do begin
                       if (PtrUInt(s) and 3) = 0 then
                         break;
                       s^ := '0';
                       Inc(s);
                       Dec(n);
                    end;
                    while n >= 4 do begin
                      PUInt32(s)^ := $30303030;
                      Inc(s, 4);
                      Dec(n, 4);
                    end;
                 end;
                 while n <> 0 do begin
                   Inc(cs);
                   if (((fl and STBSP__TRIPLET_COMMA) <> 0) and (cs = 4)) then begin
                     cs := 0;
                     s^ := stbsp__comma;
                     Inc(s);
                   end else begin
                     s^ := '0';
                     Inc(s);
                     Dec(n);
                   end;
                end;
              end;
              cs := PtrUInt(s - (num + 64)) + (3 shl 24); // cs is how many tens
              if pr <> 0 then begin
                s^ := stbsp__period;
                Inc(s);
                tz := pr;
              end;
           end else begin
              // handle xxxxx.xxxx000*000
              n := 0;
              while True do begin
                Inc(cs);
                if ((fl and STBSP__TRIPLET_COMMA) <> 0) and (cs = 4) then begin
                  cs := 0;
                  s^ := stbsp__comma;
                  Inc(s);
                end else begin
                  s^ := sn[n];
                  Inc(s);
                  Inc(n);
                  if n >= UInt32(dp) then
                    break;
                end;
              end;
              cs := PtrUInt(s - (num + 64)) + (3 shl 24); // cs is how many tens
              if pr <> 0 then begin
                s^ := stbsp__period;
                Inc(s);
              end;
              if (l - dp) > UInt32(pr) then
                l := pr + dp;
              while n < l do begin
                s^ := sn[n];
                Inc(s);
                Inc(n);
              end;
              tz := pr - (l - dp);
           end;
         end;
         pr := 0;

         // handle k,m,g,t
         if (fl and STBSP__METRIC_SUFFIX) <> 0 then begin
            idx := 1;
            if (fl and STBSP__METRIC_NOSPACE) <> 0 then
               idx := 0;
            tail[0] := AnsiChar(idx);
            tail[1] := ' ';
            begin
               if (fl shr 24) <> 0 then begin // SI kilo is 'k', JEDEC and SI kibits are 'K'.
                  if (fl and STBSP__METRIC_1024) <> 0 then begin
                    tail[idx + 1] := '_KMGT'[1 + fl shr 24];
                  end else
                    tail[idx + 1] := '_kMGT'[1 + fl shr 24];
                  Inc(idx);
                  // If printing kibits and not in jedec, add the 'i'.
                  if (fl and STBSP__METRIC_1024 <> 0) and (fl and STBSP__METRIC_JEDEC = 0) then begin
                     tail[idx + 1] := 'i';
                     Inc(idx);
                  end;
                  tail[0] := AnsiChar(idx);
               end;
            end;
         end;

      flt_lead:
         // get the length that we copied
         l := UInt32(s - (num + 64));
         s := num + 64;
         goto scopy;
      end;
{$ENDIF}

      'B', // upper binary
      'b': // lower binary
      begin
        if f[0] = 'B' then begin
          h := @hexu[0];
        end else
          h := @hex[0];
        lead[0] := AnsiChar(0);
        if (fl and STBSP__LEADING_0X) <> 0 then begin
          lead[0] := AnsiChar(2);
          lead[1] := '0';
          lead[2] := h[$b];
        end;
        l := (8 shl 4) or (1 shl 8);
        goto radixnum;
      end;

      'o': // octal
      begin
        h := hexu;
        lead[0] := AnsiChar(0);
        if (fl and STBSP__LEADING_0X) <> 0 then begin
          lead[0] := AnsiChar(1);
          lead[1] := '0';
        end;
        l := (3 shl 4) or (3 shl 8);
        goto radixnum;
      end;

      'p', // pointer
      'X', // upper hex
      'x': begin // lower hex
        if f[0] = 'p' then begin
          {$IF SizeOf(Pointer) = 8}
            fl := fl or STBSP__INTMAX;
          {$ENDIF}
          pr := 2 * SizeOf(Pointer);
          fl := fl and not STBSP__LEADINGZERO; // 'p' only prints the pointer with zeros
                                               // fall through - to X
        end;
        if f[0] = 'X' then begin
          h := @hexu[0];
        end else
          h := @hex[0];
        l := (4 shl 4) or (4 shl 8);
        lead[0] := AnsiChar(0);
        if (fl and STBSP__LEADING_0X) <> 0 then begin
          lead[0] := AnsiChar(2);
          lead[1] := '0';
          lead[2] := h[16];
        end;
      radixnum:
         // get the number
         if (fl and STBSP__INTMAX) <> 0 then begin
           n64 := va_uint64;
         end else
           n64 := va_uint32;

         s := num + STBSP__NUMSZ;
         dp := 0;
         // clear tail, and clear leading if value is zero
         tail[0] := AnsiChar(0);
         if n64 = 0 then begin
            lead[0] := AnsiChar(0);
            if pr = 0 then begin
               l := 0;
               cs := (((l shr 4) and 15)) shl 24;
               goto scopy;
            end;
         end;
         // convert to string
         while True do begin
           Dec(s);
           s^ := h[n64 and ((1 shl (l shr 8)) - 1)];
           n64 := n64 shr (l shr 8);
           if not ((n64 <> 0) or (Int32((num + STBSP__NUMSZ) - s) < pr)) then
             break;
           if (fl and STBSP__TRIPLET_COMMA) <> 0 then begin
             Inc(l);
             if (l and 15) = ((l shr 4) and 15) then begin
               l := l and not 15;
               Dec(s);
               s^ := stbsp__comma;
             end;
           end;
         end;
         // get the tens and the comma pos
         cs := UInt32((num + STBSP__NUMSZ) - s) + ((((l shr 4) and 15)) shl 24);
         // get the length that we copied
         l := UInt32((num + STBSP__NUMSZ) - s);
         // copy it
         goto scopy;
      end;

      'u', // unsigned
      'i',
      'd': begin // integer
         // get the integer and abs it
         if (fl and STBSP__INTMAX) <> 0 then begin
           i64 := va_int64;
           n64 := UInt64(i64);
           if (f[0] <> 'u') and (i64 < 0) then begin
             n64 := UInt64(-i64);
             fl := fl or STBSP__NEGATIVE;
           end;
         end else begin
           i := va_int32;
           n64 := UInt32(i);
           if (f[0] <> 'u') and (i < 0) then begin
             n64 := UInt32(-i);
             fl := fl or STBSP__NEGATIVE;
           end;
        end;

{$IF not Defined(STB_SPRINTF_NOFLOAT)}
         if (fl and STBSP__METRIC_SUFFIX) <> 0 then begin
            if n64 < 1024 then begin
              pr := 0;
            end else if pr = -1 then
              pr := 1;
            fv := Double(Int64(n64));
            goto doafloat;
         end;
{$ENDIF}

         // convert to string
         s := @num[0] + STBSP__NUMSZ;
         l := 0;

         while True do begin
           // do in 32-bit chunks (avoid lots of 64-bit divides even with constant denominators)
           o := s - 8;
           if n64 >= 100000000 then begin
             n := UInt32(n64 mod 100000000);
             n64 := n64 div 100000000;
           end else begin
             n := UInt32(n64);
             n64 := 0;
           end;
           if ((fl and STBSP__TRIPLET_COMMA) = 0) then begin
             repeat
               Dec(s, 2);
               PUInt16(s)^ := PUInt16(@stbsp__digitpair.pair[(n mod 100) * 2])^;
               n := n div 100;
             until n = 0;
           end;
           while n <> 0 do begin
             if ((fl and STBSP__TRIPLET_COMMA) <> 0) and (l = 3) then begin
               l := 0;
               Dec(s);
               s^ := stbsp__comma;
               Dec(o);
             end else begin
               Inc(l);
               Dec(s);
               s^ := AnsiChar(n mod 10 + Ord('0'));
               n := n div 10;
             end;
           end;
           if (n64 = 0) then begin
             if (s[0] = '0') and (s <> (num + STBSP__NUMSZ)) then
               Inc(s);
             break;
           end;
           while s <> o do begin
             if ((fl and STBSP__TRIPLET_COMMA) <> 0) and (l = 3) then begin
               l := 0;
               Dec(s);
               s^ := stbsp__comma;
               Dec(o);
             end else begin
               Inc(l);
               Dec(s);
               s^ := '0';
             end;
           end;
         end;

         tail[0] := AnsiChar(0);
         stbsp__lead_sign(fl, @lead[0]);

         // get the length that we copied
         l := UInt32((Pointer(@num[0]) + STBSP__NUMSZ) - Pointer(s));
         if l = 0 then begin
           Dec(s);
           s^ := '0';
           l := 1;
         end;
         cs := l + (3 shl 24);
         if pr < 0 then
            pr := 0;

      scopy:
         // get fw=leading/trailing space, pr=leading zeros
         if pr < Int32(l) then
           pr := l;
         n := pr + Ord(lead[0]) + Ord(tail[0]) + tz;
         if fw < Int32(n) then
           fw := n;
         Dec(fw, n);
         Dec(pr, l);

         // handle right justify and leading zeros
         if (fl and STBSP__LEFTJUST) = 0 then begin
            if (fl and STBSP__LEADINGZERO) <> 0 // if leading zeros, everything is in pr
            then begin
              if fw > pr then begin
                pr := fw;
              end else
                pr := pr;
              fw := 0;
            end else begin
              fl := fl and not STBSP__TRIPLET_COMMA; // if no leading zeros, then no commas
            end;
         end;

         // copy the spaces and/or zeros
         if (fw + pr) <> 0 then begin
            // copy leading spaces (or when doing %8.4d stuff)
            if (fl and STBSP__LEFTJUST) = 0 then begin
              while fw > 0 do begin
                 stbsp__cb_buf_clamp(i, fw);
                 Dec(fw, i);
                 while i <> 0 do begin
                   if (PtrUInt(bf) and 3) = 0 then
                      break;
                   bf^ := ' ';
                   Inc(bf);
                   Dec(i);
                 end;
                 while i >= 4 do begin
                   PUInt32(bf)^ := $20202020;
                   Inc(bf, 4);
                   Dec(i, 4);
                 end;
                 while i <> 0 do begin
                   bf^ := ' ';
                   Inc(bf);
                   Dec(i);
                 end;
                 if stbsp__chk_cb_buf(1) then
                   goto done;
              end;
            end;

            // copy leader
            sn := lead + 1;
            while lead[0] <> AnsiChar(0) do begin
              stbsp__cb_buf_clamp(i, Ord(lead[0]));
              Dec(Byte(lead[0]), i);
              while i <> 0 do begin
                bf^ := sn^;
                Inc(bf);
                Inc(sn);
                Dec(i);
              end;
              if stbsp__chk_cb_buf(1) then
                goto done;
            end;

            // copy leading zeros
            c := cs shr 24;
            cs := cs and $ffffff;
            if (fl and STBSP__TRIPLET_COMMA) <> 0 then begin
              cs := UInt32(c - ((pr + cs) mod (c + 1)));
            end else
              cs := 0;
            while pr > 0 do begin
               stbsp__cb_buf_clamp(i, pr);
               Dec(pr, i);
               if (fl and STBSP__TRIPLET_COMMA) = 0 then begin
                  while i <> 0 do begin
                     if (PtrUInt(bf) and 3) = 0 then
                       break;
                     bf^ := '0';
                     Inc(bf);
                     Dec(i);
                  end;
                  while i >= 4 do begin
                    PUInt32(bf)^ := $30303030;
                    Inc(bf, 4);
                    Dec(i, 4);
                  end;
               end;
               while i <> 0 do begin
                 if ((fl and STBSP__TRIPLET_COMMA) <> 0) and (cs = c) then begin
                   cs := 0;
                   bf^ := stbsp__comma;
                   Inc(bf);
                 end else begin
                   Inc(cs);
                   bf^ := '0';
                   Inc(bf);
                 end;
                 Dec(i);
               end;
               if stbsp__chk_cb_buf(1) then
                 goto done;
            end;
          end;

         // copy leader if there is still one
         sn := (@lead[0]) + 1;
         while lead[0] <> AnsiChar(0) do begin
            stbsp__cb_buf_clamp(i, Ord(lead[0]));
            Dec(Byte(lead[0]), i);
            while i <> 0 do begin
              bf^ := sn^;
              Inc(bf);
              Inc(sn);
              Dec(i);
            end;
            if stbsp__chk_cb_buf(1) then
              goto done;
         end;

         // copy the string
         n := l;
         while n <> 0 do begin
           stbsp__cb_buf_clamp(i, n);
           Dec(n, i);
{$IF Defined(STBSP__UNALIGNED)}
           while i >= 4 do begin
             PUInt32(bf)^ := PUInt32(s)^; // FIXME no volatile here, need R/W barrier?
             Inc(bf, 4);
             Inc(s, 4);
             Dec(i, 4);
           end;
{$ENDIF}
           while i <> 0 do begin
             bf^ := s^;
             Inc(bf);
             Inc(s);
             Dec(i);
           end;
           if stbsp__chk_cb_buf(1) then
             goto done;
         end;

         // copy trailing zeros
         while tz <> 0 do begin
           stbsp__cb_buf_clamp(i, tz);
           Dec(tz, i);
           while i <> 0 do begin
             if (PtrUInt(bf) and 3) = 0 then
               break;
             bf^ := '0';
             Inc(bf);
             Dec(i);
           end;
           while i >= 4 do begin
             PUInt32(bf)^ := $30303030;
             Inc(bf, 4);
             Dec(i, 4);
           end;
           while i <> 0 do begin
             bf^ := '0';
             Inc(bf);
             Dec(i);
           end;
           if stbsp__chk_cb_buf(1) then
             goto done;
         end;

         // copy tail if there is one
         sn := tail + 1;
         while tail[0] <> AnsiChar(0) do begin
           stbsp__cb_buf_clamp(i, Ord(tail[0]));
           Dec(Byte(tail[0]), i);
           while i <> 0 do begin
             bf^ := sn^;
             Inc(bf);
             Inc(sn);
             Dec(i);
           end;
           if stbsp__chk_cb_buf(1) then
             goto done;
         end;

         // handle the left justify
         if (fl and STBSP__LEFTJUST) <> 0 then begin
            if (fw > 0) then begin
               while fw <> 0 do begin
                  stbsp__cb_buf_clamp(i, fw);
                  Dec(fw, i);
                  while i <> 0 do begin
                    if (PtrUInt(bf) and 3) = 0 then
                       break;
                    bf^ := ' ';
                    Inc(bf);
                    Dec(i);
                  end;
                  while (i >= 4) do begin
                     PUInt32(bf)^ := $20202020;
                     Inc(bf, 4);
                     Dec(i, 4);
                  end;
                  while i > 0 do begin
                    Dec(i);
                    bf^ := ' ';
                    Inc(bf);
                  end;
                  if stbsp__chk_cb_buf(1) then
                    goto done;
               end;
            end;
          end;
        end;

      else // unknown, just copy code
         s := num + STBSP__NUMSZ - 1;
         s^ := f[0];
         l := 1;
         fl := 0;
         fw := 0;
         lead[0] := AnsiChar(0);
         tail[0] := AnsiChar(0);
         pr := 0;
         dp := 0;
         cs := 0;
         goto scopy;
      end;
      Inc(f);
   end;
endfmt:

   if callback = nil then begin
     bf^ := AnsiChar(0);
   end else begin
     if stbsp__flush_cb() then
       goto done;
   end;

done:
   Exit(tlen + PtrInt(bf - buf));
end;

// ============================================================================
//   wrapper functions

function stbsp_sprintf(buf: PAnsiChar; fmt: PAnsiChar; const va: array of Const): PtrInt;
begin
  Result := stbsp_vsprintfcb(nil, nil, buf, fmt, va);
end;

type
Pstbsp__context = ^stbsp__context;
stbsp__context = record
  buf: PAnsiChar;
  count: PtrInt;
  length: PtrInt;
  tmp: array[0 .. STB_SPRINTF_MIN - 1] of AnsiChar;
end;

function stbsp__clamp_callback(buf: PAnsiChar; user: Pointer; len: SizeUInt): PAnsiChar;
var
  c: Pstbsp__context;
  s, se, d: PAnsiChar;
begin
  c := Pstbsp__context(user);
  Inc(c^.length, len);

  if len > c^.count then
    len := c^.count;

  if len <> 0 then begin
    if buf <> c^.buf then begin
      d := c^.buf;
      s := buf;
      se := buf + len;
      repeat
         d^ := s^;
         Inc(d);
         Inc(s);
      until not (s < se);
    end;
    Inc(c^.buf, len);
    Dec(c^.count, len);
  end;

  if c^.count <= 0 then
    Exit(c^.tmp);
  if c^.count >= STB_SPRINTF_MIN then begin
    Exit(c^.buf); // go direct into buffer if you can
  end else
    Exit(c^.tmp);
end;

function stbsp__count_clamp_callback(buf: PAnsiChar; user: Pointer; len: SizeUInt): PAnsiChar;
var
  c: Pstbsp__context;
begin
  c := Pstbsp__context(user);
  Inc(c^.length, len);
  Exit(c^.tmp); // go direct into buffer if you can
end;

function stbsp_vsnprintf(buf: PAnsiChar; count: PtrInt; fmt: PAnsiChar; const va: array of Const): PtrInt;
var
  c: stbsp__context;
  l: PtrInt;
begin
  if (count = 0) or (buf = nil) then begin
    c.length := 0;
    stbsp_vsprintfcb(@stbsp__count_clamp_callback, @c, c.tmp, fmt, va);
  end else begin
    c.buf := buf;
    c.count := count;
    c.length := 0;

    stbsp_vsprintfcb(@stbsp__clamp_callback, @c, stbsp__clamp_callback(nil, @c, 0), fmt, va);

    // zero-terminate
    l := PtrInt(c.buf - buf);
    if l >= count then // should never be greater, only equal (or less) than count
      l := count - 1;
    buf[l] := #0;
  end;
  Exit(c.length);
end;

function stbsp_snprintf(buf: PAnsiChar; count: PtrInt; fmt: PAnsiChar; const va: array of Const): PtrInt;
begin
  Result := stbsp_vsnprintf(buf, count, fmt, va);
end;

function stbsp_vsprintf(buf: PAnsiChar; fmt: PAnsiChar; const va: array of Const): PtrInt;
begin
  Result := stbsp_vsprintfcb(nil, nil, buf, fmt, va);
end;

// =======================================================================
//   low level float utility functions

{$IF not Defined(STB_SPRINTF_NOFLOAT)}

// copies d to bits w/ strict aliasing (this compiles to nothing on /Ox)
procedure STBSP__COPYFP(var dest; const src); inline;
var
  cn: PtrInt;
begin
  for cn := 0 to 8 - 1 do
    PByte(@dest)[cn] := PByte(@src)[cn];
end;

// get float info
function stbsp__real_to_parts(bits: PInt64; expo: PInt32; value: Double): Int32;
var
  d: Double;
  b: Int64;
begin
  b := 0;

  // load value and round at the frac_digits
  d := value;

  STBSP__COPYFP(b, d);

  bits^ := b and ((UInt64(1) shl 52) - 1);
  expo^ := Int32(((b shr 52) and 2047) - 1023);

  Exit(Int32(Int64(b) shr 63));
end;

const
stbsp__bot: array[0 .. 23 - 1] of Double = (
   1e+000, 1e+001, 1e+002, 1e+003, 1e+004, 1e+005, 1e+006, 1e+007, 1e+008, 1e+009, 1e+010, 1e+011,
   1e+012, 1e+013, 1e+014, 1e+015, 1e+016, 1e+017, 1e+018, 1e+019, 1e+020, 1e+021, 1e+022
);
stbsp__negbot: array[0 .. 22 - 1] of Double = (
   1e-001, 1e-002, 1e-003, 1e-004, 1e-005, 1e-006, 1e-007, 1e-008, 1e-009, 1e-010, 1e-011,
   1e-012, 1e-013, 1e-014, 1e-015, 1e-016, 1e-017, 1e-018, 1e-019, 1e-020, 1e-021, 1e-022
);
stbsp__negboterr: array[0 .. 22 - 1] of Double = (
   -5.551115123125783e-018,  -2.0816681711721684e-019, -2.0816681711721686e-020,
   -4.7921736023859299e-021, -8.1803053914031305e-022, 4.5251888174113741e-023,
   4.5251888174113739e-024,  -2.0922560830128471e-025, -6.2281591457779853e-026,
   -3.6432197315497743e-027, 6.0503030718060191e-028,  2.0113352370744385e-029,
   -3.0373745563400371e-030, 1.1806906454401013e-032,  -7.7705399876661076e-032,
   2.0902213275965398e-033,  -7.1542424054621921e-034, -7.1542424054621926e-035,
   2.4754073164739869e-036,  5.4846728545790429e-037,  9.2462547772103625e-038,  -4.8596774326570872e-039
);
stbsp__top: array[0 .. 13 - 1] of Double = (
   1e+023, 1e+046, 1e+069, 1e+092, 1e+115, 1e+138, 1e+161, 1e+184, 1e+207, 1e+230, 1e+253, 1e+276, 1e+299
);
stbsp__negtop: array[0 .. 13 - 1] of Double = (
   1e-023, 1e-046, 1e-069, 1e-092, 1e-115, 1e-138, 1e-161, 1e-184, 1e-207, 1e-230, 1e-253, 1e-276, 1e-299
);
stbsp__toperr: array[0 .. 13 - 1] of Double = (
   8388608,
   6.8601809640529717e+028,
   -7.253143638152921e+052,
   -4.3377296974619174e+075,
   -1.5559416129466825e+098,
   -3.2841562489204913e+121,
   -3.7745893248228135e+144,
   -1.7356668416969134e+167,
   -3.8893577551088374e+190,
   -9.9566444326005119e+213,
   6.3641293062232429e+236,
   -5.2069140800249813e+259,
   -5.2504760255204387e+282
);
stbsp__negtoperr: array[0 .. 13 - 1] of Double = (
   3.9565301985100693e-040,  -2.299904345391321e-063,  3.6506201437945798e-086,  1.1875228833981544e-109,
   -5.0644902316928607e-132, -6.7156837247865426e-155, -2.812077463003139e-178,  -5.7778912386589953e-201,
   7.4997100559334532e-224,  -4.6439668915134491e-247, -6.3691100762962136e-270, -9.436808465446358e-293,
   8.0970921678014997e-317
);
stbsp__powten: array[0 .. 20 - 1] of UInt64 = (
   1,
   10,
   100,
   1000,
   10000,
   100000,
   1000000,
   10000000,
   100000000,
   1000000000,
   10000000000,
   100000000000,
   1000000000000,
   10000000000000,
   100000000000000,
   1000000000000000,
   10000000000000000,
   100000000000000000,
   1000000000000000000,
   10000000000000000000
);
stbsp__tento19th: UInt64 = 1000000000000000000;

procedure stbsp__ddmulthi(var oh, ol: Double; const xh, yh: Double); inline;
var
  ahi, alo, bhi, blo: Double;
  bt: UInt64;
begin
  ahi := 0;
  bhi := 0;
  oh := xh * yh;
  STBSP__COPYFP(bt, xh);
  bt := bt and ((not UInt64(0)) shl UInt64(27));
  STBSP__COPYFP(ahi, bt);
  alo := xh - ahi;
  STBSP__COPYFP(bt, yh);
  bt := bt and ((not UInt64(0)) shl UInt64(27));
  STBSP__COPYFP(bhi, bt);
  blo := yh - bhi;
  ol := ((ahi * bhi - oh) + ahi * blo + alo * bhi) + alo * blo;
end;

procedure stbsp__ddtoS64(var ob: Int64; const xh, xl: Double); inline;
var
  ahi, alo, vh, t: Double;
begin
  ahi := 0;
  ob := Int64(Trunc(xh));
  vh := Double(ob);
  ahi := xh - vh;
  t := ahi - xh;
  alo := (xh - (ahi - t)) - (vh + t);
  ob := ob + Trunc(ahi + alo + xl);
end;

procedure stbsp__ddrenorm(var oh, ol: Double); inline;
var
  s: Double;
begin
  s := oh + ol;
  ol := ol - (s - oh);
  oh := s;
end;

procedure stbsp__ddmultlo(const oh: Double; var ol: Double; const xh, xl, yh, yl: Double); inline;
begin
  ol := ol + (xh * yl + xl * yh);
end;

procedure stbsp__ddmultlos(const oh: Double; var ol: Double; const xh, yl: Double); inline;
begin
  ol := ol + (xh * yl);
end;

procedure stbsp__raise_to_power10(ohi, olo: PDouble; d: Double; power: Int32); // power can be -323 to +350
var
  ph, pl: Double;
  e, et, eb: Int32;
  p2h, p2l: Double;
begin
  if (power >= 0) and (power <= 22) then begin
    stbsp__ddmulthi(ph, pl, d, stbsp__bot[power]);
  end else begin
    e := power;
    if power < 0 then
      e := -e;
    et := (e * $2c9) shr 14; // %23
    if et > 13 then
      et := 13;
    eb := e - (et * 23);

    ph := d;
    pl := 0.0;
    if power < 0 then begin
      if eb <> 0 then begin
        Dec(eb);
        stbsp__ddmulthi(ph, pl, d, stbsp__negbot[eb]);
        stbsp__ddmultlos(ph, pl, d, stbsp__negboterr[eb]);
      end;
      if et <> 0 then begin
        stbsp__ddrenorm(ph, pl);
        Dec(et);
        stbsp__ddmulthi(p2h, p2l, ph, stbsp__negtop[et]);
        stbsp__ddmultlo(p2h, p2l, ph, pl, stbsp__negtop[et], stbsp__negtoperr[et]);
        ph := p2h;
        pl := p2l;
      end;
    end else begin
      if eb <> 0 then begin
        e := eb;
        if eb > 22 then
          eb := 22;
        Dec(e, eb);
        stbsp__ddmulthi(ph, pl, d, stbsp__bot[eb]);
        if e <> 0 then begin
          stbsp__ddrenorm(ph, pl);
          stbsp__ddmulthi(p2h, p2l, ph, stbsp__bot[e]);
          stbsp__ddmultlos(p2h, p2l, stbsp__bot[e], pl);
          ph := p2h;
          pl := p2l;
        end;
      end;
      if et <> 0 then begin
         stbsp__ddrenorm(ph, pl);
         Dec(et);
         stbsp__ddmulthi(p2h, p2l, ph, stbsp__top[et]);
         stbsp__ddmultlo(p2h, p2l, ph, pl, stbsp__top[et], stbsp__toperr[et]);
         ph := p2h;
         pl := p2l;
      end;
    end;
  end;
  stbsp__ddrenorm(ph, pl);
  ohi^ := ph;
  olo^ := pl;
end;

// given a float value, returns the significant bits in bits, and the position
// of the decimal point in decimal_pos.  +/-INF and NAN are specified by
// special values returned in the decimal_pos parameter.
// frac_digits is absolute normally, but if you want from first significant
// digits (got %g and %e), or in 0x80000000
function stbsp__real_to_str(start: PPAnsiChar; len: PUInt32; _out: PAnsiChar; decimal_pos: PInt32; value: Double; frac_digits: UInt32): Int32;
label
  noround, donez;
var
  d: Double;
  bits: Int64;
  expo, e, ng, tens: Int32;
  v: Int64;
  ph, pl: Double;
  dg: UInt32;
  r: UInt64;
  n: UInt32;
  o: PAnsiChar;
begin
  bits := 0;

  d := value;
  STBSP__COPYFP(bits, d);
  expo := Int32((bits shr 52) and 2047);
  ng := Int32(UInt64(bits) shr UInt64(63));
  if ng <> 0 then
    d := -d;

  if expo = 2047 then begin // is nan or inf?
    if (bits and ((UInt64(1) shl 52) - 1)) <> 0 then begin
      start^ := 'NaN';
      // FIXME for some reason 0.0/0.0 gives ng=1
      ng := 1 - ng;
    end else
      start^ := 'Inf';
    decimal_pos^ := STBSP__SPECIAL;
    len^ := 3;
    Exit(ng);
  end;

  if expo = 0 then begin// is zero or denormal
    if (UInt64(bits) shl 1) = 0 then begin // do zero
      decimal_pos^ := 1;
      start^ := _out;
      _out[0] := '0';
      len^ := 1;
      Exit(ng);
    end;
    // find the right expo for denormals
    begin
      v := UInt64(1) shl 51;
      while (bits and v) = 0 do begin
        Dec(expo);
        v := v shr 1;
      end;
    end;
  end;

  // find the decimal exponent as well as the decimal bits of the value
  begin
    // log10 estimate - very specifically tweaked to hit or undershoot by no more than 1 of log10 of all expos 1..2046
    tens := expo - 1023;
    if tens < 0 then begin
      tens := (tens * 617) div 2048;
    end else
      tens := ((tens * 1233) div 4096) + 1;

    // move the significant bits into position and stick them into an int
    stbsp__raise_to_power10(@ph, @pl, d, 18 - tens);

    // get full as much precision from double-double as possible
    stbsp__ddtoS64(bits, ph, pl);

    // check if we undershot
    if UInt64(bits) >= stbsp__tento19th then
      Inc(tens);
  end;

  // now do the rounding in integer land
  if (frac_digits and $80000000) <> 0 then begin
    frac_digits := (frac_digits and $7ffffff) + 1;
  end else
    frac_digits := tens + frac_digits;
  if frac_digits < 24 then begin
     dg := 1;
     if UInt64(bits) >= stbsp__powten[9] then
       dg := 10;
     while UInt64(bits) >= stbsp__powten[dg] do begin
       Inc(dg);
       if dg = 20 then
         goto noround;
     end;
     if frac_digits < dg then begin
       // add 0.5 at the right position and round
       e := dg - frac_digits;
       if UInt32(e) >= 24 then
         goto noround;
       r := stbsp__powten[e];
       bits := bits + (r div 2);
       if UInt64(bits) >= stbsp__powten[dg] then
         Inc(tens);
       bits := bits div r;
     end;
  noround: ;
  end;

  // kill long trailing runs of zeros
  if bits <> 0 then begin
    while True do begin
      if bits <= $ffffffff then
        break;
      if (bits mod 1000) <> 0 then
        goto donez;
      bits := bits div 1000;
    end;
    n := UInt32(bits);
    while (n mod 1000) = 0 do
      n := n div 1000;
    bits := n;
  donez: ;
  end;

  // convert to string
  Inc(_out, 64);
  e := 0;
  while True do begin
     o := _out - 8;
     // do the conversion in chunks of U32s (avoid most 64-bit divides, worth it, constant denomiators be damned)
     if bits >= 100000000 then begin
       n := UInt32(bits mod 100000000);
       bits := bits div 100000000;
     end else begin
       n := UInt32(bits);
       bits := 0;
     end;
     while n <> 0 do begin
       Dec(_out, 2);
       PUInt16(_out)^ := PUInt16(@stbsp__digitpair.pair[(n mod 100) * 2])^;
       n := n div 100;
       Inc(e, 2);
     end;
     if bits = 0 then begin
       if (e <> 0) and (_out[0] = '0') then begin
         Inc(_out);
         Dec(e);
       end;
       break;
     end;
     while _out <> o do begin
       Dec(_out);
       _out^ := '0';
       Inc(e);
     end;
  end;

  decimal_pos^ := tens;
  start^ := _out;
  len^ := e;
  Exit(ng);
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
