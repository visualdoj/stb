{$MODE OBJFPC}
{$MODESWITCH DEFAULTPARAMETERS}
{$MODESWITCH OUT}
{$MODESWITCH RESULT}

uses
  strings,
  stb_sprintf in '../stb_sprintf.pas';

{$DEFINE USE_STB}

const
  __STDC_VERSION__ = 0;
  _MSC_VER = 0;
  NaN = + 0.0 / 0.0;
  Infinity = + 1.0 / 0.0;

var
  Verbose: Boolean = False;
function CHECKRES(buf, str: PAnsiChar; ret: PtrInt): Boolean;
begin
  if (StrComp(buf, str) <> 0) or (ret <> StrLen(str)) then begin
    Writeln('< ', str);
    Writeln('> ', buf);
    Exit(False);
  end else begin
    if Verbose then
      Writeln(stderr, 'PASSED "', str, '"');
    Exit(True);
  end;
end;

var
  AllOK: Boolean = True;
  buf: array[0 .. 1024 - 1] of AnsiChar;
procedure CHECK(canon, fmt: PAnsiChar; const va: array of Const);
begin
  if not CHECKRES(@buf[0], canon, stbsp_sprintf(@buf[0], fmt, va)) then
    AllOK := False;
end;
procedure CHECKN(size: SizeUInt; fmt: PAnsiChar; const va: array of Const; ret: SizeUInt);
var
  got: SizeUInt;
begin
  got := stbsp_snprintf(@buf[0], size, fmt, va);
  if got <> ret then begin
    Writeln('FAILED: length must be ', ret, ', got ', got, ' for ', fmt);
    AllOK := False;
  end;
end;
procedure CHECKBUF(canon: PAnsiChar);
begin
  if not CHECKRES(@buf[0], canon, StrLen(canon)) then
    AllOK := False;
end;

procedure PrintBitsOfDouble(D: Double);
var
  R: packed record
    D: Double;
    U: array[0 .. 8 - 1] of Byte;
  end;
  I: LongInt;
begin
  R.D := D;
  for I := 0 to 8 - 1 do
    Write(BinStr(R.U[I], 8));
  Writeln;
end;

const
  pow_2_75: Double = 37778931862957161709568.0;
  pow_2_85: Double = 38685626227668133590597632.0;
var
  n: PtrInt = 0;
begin
  if ParamStr(1) = '-v' then
    Verbose := True;

  // integers
  CHECK('', '', []);
  CHECK('hello world!', 'hello world!', [1]);
  CHECK('a', '%c', ['a']);
  CHECK('bb', '%s', ['bb']);
  CHECK('1', '%d', [1]);
  CHECK('a b     1', '%c %s     %d', ['a', 'b', 1]);
  CHECK('abc     ', '%-8.3s', ['abcdefgh']);
  CHECK('+5', '%+2d', [5]);
  CHECK('  6', '% 3i', [6]);
  CHECK('-7  ', '%-4d', [-7]);
  CHECK('+0', '%+d', [0]);
  CHECK('     00003:     00004', '%10.5d:%10.5d', [3, 4]);
  CHECK('-100006789', '%d', [-100006789]);
  CHECK('20 0020', '%u %04u', [20, 20]);
  CHECK('12 1e 3C', '%o %x %X', [10, 30, 60]);
  CHECK(' 12 1e 3C ', '%3o %2x %-3X', [10, 30, 60]);
  CHECK('012 0x1e 0X3C', '%#o %#x %#X', [10, 30, 60]);
  CHECK('', '%.0x', [0]);
{$IF Defined(USE_STB)}
  CHECK('0', '%.0d', [0]);  // stb_sprintf gives '0'
{$ELSE}
  CHECK('',  '%.0d', [0]);  // glibc gives '' as specified by C99(?)
{$ENDIF}
  CHECK('33 555', '%hi %ld', [33, 555]);
{$IF (not Defined(_MSC_VER)) or (_MSC_VER >= 1600)}
  CHECK('9888777666', '%llu', [9888777666]);
{$ENDIF}
  CHECK('-1 2 -3', '%ji %zi %ti', [PtrInt(-1), SizeInt(2), PtrInt(-3)]);

   // floating-point numbers
  CHECK('-3.000000', '%f', [-3.0]);
  CHECK('-8.8888888800', '%.10f', [-8.88888888]);
  // Halt(0);
  CHECK('880.0888888800', '%.10f', [880.08888888]);
  CHECK('4.1', '%.1f', [4.1]);
  CHECK(' 0', '% .0f', [0.1]);
  CHECK('0.00', '%.2f', [1e-4]);
  CHECK('-5.20', '%+4.2f', [-5.2]);
  CHECK('0.0       ', '%-10.1f', [0.0]);
  CHECK('-0.000000', '%f', [-0.0]);
  CHECK('0.000001', '%f', [9.09834e-07]);
{$IF Defined(USE_STB)}  // rounding differences
  CHECK('38685626227668133600000000.0', '%.1f', [pow_2_85]);
  CHECK('0.000000499999999999999978', '%.24f', [5e-7]);
{$ELSE}
  CHECK('38685626227668133590597632.0', '%.1f', [pow_2_85]); // exact
  CHECK('0.000000499999999999999977', '%.24f', [5e-7]);
{$ENDIF}
  CHECK('0.000000000000000020000000', '%.24f', [2e-17]);
  CHECK('0.0000000100 100000000', '%.10f %.0f', [1e-8, 1e+8]);
  CHECK('100056789.0', '%.1f', [100056789.0]);
  CHECK(' 1.23 %', '%*.*f %%', [5, 2, 1.23]);
  CHECK('-3.000000e+00', '%e', [-3.0]);
  CHECK('4.1E+00', '%.1E', [4.1]);
  CHECK('-5.20e+00', '%+4.2e', [-5.2]);
  CHECK('+0.3 -3', '%+g %+g', [0.3, -3.0]);
  CHECK('4', '%.1G', [4.1]);
  CHECK('-5.2', '%+4.2g', [-5.2]);
  CHECK('3e-300', '%g', [3e-300]);
  CHECK('1', '%.0g', [1.2]);
  CHECK(' 3.7 3.71', '% .3g %.3g', [3.704, 3.706]);
  CHECK('2e-315:1e+308', '%g:%g', [2e-315, 1e+308]);

//{$IF __STDC_VERSION__ >= 199901}
 {$IF Defined(USE_STB)}
  CHECK('Inf Inf NaN', '%g %G %f', [Infinity, Infinity, NaN]);
  CHECK('N', '%.1g', [NaN]);
 {$ELSE}
  CHECK('inf INF nan', '%g %G %f', [Infinity, Infinity, NaN]);
  CHECK('nan', '%.1g', [NaN]);
 {$ENDIF}
//{$ENDIF}

   // %n
  CHECK('aaa ', '%.3s %n', ['aaaaaaaaaaaaa', @n]);
  assert(n = 4);

{$IF __STDC_VERSION__ >= 199901}
   // hex floats
  // CHECK('0x1.fedcbap+98', '%a', [0x1.fedcbap+98]); // TODO
  CHECK('0x1.999999999999a0p-4', '%.14a', [0.1]);
  // CHECK('0x1.0p-1022', '%.1a', [0x1.ffp-1023]); // TODO
 {$IF Defined(USE_STB)} // difference in default precision and x vs X for %A
  CHECK('0x1.009117p-1022', '%a', [2.23e-308]);
  CHECK('-0x1.AB0P-5', '%.3A', [-0x1.abp-5]);
 {$ELSE}
  CHECK('0x1.0091177587f83p-1022', '%a', [2.23e-308]);
  CHECK('-0X1.AB0P-5', '%.3A', [-0X1.abp-5]);
 {$ENDIF}
{$ENDIF}

   // %p
{$IF Defined(USE_STB)}
 {$IF Defined(CPU32)}
  CHECK('00000000', '%p', [nil]);
 {$ELSE}
  CHECK('0000000000000000', '%p', [nil]);
 {$ENDIF}
{$ELSE}
  CHECK('(nil)', '%p', [nil]);
{$ENDIF}

  // snprintf
  CHECKN(100, ' %s     %d',  ['b', 123], 10);
  CHECKBUF(' b     123');
  CHECKN(100, '%f', [pow_2_75], 30);
  if CompareByte(buf[0], '37778931862957161709568.000000'[1], 17) <> 0 then begin
    Writeln('FAILED first 17 letters of pow_2_75: ', buf);
  end;
  CHECKN(10, 'number %f', [123.456789], 17); // written vs would-be written bytes
  CHECKBUF('number 12');
  CHECKN(0, '7 chars', [], 7);
  // stb_sprintf uses internal buffer of 512 chars - test longer string
  if (stbsp_sprintf(@buf[0], '%d  %600s', [3, 'abc']) <> 603) or
     (StrLen(@buf[0]) <> 603) then begin
    Writeln('FAILED string of length 603');
  end;
  if (stbsp_snprintf(@buf[0], 550, '%d  %600s', [3, 'abc']) = 0) or
     (StrLen(@buf[0]) <> 549) then begin
    Writeln('FAILED string of length 549');
  end;
  if stbsp_snprintf(@buf[0], 600, '%510s     %c', ['a', 'b']) <> 516 then
    Writeln('FAILED string of length 516');

  // length check
  if stbsp_snprintf(nil, 0, ' %s     %d', ['b', 123]) <> 10 then
    Writeln('FAILED test for nil buffer');

  // ' modifier. Non-standard, but supported by glibc.
{$IF not Defined(USE_STB)}
  // setlocale(LC_NUMERIC, '');  // C locale does not group digits
{$ENDIF}
  CHECK('1,200,000', '%''d', [1200000]);
  CHECK('-100,006,789', '%''d', [-100006789]);
{$IF Defined(_MSC_VER) or (_MSC_VER >= 1600)}
  CHECK('9,888,777,666', '%''lld', [9888777666]);
{$ENDIF}
  CHECK('200,000,000.000000', '%''18f', [2e8]);
  CHECK('100,056,789', '%''.0f', [100056789.0]);
  CHECK('100,056,789.0', '%''.1f', [100056789.0]);
{$IF Defined(USE_STB)} // difference in leading zeros
  CHECK('000,001,200,000', '%''015d', [1200000]);
{$ELSE}
  CHECK('0000001,200,000', '%''015d', [1200000]);
{$ENDIF}

   // things not supported by glibc
{$IF Defined(USE_STB)}
  CHECK('null', '%s', [PAnsiChar(nil)]);
  CHECK('123,4abc:', '%''x:', [$1234ABC]);
  CHECK('100000000', '%b', [256]);
  CHECK('0b10 0B11', '%#b %#B', [2, 3]);
{$IF (not Defined(_MSC_VER)) or (_MSC_VER >= 1600)}
  CHECK('2 3 4', '%I64d %I32d %Id', [2, 3, 4]);
{$ENDIF}
  CHECK('1k 2.54 M', '%$_d %$.2d', [1000, 2536000]);
  CHECK('2.42 Mi 2.4 M', '%$$.2d %$$$d', [2536000, 2536000]);

  // different separators
  stbsp_set_separators(' ', ',');
  CHECK('12 345,678900', '%''f', [12345.6789]);
{$ENDIF}

  if not AllOK then
    Halt(1);
end.
