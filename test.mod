(********************************************************)
(* Program shows ASCII codes                            *)
(* Compilation:                                         *)
(*   m2c -all test.mod -o test                          *)
(* Running:                                             *)
(*   ./test                                             *)
(********************************************************)
MODULE test;

FROM InOut IMPORT Write, WriteCard, WriteString, WriteLn;
CONST
  FromAscii = 32;
  ToAscii = 127;
VAR
  i : CARDINAL;
  fl : REAL;
  t : ARRAY[1 .. 10] OF CARDINAL;
  d : RECORD
       year, month : CARDINAL;
       day : CARDINAL;
  END;

  PROCEDURE ListAscii(StartCode, EndCode: CARDINAL; Precision: CARDINAL);
  VAR
       i: CARDINAL;
       t1 : ARRAY[1 .. 10] OF CARDINAL;
       d : RECORD
	    year:            CARDINAL;
	    month, day     : CARDINAL;
       END;
BEGIN
       WriteString("ASCII codes");
       WriteLn;
       FOR i := FromAscii TO ToAscii DO
	    WriteCard(i, 3);
	    Write(' ');
	    Write(CHR(i));
	    WriteLn
       END;
       t1[0] := t[0];
       d.year := 2018
  END ListAscii;
       
BEGIN
  fl := 1.1 + 1.0E-2 + 1.0E+2 + 1.0E1; (* real numbers *)
  IF (fl <= 11.11) AND (fl >= 1.111E1) THEN
    WriteString("As expected!")
  ELSE
    WriteString("Gosh!")
  END;
  WriteLn;
  i := 1;
  WHILE i < 5 DO
       WriteLn(i); i := i + 1
  END;
  REPEAT
       WriteLn(i); i := i - 1
  UNTIL i = 1;
  LOOP
       WriteLn("Spam")
  END;
  CASE CHR(FromAscii+16) OF
       '0': WriteLn("Aha!")
     | 'A','a': Writeln("Yes?")
  ELSE
       Writeln("O!")
  END;
  t[10] := 10;
  FOR i := 9 DOWNTO 1 DO t[i] := t[i+1] * i * i END;
  d.year := 2018; d.day := 1;
  d.month := d.day * 10
END test.
