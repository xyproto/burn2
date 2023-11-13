UNIT BurnIt;

INTERFACE

TYPE
   sp = ^String;
   ip = ^Integer;
   wp = ^Word;
   bp = ^Byte;

CONST
   system1 = 0;
   system2 = 1;
   system3 = 2;
   vstring = 'B U R N';
   prov = ' - where do you want your pixels today?';
   prompt = 'burn>';
   my_edress = 'int19h@online.no';
   initcx = 169; { 24*7+1 }
   initcy = 29; { 4*7+1 }
   initpx = 15;
   initpy = 15;

VAR
   g_linje_teller : Byte;
   cbreak : Boolean;
   MoreMem : Array [0..1023] of Byte absolute $a000:64000;
   theimage : Array [0..63999] of byte;
   thepal : Array [0..768] of Byte;
   lasterror : Byte;
   bilde : Boolean;
   palette : Boolean;
   bc : Integer;
   first_color : Byte;
   first_done : Boolean;
   nofade : Boolean;
   acticolx, acticoly : Byte;
   CursorX, CursorY : Byte;
   build : Word;
   n, xpos, ypos, heading : Integer;
   o1,o2 : Byte;
   framebase : String[8];

PROCEDURE Shittyblurall(hm : Byte);
PROCEDURE RotateRight(saymore : Boolean);
PROCEDURE MovePictureUp;
PROCEDURE MovePictureLeft;
PROCEDURE MovePictureRight;
PROCEDURE MovePictureDown;
PROCEDURE NextColor(b : Boolean);
PROCEDURE PrevColor(b : Boolean);
PROCEDURE SetColor(n : Byte; b : Boolean);
PROCEDURE Plot;
PROCEDURE DistortAll(w : Word);
PROCEDURE Forward(lengde : Byte; b : Boolean);
PROCEDURE Backward(lengde : Byte; b : Boolean);
PROCEDURE Turnleft(b : Boolean);
PROCEDURE Turnright(b : Boolean);
PROCEDURE DrawPlot;
PROCEDURE RGBinfo(x : Word;y,c : Byte);
PROCEDURE SetVar(cmds : String;v : ip;what : String;range : boolean;min : Byte;max : Word);
PROCEDURE Fill(startx : Word; starty,c : Byte; gradient,cool : boolean);
FUNCTION FindCorner(startx,starty : Integer; x,y : Ip; c, opp, left : Byte) : Boolean;
PROCEDURE Circle(x,y,xr,yr,c : Byte);
FUNCTION SaveICO(filename : String; transe : Boolean) : Boolean;
FUNCTION SaveBMP(filename : String) : Boolean;
PROCEDURE Save(cmds : String);
FUNCTION GetVerS : String;
FUNCTION GetVer : String;
PROCEDURE PLinje(x1,y1,x2,y2,c : Byte);
PROCEDURE Linje(x1,y1,x2,y2 : Word; c : Byte);
PROCEDURE MsgBox(s : String; h : Byte; w : Boolean);
PROCEDURE GetK(k : wp);
PROCEDURE SkyvSkjermOpp;
PROCEDURE SkrivXY(s : String; x2 : Word; y : Byte; c : Word; pp : Boolean);
FUNCTION LesXY(x,y,c : Word) : String;
PROCEDURE SkrivLn(s : String; c : Word);
FUNCTION LesLn(c : Word) : String;
FUNCTION Les(c : Word) : String;
PROCEDURE Skriv(s : String; c : Word);
PROCEDURE SwapAllColors(nr1, nr2 : Byte);
PROCEDURE SelectSortLeft(x,y : Byte);
PROCEDURE SelectSortRight(x,y : Byte);
PROCEDURE SelectKillSortRemoveReallyDarkAll(x,y : Byte);
FUNCTION SavePCX(filename : String) : Boolean;
FUNCTION GetActiveColor : Byte;
PROCEDURE SelectInvert(x,y : Byte);
PROCEDURE SelectRemoveReallyDark(x,y : Byte);
PROCEDURE SelectKill(x,y : Byte);
PROCEDURE SelectionOn(x,y : Byte);
PROCEDURE FadeReady;
PROCEDURE WhiteFadeReady;
PROCEDURE SelectMono(x,y : Byte);
PROCEDURE ClearKb;
PROCEDURE SelectColorGradient(x,y : Byte);
PROCEDURE SelectBright(x,y : Byte);
PROCEDURE SelectDark(x,y : Byte);
PROCEDURE Bilde2MoreMem;
PROCEDURE MoreMem2Bilde;
PROCEDURE SelectMoreRed(x,y : Byte);
PROCEDURE SelectMoreGreen(x,y : Byte);
PROCEDURE SelectMoreBlue(x,y : Byte);
PROCEDURE SelectLessRed(x,y : Byte);
PROCEDURE SelectLessGreen(x,y : Byte);
PROCEDURE SelectLessBlue(x,y : Byte);
PROCEDURE Letter(xs : Word;ys : Byte;c : Char;co : Byte);
PROCEDURE TegnRuter;
PROCEDURE TegnThumb;
PROCEDURE SetColorGradient(c1,c2 : Byte);
PROCEDURE MonoPalette;
PROCEDURE DefaultPalette;
FUNCTION HvorMangeForover(x,y : Byte) : Byte;
PROCEDURE FjernDuplikatfarger;
FUNCTION FinnesDenneFargen(r,g,b,m : Byte) : Integer;
PROCEDURE TegnPalett;
PROCEDURE Pswap(x1,x2,y1,y2 : Byte);
PROCEDURE Echo(cmds : String);
FUNCTION Right(s : String; l : Byte) : String;
PROCEDURE Randomerror(cmds : String);
PROCEDURE SetPalette;
PROCEDURE GetPalette;
PROCEDURE Cursor(x : Word; y : Byte; pal : Boolean);
FUNCTION FinnLedigFarge : Byte;
PROCEDURE Remove(cmds : String);
FUNCTION Opph(a : Longint; b : Word) : Longint;
FUNCTION nTrim(t : String) : String;
FUNCTION SuperTrim(s : String) : String;
PROCEDURE Typefile(cmds : String);
PROCEDURE P(x,y,c : Byte);
FUNCTION G(x,y : Byte) : Byte;
PROCEDURE DrawImage;
PROCEDURE WFK;
PROCEDURE NextPage;
FUNCTION CapsOn : Boolean;
FUNCTION ShiftOn : Boolean;
FUNCTION CtrlOn : Boolean;
PROCEDURE LinjeFarge(y,lengde,c : Byte);
FUNCTION GetCursorY : Byte;
FUNCTION Fsize(fn : sp) : Longint;
FUNCTION Finnes(fn : sp) : Boolean;
FUNCTION GetParam(s : sp) : String;
PROCEDURE SetCaps;
PROCEDURE KillCaps;
PROCEDURE Txtcls;
PROCEDURE Cls;
PROCEDURE Graphmode;
PROCEDURE Textmode;
FUNCTION d(s2 : String) : Longint;
FUNCTION h(w : Word) : String;

IMPLEMENTATION

PROCEDURE Shittyblurall(hm : Byte);
VAR
	x, y : Byte;
   c : Word;
   xi, yi : Byte;
BEGIN
   Bilde2MoreMem;
   FOR x := 0 TO 31 DO BEGIN
      FOR y := 0 TO 31 DO BEGIN
         { denne kan sikkert optimiseres litt }
         xi := x;
         yi := y;
         c := MoreMem[yi shl 5+xi];
         IF x = 0 THEN xi := 32;
         IF y = 0 THEN yi := 32;
	      c := c + MoreMem[(yi-1) shl 5+(xi-1)];
	      c := c + MoreMem[(yi-1) shl 5+x];
	      c := c + MoreMem[y shl 5+(xi-1)];
         xi := x+1;
         IF x = 31 THEN xi := 0;
	      c := c + MoreMem[(yi-1) shl 5+xi];
	      c := c + MoreMem[y shl 5+xi];
         yi := y+1;
         IF y = 31 THEN yi := 0;
	      c := c + MoreMem[yi shl 5+xi];
	      c := c + MoreMem[yi shl 5+x];
         xi := x;
         yi := y;
         IF x = 0 THEN xi := 31 ELSE Dec(xi);
         IF y = 31 THEN yi := 0 ELSE Inc(yi);
	      c := c + MoreMem[yi shl 5+xi];
         c := (c div (9-hm))+hm;
         IF c < 3 THEN c := 3;
         P(x,y,c);
	   END;
   END;
END;

PROCEDURE RotateRight(saymore : Boolean);
VAR
   x,y : Byte;
   i : Word;
BEGIN
   IF saymore THEN Writeln;
	IF bilde THEN BEGIN
      Bilde2MoreMem;
	   i := 0;
	   FOR x := 0 TO 31 DO BEGIN
	      FOR y := 0 TO 31 DO BEGIN
	         P(x,y,MoreMem[i]);
	         Inc(i);
	      END;
	   END;
      IF saymore THEN Writeln('Done. Your image has been floipped.');
	END ELSE IF saymore THEN Writeln('You must load an image before you can floipp it.');
	IF saymore THEN Writeln;
END;

PROCEDURE MovePictureUp;
VAR
   x,y : Byte;
BEGIN
   IF bilde THEN BEGIN
	   FOR y := 0 TO 31 DO BEGIN
	      MoreMem[y] := G(y,0);
	      FOR x := 31 DOWNTO 0 DO BEGIN
	         MoreMem[x shl 5+y] := G(y,x);
	         P(y,x,MoreMem[(x+1) shl 5+y]);
	      END;
	      P(y,31,MoreMem[y]);
	   END;
   END;
END;

PROCEDURE MovePictureLeft;
VAR
   x,y : Byte;
BEGIN
   IF bilde THEN BEGIN
	   FOR y := 0 TO 31 DO BEGIN
	      MoreMem[y shl 5] := G(0,y);
	      FOR x := 31 DOWNTO 0 DO BEGIN
	         MoreMem[y*32+x] := G(x,y);
	         P(x,y,MoreMem[y shl 5+x+1]);
	      END;
	      P(31,y,MoreMem[y shl 5]);
	   END;
   END;
END;

PROCEDURE MovePictureRight;
VAR
   x,y : Byte;
BEGIN
   IF bilde THEN BEGIN
	   FOR y := 0 TO 31 DO BEGIN
	      MoreMem[y shl 5] := G(0,y);
	      FOR x := 1 TO 31 DO BEGIN
	         MoreMem[y shl 5+x] := G(x,y);
	         P(x,y,MoreMem[y shl 5+x-1]);
	      END;
	      P(0,y,MoreMem[y shl 5+31]);
	   END;
	END;
END;

PROCEDURE MovePictureDown;
VAR
   x,y : Byte;
BEGIN
   IF bilde THEN BEGIN
	   FOR y := 0 TO 31 DO BEGIN
	      MoreMem[y] := G(y,0);
	      FOR x := 1 TO 31 DO BEGIN
	         MoreMem[x shl 5+y] := G(y,x);
	         P(y,x,MoreMem[(x-1) shl 5+y]);
	      END;
	      P(y,0,MoreMem[31 shl 5+y]);
	   END;
	END;
END;

PROCEDURE NextColor(b : Boolean);
BEGIN
   Cursor(acticolx,acticoly,true);
   Inc(acticolx,7);
   IF acticolx > (7*31)+1 THEN BEGIN
      acticolx := 1;
      Inc(acticoly,7);
      IF acticoly > (7*7)+1 THEN acticoly := 1;
   END;
   Cursor(acticolx,acticoly,true);
   DrawImage;
   IF b THEN BEGIN
      Writeln;
      Writeln('Done. Next color was chosen.');
      Writeln;
   END;
END;

PROCEDURE PrevColor(b : Boolean);
BEGIN
   Cursor(acticolx,acticoly,true);
   IF acticolx < 2 THEN BEGIN
      acticolx := 225;   {7*32+1 = 225}
      IF acticoly < 2 THEN acticoly := 57;   {7*8+1}
      Dec(acticoly,7);
   END;
   Dec(acticolx,7);
   Cursor(acticolx,acticoly,true);
   DrawImage;
   IF b THEN BEGIN
      Writeln;
      Writeln('Done. Previous color was chosen.');
      Writeln;
   END;
END;

PROCEDURE SetColor(n : Byte; b : Boolean);
BEGIN
   Cursor(acticolx,acticoly,true);
   acticolx := (n mod 32)*7+1;
   acticoly := (n div 32)*7+1;
   Cursor(acticolx,acticoly,true);
   DrawImage;
   IF b THEN Writeln('Color number ',n,' was chosen.');
END;

PROCEDURE Plot;
BEGIN
   IF (G(xpos,ypos) <> bc) and (G(xpos,ypos) = GetActiveColor) THEN BEGIN
      P(xpos,ypos,bc);
      bilde := true;
   END ELSE BEGIN
      P(xpos,ypos,GetActiveColor);
      bilde := true;
   END;
END;

PROCEDURE DistortAll(w : Word);
VAR
   x,y,i : Byte;
   c : Word;
   ox, oy : Byte;
BEGIN
   ox := xpos;
   oy := ypos;
   x := 0;
   y := 0;
   FOR c := 0 TO w DO BEGIN
      FOR i := 0 TO 15 DO BEGIN
         xpos := i;
         ypos := i;
         Repeat
            Inc(xpos);
            Pswap(x,xpos,y,ypos);
            x := xpos;
            y := ypos;
         UNTIL xpos = (31-i);
         Repeat
            Inc(ypos);
            Pswap(x,xpos,y,ypos);
            x := xpos;
            y := ypos;
         UNTIL ypos = (31-i);
         Repeat
            Dec(xpos);
            Pswap(x,xpos,y,ypos);
            x := xpos;
            y := ypos;
         UNTIL xpos = i;
         Repeat
            Dec(ypos);
            Pswap(x,xpos,y,ypos);
            x := xpos;
            y := ypos;
         UNTIL ypos = i;
      END;
   END;
   xpos := ox;
   ypos := oy;
END;

PROCEDURE Forward(lengde : Byte; b : Boolean);
VAR
   by : Byte;
   rxpos, rypos : Real;
   husk : Boolean;
BEGIN
   IF heading = 0 THEN BEGIN
	   heading := 360;
      husk := true;
   END ELSE
      husk := false;
   IF (lengde = 1) THEN BEGIN
      Dec(ypos,Trunc(sin(heading/57.29)*100) div 50);
      Inc(xpos,Trunc(cos(heading/57.29)*100) div 50);
      IF ypos > 31 THEN ypos := 0;
      IF ypos < 0 THEN ypos := 31;
      IF xpos > 31 THEN xpos := 0;
      IF xpos < 0 THEN xpos := 31;
      IF b THEN BEGIN
         Writeln;
         Writeln('Done. Turtle has moved one step.');
         Writeln;
      END;
   END ELSE IF b THEN BEGIN
      rypos := ypos;
      rxpos := xpos;
      FOR by := 1 TO lengde DO BEGIN
         rypos := rypos - sin(heading/57.29);
         rxpos := rxpos + cos(heading/57.29);
         ypos := Round(rypos);
         xpos := Round(rxpos);
         IF CapsOn THEN DrawPlot;
         IF ypos > 31 THEN ypos := 0;
         IF ypos < 0 THEN ypos := 31;
         IF xpos > 31 THEN xpos := 0;
         IF xpos < 0 THEN xpos := 31;
      END;
      IF b THEN BEGIN
         Writeln;
         Writeln('Done. Turtle has moved ',lengde,' steps.');
         Writeln;
      END;
   END;
   IF husk THEN heading := 0;
END;

PROCEDURE Backward(lengde : Byte; b : Boolean);
BEGIN
   heading := heading + 180;
   IF heading > 360 THEN heading := heading - 360;
   Forward(lengde, b);
   heading := heading + 180;
   IF heading > 360 THEN heading := heading - 360;
END;

PROCEDURE Turnleft(b : Boolean);
BEGIN
   Inc(heading,45);
   IF heading > 360 then Dec(heading,360);
   IF heading = 0 THEN heading := 360;
   IF b THEN BEGIN
      Writeln;
      Writeln('Done. Turtle has turned 45 degrees left.');
      Writeln;
   END;
END;

PROCEDURE Turnright(b : Boolean);
BEGIN
   Dec(heading,45);
   IF heading < 0 then Inc(heading,360);
   IF heading = 0 THEN heading := 360;
   IF b THEN BEGIN
      Writeln;
      Writeln('Done. Turtle has turned 45 degrees right.');
      Writeln;
   END;
END;

PROCEDURE DrawPlot;
BEGIN
   IF NOT bilde THEN BEGIN
      Graphmode;
      IF NOT palette THEN DefaultPalette;
      IF NOT palette THEN SetPalette;
      Tegnpalett;
      Cursor(acticolx,acticoly,true);
      TegnRuter;
      TegnThumb;
   END;
   P(xpos,ypos,GetActiveColor);
   IF NOT bilde THEN CLS;
   bilde := true;
   palette := true;
   Writeln;
   Writeln('Done. A pixel was plotted at the turtle''s position.');
   Writeln;
END;

PROCEDURE RGBinfo(x : Word;y,c : Byte);
VAR
   r,g,b,n : Byte;
   s : String;
   pre,i : Word;
BEGIN
      Letter(250,10,'N',c);
      Letter(250,20,'R',c);
      Letter(250,28,'G',c);
      Letter(250,36,'B',c);
      { TheImage[18*320+i] := c; }
      FillChar(TheImage[6010],30,c);
      pre := (y shl 8)+(y shl 6)+321+x;
      n := TheImage[pre];
      r := ThePal[TheImage[pre]*3];
      g := ThePal[TheImage[pre]*3+1];
      b := ThePal[TheImage[pre]*3+2];
      IF n in[0,1,2] THEN Letter(285,10,'!',c);
      { (260+7*i-7,10,s[i],c); }
      Str(n,s);
      FOR i := 1 TO Ord(s[0]) DO Letter(253+7*i,10,s[i],c);
      Str(r,s);
      FOR i := 1 TO Ord(s[0]) DO Letter(260+7*i,20,s[i],c);
      Str(g,s);
      FOR i := 1 TO Ord(s[0]) DO Letter(260+7*i,28,s[i],c);
      Str(b,s);
      FOR i := 1 TO Ord(s[0]) DO Letter(260+7*i,36,s[i],c);
      IF (first_done) AND (NOT (c = system2)) THEN BEGIN
         Str(first_color,s);
         IF c = 0 THEN c := first_color;
         FOR i := 1 TO Ord(s[0]) DO Letter(235+7*i,48,s[i],c);
         Letter(235+7*(i+1),48,'-',system1);
         pre := i + 1;
         IF (o1 > 0) AND (o2 > 0) THEN BEGIN
            Str(o1,s);
	         FOR i := 1 TO Ord(s[0]) DO Letter(235+7*(o2+i),48,s[i],system2);
         END;
         Str(n,s);
         FOR i := 1 TO Ord(s[0]) DO Letter(235+7*(pre+i),48,s[i],n);
         o1 := n;
         o2 := pre;
         Str(o1,s);
         FOR i := 1 TO Ord(s[0]) DO Letter(235+7*(o2+i),48,s[i],n);
      END ELSE BEGIN
         Str(o1,s);
         FOR i := 1 TO Ord(s[0]) DO Letter(235+7*(o2+i),48,s[i],system2);
         Str(first_color,s);
         FOR i := 1 TO Ord(s[0]) DO Letter(235+7*i,48,s[i],system2);
         Letter(235+7*(i+1),48,'-',system2);
         o1 := 0;
         o2 := 0;
      END;
END;

PROCEDURE SetVar(cmds : String;v : ip;what : String;range : boolean;min : Byte;max : Word);
VAR
   i,c : Integer;
   s,s2 : String;
BEGIN
   Writeln;
   FOR i := (Length(what)+1) TO Length(cmds) DO s[i-Length(what)] := cmds[i];
   s[0] := Chr((Length(cmds)-Length(what)));
   IF s[1]=' ' THEN BEGIN
      FOR i := 1 TO (Length(s)-1) DO s[i] := s[i+1];
      Dec(s[0]);
   END;
   IF s = '' THEN BEGIN
      Writeln(what,' = ',v^);
   END ELSE IF pos('n',s) = 1 THEN BEGIN
      v^ := n;
      Writeln(what,' = ',v^);
   END ELSE IF pos('bc',s) = 1 THEN BEGIN
      v^ := bc;
      Writeln(what,' = ',v^);
   END ELSE IF pos('heading',s) = 1 THEN BEGIN
      v^ := heading;
      Writeln(what,' = ',v^);
   END ELSE IF pos('xpos',s) = 1 THEN BEGIN
      v^ := xpos;
      Writeln(what,' = ',v^);
   END ELSE IF pos('ypos',s) = 1 THEN BEGIN
      v^ := ypos;
      Writeln(what,' = ',v^);
   END ELSE BEGIN
      Val(s,i,c);
      IF pos('h',s) > 1 THEN BEGIN
         i := d(ntrim(s));
         c := 0;
      END;
      IF c <> 0 THEN
         Writeln('Syntax: ',what,' [integer]')
      ELSE BEGIN
         IF NOT Pos('h',s) > 1 THEN
			   Str(i,s2)
         ELSE
            s2 := s;
         IF s <> s2 THEN
            Writeln('Syntax: ',what,' [integer]')
         ELSE BEGIN
            IF (range AND ((i >= min) and (i <= max))) OR (NOT range) THEN
               v^ := i
            ELSE
               Writeln('Out of range.');
            Writeln(what,' = ',v^);
         END;
      END;
   END;
   Writeln;
END;

PROCEDURE Fill(startx : Word; starty,c : Byte; gradient,cool : boolean);
VAR
   fargen : Byte;
   x,y : Integer;
   mer : Boolean;
   i : Byte;
BEGIN
   fargen := G(startx,starty);
   IF gradient AND (fargen = c) THEN Inc(c);
   IF fargen = c THEN Exit;
   bilde := true;
   FOR i := 0 TO 1 DO BEGIN
      Repeat
         mer := FindCorner(startx,starty,Addr(x),Addr(y),fargen,i,i);
         IF ((x > -1) AND (x < 32)) AND ((y > -1) AND (y < 32)) THEN BEGIN
            Repeat
               IF gradient THEN BEGIN
                  CASE heading OF
                   0,360: IF cool THEN
                             P(x,y,(((((y-starty)*-1)+31)*(x-startx)) div 16)+c)
                          ELSE
                             P(x,y,c+x-startx);
                      90: IF cool THEN
                             P(x,y,(((((x-startx)*-1)+31)*(starty-y)) div 16)+c)
                          ELSE
                             P(x,y,c+starty-y);
                     180: IF cool THEN
                             P(x,y,(((((y-starty)*-1)+31)*(startx-x)) div 16)+c)
                          ELSE
                             P(x,y,c+startx-x);
                     270: IF cool THEN
                             P(x,y,(((((x-startx)*-1)+31)*(y-starty)) div 16)+c)
                          ELSE
                             P(x,y,c+y-starty);
                      45: IF cool THEN
                             P(x,y,((y*((x*-1)+31)) div 16)+c)
                          ELSE
                             P(x,y,c+x-y+startx-starty);
                     135: IF cool THEN
                             P(x,y,((y*x) div 16)+c)
                          ELSE
                             P(x,y,c+startx+starty-x-y);
                     225: IF cool THEN
                             P(x,y,((x*((y*-1)+31)) div 16)+c)
                          ELSE
                             P(x,y,c-x+y-startx+starty);
                     315: IF cool THEN
                             P(x,y,((((x*-1)+31)*((y*-1)+31)) div 16)+c)
                          ELSE
                             P(x,y,c+x+y-startx-starty);
                  END;
                  IF (G(x,y) = fargen) THEN P(x,y,c);
               END ELSE P(x,y,c);
               IF i = 1 THEN Inc(x) ELSE Dec(x);
               IF (x > 31) THEN x := 31;
               IF (x < 0) THEN x := 0;
            UNTIL (G(x,y) <> fargen);
         END;
      UNTIL (mer = false);
   END;
END;

FUNCTION FindCorner(startx,starty : Integer; x,y : Ip; c, opp, left : Byte) : Boolean;
VAR
   oldy : Byte;
   oldx : Word;
   fremgang : Boolean;
   fremgang_once : Boolean;
BEGIN
   x^ := startx;
   y^ := starty;
   fremgang_once := false;
   Repeat
      fremgang := false;
      {oppover til man m›ter p† noe}
      oldy := y^;
      Repeat
         IF opp = 1 THEN Dec(y^) ELSE Inc(y^);
      UNTIL (y^ > 31) OR (y^ < 0) OR (G(x^,y^) <> c);
      IF opp = 1 THEN Inc(y^) ELSE Dec(y^);
      IF NOT (oldy = y^) THEN fremgang := true;
      {til venstre til man m›ter p† noe}
      oldx := x^;
      Repeat
         IF left = 1 THEN Dec(x^) ELSE Inc(x^);
      UNTIL (x^ > 31) OR (x^ < 0) OR (G(x^,y^) <> c);
      IF left = 1 THEN Inc(x^) ELSE Dec(x^);
      IF NOT (oldx = x^) THEN fremgang := fremgang AND true;
      IF fremgang THEN fremgang_once := true;
   UNTIL NOT fremgang;
   FindCorner := fremgang_once;
END;

PROCEDURE Circle(x,y,xr,yr,c : Byte);
VAR
   g : Real;
   cx,cy : Byte;
BEGIN
   g := 0;
   Repeat
      g := g + Pi/180;
      cx := Round(cos(g)*xr+x);
      cy := Round(sin(g)*yr+y);
      IF ((cx >= 0) AND (cx <= 31)) AND ((cy >= 0) AND (cy <= 31)) THEN P(cx,cy,c);
   UNTIL g > 6.28;
   bilde := true;
END;

FUNCTION SaveICO(filename : String; transe : Boolean) : Boolean;
VAR
   f : File;
   b : Byte;
   w : Word;
   x,y : Byte;
   l : Longint;
   bitmask : Array [0..127] of Byte;
BEGIN
   {$I-}
   Assign(f,filename);
   Rewrite(f,1);
   { ICODIR }
   w := 0;  BlockWrite(f,w,2); { idReserved (0) }
   {$I+}
   IF IOResult <> 0 THEN BEGIN
      SaveICO := false;
      Exit;
   END ELSE SaveICO := true;
   w := 1;  BlockWrite(f,w,2); { idType (1) }
   w := 1;  BlockWrite(f,w,2); { idCount (antall bilder) }
   { ICODIR ENTRY #1 }
   b := 32; BlockWrite(f,b,1); { bWidth (width) }
   b := 32; BlockWrite(f,b,1); { bHeight (height) }
   { tidl. 8 }
   b := 0;  BlockWrite(f,b,1); { bColorCount (4, 8 eller 16) }
   b := 0;  BlockWrite(f,b,1); { bReserved (0) }
   { tidl. 1}
   w := 0;  BlockWrite(f,w,2); { wPlanes (1) }
   { tidl. 8}
   w := 0;  BlockWrite(f,w,2); { wBitCount (0?) }
   { tidl. feil }
   l := $8a8;  BlockWrite(f,l,4); { dwBytesInRes () }
   l := FilePos(f)+4; BlockWrite(f,l,4); { dwImageOffset () }
   { BITMAP INFO HEADER - ofs 20 (?) }
   l := 40;   BlockWrite(f,l,4); { Size of the image header (40) }
   l := 32;   BlockWrite(f,l,4); { Image width in pixels }
   { tidl. 32 }
   l := 64;   BlockWrite(f,l,4); { Image height in pixels }
   w := 1;    BlockWrite(f,w,2); { Number of planes (1) }
   w := 8;    BlockWrite(f,w,2); { Bits per pixel }
   l := 0;    BlockWrite(f,l,4); { RLE Compression, 0 or 1 }
   { tidl. feil }
   l := $480; BlockWrite(f,l,4); { Compressed image size (uncompr, 0 ok) }
   l := 0;    BlockWrite(f,l,4); { Horizontal resolutions, ppm }
   l := 0;    BlockWrite(f,l,4); { Vertical resolutions, ppm }
   { tidl. 0 }
   l := 256;  BlockWrite(f,l,4); { Palette entries }
   l := 0;    BlockWrite(f,l,4); { Most important colors (!?) }
   { RGB QUAD }
   IF transe THEN BEGIN
      w := 0;
      BlockWrite(f,w,2);
      BlockWrite(f,w,2);
   END ELSE BEGIN
      b := thepal[w*3+2] shl 2;
      BlockWrite(f,b,1);
      b := thepal[w*3+1] shl 2;
      BlockWrite(f,b,1);
      b := thepal[w*3] shl 2;
      BlockWrite(f,b,1);
      b := 0;
      BlockWrite(f,b,1);
   END;
   FOR w := 1 TO 255 DO BEGIN
      b := thepal[w*3+2] shl 2;
      BlockWrite(f,b,1);
      b := thepal[w*3+1] shl 2;
      BlockWrite(f,b,1);
      b := thepal[w*3] shl 2;
      BlockWrite(f,b,1);
      b := 0;
      BlockWrite(f,b,1);
   END;
   { IMAGE DATA }
   IF transe THEN FillChar(bitmask,128,0);
   x := 0;
   y := 0;
   Repeat
      b := G(x,(y*-1)+31);
      BlockWrite(f,b,1);
      IF transe AND (b = 0) THEN BEGIN
         w := y shl 2+x div 8;
         b := x mod 8;
         CASE b OF
            0 : bitmask[w] := bitmask[w] or 128;
            1 : bitmask[w] := bitmask[w] or 64;
            2 : bitmask[w] := bitmask[w] or 32;
            3 : bitmask[w] := bitmask[w] or 16;
            4 : bitmask[w] := bitmask[w] or 8;
            5 : bitmask[w] := bitmask[w] or 4;
            6 : bitmask[w] := bitmask[w] or 2;
            7 : bitmask[w] := bitmask[w] or 1;
         END;
		END;
      Inc(x);
      IF x > 31 THEN BEGIN
         Dec(x,32);
         Inc(y);
      END;
   UNTIL (x = 32) OR (y = 32);
   { BIT XOR MASK }
   IF transe THEN BEGIN
      BlockWrite(f,bitmask,128);
   END ELSE BEGIN
      b := $00;
      FOR w := 1 TO 128 DO BlockWrite(f,b,1);
   END;
   Close(f);
END;

FUNCTION SaveBMP(filename : String) : Boolean;
VAR
   f : File;
   b : Byte;
   w : Word;
   x,y : Byte;
   fibp : Word;
   l : Longint;
   iofv, iofp : Word;
BEGIN
   {$I-}
   Assign(f,filename);
   Rewrite(f,1);
   w := Ord('B')+Ord('M') shl 8;
	BlockWrite(f,w,2); {ASCII string BM}
   {$I+}
   IF IOResult <> 0 THEN BEGIN
      SaveBMP := false;
      Exit;
   END ELSE SaveBMP := true;
   fibp := FilePos(f);
   l := 0;    BlockWrite(f,l,4); { Filesize in bytes }
   w := 0;    BlockWrite(f,w,2); { Reserved (0) }
   w := 0;    BlockWrite(f,w,2); { Reserved (0) }
   iofp := FilePos(f);
   l := 0;    BlockWrite(f,l,4); { Image offset from filetop (40+1024) }
   l := 40;   BlockWrite(f,l,4); { Size of the image header (40) }
   l := 32;   BlockWrite(f,l,4); { Image width in pixels }
   l := 32;   BlockWrite(f,l,4); { Image height in pixels }
   w := 1;    BlockWrite(f,w,2); { Number of planes (1) }
   w := 8;    BlockWrite(f,w,2); { Bits per pixel }
   l := 0;    BlockWrite(f,l,4); { RLE Compression, 0 or 1 }
   l := 0;    BlockWrite(f,l,4); { Compressed image size }
   l := 2834; BlockWrite(f,l,4); { Horizontal resolutions, ppm }
   l := 2834; BlockWrite(f,l,4); { Vertical resolutions, ppm }
   l := 256;  BlockWrite(f,l,4); { Palette entries }
   l := 0;    BlockWrite(f,l,4); { Most important colors (!?) }
   l := 1024; BlockWrite(f,l,4); { Palette size }
   FOR w := 0 TO 255 DO BEGIN
      b := thepal[w*3+2] shl 2;
      BlockWrite(f,b,1);
      b := thepal[w*3+1] shl 2;
      BlockWrite(f,b,1);
      b := thepal[w*3] shl 2;
      BlockWrite(f,b,1);
      b := w; {!?}
      BlockWrite(f,b,1);
   END;
   iofv := FilePos(f);
   x := 0;
   y := 0;
   Repeat
      b := G(x,(y*-1)+31);
      Inc(x);
      IF x > 31 THEN BEGIN
         Dec(x,32);
         Inc(y);
      END;
      BlockWrite(f,b,1);
   UNTIL (x = 32) OR (y = 32);
   Seek(f,fibp);
   l := FileSize(f);    BlockWrite(f,l,4);  { Filesize in bytes }
   Seek(f,iofp);
   l := iofv;           BlockWrite(f,l,4);  { Image offset }
   Close(f);
END;

PROCEDURE Save(cmds : String);
CONST
  syntaxs = 'Syntax: save [readme|info|ico|pcx|bmp|pal|ascii|bim|bip] [filename]';
VAR
  filename : String[12];
  t : Text;
  f : File;
  temp,s : String;
  x,y,c,color : Byte;
  w : Word;
  svar : String[3];
BEGIN
   Writeln;
   s := GetParam(Addr(cmds));
   filename := GetParam(Addr(s));
   IF filename = 'screen' THEN BEGIN
      x := 0;
      temp := '';
      Repeat
         filename := 'screen' + temp;
         Inc(x);
         Str(x,temp);
      UNTIL (NOT finnes(Addr(filename))) OR (x = 99);
      IF x = 99 THEN BEGIN
         Writeln('Couldn''t generate a tempfile for screen. Giving up.');
         Writeln;
         Exit;
      END;
   END;
   IF (s = '') or (filename = '') THEN
      Writeln(syntaxs)
   ELSE BEGIN
      IF s[1] = 'r' THEN BEGIN
         IF finnes(Addr(filename)) THEN BEGIN
            Writeln('File already exist. Let''s overwrite it! :>');
            Remove('del '+filename);
         END;
         IF NOT finnes(Addr(filename)) THEN BEGIN
            Assign(t,filename);
            Rewrite(t);

               Writeln(t,'Oups, I haven''t finished this readme thingy yet.');
               Writeln(t,'I will finish it some time in the future. =)');

            Close(t);
            IF Pos('screen',filename) <> 1 THEN Writeln('Readme file successfully saved.');
         END ELSE
            Writeln('Didn''t overwrite ',filename,'.');
      END ELSE IF (pos('info',s)>0) THEN BEGIN
         IF finnes(Addr(filename)) THEN BEGIN
            Writeln('File already exist. Let''s overwrite it! :>');
            Remove('del '+filename);
         END;
         IF NOT finnes(Addr(filename)) THEN BEGIN
            Assign(t,filename);
            Rewrite(t);
               Writeln(t,'Various info for ',vstring,GetVer);
               Writeln(t,'----------------------------------------------');
               Writeln(t,'Version number            : ',h(build)+'h');
               Writeln(t,'Width (in pixels)         : 32');
               Writeln(t,'Height (in pixels)        : 32');
               Writeln(t,'Screen buffer segment     : ',h(Seg(TheImage)),'h');
               Writeln(t,'Screen buffer offset      : ',h(Ofs(TheImage)),'h');
               Writeln(t,'768 byte palette segment  : ',h(Seg(ThePal)),'h');
               Writeln(t,'768 byte palette offset   : ',h(Ofs(ThePal)),'h');
               Writeln(t,'Active color number       : ',GetActiveColor);
               Writeln(t,'Backround color           : ',bc);
               Writeln(t,'Cursor X position         : ',xpos);
               Writeln(t,'Cursor Y position         : ',ypos);
               Writeln(t,'Turtle heading (degrees)  : ',heading);
               Writeln(t,'N variable                : ',n);
               Writeln(t,'Image                     : ',bilde);
               Writeln(t,'Palette                   : ',palette);
               Writeln(t,'Capslock turned on?       : ',CapsOn);
               IF framebase = 'int19h' THEN BEGIN
                  Writeln(t,'Animation                 : NO');
                  Writeln(t,'Animation framebase       : N/A');
               END ELSE BEGIN
                  Writeln(t,'Animation                 : YES');
                  Writeln(t,'Animation framebase       : ',framebase);
               END;
               Writeln(t,'Last random error nr      : ',lasterror);
               Writeln(t,'Author, nick              : Int',19,'h');
               Writeln(t,'Author, real-name         : Alexander R›dseth');
               Writeln(t,'Author, e-mail            : ',my_edress);
               Writeln(t,'Author, demogroup         : Kamelite');
            Close(t);
            IF Pos('screen',filename) <> 1 THEN Writeln('Info file successfully saved.');
         END ELSE
            Writeln('Didn''t overwrite ',filename,'.');
      END ELSE IF (pos('ico',s)>0) THEN BEGIN
         IF bilde THEN BEGIN
            IF finnes(Addr(filename)) THEN BEGIN
               Writeln('File already exist. Let''s overwrite it! :>');
               Remove('del '+filename);
            END;
            IF NOT finnes(Addr(filename)) THEN BEGIN
               Write('Would you like color nr. 0 to be transparent? ');
               Readln(svar);
               Writeln;
               IF UpCase(svar[1]) <> 'Y' THEN BEGIN
                  IF SaveIco(filename,false) AND (Pos('screen',filename) <> 1) THEN Writeln('ICO successfully saved.');
               END ELSE BEGIN
                  IF SaveIco(filename,true) AND (Pos('screen',filename) <> 1) THEN Writeln('ICO successfully saved.');
               END;
            END ELSE
               Writeln('Didn''t overwrite ',filename,'.');
         END ELSE
            Writeln('You haven''t made an image yet.');
      END ELSE IF (pos('pcx',s)>0) THEN BEGIN
         IF bilde THEN BEGIN
            IF finnes(Addr(filename)) THEN BEGIN
               Writeln('File already exist. Let''s overwrite it! :>');
               Remove('del '+filename);
            END;
            IF NOT finnes(Addr(filename)) THEN BEGIN
               IF SavePcx(filename) AND (Pos('screen',filename) <> 1) THEN Writeln('PCX successfully saved.');
            END ELSE
               Writeln('Didn''t overwrite ',filename,'.');
         END ELSE
            Writeln('You haven''t made an image yet.');
      END ELSE IF (pos('bmp',s)>0) THEN BEGIN
         IF bilde THEN BEGIN
            IF finnes(Addr(filename)) THEN BEGIN
               Writeln('File already exist. Let''s overwrite it! :>');
               Remove('del '+filename);
            END;
            IF NOT finnes(Addr(filename)) THEN BEGIN
               IF SaveBmp(filename) AND (Pos('screen',filename) <> 1) THEN Writeln('BMP successfully saved.');
            END ELSE
               Writeln('Didn''t overwrite ',filename,'.');
         END ELSE
            Writeln('You haven''t made an image yet.');
      END ELSE IF (pos('pal',s)>0) OR (pos('bpl',s)>0)  THEN BEGIN
         IF NOT palette THEN BEGIN
       	   Writeln('Note: The palette hasen''t been edited, so it might look like a total mess!');
         END;
         IF finnes(Addr(filename)) THEN BEGIN
            Writeln('File already exist. Let''s overwrite it! :>');
            Remove('del '+filename);
         END;
         IF NOT finnes(Addr(filename)) THEN BEGIN
            Assign(f,filename);
            Rewrite(f,1);
            BlockWrite(f,thepal,768);
            Close(f);
            IF Pos('screen',filename) <> 1 THEN Writeln('Palette successfully saved.');
         END ELSE
               Writeln('Didn''t overwrite ',filename,'.');
      END ELSE IF (pos('ascii',s)>0) THEN BEGIN
         IF bilde THEN BEGIN
            IF finnes(Addr(filename)) THEN BEGIN
               Writeln('File already exist. Let''s overwrite it! :>');
               Remove('del '+filename);
            END;
            IF NOT finnes(Addr(filename)) THEN BEGIN
               Assign(t,filename);
               Rewrite(t);
			      FOR y := 0 TO 31 DO BEGIN
			         FOR x := 0 TO 31 DO BEGIN
			            c := ((ThePal[G(x,y)*3+0]+ThePal[G(x,y)*3+1]+ThePal[G(x,y)*3+2]) shr 3);
			            CASE c OF
							   23: Write(t,'W');
							   22: Write(t,'M');
							   21: Write(t,'H');
						      20: Write(t,'X');
						      19: Write(t,'&');
						      18: Write(t,'O');
						      17: Write(t,'$');
						      16: Write(t,'#');
						      15: Write(t,'@');
						      14: Write(t,'ç');
						      13: Write(t,'z');
						      12: Write(t,'n');
								11: Write(t,'a');
								10: Write(t,'¸');
								9: Write(t,'o');
								8: Write(t,'c');
								7: Write(t,'*');
								6: Write(t,'=');
								5: Write(t,'+');
								4: Write(t,';');
								3: Write(t,':');
								2: Write(t,'-');
								1: Write(t,'.');
								0: Write(t,' ');
							END;
						END;
                  Writeln(t);
               END;
               Close(t);
               IF Pos('screen',filename) <> 1 THEN Writeln('Ascii representation of image sucessfully saved.');
            END ELSE
               Writeln('Didn''t overwrite ',filename,'.');
         END ELSE
            Writeln('You don''t have an image ready to save as ascii.');
      END ELSE IF ((pos('image',s)>0) OR (pos('raw',s)>0)) OR (pos('bim',s)>0) THEN BEGIN
         IF bilde THEN BEGIN
            IF finnes(Addr(filename)) THEN BEGIN
               Writeln('File already exist. Let''s overwrite it! :>');
               Remove('del '+filename);
            END;
            IF NOT finnes(Addr(filename)) THEN BEGIN
               Assign(f,filename);
               Rewrite(f,1);
               FOR y := 0 TO 31 DO BEGIN
                  FOR x := 0 TO 31 DO BEGIN
                     c := G(x,y);
                     BlockWrite(f,c,1);
                  END;
               END;
               Close(f);
               IF Pos('screen',filename) <> 1 THEN Writeln('Image successfully saved.');
            END ELSE
               Writeln('Didn''t overwrite ',filename,'.');
         END ELSE
            Writeln('You haven''t made an image yet.');
      END ELSE IF (pos('all',s)>0) OR (pos('bip',s)>0) THEN BEGIN
         IF bilde OR palette THEN BEGIN
            IF finnes(Addr(filename)) THEN BEGIN
               Writeln('File already exist. Let''s overwrite it! :>');
               Remove('del '+filename);
            END;
            IF NOT finnes(Addr(filename)) THEN BEGIN
               Assign(f,filename);
               Rewrite(f,1);
               c := Ord('B');
               BlockWrite(f,c,1);       { Burn signature }
               BlockWrite(f,build,2);   { Burn version }
               w := 0;
               BlockWrite(f,w,2);       { Reserved for animdelay... }
               {
               1, image
               2, palette
               3, image + palette
               }
               IF bilde THEN c := 1 ELSE c := 0;
               IF palette THEN Inc(c,2);
					BlockWrite(f,c,1);
               IF bilde THEN BEGIN
                  FOR y := 0 TO 31 DO BEGIN
                     FOR x := 0 TO 31 DO BEGIN
                        c := G(x,y);
                        BlockWrite(f,c,1);
                     END;
                  END;
               END;
               IF palette THEN BlockWrite(f,thepal,768);
               Close(f);
               IF Pos('screen',filename) <> 1 THEN Writeln('Image successfully saved.');
            END ELSE
               Writeln('Didn''t overwrite ',filename,'.');
         END ELSE
            Writeln('You haven''t made an image or a palette yet.');
      END ELSE Writeln(syntaxs);
      IF Pos('screen',filename) = 1 THEN BEGIN
         IF finnes(addr(filename)) THEN BEGIN
				TypeFile('type '+filename);
	         Remove('silent '+filename);
         END;
      END;
   END;
   Writeln;
END;

FUNCTION GetVerS : String;
VAR s, s2 : String;
BEGIN
   IF odd(hi(build)) THEN s := ' (unstable ' ELSE s := ' (stable ';
   IF (build AND 128) <> 0 THEN BEGIN
      s := s + 'release)'
   END ELSE BEGIN
      str((hi(build)-128) div 2,s2);
      s := s + 'beta ' + s2 + ')';
   END;
   GetVerS := s;
END;

FUNCTION GetVer : String;
VAR s : String[8];
BEGIN
   IF build = 0 THEN BEGIN
      GetVer := ' v.x.x';
      Exit;
	END;
   s := h(lo(build));
   s := s[1] + '.' + s[2];
   GetVer := ' v.' + s;
END;

PROCEDURE PLinje(x1, y1, x2, y2, c : Byte);
VAR
   sx,mx,x,sy,my,y : Byte;
   pre : Real;
BEGIN
   IF x1 > x2 THEN BEGIN
	   sx := x1;
	   mx := x2;
	END ELSE BEGIN
	   sx := x2;
	   mx := x1;
	END;
   IF y1 > y2 THEN BEGIN
	   sy := y1;
	   my := y2;
	END ELSE BEGIN
	   sy := y2;
	   my := y1;
   END;
   IF (sx-mx) >= (sy-my) THEN BEGIN
      IF (x2-x1) = 0 THEN
         pre := (y2-y1)
      ELSE
         pre := ((y2-y1)/(x2-x1));
      FOR x := mx TO sx DO P(x,Round(pre*(x-x1)+y1),c)
   END ELSE BEGIN
      pre := (x2-x1);
      IF (y2-y1) = 0 THEN
         FOR y := my TO sy DO P(Round((((y-y1)*pre)*0)+x1),y,c)
      ELSE
         FOR y := my TO sy DO P(Round((((y-y1)*pre)/(y2-y1))+x1),y,c);
   END;
END;

PROCEDURE Linje(x1,y1,x2,y2 : Word; c : Byte);
VAR
   sx,mx,x,sy,my,y : Byte;
   pre : Real;
   w : Word;
   skjermen : Array [0..63999] of Byte absolute $A000:0000;
BEGIN
   IF x1 > x2 THEN BEGIN
	   sx := x1;
	   mx := x2;
	END ELSE BEGIN
	   sx := x2;
	   mx := x1;
	END;
   IF y1 > y2 THEN BEGIN
	   sy := y1;
	   my := y2;
	END ELSE BEGIN
	   sy := y2;
	   my := y1;
   END;
   IF (sx-mx) >= (sy-my) THEN BEGIN
      IF (x2-x1) = 0 THEN
         pre := (y2-y1)
      ELSE
         pre := ((y2-y1)/(x2-x1));
      FOR x := mx TO sx DO BEGIN
         w := Round(pre*(x-x1)+y1);
         skjermen[w shl 8+w shl 6+x] := c;
      END;
   END ELSE BEGIN
      pre := (x2-x1);
      IF (y2-y1) = 0 THEN
         FOR y := my TO sy DO BEGIN
            w := Round((((y-y1)*pre)*0)+x1);
            skjermen[y shl 8+y shl 6+x] := c;
         END
      ELSE
         FOR y := my TO sy DO BEGIN
            w := Round((((y-y1)*pre)/(y2-y1))+x1);
            skjermen[y shl 8+y shl 6+x] := c;
         END;
   END;
END;

PROCEDURE MsgBox(s : String; h : Byte; w : Boolean);
VAR
   skjermen : Array [0..63999] of Byte absolute $a000:0000;
   y : Byte;
BEGIN
   FOR y := h+1 TO 198-h DO BEGIN
      FillChar(skjermen[y shl 8+y shl 6+h],319-(h*2),system2);
   END;
   { Svart nede }
   FillChar(skjermen[(199-h) shl 8+(199-h) shl 6+h],319-(h*2),system1);
   { Svart til h›yre }
   FOR y := h+1 TO 199-h DO skjermen[y shl 8+y shl 6+319-h] := system1;
   { Hvitt oppe }
   FillChar(skjermen[h shl 8+h shl 6+h],320-(h*2),system3);
   { Hvitt til venstre }
   FOR y := h+1 TO 199-h DO skjermen[y shl 8+y shl 6+h] := system3;
   { h + (bokslengde - tekstlengde) div 2, $0100 er for 01-blur, 00-text }
   SkrivXY(s,h+((319-h*2)-(ord(s[0])*7)) div 2+2,h+((199-h*2)-7) div 2,$0100,false);
   { wait for keypress }
   IF w THEN wfk;
END;

PROCEDURE GetK(k : wp);
VAR
   tk : Word;
   oldcaps, newcaps : Boolean;
BEGIN
      oldcaps := CapsOn;
      tk := 0;
      Repeat
         asm
            mov ax,0100h
            int 16h
            jz @nei
            mov tk,ax
            @nei:
         end;
         newcaps := CapsOn;
      UNTIL (tk > 0) OR (oldcaps <> newcaps);
      IF (newcaps = oldcaps) THEN BEGIN
         asm
            xor ah,ah
            int 16h
            mov tk,ax
         end;
      END;
      k^ := tk;
END;

PROCEDURE SkyvSkjermOpp;
VAR skjermen : Array [0..63999] of Byte absolute $a000:0000;
BEGIN
   Move(skjermen[3520],skjermen,60480);
   FillChar(skjermen[60480],3520,0);
END;

PROCEDURE SkrivXY(s : String; x2 : Word; y : Byte; c : Word; pp : Boolean);
VAR
   x,i : Word;
	l : Char;
   skjermen : Array [0..63999] of Byte absolute $a000:0000;
 PROCEDURE Plot(s : String; x : Word; y : Byte; c : Word);
 VAR i : Byte;
 BEGIN
    IF pp THEN BEGIN
       FOR i := 1 TO Ord(s[0]) DO IF s[i] = '*' THEN P(x+i,y,lo(c));
    END ELSE BEGIN
       FOR i := 1 TO Ord(s[0]) DO BEGIN
          IF s[i] = '*' THEN skjermen[y shl 6+y shl 8+x+i-1] := lo(c);
          IF s[i] = '-' THEN skjermen[y shl 6+y shl 8+x+i-1] := hi(c);
       END;
    END;
 END;
BEGIN
     FOR i := 1 TO ord(s[0]) DO BEGIN
         l := s[i];
         x := x2+(7*(i-1));

         { Why not just as well implement a font like this? =) }

     CASE l OF
          'a':BEGIN Plot('***-',x+1,y+1,c);
                    Plot('--**',x+1,y+2,c);
                   Plot('-****',x,y+3,c);
                   Plot('**-**-',x,y+4,c);
                   Plot('-**-**',x,y+5,c);
              END;
          'A':BEGIN  Plot('-**-',x+1,y,c);
                    Plot('-****-',x,y+1,c);
                    Plot('**--**',x,y+2,c);
                    Plot('******',x,y+3,c);
                    Plot('**--**',x,y+4,c);
                    Plot('**  **',x,y+5,c);
              END;
          'b':BEGIN Plot('***',x,y,c);
                    Plot('-**-',x,y+1,c);
                     Plot('****-',x+1,y+2,c);
                     Plot('** **',x+1,y+3,c);
                    Plot('-**-**',x,y+4,c);
                    Plot('**-**-',x,y+5,c);
              END;
          'B':BEGIN Plot('*****-',x,y,c);
                    Plot('-**-**',x,y+1,c);
                     Plot('****-',x+1,y+2,c);
                     Plot('** **',x+1,y+3,c);
                    Plot('-** **',x,y+4,c);
                    Plot('*****-',x,y+5,c);
              END;
          'c':BEGIN Plot('-****-',x,y+1,c);
                    Plot('**--**',x,y+2,c);
                    Plot('** ---',x,y+3,c);
                    Plot('**--**',x,y+4,c);
                    Plot('-****-',x,y+5,c);
              END;
          'C':BEGIN Plot('-****-',x,y,c);
                    Plot('**- -*',x,y+1,c);
                    Plot('**',x,y+2,c);
                    Plot('**',x,y+3,c);
                    Plot('**- -*',x,y+4,c);
                    Plot('-****-',x,y+5,c);
              END;
          'd':BEGIN Plot('***',x+3,y,c);
                    Plot('-**',x+3,y+1,c);
                 Plot('-*****',x,y+2,c);
                 Plot('**--**',x,y+3,c);
                 Plot('**  **',x,y+4,c);
                 Plot('-***-*',x,y+5,c);
              END;
          'D':BEGIN Plot('****-',x,y,c);
                    Plot('**-**-',x,y+1,c);
                    Plot('** -**',x,y+2,c);
                    Plot('** -**',x,y+3,c);
                    Plot('**-**-',x,y+4,c);
                    Plot('****-',x,y+5,c);
              END;
          'e':BEGIN Plot('-****-',x,y+1,c);
                    Plot('** -**',x,y+2,c);
                    Plot('******',x,y+3,c);
                    Plot('**-',x,y+4,c);
                    Plot('-****-',x,y+5,c);
              END;
          'E':BEGIN Plot('-*****',x,y,c);
                    Plot('**---*',x,y+1,c);
                    Plot('****-',x,y+2,c);
                    Plot('**--',x,y+3,c);
                    Plot('**- **',x,y+4,c);
                    Plot('*****-',x,y+5,c);
              END;
          'f':BEGIN Plot('-***-',x+1,y,c);
                   Plot('-**--*',x,y+1,c);
                   Plot('****',x,y+2,c);
                   Plot('-**-',x,y+3,c);
                    Plot('**',x+1,y+4,c);
                   Plot('****',x,y+5,c);
              END;
          'F':BEGIN Plot('*****-',x,y,c);
                    Plot('**--**',x,y+1,c);
                    Plot('**-',x,y+2,c);
                    Plot('****',x,y+3,c);
                    Plot('**-',x,y+4,c);
                    Plot('*-',x,y+5,c);
              END;
          'g':BEGIN Plot('-**-**',x,y+1,c);
                    Plot('** **-',x,y+2,c);
                    Plot('-****',x,y+3,c);
                      Plot('-**',x+2,y+4,c);
                    Plot('****-',x,y+5,c);
              END;
          'G':BEGIN Plot('-****-',x,y,c);
                    Plot('**--**',x,y+1,c);
                    Plot('**',x,y+2,c);
                    Plot('** ***',x,y+3,c);
                    Plot('**--**',x,y+4,c);
                    Plot('-***-*',x,y+5,c);
              END;
          'h':BEGIN Plot('***',x,y,c);
                    Plot('-**-',x,y+1,c);
                     Plot('****-',x+1,y+2,c);
                     Plot('**-**',x+1,y+3,c);
                    Plot('-** **',x,y+4,c);
                    Plot('*** **',x,y+5,c);
              END;
          'H':BEGIN Plot('**  **',x,y,c);
                    Plot('**--**',x,y+1,c);
                    Plot('******',x,y+2,c);
                    Plot('**--**',x,y+3,c);
                    Plot('**  **',x,y+4,c);
                    Plot('**  **',x,y+5,c);
              END;
          'i':BEGIN Plot('**',x+2,y,c);
                    Plot('--',x+2,y+1,c);
                   Plot('***',x+1,y+2,c);
                   Plot('-**',x+1,y+3,c);
                   Plot('-**-',x+1,y+4,c);
                   Plot('****',x+1,y+5,c);
              END;
          'I':BEGIN Plot('****',x+1,y,c);
                    Plot('-**-',x+1,y+1,c);
                     Plot('**',x+2,y+2,c);
                     Plot('**',x+2,y+3,c);
                    Plot('-**-',x+1,y+4,c);
                    Plot('****',x+1,y+5,c);
              END;
          'j':BEGIN Plot('**',x+3,y,c);
                    Plot('--',x+3,y+1,c);
                   Plot('***',x+2,y+2,c);
                   Plot('-**',x+2,y+3,c);
                 Plot('**-**',x,y+4,c);
                 Plot('-***-',x,y+5,c);
              END;
          'J':BEGIN Plot('****',x+2,y,c);
                    Plot('-**-',x+2,y+1,c);
                     Plot('**',x+3,y+2,c);
                  Plot('** **',x,y+3,c);
                  Plot('**-**',x,y+4,c);
                  Plot('-***-',x,y+5,c);
              END;
          'k':BEGIN Plot('***',x,y,c);
                    Plot('-**',x,y+1,c);
                     Plot('**-**',x+1,y+2,c);
                     Plot('****-',x+1,y+3,c);
                    Plot('-**-**',x,y+4,c);
                    Plot('*** **',x,y+5,c);
              END;
          'K':BEGIN Plot('***-**',x,y,c);
                    Plot('-****-',x,y+1,c);
                     Plot('***-',x+1,y+2,c);
                     Plot('****-',x+1,y+3,c);
                    Plot('-**-**',x,y+4,c);
                    Plot('*** **',x,y+5,c);
              END;
          'l':BEGIN Plot('***',x+1,y,c);
                    Plot('-**',x+1,y+1,c);
                     Plot('**',x+2,y+2,c);
                     Plot('**',x+2,y+3,c);
                    Plot('-**-',x+1,y+4,c);
                    Plot('****',x+1,y+5,c);
              END;
          'L':BEGIN Plot('****',x,y,c);
                    Plot('-**-',x,y+1,c);
                     Plot('**',x+1,y+2,c);
                     Plot('**',x+1,y+3,c);
                    Plot('-**--*',x,y+4,c);
                    Plot('******',x,y+5,c);
              END;
          'm':BEGIN Plot('**-**-',x,y+1,c);
                    Plot('-*****',x,y+2,c);
                     Plot('*-*-*',x+1,y+3,c);
                     Plot('*-*-*',x+1,y+4,c);
                     Plot('*-*-*',x+1,y+5,c);
              END;
          'M':BEGIN Plot('**-**',x+1,y,c);
                    Plot('**-**',x+1,y+1,c);
                    Plot('*-*-*',x+1,y+2,c);
                    Plot('*- -*',x+1,y+3,c);
                    Plot('*- -*',x+1,y+4,c);
                    Plot('*   *',x+1,y+5,c);
              END;
          'n':BEGIN Plot('**-**-',x,y+1,c);
                    Plot('-*****',x,y+2,c);
                     Plot('**-**',x+1,y+3,c);
                     Plot('** **',x+1,y+4,c);
                     Plot('** **',x+1,y+5,c);
              END;
          'N':BEGIN Plot('**  **',x,y,c);
                    Plot('**- **',x,y+1,c);
                    Plot('***-**',x,y+2,c);
                    Plot('**-***',x,y+3,c);
                    Plot('** -**',x,y+4,c);
                    Plot('**  **',x,y+5,c);
              END;
          'o':BEGIN Plot('-****-',x,y+1,c);
                    Plot('**--**',x,y+2,c);
                    Plot('**  **',x,y+3,c);
                    Plot('**--**',x,y+4,c);
                    Plot('-****-',x,y+5,c);
              END;
          'O':BEGIN Plot('-****-',x,y,c);
                    Plot('**--**',x,y+1,c);
                    Plot('**  **',x,y+2,c);
                    Plot('**  **',x,y+3,c);
                    Plot('**--**',x,y+4,c);
                    Plot('-****-',x,y+5,c);
              END;
          'p':BEGIN Plot('**-**-',x,y+1,c);
                    Plot('-**--*',x,y+2,c);
                     Plot('****',x+1,y+3,c);
                    Plot('-**-',x,y+4,c);
                    Plot('****',x,y+5,c);
              END;
          'P':BEGIN Plot('*****-',x,y,c);
                    Plot('-**-**',x,y+1,c);
                     Plot('****-',x+1,y+2,c);
                     Plot('**-',x+1,y+3,c);
                    Plot('-**-',x,y+4,c);
                    Plot('****',x,y+5,c);
              END;
          'q':BEGIN Plot('-**-**',x,y+1,c);
                    Plot('*--**-',x,y+2,c);
                    Plot('-****',x,y+3,c);
                      Plot('-**-',x+2,y+4,c);
                      Plot('****',x+2,y+5,c);
              END;
          'Q':BEGIN Plot('-****-',x,y,c);
                    Plot('**--**',x,y+1,c);
                    Plot('** -**',x,y+2,c);
                    Plot('**-***',x,y+3,c);
                    Plot('-****-',x,y+4,c);
                       Plot('-**',x+3,y+5,c);
              END;
          'r':BEGIN Plot('**-**-',x,y+1,c);
                    Plot('-*****',x,y+2,c);
                     Plot('**--*',x+1,y+3,c);
                    Plot('-**-',x,y+4,c);
                    Plot('****',x,y+5,c);
              END;
          'R':BEGIN Plot('****-',x,y,c);
                    Plot('-*--*-',x,y+1,c);
                     Plot('*--*-',x+1,y+2,c);
                     Plot('***-',x+1,y+3,c);
                    Plot('-*-**-',x,y+4,c);
                    Plot('**--**',x,y+5,c);
              END;
          's':BEGIN Plot('-*****',x,y+1,c);
                    Plot('**-',x,y+2,c);
                    Plot('-****-',x,y+3,c);
                       Plot('-**',x+3,y+4,c);
                    Plot('*****-',x,y+5,c);
              END;
          'S':BEGIN Plot('-****-',x,y,c);
                    Plot('**--**',x,y+1,c);
                    Plot('-***-',x,y+2,c);
                      Plot('-**-',x+2,y+3,c);
                    Plot('**--**',x,y+4,c);
                    Plot('-****-',x,y+5,c);
              END;
          't':BEGIN Plot('-*',x+1,y,c);
                   Plot('-**-',x,y+1,c);
                   Plot('****',x,y+2,c);
                   Plot('-**-',x,y+3,c);
                    Plot('**-*',x+1,y+4,c);
                    Plot('-**-',x+1,y+5,c);
              END;
          'T':BEGIN Plot('******',x,y,c);
                    Plot('*-**-*',x,y+1,c);
                      Plot('**',x+2,y+2,c);
                      Plot('**',x+2,y+3,c);
                     Plot('-**-',x+1,y+4,c);
                     Plot('****',x+1,y+5,c);
              END;
          'u':BEGIN Plot('** **',x,y+1,c);
                    Plot('** **',x,y+2,c);
                    Plot('** **',x,y+3,c);
                    Plot('**-**-',x,y+4,c);
                    Plot('-**-**',x,y+5,c);
              END;
          'U':BEGIN Plot('**  **',x,y,c);
                    Plot('**  **',x,y+1,c);
                    Plot('**  **',x,y+2,c);
                    Plot('**  **',x,y+3,c);
                    Plot('**--**',x,y+4,c);
                    Plot('-****-',x,y+5,c);
              END;
          'v':BEGIN Plot('**  **',x,y+1,c);
                    Plot('**  **',x,y+2,c);
                    Plot('**--**',x,y+3,c);
                    Plot('-****-',x,y+4,c);
                     Plot('-**-',x+1,y+5,c);
              END;
          'V':BEGIN Plot('**  **',x,y,c);
                    Plot('**  **',x,y+1,c);
                    Plot('**  **',x,y+2,c);
                    Plot('**--**',x,y+3,c);
                    Plot('-****-',x,y+4,c);
                     Plot('-**-',x+1,y+5,c);
              END;
          'w':BEGIN Plot('*   *',x+1,y+1,c);
                    Plot('*- -*',x+1,y+2,c);
                    Plot('*-*-*',x+1,y+3,c);
                    Plot('*-*-*',x+1,y+4,c);
                    Plot('-*-*-',x+1,y+5,c);
              END;
          'W':BEGIN Plot('*   *',x+1,y,c);
                    Plot('*- -*',x+1,y+1,c);
                    Plot('*- -*',x+1,y+2,c);
                    Plot('*-*-*',x+1,y+3,c);
                    Plot('**-**',x+1,y+4,c);
                    Plot('-*-*-',x+1,y+5,c);
              END;
          'x':BEGIN Plot('**--**',x,y+1,c);
                    Plot('-****-',x,y+2,c);
                     Plot('-**-',x+1,y+3,c);
                    Plot('-****-',x,y+4,c);
                    Plot('**--**',x,y+5,c);
              END;
          'X':BEGIN Plot('**--**',x,y,c);
                    Plot('-****-',x,y+1,c);
                     Plot('-**-',x+1,y+2,c);
                     Plot('-**-',x+1,y+3,c);
                    Plot('-****-',x,y+4,c);
                    Plot('**--**',x,y+5,c);
              END;
          'y':BEGIN Plot('**  **',x,y+1,c);
                    Plot('**--**',x,y+2,c);
                    Plot('-****-',x,y+3,c);
                        Plot('-*',x+4,y+4,c);
                    Plot('*****-',x,y+5,c);
              END;
          'Y':BEGIN Plot('**  **',x,y,c);
                    Plot('**--**',x,y+1,c);
                    Plot('-****-',x,y+2,c);
                     Plot('-**-',x+1,y+3,c);
                     Plot('-**-',x+1,y+4,c);
                     Plot('****',x+1,y+5,c);
              END;
          'z':BEGIN Plot('******',x,y+1,c);
                    Plot('*--**-',x,y+2,c);
                     Plot('-**-',x+1,y+3,c);
                    Plot('-**--*',x,y+4,c);
                    Plot('******',x,y+5,c);
              END;
          'Z':BEGIN Plot('******',x,y,c);
                    Plot('*- -**',x,y+1,c);
                      Plot('-**-',x+2,y+2,c);
                     Plot('-**-',x+1,y+3,c);
                    Plot('-**--*',x,y+4,c);
                    Plot('******',x,y+5,c);
              END;
          '‘':BEGIN Plot('****-',x,y+1,c);
                     Plot('-*-*',x+1,y+2,c);
                    Plot('-***-',x,y+3,c);
                    Plot('*-*-',x,y+4,c);
                    Plot('-****',x,y+5,c);
              END;
          '’':BEGIN Plot('-*****',x,y,c);
                    Plot('**-**-',x,y+1,c);
                    Plot('******',x,y+2,c);
                    Plot('**-**-',x,y+3,c);
                    Plot('** **-',x,y+4,c);
                    Plot('** ***',x,y+5,c);
              END;
          '›':BEGIN Plot('-***-*',x,y+1,c);
                    Plot('*--**-',x,y+2,c);
                    Plot('*-**-*',x,y+3,c);
                    Plot('-**--*',x,y+4,c);
                    Plot('*-***-',x,y+5,c);
              END;
          '':BEGIN Plot('-***-*',x,y,c);
                    Plot('*--**-',x,y+1,c);
                    Plot('*-**-*',x,y+2,c);
                    Plot('*-**-*',x,y+3,c);
                    Plot('-**--*',x,y+4,c);
                    Plot('*-***-',x,y+5,c);
              END;
          '†':BEGIN Plot('-**-',x+1,y,c);
                    Plot('***-',x+1,y+1,c);
                    Plot('--**',x+1,y+2,c);
                   Plot('-****',x,y+3,c);
                   Plot('**-**-',x,y+4,c);
                   Plot('-**-**',x,y+5,c);
              END;
          '':BEGIN Plot('**',x+2,y,c);
                    Plot('--',x+2,y+1,c);
                  Plot('-****-',x,y+2,c);
                  Plot('**--**',x,y+3,c);
                  Plot('******',x,y+4,c);
                  Plot('**--**',x,y+5,c);
              END;
          '.':BEGIN Plot('**-',x+1,y+4,c);
                   Plot('-**',x,y+5,c);
              END;
          ':':BEGIN Plot('**-',x+1,y,c);
                   Plot('-**',x,y+1,c);
                    Plot('**-',x+1,y+4,c);
                   Plot('-**',x,y+5,c);
              END;
          ';':BEGIN Plot('**-',x+1,y,c);
                   Plot('-**',x,y+1,c);
                    Plot('-*',x+1,y+4,c);
                    Plot('*-',x+1,y+5,c);
              END;
          ',':BEGIN Plot('-*',x+1,y+4,c);
                    Plot('*-',x+1,y+5,c);
              END;
          '''':BEGIN Plot('-*',x+1,y,c);
                    Plot('*-',x+1,y+1,c);
              END;
          '"':BEGIN Plot('-* -*',x,y,c);
                    Plot('*- *-',x,y+1,c);
              END;
          '*':BEGIN Plot('-*-',x,y,c);
                     Plot('-*-',x+1,y+1,c);
                 Plot('-******',x-3,y+2,c);
                     Plot('-*-',x+1,y+3,c);
                    Plot('-*-',x,y+4,c);
              END;
          '+':BEGIN
                 Plot('  **   ',x,y+2,c);
			        Plot('-****-',x,y+3,c);
                 Plot('  **   ',x,y+4,c);
              END;
          '!':BEGIN Plot('**',x+1,y,c);
                   Plot('-**-',x,y+1,c);
                   Plot('-**-',x,y+2,c);
                    Plot('**',x+1,y+3,c);
                    Plot('--',x+1,y+4,c);
                    Plot('**',x+1,y+5,c);
              END;
          '?':BEGIN Plot('-****-',x,y,c);
                    Plot('**--**',x,y+1,c);
                      Plot('-**-',x+2,y+2,c);
                      Plot('**-',x+2,y+3,c);
                      Plot('--',x+2,y+4,c);
                      Plot('**',x+2,y+5,c);
              END;
          '-':Plot('-****-',x,y+3,c);
          '=':BEGIN
                Plot('-****-',x,y+2,c);
                Plot('-****-',x,y+4,c);
              END;
          '_':Plot('-****-',x,y+5,c);
          '/':BEGIN     Plot('*',x+4,y+1,c);
                       Plot('*-',x+3,y+2,c);
                      Plot('*-',x+2,y+3,c);
                     Plot('*-',x+1,y+4,c);
                    Plot('*-',x+0,y+5,c);
              END;
          '1':BEGIN Plot('-**',x+1,y,c);
                    Plot('***',x+1,y+1,c);
                    Plot('-**',x+1,y+2,c);
                     Plot('**',x+2,y+3,c);
                     Plot('**-',x+2,y+4,c);
                    Plot('****',x+1,y+5,c);
              END;
          '2':BEGIN Plot('-****-',x,y,c);
                    Plot('*- -**',x,y+1,c);
                      Plot('-**-',x+2,y+2,c);
                     Plot('-**-',x+1,y+3,c);
                    Plot('-**--*',x,y+4,c);
                    Plot('******',x,y+5,c);
              END;
          '3':BEGIN Plot('-****-',x,y,c);
                    Plot('**--**',x,y+1,c);
                       Plot('**-',x+3,y+2,c);
                       Plot('-**',x+3,y+3,c);
                    Plot('**--**',x,y+4,c);
                    Plot('-****-',x,y+5,c);
              END;
          '4':BEGIN Plot('-***',x+1,y,c);
                   Plot('-*-**',x,y+1,c);
                   Plot('*--**-',x,y+2,c);
                   Plot('******',x,y+3,c);
                     Plot('-**-',x+2,y+4,c);
                     Plot('****',x+2,y+5,c);
              END;
          '5':BEGIN Plot('******',x,y,c);
                    Plot('**-',x,y+1,c);
                    Plot('*****-',x,y+2,c);
                       Plot('-**',x+3,y+3,c);
                    Plot('**--**',x,y+4,c);
                    Plot('-****-',x,y+5,c);
              END;
          '6':BEGIN Plot('-***',x+1,y,c);
                   Plot('-**-',x,y+1,c);
                   Plot('**-',x,y+2,c);
                   Plot('*****-',x,y+3,c);
                   Plot('**--**',x,y+4,c);
                   Plot('-****-',x,y+5,c);
              END;
          '7':BEGIN Plot('******',x,y,c);
                    Plot('*- -**',x,y+1,c);
                       Plot('-**',x+3,y+2,c);
                      Plot('-**-',x+2,y+3,c);
                      Plot('**-',x+2,y+4,c);
                      Plot('**',x+2,y+5,c);
              END;
          '8':BEGIN Plot('-****-',x,y,c);
                    Plot('**--**',x,y+1,c);
                    Plot('-****-',x,y+2,c);
                    Plot('**--**',x,y+3,c);
                    Plot('**--**',x,y+4,c);
                    Plot('-****-',x,y+5,c);
              END;
          '9':BEGIN Plot('-****-',x,y,c);
                    Plot('**--**',x,y+1,c);
                    Plot('-*****',x,y+2,c);
                       Plot('-**',x+3,y+3,c);
                      Plot('-**-',x+2,y+4,c);
                    Plot('-***-',x,y+5,c);
              END;
          '0':BEGIN Plot('-****-',x,y,c);
                    Plot('**--**',x,y+1,c);
                    Plot('**--**',x,y+2,c);
                    Plot('**--**',x,y+3,c);
                    Plot('**--**',x,y+4,c);
                    Plot('-****-',x,y+5,c);
              END;
          '(':BEGIN Plot('-***',x+2,y,c);
                    Plot('**-',x+2,y+1,c);
                   Plot('**-',x+1,y+2,c);
                   Plot('**-',x+1,y+3,c);
                    Plot('**-',x+2,y+4,c);
                    Plot('-***',x+2,y+5,c);
              END;
          ')':BEGIN Plot('***-',x,y,c);
                     Plot('-**',x+1,y+1,c);
                      Plot('-**',x+2,y+2,c);
                      Plot('-**',x+2,y+3,c);
                     Plot('-**',x+1,y+4,c);
                    Plot('***-',x,y+5,c);
              END;
          END;
     END;
     Inc(CursorX,ord(s[0]));
END;

FUNCTION LesXY(x,y,c : Word) : String;
VAR i : Byte; k : Char; st : String;
BEGIN
    st := '';
    Repeat
          GetK(Addr(k));
          IF Ord(k) = 8 THEN BEGIN
			    SkrivXY(st[Length(st)],x+(Length(st)*7),y,0,false);
				 Delete(st,Length(st),1);
				 DrawImage;
			 END;
          IF Ord(k) >= 32 THEN BEGIN
			    st := st + k;
				 SkrivXY(k,x+(Length(st)*7),y,c,false);
             DrawImage;
			 END;
    UNTIL Ord(k) = 13;
    LesXY := st;
END;

PROCEDURE SkrivLn(s : String; c : Word);
BEGIN
     SkrivXY(s,(CursorX*7),(CursorY*7),c,false);
     Inc(CursorY);
     IF CursorY >= 28 THEN BEGIN
	     SkyvSkjermOpp;
		  CursorY := 27;
	  END;
     CursorX := 0;
END;

FUNCTION LesLn(c : Word) : String;
BEGIN
     LesLn := LesXY((CursorX-1)*7,(CursorY*7),c);
     Inc(CursorY);
     IF CursorY >= 28 THEN BEGIN
	     SkyvSkjermOpp;
		  CursorY := 27;
	  END;
     CursorX := 0;
END;

FUNCTION Les(c : Word) : String;
BEGIN
     Les := LesXY((CursorX-1)*7,(CursorY*7),c);
     IF CursorY >= 28 THEN BEGIN
	     SkyvSkjermOpp;
		  CursorY := 27;
	  END;
END;

PROCEDURE Skriv(s : String; c : Word);
BEGIN
     SkrivXY(s,(CursorX*7),(CursorY*7),c,false);
     IF CursorY >= 28 THEN BEGIN
	     SkyvSkjermOpp;
		  CursorY := 27;
	  END;
END;

PROCEDURE SwapAllColors(nr1, nr2 : Byte);
VAR
   temp : Byte;
   x,y  : Byte;
BEGIN
   FOR x := 0 TO 31 DO BEGIN
      FOR y := 0 TO 31 DO BEGIN
         temp := G(x,y);
         IF temp = nr1 THEN BEGIN
            P(x,y,nr2);
         END ELSE BEGIN
            IF temp = nr2 THEN BEGIN
               P(x,y,nr1);
            END;
         END;
      END;
   END;
END;

PROCEDURE SelectSortLeft(x,y : Byte);
VAR
   second_color : Byte;
   b,t : Byte;
   c1,c2 : Byte;
   rgb : Array [0..2] of Byte;
   ofc : Byte;
   b13, b3 : Word;
   s : String[3];
BEGIN
   ofc := first_color;
   IF first_done THEN BEGIN
      IF NOT (first_color = TheImage[(y shl 8)+(y shl 6)+x+321]) THEN BEGIN
         second_color := TheImage[(y shl 8)+(y shl 6)+x+321];
         IF first_color > second_color THEN BEGIN
            b := first_color;
            first_color := second_color;
            second_color := b;
			END ELSE IF first_color = second_color THEN Exit;
         SkrivXY('Sorting',245,15,$0100,false);
         FOR t := first_color TO second_color DO BEGIN
            Str(t,s);
            SkrivXY(s,260,28,$0100,false);
            FOR b := first_color TO second_color-1 DO BEGIN
               IF b > 2 THEN BEGIN
                  b3 := b*3;
                  b13 := b3+3;
   	            c1 := ThePal[b3]+ThePal[b3+1]+ThePal[b3+2];
   	            c2 := ThePal[b13]+ThePal[b13+1]+ThePal[b13+2];
   	            IF c1 < c2 THEN BEGIN
                     Move(ThePal[b3],rgb[0],3);
                     Move(ThePal[b13],ThePal[b3],3);
                     Move(rgb[0],ThePal[b13],3);
                     SwapAllColors(b,b+1);
   	            END;
               END;
            END;
            SkrivXY(s,260,28,$0101,false);
         END;
         TegnThumb;
         DrawImage;
         SetPalette;
         palette := true;
         first_done := false; {reset the two-selection}
      END;
   END;
   first_color := ofc;
END;

PROCEDURE SelectSortRight(x,y : Byte);
VAR
   second_color : Byte;
   b,t : Byte;
   c1,c2 : Byte;
   rgb : Array [0..2] of Byte;
   ofc : Byte;
   b13, b3 : Word;
   s : String[3];
BEGIN
   ofc := first_color;
   IF first_done THEN BEGIN
      IF NOT (first_color = TheImage[(y shl 8)+(y shl 6)+x+321]) THEN BEGIN
         second_color := TheImage[(y shl 8)+(y shl 6)+x+321];
         IF first_color > second_color THEN BEGIN
            b := first_color;
            first_color := second_color;
            second_color := b;
			END ELSE IF first_color = second_color THEN Exit;
         SkrivXY('Sorting',245,15,$0100,false);
         FOR t := first_color TO second_color DO BEGIN
            Str(t,s);
            SkrivXY(s,260,28,$0100,false);
            FOR b := first_color TO second_color-1 DO BEGIN
               IF b > 2 THEN BEGIN
                  b3 := b*3;
                  b13 := b3+3;
   	            c1 := ThePal[b3]+ThePal[b3+1]+ThePal[b3+2];
   	            c2 := ThePal[b13]+ThePal[b13+1]+ThePal[b13+2];
   	            IF c1 > c2 THEN BEGIN
                     Move(ThePal[b3],rgb[0],3);
                     Move(ThePal[b13],ThePal[b3],3);
                     Move(rgb[0],ThePal[b13],3);
                     SwapAllColors(b,b+1);
   	            END;
               END;
            END;
            SkrivXY(s,260,28,$0101,false);
         END;
         TegnThumb;
         DrawImage;
         SetPalette;
         palette := true;
         first_done := false; {reset the two-selection}
      END;
   END;
   first_color := ofc;
END;

PROCEDURE SelectKillSortRemoveReallyDarkAll(x,y : Byte);
VAR fc : Byte;
BEGIN
   fc := first_color;
   IF first_done THEN BEGIN
      SelectKill(x,y);
      first_color := fc;
      first_done := true;
      SelectRemoveReallyDark(x,y);
      first_color := fc;
      first_done := true;
      SelectSortRight(x,y);
   END;
END;


FUNCTION SavePCX(filename : String) : Boolean;
VAR
   f : File;
   b : Byte;
   w : Word;
   s : String;
   x,y : Byte;
 PROCEDURE EncPut(byt, cnt : Byte);
 BEGIN
    IF ((cnt = 1) AND (($C0 AND byt) <> $C0)) THEN BEGIN
       BlockWrite(f,byt,1); {Skal denne egentlig v‘re ett 2-byte word?}
    END ELSE BEGIN
       cnt := ($C0 OR cnt);
       BlockWrite(f,cnt,1);
       BlockWrite(f,byt,1);
    END;
 END;
BEGIN
   {$I-}
   Assign(f,filename);
   Rewrite(f,1);
   b := $0a; BlockWrite(f,b,1); {Zsoft Flag}
   {$I+}
   IF IOResult <> 0 THEN BEGIN
      SavePCX := false;
      Exit;
   END ELSE SavePCX := true;
   b := 5;   BlockWrite(f,b,1); {Version}
   b := 0;   BlockWrite(f,b,1); {RLE encoded}
   b := 8;   BlockWrite(f,b,1); {Bits per pixel per plane}
   w := 0;   BlockWrite(f,w,2); {X left}
   w := 0;   BlockWrite(f,w,2); {Y upper}
   w := 31;  BlockWrite(f,w,2); {X right}
   w := 31;  BlockWrite(f,w,2); {Y lower}
   w := 96;  BlockWrite(f,w,2); {DPI horizontal}
   w := 96;  BlockWrite(f,w,2); {DPI vertical}
   b := 0;   FOR w := 1 TO 48 DO BlockWrite(f,b,1); {48 zero bytes}
   b := 0;   BlockWrite(f,b,1); {Reserved 0}
   b := 1;   BlockWrite(f,b,1); {Number of planes}
   w := 32;  BlockWrite(f,w,2); {Bytes per each plane line}
   w := 1;   BlockWrite(f,w,2); {Header palette interpretation}
   w := 0;   BlockWrite(f,w,2); {pbrush 4 x}
   w := 0;   BlockWrite(f,w,2); {pbrush 4 y}
   b := 0;   FOR w := 1 TO 54 DO BlockWrite(f,b,1); {54 zero bytes}
   x := 0;
   y := 0;
   Repeat
      b := G(x,y);
      Inc(x);
      IF x > 31 THEN BEGIN
         Dec(x,32);
         Inc(y);
      END;
      EncPut(b,1);
   UNTIL (x > 31) OR (y > 31);
   FOR w := 0 TO 767 DO thepal[w] := thepal[w] shl 2;
   b := $0c;
   BlockWrite(f,b,1);
   BlockWrite(f,thepal,768);
   FOR w := 0 TO 767 DO thepal[w] := thepal[w] shr 2;
   Close(f);
END;

FUNCTION GetActiveColor : Byte;
BEGIN
   {GetActiveColor := ((acticoly-1) div 7 * 32 + (acticolx-1) div 7);}
   GetActiveColor := (acticolx+acticoly shl 5-33) div 7;
END;

PROCEDURE SelectInvert(x,y : Byte);
VAR
   second_color : Byte;
   b : Byte;
   ofc : Byte;
LABEL si;
BEGIN
   ofc := first_color;
   IF first_done THEN BEGIN
      IF NOT (first_color = TheImage[(y shl 8)+(y shl 6)+x+321]) THEN BEGIN
         second_color := TheImage[(y shl 8)+(y shl 6)+x+321];
         IF first_color > second_color THEN BEGIN
            b := first_color;
            first_color := second_color;
            second_color := b;
			END ELSE IF first_color = second_color THEN GOTO si;
         FOR b := first_color TO second_color DO BEGIN
            ThePal[b*3] := ThePal[b*3]*-1+63;
            ThePal[b*3+1] := ThePal[b*3+1]*-1+63;
            ThePal[b*3+2] := ThePal[b*3+2]*-1+63;
         END;
         SetPalette;
         palette := true;
      END ELSE goto si;
      first_done := false; {reset the two-selection}
   END ELSE BEGIN
      si:
      b := TheImage[(y shl 8)+(y shl 6)+x+321];
      ThePal[b*3] := ThePal[b*3]*-1+63;
      ThePal[b*3+1] := ThePal[b*3+1]*-1+63;
      ThePal[b*3+2] := ThePal[b*3+2]*-1+63;
      SetPalette;
      palette := true;
   END;
   first_color := ofc;
END;

PROCEDURE SelectRemoveReallyDark(x,y : Byte);
VAR
   second_color : Byte;
   b : Byte;
   k : Boolean;
LABEL jau, jau2, sk;
BEGIN
   IF first_done THEN BEGIN
      IF NOT (first_color = TheImage[(y shl 8)+(y shl 6)+x+321]) THEN BEGIN
         second_color := TheImage[(y shl 8)+(y shl 6)+x+321];
         IF first_color > second_color THEN BEGIN
            b := first_color;
            first_color := second_color;
            second_color := b;
			END ELSE IF first_color = second_color THEN GOTO sk;
         FOR b := first_color TO second_color DO BEGIN
            IF (ThePal[b*3] < 20) AND
				   (ThePal[b*3+1] < 20) AND
					(ThePal[b*3+2] < 20) THEN BEGIN
					   ThePal[b*3] := 0;
                  ThePal[b*3+1] := 0;
                  ThePal[b*3+2] := 0;
               END;
         END;
         SetPalette;
         palette := true;
      END ELSE goto sk;
      first_done := false; {reset the two-selection}
   END ELSE BEGIN
      sk:
      b := TheImage[(y shl 8)+(y shl 6)+x+321];
      k := false;
      IF (ThePal[b*3] < 10) AND
		   (ThePal[b*3+1] < 10) AND
			(ThePal[b*3+2] < 10) THEN BEGIN
			   ThePal[b*3] := 0;
            ThePal[b*3+1] := 0;
            ThePal[b*3+2] := 0;
      END;
      SetPalette;
      palette := true;
   END;
END;

PROCEDURE SelectKill(x,y : Byte);
VAR
   second_color : Byte;
   b : Byte;
   k : Boolean;
LABEL jau, jau2, sk;
BEGIN
   IF first_done THEN BEGIN
      IF NOT (first_color = TheImage[(y shl 8)+(y shl 6)+x+321]) THEN BEGIN
         second_color := TheImage[(y shl 8)+(y shl 6)+x+321];
         IF first_color > second_color THEN BEGIN
            b := first_color;
            first_color := second_color;
            second_color := b;
			END ELSE IF first_color = second_color THEN GOTO sk;
         FOR b := first_color TO second_color DO BEGIN
            k := false;
            IF b < 3 THEN BEGIN k := true; GOTO jau; END;
            FOR x := 0 TO 31 DO FOR y := 0 TO 31 DO BEGIN
               IF G(x,y) = b THEN BEGIN k := true; GOTO jau; END;
            END;
            jau:
            IF NOT k THEN BEGIN
				   ThePal[b*3] := ThePal[bc*3];
               ThePal[b*3+1] := ThePal[bc*3+1];
               ThePal[b*3+2] := ThePal[bc*3+2];
            END;
         END;
         SetPalette;
         palette := true;
      END ELSE goto sk;
      first_done := false; {reset the two-selection}
   END ELSE BEGIN
      sk:
      b := TheImage[(y shl 8)+(y shl 6)+x+321];
      k := false;
      IF b < 3 THEN BEGIN k := true; GOTO jau; END;
      FOR x := 0 TO 31 DO FOR y := 0 TO 31 DO BEGIN
         IF G(x,y) = b THEN BEGIN k := true; GOTO jau2; END;
      END;
      jau2:
      IF NOT k THEN BEGIN
   	   ThePal[b*3] := ThePal[bc*3];
         ThePal[b*3+1] := ThePal[bc*3+1];
         ThePal[b*3+2] := ThePal[bc*3+2];
      END;
      SetPalette;
      palette := true;
   END;
END;

PROCEDURE SelectionOn(x,y : Byte);
VAR
   second_color : Byte;
BEGIN
   IF first_done THEN
      first_done := false
   ELSE BEGIN
      first_done := true;
    	first_color := TheImage[(y shl 8)+(y shl 6)+x+321];
   END;
END;

PROCEDURE FadeReady;
BEGIN
   IF nofade THEN
      SetPalette
   ELSE BEGIN
      Move(ThePal,MoreMem,768);
	   FillChar(ThePal,768,#0);
	   SetPalette;
   END;
END;

PROCEDURE WhiteFadeReady; {thepal->moremem}
VAR
   temp, b : Byte;
BEGIN
   IF nofade THEN
      SetPalette
   ELSE BEGIN
      FillChar(MoreMem,768,63);
      SetPalette;
   END;
END;

PROCEDURE SelectMono(x,y : Byte);
VAR
   second_color : Byte;
   b,t : Byte;
LABEL sm;
BEGIN
   IF first_done THEN BEGIN
      IF NOT (first_color = TheImage[(y shl 8)+(y shl 6)+x+321]) THEN BEGIN
         second_color := TheImage[(y shl 8)+(y shl 6)+x+321];
         IF first_color > second_color THEN BEGIN
            b := first_color;
            first_color := second_color;
            second_color := b;
			END ELSE IF first_color = second_color THEN GOTO sm;
         FOR b := first_color TO second_color DO BEGIN
            t := (ThePal[b*3]+ThePal[b*3+1]+ThePal[b*3+2]) div 3;
            FillChar(ThePal[b*3],3,t);
         END;
         SetPalette;
         palette := true;
      END ELSE goto sm;
      first_done := false; {reset the two-selection}
   END ELSE BEGIN
      sm:
      b := TheImage[(y shl 8)+(y shl 6)+x+321];
      t := (ThePal[b*3]+ThePal[b*3+1]+ThePal[b*3+2]) div 3;
      FillChar(ThePal[b*3],3,t);
      SetPalette;
      palette := true;
   END;
END;

PROCEDURE ClearKb;
BEGIN
   Inline($FA);
   memw[$40:$1A] := memw[$40:$1C];
   Inline($FB);
END;

PROCEDURE SelectColorGradient(x,y : Byte);
VAR
   second_color : Byte;
BEGIN
   IF first_done THEN BEGIN
      IF NOT (first_color = TheImage[(y shl 8)+(y shl 6)+x+321]) THEN BEGIN
         second_color := TheImage[(y shl 8)+(y shl 6)+x+321];
         SetColorGradient(first_color,second_color);
         SetPalette;
         palette := true;
      END;
      first_done := false; {reset the two-selection}
   END;
END;

PROCEDURE SelectBright(x,y : Byte);
VAR
   second_color : Byte;
   b,t : Byte;
   ofc : Byte;
LABEL sb;
BEGIN
   ofc := first_color;
   IF first_done THEN BEGIN
      IF NOT (first_color = TheImage[(y shl 8)+(y shl 6)+x+321]) THEN BEGIN
         second_color := TheImage[(y shl 8)+(y shl 6)+x+321];
         IF first_color > second_color THEN BEGIN
            b := first_color;
            first_color := second_color;
            second_color := b;
			END ELSE IF first_color = second_color THEN GOTO sb;
         FOR b := first_color TO second_color DO BEGIN
            IF ThePal[b*3] < 63 THEN
               Inc(ThePal[b*3],1)
            ELSE
               ThePal[b*3] := 63;
            IF ThePal[b*3+1] < 63 THEN
               Inc(ThePal[b*3+1],1)
            ELSE
               ThePal[b*3+1] := 63;
            IF ThePal[b*3+2] < 63 THEN
               Inc(ThePal[b*3+2],1)
            ELSE
               ThePal[b*3+2] := 63;
         END;
         SetPalette;
         palette := true;
      END ELSE goto sb;
   END ELSE BEGIN
      sb:
      b := TheImage[(y shl 8)+(y shl 6)+x+321];
      IF ThePal[b*3] < 63 THEN
         Inc(ThePal[b*3],1)
      ELSE
         ThePal[b*3] := 63;
      IF ThePal[b*3+1] < 63 THEN
         Inc(ThePal[b*3+1],1)
      ELSE
         ThePal[b*3+1] := 63;
      IF ThePal[b*3+2] < 63 THEN
         Inc(ThePal[b*3+2],1)
      ELSE
         ThePal[b*3+2] := 63;
      SetPalette;
      palette := true;
   END;
   first_color := ofc;
END;

PROCEDURE SelectDark(x,y : Byte);
VAR
   second_color : Byte;
   b,t : Byte;
   ofc : Byte;
LABEL sd;
BEGIN
   ofc := first_color;
   IF first_done THEN BEGIN
      IF NOT (first_color = TheImage[(y shl 8)+(y shl 6)+x+321]) THEN BEGIN
         second_color := TheImage[(y shl 8)+(y shl 6)+x+321];
         IF first_color > second_color THEN BEGIN
            b := first_color;
            first_color := second_color;
            second_color := b;
			END ELSE IF first_color = second_color THEN GOTO sd;
         FOR b := first_color TO second_color DO BEGIN
            IF ThePal[b*3] > 0 THEN
               Dec(ThePal[b*3],1)
            ELSE
               ThePal[b*3] := 0;
            IF ThePal[b*3+1] > 0 THEN
               Dec(ThePal[b*3+1],1)
            ELSE
               ThePal[b*3+1] := 0;
            IF ThePal[b*3+2] > 0 THEN
               Dec(ThePal[b*3+2],1)
            ELSE
               ThePal[b*3+2] := 0;
         END;
         SetPalette;
         palette := true;
      END ELSE goto sd;
   END ELSE BEGIN
      sd:
      b := TheImage[(y shl 8)+(y shl 6)+x+321];
      IF ThePal[b*3] > 0 THEN
         Dec(ThePal[b*3],1)
      ELSE
         ThePal[b*3] := 0;
      IF ThePal[b*3+1] > 0 THEN
         Dec(ThePal[b*3+1],1)
      ELSE
         ThePal[b*3+1] := 0;
      IF ThePal[b*3+2] > 0 THEN
         Dec(ThePal[b*3+2],1)
      ELSE
         ThePal[b*3+2] := 0;
      SetPalette;
      palette := true;
   END;
   first_color := ofc;
END;

PROCEDURE Bilde2MoreMem;
VAR
	x,y : Byte;
   i : Word;
BEGIN
   i := 0; { Tror det g†r kjappere med i }
   FOR y := 0 TO 31 DO BEGIN
      FOR x := 0 TO 31 DO BEGIN
	      MoreMem[i] := G(x,y);
         Inc(i);
      END;
   END;
END;

PROCEDURE MoreMem2Bilde;
VAR
	x,y : Byte;
   i : Word;
BEGIN
   i := 0; { Tror det g†r kjappere med i }
   FOR y := 0 TO 31 DO BEGIN
      FOR x := 0 TO 31 DO BEGIN
	      P(x,y,MoreMem[i]);
         Inc(i);
      END;
   END;
END;

PROCEDURE SelectMoreRed(x,y : Byte);
VAR
   second_color : Byte;
   b,t : Byte;
   ofc : Byte;
LABEL smr;
BEGIN
   ofc := first_color;
   IF first_done THEN BEGIN
      IF NOT (first_color = TheImage[(y shl 8)+(y shl 6)+x+321]) THEN BEGIN
         second_color := TheImage[(y shl 8)+(y shl 6)+x+321];
         IF first_color > second_color THEN BEGIN
            b := first_color;
            first_color := second_color;
            second_color := b;
			END ELSE IF first_color = second_color THEN GOTO smr;
         FOR b := first_color TO second_color DO BEGIN
            IF ThePal[b*3] < 63 THEN
               Inc(ThePal[b*3],1)
            ELSE
               ThePal[b*3] := 63;
         END;
         SetPalette;
         palette := true;
      END;
   END ELSE BEGIN
      smr:
      b := TheImage[(y shl 8)+(y shl 6)+x+321];
      IF ThePal[b*3] < 63 THEN
         Inc(ThePal[b*3],1)
      ELSE
         ThePal[b*3] := 63;
      SetPalette;
      palette := true;
   END;
   first_color := ofc;
END;

PROCEDURE SelectMoreGreen(x,y : Byte);
VAR
   second_color : Byte;
   b,t : Byte;
   ofc : Byte;
LABEL smg;
BEGIN
   ofc := first_color;
   IF first_done THEN BEGIN
      IF NOT (first_color = TheImage[(y shl 8)+(y shl 6)+x+321]) THEN BEGIN
         second_color := TheImage[(y shl 8)+(y shl 6)+x+321];
         IF first_color > second_color THEN BEGIN
            b := first_color;
            first_color := second_color;
            second_color := b;
			END ELSE IF first_color = second_color THEN GOTO smg;
         FOR b := first_color TO second_color DO BEGIN
            IF ThePal[b*3+1] < 63 THEN
               Inc(ThePal[b*3+1],1)
            ELSE
               ThePal[b*3+1] := 63;
         END;
         SetPalette;
         palette := true;
      END;
   END ELSE BEGIN
      smg:
      b := TheImage[(y shl 8)+(y shl 6)+x+321];
      IF ThePal[b*3+1] < 63 THEN
         Inc(ThePal[b*3+1],1)
      ELSE
         ThePal[b*3+1] := 63;
      SetPalette;
      palette := true;
   END;
END;

PROCEDURE SelectMoreBlue(x,y : Byte);
VAR
   second_color : Byte;
   b,t : Byte;
   ofc : Byte;
LABEL smb;
BEGIN
   ofc := first_color;
   IF first_done THEN BEGIN
      IF NOT (first_color = TheImage[(y shl 8)+(y shl 6)+x+321]) THEN BEGIN
         second_color := TheImage[(y shl 8)+(y shl 6)+x+321];
         IF first_color > second_color THEN BEGIN
            b := first_color;
            first_color := second_color;
            second_color := b;
			END ELSE IF first_color = second_color THEN GOTO smb;
         FOR b := first_color TO second_color DO BEGIN
            IF ThePal[b*3+2] < 63 THEN
               Inc(ThePal[b*3+2],1)
            ELSE
               ThePal[b*3+2] := 63;
         END;
         SetPalette;
         palette := true;
      END;
   END ELSE BEGIN
      smb:
      b := TheImage[(y shl 8)+(y shl 6)+x+321];
      IF ThePal[b*3+2] < 63 THEN
         Inc(ThePal[b*3+2],2)
      ELSE
         ThePal[b*3+2] := 63;
      SetPalette;
      palette := true;
   END;
   first_color := ofc;
END;

PROCEDURE SelectLessRed(x,y : Byte);
VAR
   second_color : Byte;
   b,t : Byte;
   ofc : Byte;
LABEL slr;
BEGIN
   ofc := first_color;
   IF first_done THEN BEGIN
      IF NOT (first_color = TheImage[(y shl 8)+(y shl 6)+x+321]) THEN BEGIN
         second_color := TheImage[(y shl 8)+(y shl 6)+x+321];
         IF first_color > second_color THEN BEGIN
            b := first_color;
            first_color := second_color;
            second_color := b;
			END ELSE IF first_color = second_color THEN GOTO slr;
         FOR b := first_color TO second_color DO BEGIN
            IF ThePal[b*3] > 0 THEN
               Dec(ThePal[b*3],1)
            ELSE
               ThePal[b*3] := 0;
         END;
         SetPalette;
         palette := true;
      END;
   END ELSE BEGIN
      slr:
      b := TheImage[(y shl 8)+(y shl 6)+x+321];
      IF ThePal[b*3] > 0 THEN
         Dec(ThePal[b*3],1)
      ELSE
         ThePal[b*3] := 0;
      SetPalette;
      palette := true;
   END;
   first_color := ofc;
END;

PROCEDURE SelectLessGreen(x,y : Byte);
VAR
   second_color : Byte;
   b,t : Byte;
   ofc : Byte;
LABEL slg;
BEGIN
   ofc := first_color;
   IF first_done THEN BEGIN
      IF NOT (first_color = TheImage[(y shl 8)+(y shl 6)+x+321]) THEN BEGIN
         second_color := TheImage[(y shl 8)+(y shl 6)+x+321];
         IF first_color > second_color THEN BEGIN
            b := first_color;
            first_color := second_color;
            second_color := b;
			END ELSE IF first_color = second_color THEN GOTO slg;
         FOR b := first_color TO second_color DO BEGIN
            IF ThePal[b*3+1] > 0 THEN
               Dec(ThePal[b*3+1],1)
            ELSE
               ThePal[b*3+1] := 0;
         END;
         SetPalette;
         palette := true;
      END;
   END ELSE BEGIN
      slg:
      b := TheImage[(y shl 8)+(y shl 6)+x+321];
      IF ThePal[b*3+1] > 0 THEN
         Dec(ThePal[b*3+1],1)
      ELSE
         ThePal[b*3+1] := 0;
      SetPalette;
      palette := true;
   END;
   first_color := ofc;
END;

PROCEDURE SelectLessBlue(x,y : Byte);
VAR
   second_color : Byte;
   b,t : Byte;
   ofc : Byte;
LABEL slb;
BEGIN
   ofc := first_color;
   IF first_done THEN BEGIN
      IF NOT (first_color = TheImage[(y shl 8)+(y shl 6)+x+321]) THEN BEGIN
         second_color := TheImage[(y shl 8)+(y shl 6)+x+321];
         IF first_color > second_color THEN BEGIN
            b := first_color;
            first_color := second_color;
            second_color := b;
			END ELSE IF first_color = second_color THEN GOTO slb;
         FOR b := first_color TO second_color DO BEGIN
            IF ThePal[b*3+2] > 0 THEN
               Dec(ThePal[b*3+2],1)
            ELSE
               ThePal[b*3+2] := 0;
         END;
         SetPalette;
         palette := true;
      END;
   END ELSE BEGIN
      slb:
      b := TheImage[(y shl 8)+(y shl 6)+x+321];
      IF ThePal[b*3+2] > 0 THEN
         Dec(ThePal[b*3+2],1)
      ELSE
         ThePal[b*3+2] := 0;
      SetPalette;
      palette := true;
   END;
   first_color := ofc;
END;

PROCEDURE Letter(xs : Word;ys : Byte;c : Char;co : Byte); {4x6}
VAR
   x : Word;
   y : Byte;

   PROCEDURE PlotLine(s : String);
   VAR i : Byte;
   BEGIN
      FOR i := 1 TO Ord(s[0]) DO IF s[i] = '*' THEN TheImage[(y shl 8)+(y shl 6)+x+i-1] := co;
      Inc(y);
   END;
BEGIN
   x := xs;
   CASE c OF
      '0': BEGIN
            y := ys;
            PlotLine(' **** ');
            PlotLine('*    *');
            PlotLine('*    *');
            PlotLine('*    *');
            PlotLine('*    *');
            PlotLine('*    *');
            PlotLine(' **** ');
         END;
      '1': BEGIN
            y := ys;
            PlotLine('   *   ');
            PlotLine('  **   ');
            PlotLine('   *   ');
            PlotLine('   *   ');
            PlotLine('   *   ');
            PlotLine('   *   ');
            PlotLine('  ***  ');
         END;
      '2': BEGIN
            y := ys;
            PlotLine(' **** ');
            PlotLine('*    *');
            PlotLine('    * ');
            PlotLine('   *  ');
            PlotLine('  *   ');
            PlotLine(' *    ');
            PlotLine('******');
         END;
      '3': BEGIN
            y := ys;
            PlotLine(' **** ');
            PlotLine('*    *');
            PlotLine('     *');
            PlotLine('   ** ');
            PlotLine('     *');
            PlotLine('*    *');
            PlotLine(' **** ');
         END;
      '4': BEGIN
            y := ys;
            PlotLine('*   * ');
            PlotLine('*   * ');
            PlotLine('*   * ');
            PlotLine('******');
            PlotLine('    * ');
            PlotLine('    * ');
            PlotLine('    * ');
         END;
      '5': BEGIN
            y := ys;
            PlotLine('******');
            PlotLine('*     ');
            PlotLine('***** ');
            PlotLine('     *');
            PlotLine('     *');
            PlotLine('*    *');
            PlotLine(' **** ');
         END;
      '6': BEGIN
            y := ys;
            PlotLine(' **** ');
            PlotLine('*    *');
            PlotLine('*     ');
            PlotLine('***** ');
            PlotLine('*    *');
            PlotLine('*    *');
            PlotLine(' **** ');
         END;
      '7': BEGIN
            y := ys;
            PlotLine('******');
            PlotLine('     *');
            PlotLine('    * ');
            PlotLine('   *  ');
            PlotLine('  *   ');
            PlotLine('  *   ');
            PlotLine('  *   ');
         END;
      '8': BEGIN
            y := ys;
            PlotLine(' **** ');
            PlotLine('*    *');
            PlotLine('*    *');
            PlotLine(' **** ');
            PlotLine('*    *');
            PlotLine('*    *');
            PlotLine(' **** ');
         END;
      '9': BEGIN
            y := ys;
            PlotLine(' **** ');
            PlotLine('*    *');
            PlotLine('*    *');
            PlotLine(' *****');
            PlotLine('     *');
            PlotLine('*    *');
            PlotLine(' **** ');
         END;
      'R': BEGIN
            y := ys;
            PlotLine('***** ');
            PlotLine('*    *');
            PlotLine('*    *');
            PlotLine('***** ');
            PlotLine('*    *');
            PlotLine('*    *');
            PlotLine('*    *');
         END;
      'G': BEGIN
            y := ys;
            PlotLine(' **** ');
            PlotLine('*    *');
            PlotLine('*     ');
            PlotLine('*  ***');
            PlotLine('*    *');
            PlotLine('*    *');
            PlotLine(' **** ');
         END;
      'B': BEGIN
            y := ys;
            PlotLine('***** ');
            PlotLine('*    *');
            PlotLine('*    *');
            PlotLine('***** ');
            PlotLine('*    *');
            PlotLine('*    *');
            PlotLine('***** ');
         END;
      'N': BEGIN
            y := ys;
            PlotLine('*    *');
            PlotLine('**   *');
            PlotLine('* *  *');
            PlotLine('*  * *');
            PlotLine('*   **');
            PlotLine('*    *');
            PlotLine('*    *');
         END;
      'a','i': BEGIN
            y := ys;
            PlotLine('');
            PlotLine('   ***   ');
            PlotLine('  *   *  ');
            PlotLine(' *     * ');
            PlotLine(' *   ****');
            PlotLine(' *     * ');
            PlotLine('  *   *  ');
            PlotLine('   ***   ');
         END;
      'b': BEGIN
            y := ys;
            PlotLine('');
            PlotLine('   *** * ');
            PlotLine('  *   *  ');
            PlotLine(' *   * * ');
            PlotLine(' *     * ');
            PlotLine(' *     * ');
            PlotLine('  *   *  ');
            PlotLine('   ***   ');
         END;
      'c': BEGIN
            y := ys;
            PlotLine('    *    ');
            PlotLine('   ***   ');
            PlotLine('  * * *  ');
            PlotLine(' *  *  * ');
            PlotLine(' *     * ');
            PlotLine(' *     * ');
            PlotLine('  *   *  ');
            PlotLine('   ***   ');
         END;
      'd': BEGIN
            y := ys;
            PlotLine('');
            PlotLine(' * ***   ');
            PlotLine('  *   *  ');
            PlotLine(' * *   * ');
            PlotLine(' *     * ');
            PlotLine(' *     * ');
            PlotLine('  *   *  ');
            PlotLine('   ***   ');
         END;
      'e': BEGIN
            y := ys;
            PlotLine('');
            PlotLine('   ***   ');
            PlotLine('  *   *  ');
            PlotLine(' *     * ');
            PlotLine('***    * ');
            PlotLine(' *     * ');
            PlotLine('  *   *  ');
            PlotLine('   ***   ');
         END;
      'f': BEGIN
            y := ys;
            PlotLine('');
            PlotLine('   ***   ');
            PlotLine('  *   *  ');
            PlotLine(' *     * ');
            PlotLine(' *     * ');
            PlotLine(' * *   * ');
            PlotLine('  *   *  ');
            PlotLine(' * ***   ');
         END;
      'g': BEGIN
            y := ys;
            PlotLine('         ');
            PlotLine('   ***   ');
            PlotLine('  *   *  ');
            PlotLine(' *     * ');
            PlotLine(' *     * ');
            PlotLine(' *  *  * ');
            PlotLine('  * * *  ');
            PlotLine('   ***   ');
            PlotLine('    *    ');
         END;
      'h': BEGIN
            y := ys;
            PlotLine('');
            PlotLine('   ***   ');
            PlotLine('  *   *  ');
            PlotLine(' *     * ');
            PlotLine(' *     * ');
            PlotLine(' *   * * ');
            PlotLine('  *   *  ');
            PlotLine('   *** * ');
         END;
      '-': BEGIN
            y := ys;
            PlotLine('');
            PlotLine('');
            PlotLine('');
            PlotLine(' **** ');
         END;
      ' ': BEGIN
            y := ys;
            PlotLine('*********');
            PlotLine('*********');
            PlotLine('*********');
            PlotLine('*********');
            PlotLine('*********');
            PlotLine('*********');
            PlotLine('*********');
            PlotLine('*********');
            PlotLine('*********');
         END;
      '!': BEGIN
            y := ys;
            PlotLine(' ***** ****  *****');
            PlotLine('*      *   * *');
            PlotLine(' ****  ****  ***');
            PlotLine('     * *     *');
            PlotLine('*****  *     *****');
       END;
       't': BEGIN
           y := ys;
           PlotLine('*** *   * ***  *** *   ***   *   *  **  ***  ***');
           PlotLine(' *  *   * *  *  *  *   *     ** ** *  * *  * *');
           PlotLine(' *  *   * ***   *  *   **    * * * *  * *  * **');
           PlotLine(' *  *   * *  *  *  *   *     *   * *  * *  * *');
           PlotLine(' *   ***  *  *  *  *** ***   *   *  **  ***  ***');
       END;
   END;
END;

PROCEDURE TegnRuter;
VAR
   x : Word;
   y : Byte;
BEGIN
   IF bilde THEN exit;
   FOR y := 0 TO 31 DO BEGIN
      FOR x := 0 TO 31 DO BEGIN
         P(x,y,bc);
      END;
   END;
END;

PROCEDURE TegnThumb;
VAR
   x : Word;
   y : Byte;
BEGIN
   FOR y := 66 TO 97 DO BEGIN
      FOR x := 195 TO 226 DO BEGIN
         TheImage[(y shl 8)+(y shl 6)+x] := G(x-195,y-66);
      END;
   END;
END;

PROCEDURE SetColorGradient(c1,c2 : Byte);
VAR
   colorspan : Integer;
   red_begin, red_end : Byte;
   green_begin, green_end : Byte;
   blue_begin, blue_end : Byte;
   rinc, ginc, binc : Real;
   r, g, b : Real;
   i : Byte;
   up : Boolean;
BEGIN
   colorspan := abs(c1-c2)+1;
   red_begin := ThePal[c1*3];
   red_end  := ThePal[c2*3];
   green_begin := ThePal[c1*3+1];
   green_end  := ThePal[c2*3+1];
   blue_begin := ThePal[c1*3+2];
   blue_end  := ThePal[c2*3+2];
   rinc := ((red_end - red_begin) / colorspan);
   ginc := ((green_end - green_begin) / colorspan);
   binc := ((blue_end - blue_begin) / colorspan);
   IF c2 > c1 THEN up := true ELSE up := false;
   r := red_begin;
   g := green_begin;
   b := blue_begin;
   i := c1;
   Repeat
      ThePal[i*3] := Round(r);
      r := r + rinc;
      ThePal[i*3+1] := Round(g);
      g := g + ginc;
      ThePal[i*3+2] := Round(b);
      b := b + binc;
      IF up THEN Inc(i) ELSE Dec(i);
   UNTIL i = c2;
END;

PROCEDURE MonoPalette;
VAR b : Byte;
BEGIN
   ThePal[system1*3+0] := 0;
   ThePal[system1*3+1] := 0;
   ThePal[system1*3+2] := 0;
   ThePal[system2*3+0] := 20;
   ThePal[system2*3+1] := 20;
   ThePal[system2*3+2] := 20;
   ThePal[system3*3+0] := 34;
   ThePal[system3*3+1] := 34;
   ThePal[system3*3+2] := 34;
   ThePal[3*3+0] := 0;
	ThePal[3*3+1] := 0;
	ThePal[3*3+2] := 0;
   ThePal[255*3+0] := 63;
   ThePal[255*3+1] := 63;
   ThePal[255*3+2] := 63;
   SetColorGradient(3,255);
END;

PROCEDURE DefaultPalette;
VAR b : Byte;
BEGIN
   IF palette THEN Exit;
   ThePal[system1*3+0] := 0;
   ThePal[system1*3+1] := 0;
   ThePal[system1*3+2] := 0;
   ThePal[system2*3+0] := 20;
   ThePal[system2*3+1] := 20;
   ThePal[system2*3+2] := 20;
   ThePal[system3*3+0] := 34;
   ThePal[system3*3+1] := 34;
   ThePal[system3*3+2] := 34;
   ThePal[3*3+0] := 0;
	ThePal[3*3+1] := 0;
	ThePal[3*3+2] := 0;
   ThePal[31*3+0] := 63;
   ThePal[31*3+1] := 63;
   ThePal[31*3+2] := 63;
   SetColorGradient(3,31);
   ThePal[32*3+0] := 63;
   ThePal[32*3+1] := 63;
   ThePal[32*3+2] := 63;
   ThePal[63*3+0] := 63;
	ThePal[63*3+1] := 0;
	ThePal[63*3+2] := 0;
   SetColorGradient(32,63);
   ThePal[64*3+0] := 63;
	ThePal[64*3+1] := 0;
	ThePal[64*3+2] := 0;
   ThePal[95*3+0] := 63;
	ThePal[95*3+1] := 63;
	ThePal[95*3+2] := 0;
   SetColorGradient(64,95);
   ThePal[96*3+0] := 63;
	ThePal[96*3+1] := 63;
	ThePal[96*3+2] := 0;
   ThePal[127*3+0] := 0;
	ThePal[127*3+1] := 32;
	ThePal[127*3+2] := 32;
   SetColorGradient(96,127);
   ThePal[128*3+0] := 0;
	ThePal[128*3+1] := 32;
	ThePal[128*3+2] := 32;
   ThePal[159*3+0] := 20;
	ThePal[159*3+1] := 20;
	ThePal[159*3+2] := 63;
   SetColorGradient(128,159);
   ThePal[160*3+0] := 20;
	ThePal[160*3+1] := 20;
	ThePal[160*3+2] := 63;
   ThePal[191*3+0] := 63;
	ThePal[191*3+1] := 63;
	ThePal[191*3+2] := 50;
   SetColorGradient(160,191);
   ThePal[192*3+0] := 63;
	ThePal[192*3+1] := 63;
	ThePal[192*3+2] := 50;
   ThePal[223*3+0] := 32;
	ThePal[223*3+1] := 0;
	ThePal[223*3+2] := 0;
   SetColorGradient(192,223);
   ThePal[224*3+0] := 32;
	ThePal[224*3+1] := 0;
	ThePal[224*3+2] := 0;
   ThePal[239*3+0] := 52;
	ThePal[239*3+1] := 52;
	ThePal[239*3+2] := 63;
   SetColorGradient(224,239);
   ThePal[240*3+0] := 52;
	ThePal[240*3+1] := 52;
	ThePal[240*3+2] := 63;
   ThePal[255*3+0] := 0;
	ThePal[255*3+1] := 0;
	ThePal[255*3+2] := 0;
   SetColorGradient(240,255);
END;

FUNCTION HvorMangeForover(x,y : Byte) : Byte;
VAR xt,yt,c,teller : Byte;
BEGIN
   teller := 0;
   c := G(x,y);
   FOR xt := x TO 31 DO FOR yt := y TO 31 DO BEGIN
      IF G(xt,yt) <> c THEN Exit;
      c := G(xt,yt);
      Inc(teller);
      IF teller = 63 THEN Exit;
   END;
   HvorMangeForover := teller;
END;

PROCEDURE FjernDuplikatfarger;
VAR
   source,target : Byte;
   s3,t3 : Word;
   rc,gc,bc : Byte;
   x,y : Byte;
BEGIN
   FOR source := 0 TO 255 DO BEGIN
      s3 := source*3;
      rc := ThePal[s3]; {optimiser med repeat (step)}
      gc := ThePal[s3+1];
      bc := ThePal[s3+2];
      IF (source < 255) THEN target := source+1;
      WHILE (target < 255) DO BEGIN
         t3 := target*3;
         IF (ThePal[t3] = rc) AND (ThePal[t3+1] = gc) AND (ThePal[t3+2] = bc) THEN BEGIN
            FOR x := 0 TO 31 DO FOR y := 0 TO 31 DO IF G(x,y) = target THEN P(x,y,source);
         END;
         Inc(target);
      END;
   END;
END;

FUNCTION FinnesDenneFargen(r,g,b,m : Byte) : Integer;
VAR
   i,i3 : Integer;
   r_sub_m, r_add_m, g_sub_m, g_add_m, b_sub_m, b_add_m : Integer;
LABEL joda;
BEGIN
   r_sub_m := r - m;
   r_add_m := r + m;
   g_sub_m := g - m;
   g_add_m := g + m;
   b_sub_m := b - m;
   b_add_m := b + m;
   FOR i := 0 TO 255 DO BEGIN { denne kan optimiseres med repeat (step 3) }
      i3 := i*3;
      IF (ThePal[i3] > (r_sub_m)) AND (ThePal[i3] < (r_add_m)) THEN
         IF (ThePal[i3+1] > (g_sub_m)) AND (ThePal[i3+1] < (g_add_m)) THEN
            IF (ThePal[i3+2] > (b_sub_m)) AND (ThePal[i3+2] < (b_add_m)) THEN
               GOTO joda;
   END;
   i := -1;
joda:
   FinnesDenneFargen := i; { Hvis i er -1 s† finnes den ikke }
END;

PROCEDURE TegnPalett;
VAR
   c : Byte;
   x,xc : Word;
   y,yc : Byte;
   pre_calc : Word;
BEGIN
   IF bilde THEN Exit;
   c := 0;
   FillChar(TheImage,64000,system3);
   FOR y := 0 TO 58 DO FillChar(TheImage[(y shl 8)+(y shl 6)],226,0);
   FillChar(TheImage[18560],227,system2);
   FOR y := 0 TO 58 DO TheImage[(y shl 8)+(y shl 6)+226] := system2;
   FillChar(TheImage[0],227,system2);
   FOR y := 0 TO 58 DO TheImage[(y shl 8)+(y shl 6)] := system2;
   FOR y := 0 TO 7 DO BEGIN
      FOR x := 0 TO 31 DO BEGIN
         pre_calc := x*7+(y shl 11)+(y shl 7)+(y shl 6)+642;
         FillChar(TheImage[pre_calc],6,c);
         FillChar(TheImage[pre_calc+320],6,c);
         FillChar(TheImage[pre_calc+640],6,c);
         FillChar(TheImage[pre_calc+960],6,c);
         FillChar(TheImage[pre_calc+1280],6,c);
         FillChar(TheImage[pre_calc+1600],6,c);
         Inc(c);
   	END;
   END;
   FOR y := 2 TO 56 DO FillChar(TheImage[(y shl 8)+(y shl 6)+230],86,system2);
END;

PROCEDURE Pswap(x1,x2,y1,y2 : Byte);
VAR temp : Byte;
BEGIN
   temp := G(x1,y1);
   P(x1,y1,G(x2,y2));
   P(x2,y2,temp);
END;

PROCEDURE Echo(cmds : String);
VAR s : String;
BEGIN
   s := GetParam(Addr(cmds));
   IF s = '' THEN
      Writeln('Syntax: echo [text]')
   ELSE
      Writeln(s);
END;

FUNCTION Right(s : String;l : Byte) : String;
VAR ts : String; x : Byte;
BEGIN
   ts[0] := Chr(l);
   FOR x := 0 TO (l-1) DO ts[l-x] := s[Ord(s[0])-x];
   IF Ord(s[0]) < l THEN Right := s ELSE Right := ts;
END;

PROCEDURE Randomerror(cmds : String);
LABEL newerror;
BEGIN
   Writeln;
newerror:
   CASE Random(12) OF
      0: IF lasterror <> 0 THEN BEGIN
            Writeln('Bad command.');
            lasterror := 0;
         END ELSE GOTO newerror;
      1: IF lasterror <> 1 THEN BEGIN
            Writeln('Very bad command indeed.');
            lasterror := 1;
         END ELSE GOTO newerror;
      2: IF lasterror <> 2 THEN BEGIN
            Writeln('That was wrong.');
            lasterror := 2;
         END ELSE GOTO newerror;
      3: IF lasterror <> 3 THEN BEGIN
            Writeln('What do you mean "', cmds, '"?');
            lasterror := 3;
         END ELSE GOTO newerror;
      4: IF lasterror <> 4 THEN BEGIN
            Writeln('That didn''t make sense.');
            lasterror := 4;
         END ELSE GOTO newerror;
      5: IF lasterror <> 5 THEN BEGIN
            Writeln('The command was bad.');
            lasterror := 5;
         END ELSE GOTO newerror;
      6: IF lasterror <> 6 THEN BEGIN
            Writeln('I haven''t got words for how bad that command was.');
            lasterror := 6;
         END ELSE GOTO newerror;
      7: IF lasterror <> 7 THEN BEGIN
            Writeln('Which command was that meant to be?');
            lasterror := 7;
         END ELSE GOTO newerror;
      8: IF lasterror <> 8 THEN BEGIN
            Writeln('You''ve got it all wrong.');
            lasterror := 8;
         END ELSE GOTO newerror;
      9: IF lasterror <> 9 THEN BEGIN
            Writeln('Are you drunk?');
            lasterror := 9;
         END ELSE GOTO newerror;
     10: IF lasterror <> 10 THEN BEGIN
            Writeln('I can''t do that, Dave.');
            lasterror := 10;
         END ELSE GOTO newerror;
     11: IF lasterror <> 11 THEN BEGIN
            Writeln('You can''t do that.');
            lasterror := 11;
         END ELSE GOTO newerror;
     ELSE Writeln('Error during error.');
   END;
   Writeln;
END;

PROCEDURE SetPalette;
VAR
   w,w2 : Word;
BEGIN
   { B›r skrives i asm }
   w := 0;
   Port[$3C8] := 0;
   FOR w2 := 767 DOWNTO 0 DO BEGIN
      Port[$3C9] := ThePal[w];
      Inc(w);
   END;
END;

PROCEDURE GetPalette;
VAR
   w,w2 : Word;
BEGIN
   { B›r absolutt skrives i asm }
   w := 0;
   Port[$3C8] := 0;
   FOR w2 := 767 DOWNTO 0 DO BEGIN
      ThePal[w] := Port[$3C9];
      Inc(w);
   END;
END;

PROCEDURE Cursor(x : Word;y : Byte;pal : Boolean);
VAR
   y320x : Word;
BEGIN
   y320x := (y shl 8)+(y shl 6)+x;
   IF pal THEN BEGIN
      { Palmode cursor }
      IF TheImage[y320x] = system1 THEN BEGIN
         FillChar(TheImage[y320x],8,system3);
			TheImage[y320x+320] := system3;
			TheImage[y320x+327] := system3;
			TheImage[y320x+640] := system3;
			TheImage[y320x+647] := system3;
			TheImage[y320x+960] := system3;
			TheImage[y320x+967] := system3;
			TheImage[y320x+1280] := system3;
			TheImage[y320x+1287] := system3;
			TheImage[y320x+1600] := system3;
			TheImage[y320x+1607] := system3;
			TheImage[y320x+1920] := system3;
 			TheImage[y320x+1927] := system3;
         FillChar(TheImage[y320x+2240],8,system3);
      END ELSE BEGIN
         FillChar(TheImage[y320x],8,system1);
			TheImage[y320x+320] := system1;
			TheImage[y320x+327] := system1;
			TheImage[y320x+640] := system1;
			TheImage[y320x+647] := system1;
			TheImage[y320x+960] := system1;
			TheImage[y320x+967] := system1;
			TheImage[y320x+1280] := system1;
			TheImage[y320x+1287] := system1;
			TheImage[y320x+1600] := system1;
			TheImage[y320x+1607] := system1;
			TheImage[y320x+1920] := system1;
 			TheImage[y320x+1927] := system1;
         FillChar(TheImage[y320x+2240],8,system1);
      END;
   END ELSE BEGIN
      { Drawmode cursor }
      IF TheImage[y320x-2] = system3 THEN BEGIN
         FillChar(TheImage[y320x+638],6,system2);
         TheImage[y320x] := system2;
         TheImage[y320x+1] := system2;
         TheImage[y320x+3] := system2;
         TheImage[y320x+318] := system2;
         TheImage[y320x+323] := system2;
         TheImage[y320x-2] := system2;
         TheImage[y320x-317] := system2;
         TheImage[y320x-322] := system2;
         FillChar(TheImage[y320x-642],6,system2);
      END ELSE BEGIN
         FillChar(TheImage[y320x+638],6,system3);
         TheImage[y320x] := TheImage[y320x-1]; {orig farge}
         TheImage[y320x+1] := TheImage[y320x]; {orig farge}
         TheImage[y320x+3] := system3;
         TheImage[y320x+318] := system3;
         TheImage[y320x+323] := system3;
         TheImage[y320x-2] := system3;
         TheImage[y320x-317] := system3;
         TheImage[y320x-322] := system3;
         FillChar(TheImage[y320x-642],6,system3);
      END;
   END;
END;

FUNCTION FinnLedigFarge : Byte;
VAR
   farge,x,y : Byte;
   brukt : Boolean;
LABEL fant;
BEGIN
   farge := 255;
   Repeat
      brukt := false;
      FOR x := 0 TO 31 DO FOR y := 0 TO 31 DO IF G(x,y) = farge THEN brukt := true;
      IF NOT brukt THEN GOTO fant;
   Dec(farge);
   UNTIL farge = 1; { egentlig 2 fordi den dec'es rett ovenfor. }
fant:
   FinnLedigFarge := farge; { Hvis farge blir <3 s† har man ikke funnet noe. }
END;

PROCEDURE Remove(cmds : String);
VAR
   s,s2 : String;
   f : File;
   silent : Boolean;
BEGIN
   silent := (Pos('silent',cmds)=1);
   IF NOT silent THEN Writeln;
   s := GetParam(Addr(cmds));
   IF supertrim(s) = 'burn.exe' then BEGIN
      Writeln('I refuse to delete anything named "',s,'" - it''s against my principles!');
      Writeln;
      Exit;
   END;
   IF silent THEN BEGIN
         IF Finnes(Addr(s)) THEN BEGIN
            Assign(f,s);
            Erase(f);
         END;
   END ELSE IF Finnes(Addr(s)) THEN BEGIN
      Write('Do you really want to delete ',s,'? ');
      Readln(s2);
      Writeln;
      IF UpCase(s2[1]) <> 'Y' THEN BEGIN
         Writeln('File not deleted.');
      END ELSE BEGIN
         Assign(f,s);
         Erase(f);
         Writeln('File deleted.');
      END;
   END ELSE
      IF Pos('*',s) > 0 THEN
         Writeln('Deleting multiple files is not supported yet. :]')
      ELSE IF s = '' THEN
         Writeln('Syntax: del [filename]')
      ELSE
         Writeln('Couldn''t find ',s,'.');
   IF NOT silent THEN Writeln;
END;

FUNCTION Opph(a : Longint; b : Word) : Longint;
VAR
   t : Longint;
   i : Word;
BEGIN
   t := a;
   IF b = 0 THEN
	   opph := 1
	ELSE BEGIN
      FOR i := 1 TO b-1 DO t := t * a;
      opph := t;
   END;
END;

FUNCTION nTrim(t : String) : String;
VAR
   s : String;
   i : Byte;
BEGIN
   s := '';
   FOR i := 0 TO Ord(t[0]) DO BEGIN
      CASE UPCASE(t[i]) OF
         '0'..'9','A'..'F': s := s + t[i];
      END;
   END;
   ntrim := s;
END;

FUNCTION SuperTrim(s : String) : String;
VAR
	i : Byte;
   s2 : String;
BEGIN
   s2 := '  ';
   Repeat
	   Repeat
	      i := Pos(s2,s);
	      IF i > 0 THEN Delete(s,i,Ord(s2[0])-1);
	   UNTIL i = 0;
	   s2 := s2 + ' ';
   UNTIL Length(s2) = 255;
   IF s[1] = ' ' THEN Delete(s,1,1);
   IF s[Ord(s[0])] = ' ' THEN Delete(s,Ord(s[0]),1);
   FOR i := 1 TO Ord(s[0]) DO IF s[i] in ['A'..'Z'] THEN Inc(s[i],32);
   SuperTrim := s;
END;

PROCEDURE Typefile(cmds : String);
VAR
   filename : String[12];
   f : File;
   c : Char;
   counter : Word;
BEGIN
   filename := GetParam(Addr(cmds));
   IF finnes(Addr(filename)) THEN BEGIN
      Assign(f,filename);
      Reset(f,1);
      Writeln;
      g_linje_teller := 0;
      Repeat
         BlockRead(f,c,1,counter);
         Write(c);
         NextPage;
         IF (c <> Chr(10)) THEN Dec(g_linje_teller);
      UNTIL EOF(f) OR cbreak;
      cbreak := false;
      Close(f);
      Writeln;
   END ELSE BEGIN
      Writeln;
      IF Pos('*',filename) > 0 THEN
         Writeln('Viewing multiple files is not supported yet. :]')
      ELSE IF filename = '' THEN
         Writeln('Syntax: type [filename]')
      ELSE
         Writeln('Couldn''t find ',filename,'.');
      Writeln;
   END;
END;

PROCEDURE P(x,y,c : Byte);
VAR
   pre_calc : Word;
   xc,yc : Byte;
BEGIN
   { Det hadde egentlig v‘rt fint med litt asm her... }
   pre_calc := x*5+(y shl 10)+(y shl 8)+21140;
   FillChar(TheImage[pre_calc],4,c);
   Inc(pre_calc,320);
   FillChar(TheImage[pre_calc],4,c);
   Inc(pre_calc,320);
   FillChar(TheImage[pre_calc],4,c);
END;

FUNCTION G(x : Byte;y : Byte) : Byte;
BEGIN
   {G := TheImage[((y*4)+66)*320+((x*5)+20)];}
   G := TheImage[5*(x+(y shl 8)+4228)]; { Optimized vha kalkis. Jiha. }
END;

PROCEDURE DrawImage;
BEGIN
   asm
      mov dx,3dah
      @l1:
      in al,dx
      and al,8
      jnz @l1
      @l2:
      in al,dx
      and al,8
      jz @l2
      mov si,SEG theimage
      mov ds,si
      lea si,theimage
      mov di,0a000h
      mov es,di
      xor di,di
      mov cx,7d00h
      rep movsw
   end;
END;

PROCEDURE WFK;
BEGIN
   asm
      xor ax,ax
      int 16h
      cmp ax,11779
      jne @slutt
      mov cbreak,1
      @slutt:
   end;
END;

PROCEDURE NextPage;
BEGIN
   Inc(g_linje_teller);
   IF (g_linje_teller > 44) THEN BEGIN
      Writeln;
      Writeln('   ---- Press any key to see next page ----');
      WFK;
      IF cbreak THEN Exit;
      txtcls;
      g_linje_teller := 0;
   END;
END;

FUNCTION CapsOn : Boolean;
VAR b : Byte;
BEGIN
   asm
      xor bx,bx
      mov ah,2
      int 16h
      mov b,al
   end;
   CapsOn := ((b AND 64) > 0);
END;

FUNCTION ShiftOn : Boolean;
VAR b : Byte;
BEGIN
   asm
      xor bx,bx
      mov ah,2
      int 16h
      mov b,al
   end;
   ShiftOn := ((b AND 2) > 0) OR ((b AND 1) > 0);
END;

FUNCTION CtrlOn : Boolean;
VAR b : Byte;
BEGIN
   asm
      xor bx,bx
      mov ah,2
      int 16h
      mov b,al
   end;
   CtrlOn := ((b AND 4) > 0);
END;

PROCEDURE LinjeFarge(y,lengde,c : Byte);
VAR x : Byte;
BEGIN
   FOR x := 0 TO (lengde*2)-1 DO BEGIN
      Inc(x);
      Mem[$b800:y*160+x] := c;
   END;
END;

FUNCTION GetCursorY : Byte;
VAR b : Byte;
BEGIN
   asm
      xor bx,bx
      mov ah,03h
      int 10h
      mov b,dh
   end;
   GetCursorY := b;
END;

FUNCTION Fsize(fn : sp) : Longint;
VAR f : File;
BEGIN
   {$I-}
   Assign(f,fn^);
   Reset(f,1);
   fsize := filesize(f);
   Close(f);
   {$I+}
   IF ((IOResult <> 0) or (fn^ = '')) THEN fsize := 0;
END;

FUNCTION Finnes(fn : sp) : Boolean;
VAR f : File;
BEGIN
   {$I-}
   Assign(f,fn^);
   Reset(f,1);
   Close(f);
   {$I+}
   finnes := ((IOResult = 0) and (fn^ <> ''));
END;

FUNCTION GetParam(s : sp) : String;
VAR i : Integer; ts : String;
BEGIN
   ts := '';
   IF (pos(' ',s^) > 0) and (Ord(s^[0]) > 0) THEN BEGIN
      ts[0] := Chr(Ord(s^[0])-pos(' ',s^));
      FOR i := (pos(' ',s^)+1) TO Ord(s^[0]) DO ts[i-pos(' ',s^)] := s^[i];
   END;
   GetParam := ts;
END;

PROCEDURE SetCaps;
BEGIN
  Mem[$40:$17] := Mem[$40:$17] OR 64;
  asm
     mov ah,1
     int 16h
  end;
END;

PROCEDURE KillCaps;
BEGIN
  Mem[$40:$17] := Mem[$40:$17] AND (NOT 64);
  asm
     mov ah,1
     int 16h
  end;
END;

PROCEDURE Txtcls;
BEGIN
   asm
      mov ax,0b800h
      mov es,ax
      xor di,di
      mov ax,0700h
      mov cx,4000
      rep stosw
      xor bx,bx
      mov dh,1
      xor dl,dl
      mov ah,02h
      int 10h
   end;
END;

PROCEDURE Cls;
BEGIN
   asm
      mov ax,0003h
      int 10h
      mov ax,1112h
      xor bl,bl
      int 10h
   end;
   Writeln;
END;

PROCEDURE Graphmode;
BEGIN
   asm
      mov ax,0013h
      int 10h
   end;
END;

PROCEDURE Textmode;
BEGIN
   asm
      mov ax,0003h
      int 10h
   end;
END;

FUNCTION d(s2 : String) : Longint;
VAR
   t : Longint;
   st,c,i : Word;
   s : String;
BEGIN
   t := 0;
   s := supertrim(s2);
   FOR i := 1 TO Ord(s[0]) DO BEGIN
      Val(s[i],st,c);
      c := (i*-1)+Ord(s[0]);
      CASE s[i] OF
         '1'..'9' : t := t+st*opph(16,c);
			'a'..'f' : t := t+(Ord(s[i])-87)*opph(16,c);
      END;
   END;
   d := t;
END;

FUNCTION h(w : Word) : String;
VAR
	s : String[4];
   t : String[4];
   b : Byte;
BEGIN
   t := '';
   b := (hi(w) shr 4);
   CASE b OF
        0..9 : BEGIN Str(b,s); t := t + s; END;
      10..15 : BEGIN s := Chr(55+b);  t := t + s; END;
   END;
   b := (hi(w) mod 16);
   CASE b OF
        0..9 : BEGIN Str(b,s); t := t + s; END;
      10..15 : BEGIN s := Chr(55+b);  t := t + s; END;
   END;
   b := (lo(w) shr 4);
   CASE b OF
        0..9 : BEGIN Str(b,s); t := t + s; END;
      10..15 : BEGIN s := Chr(55+b);  t := t + s; END;
   END;
   b := (lo(w) mod 16);
   CASE b OF
        0..9 : BEGIN Str(b,s); t := t + s; END;
      10..15 : BEGIN s := Chr(55+b);  t := t + s; END;
   END;
   IF (w<256) AND (Pos('00',t)=1) THEN t := Copy(t,3,Ord(t[0]));
   h := t;
END;

END.
