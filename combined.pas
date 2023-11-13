PROGRAM Burn;
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
   initcx = 169;
   initcy = 29;
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
VAR
  x, y : Byte;
   c : Word;
   xi, yi : Byte;
BEGIN
   Bilde2MoreMem;
   FOR x := 0 TO 31 DO BEGIN
      FOR y := 0 TO 31 DO BEGIN
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
      acticolx := 225;
      IF acticoly < 2 THEN acticoly := 57;
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
      FillChar(TheImage[6010],30,c);
      pre := (y shl 8)+(y shl 6)+321+x;
      n := TheImage[pre];
      r := ThePal[TheImage[pre]*3];
      g := ThePal[TheImage[pre]*3+1];
      b := ThePal[TheImage[pre]*3+2];
      IF n in[0,1,2] THEN Letter(285,10,'!',c);
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
      oldy := y^;
      Repeat
         IF opp = 1 THEN Dec(y^) ELSE Inc(y^);
      UNTIL (y^ > 31) OR (y^ < 0) OR (G(x^,y^) <> c);
      IF opp = 1 THEN Inc(y^) ELSE Dec(y^);
      IF NOT (oldy = y^) THEN fremgang := true;
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
               Writeln(t,'Author, real-name         : Alexander R�dseth');
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
                  14: Write(t,'�');
                  13: Write(t,'z');
                  12: Write(t,'n');
                11: Write(t,'a');
                10: Write(t,'�');
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
   FillChar(skjermen[(199-h) shl 8+(199-h) shl 6+h],319-(h*2),system1);
   FOR y := h+1 TO 199-h DO skjermen[y shl 8+y shl 6+319-h] := system1;
   FillChar(skjermen[h shl 8+h shl 6+h],320-(h*2),system3);
   FOR y := h+1 TO 199-h DO skjermen[y shl 8+y shl 6+h] := system3;
   SkrivXY(s,h+((319-h*2)-(ord(s[0])*7)) div 2+2,h+((199-h*2)-7) div 2,$0100,false);
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
          '�':BEGIN Plot('****-',x,y+1,c);
                     Plot('-*-*',x+1,y+2,c);
                    Plot('-***-',x,y+3,c);
                    Plot('*-*-',x,y+4,c);
                    Plot('-****',x,y+5,c);
              END;
          '�':BEGIN Plot('-*****',x,y,c);
                    Plot('**-**-',x,y+1,c);
                    Plot('******',x,y+2,c);
                    Plot('**-**-',x,y+3,c);
                    Plot('** **-',x,y+4,c);
                    Plot('** ***',x,y+5,c);
              END;
          '�':BEGIN Plot('-***-*',x,y+1,c);
                    Plot('*--**-',x,y+2,c);
                    Plot('*-**-*',x,y+3,c);
                    Plot('-**--*',x,y+4,c);
                    Plot('*-***-',x,y+5,c);
              END;
          '�':BEGIN Plot('-***-*',x,y,c);
                    Plot('*--**-',x,y+1,c);
                    Plot('*-**-*',x,y+2,c);
                    Plot('*-**-*',x,y+3,c);
                    Plot('-**--*',x,y+4,c);
                    Plot('*-***-',x,y+5,c);
              END;
          '�':BEGIN Plot('-**-',x+1,y,c);
                    Plot('***-',x+1,y+1,c);
                    Plot('--**',x+1,y+2,c);
                   Plot('-****',x,y+3,c);
                   Plot('**-**-',x,y+4,c);
                   Plot('-**-**',x,y+5,c);
              END;
          '�':BEGIN Plot('**',x+2,y,c);
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
         first_done := false;
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
         first_done := false;
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
       BlockWrite(f,byt,1);
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
      first_done := false;
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
      first_done := false;
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
   i := 0; { Tror det g�r kjappere med i }
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
   i := 0; { Tror det g�r kjappere med i }
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
   FinnesDenneFargen := i; { Hvis i er -1 s� finnes den ikke }
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
   FinnLedigFarge := farge; { Hvis farge blir <3 s� har man ikke funnet noe. }
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
   pre_calc := x*5+(y shl 10)+(y shl 8)+21140;
   FillChar(TheImage[pre_calc],4,c);
   Inc(pre_calc,320);
   FillChar(TheImage[pre_calc],4,c);
   Inc(pre_calc,320);
   FillChar(TheImage[pre_calc],4,c);
END;
FUNCTION G(x : Byte;y : Byte) : Byte;
BEGIN
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

PROCEDURE Dir(cmds : String);
VAR
   di : SearchRec;
   dt : DateTime;
   name,size,attr : String[12];
   time,temp : String[16];
   filecounter,dircounter : Word;
   wildcard : String[12];
   s : String;
   b : Byte;
BEGIN
   wildcard := GetParam(Addr(cmds));
   IF wildcard = '' THEN wildcard := '*.*';
   IF NOT (pos('.',wildcard) > 0) THEN wildcard := wildcard + '*.*';
   IF (NOT (pos('*',wildcard) > 0)) AND (NOT (pos('.',wildcard) > 0)) THEN wildcard := wildcard + '*.*';
   filecounter := 0;
   dircounter := 0;
   Writeln;
   g_linje_teller := 0;
   GetDir(0,s);
   IF NOT (Right(s,1) = '\') THEN s := s + '\' + wildcard ELSE s := s + wildcard;
   FOR b := 1 TO Ord(wildcard[0]) DO IF wildcard[b] = '\' THEN s := wildcard;
   Writeln('Currently viewing: ',s);
   Writeln;
   FindFirst(wildcard,AnyFile,di);
   WHILE (DosError = 0) AND (cbreak = false) DO BEGIN
      name := di.name;
      IF (di.attr and $01) > 0 THEN attr := 'r' ELSE attr := '-';
      IF (di.attr and $02) > 0 THEN attr := attr + 'h' ELSE attr := attr + '-';
      IF (di.attr and $04) > 0 THEN attr := attr + 's' ELSE attr := attr + '-';
      IF (di.attr and $08) > 0 THEN attr := attr + 'v' ELSE attr := attr + '-';
      IF (di.attr and $10) > 0 THEN attr := attr + 'd' ELSE attr := attr + '-';
      IF (di.attr and $20) > 0 THEN attr := attr + 'a' ELSE attr := attr + '-';
      if di.attr = $10 then inc(dircounter) else inc(filecounter);
      Str(di.size,size);
      UnpackTime(di.time,dt);
      Str(dt.day,temp);
      IF temp[0] = Chr(1) then temp := '0' + temp;
      time := temp;
      Str(dt.month,temp);
      IF temp[0] = Chr(1) then temp := '0' + temp;
      time := time + '/' + temp;
      Str(dt.year,temp);
      time := time + ' ' + temp;
      Str(dt.hour,temp);
      IF temp[0] = Chr(1) then temp := '0' + temp;
      time := time + ' ' + temp;
      Str(dt.min,temp);
      IF temp[0] = Chr(1) then temp := '0' + temp;
      time := time + ':' + temp;
      Writeln(name:12,' ',size:9,' ',time:19,'   ',attr);
      IF name = 'BURN.EXE' THEN LinjeFarge(GetCursorY-1,79,4);
      name := Right(name,4);
      IF ((name = '.BIP') OR (name = '.BPL')) OR
         ((name = '.BIM') OR (name = '.RAW')) THEN
      LinjeFarge(GetCursorY-1,79,9) ELSE
          IF ((name = '.BMP') OR (name = '.TGA')) OR
             ((name = '.PCX') OR (name = '.ICO')) THEN
                  LinjeFarge(GetCursorY-1,79,1)
                     ELSE
                        IF name = '.BSC' THEN
                           LinjeFarge(GetCursorY-1,79,12);
      IF attr[2] = 'h' THEN LinjeFarge(GetCursorY-1,79,8);
      IF attr[5] = 'd' THEN LinjeFarge(GetCursorY-1,79,10);
      IF (attr[5] = 'd') AND (attr[2] = 'h') THEN LinjeFarge(GetCursorY-1,79,2);
      FindNext(di);
      NextPage;
   END;
   IF cbreak THEN cbreak := false ELSE BEGIN
      Writeln;
      IF (dircounter = 0) and (filecounter > 1) then Writeln('         ',filecounter,' files and no dirs.')
      ELSE IF (dircounter = 0) and (filecounter = 1) then Writeln('         One file and no dirs.')
      ELSE IF (dircounter > 1) and (filecounter > 1) then Writeln('         ',filecounter,' files and ',dircounter,' dirs.')
      ELSE IF (dircounter = 1) and (filecounter > 1) then Writeln('         ',filecounter,' files and one dir.')
      ELSE IF (dircounter > 1) and (filecounter = 1) then Writeln('         One file and ',dircounter,' dirs.')
      ELSE IF (dircounter = 1) and (filecounter = 1) then Writeln('         One file and one dir.')
      ELSE IF (dircounter = 1) and (filecounter = 0) then Writeln('         No files and one dir.')
      ELSE IF (dircounter = 0) and (filecounter = 0) then Writeln('         No dirs or files.')
      ELSE IF (dircounter > 1) and (filecounter = 0) then Writeln('         ',dircounter,' dirs and no files.');
   END;
   Writeln;
END;
PROCEDURE FadeIn(vent : Word);
VAR
  i : Word;
   workleft : Boolean;
   t,m1,s1,ms1 : Word;
   h2,m2,s2,ms2 : Word;
   venta : Word;
BEGIN
   IF nofade THEN
      SetPalette
   ELSE BEGIN
      venta := 10;
      GetTime(t,m1,s1,ms1);
      t := m1*60+s1+4;
     Repeat
       workleft := false;
       FOR i := 0 TO 767 DO BEGIN
          IF ThePal[i] < MoreMem[i] THEN BEGIN
           Inc(ThePal[i]);
          workleft := true;
          END;
       END;
       SetPalette;
        GetTime(h2,m2,s2,ms2);
         IF m2*60+s2 < t THEN Inc(venta) ELSE Dec(venta);
         IF NOT KeyEntered THEN WaitPeriod(venta);
    UNTIL NOT workleft;
   END;
END;
PROCEDURE FadeOut(vent : Word);
VAR
  i : Word;
   workleft : Boolean;
   t,m1,s1,ms1 : Word;
   h2,m2,s2,ms2 : Word;
   venta : Word;
BEGIN
   IF nofade THEN
      SetPalette
   ELSE BEGIN
      venta := 10;
     FOR i := 0 TO 767 DO MoreMem[i] := ThePal[i];
      venta := 10;
      GetTime(t,m1,s1,ms1);
      t := m1*60+s1+4;
     Repeat
       workleft := false;
       FOR i := 0 TO 767 DO BEGIN
          IF ThePal[i] > 0 THEN BEGIN
           Dec(ThePal[i]);
          workleft := true;
          END;
       END;
       SetPalette;
        GetTime(h2,m2,s2,ms2);
         IF m2*60+s2 < t THEN Inc(venta) ELSE Dec(venta);
         IF NOT KeyEntered THEN WaitPeriod(venta);
    UNTIL NOT workleft;
     FOR i := 0 TO 767 DO ThePal[i] := MoreMem[i];
   END;
END;
FUNCTION Command(cmd : String) : Boolean; Forward;
PROCEDURE xyfor(cmds : String); Forward;
PROCEDURE Load(cmds : String);
VAR
  filename : String[12];
  f : File;
  t,t2 : Text;
  s,s2 : String;
  x,y,c : Byte;
  svar : String;
  whocares : Boolean;
  callarray : Array [0..255] of Word;
  retarray : Array [0..255] of Word;
  startpunkt : Word;
  w,cw : Word;
  oldca : Word;
  oldlasterror : Byte;
  p1,p2,p3,p4 : String [20];
  prevail : Boolean;
  oldinaforloop, inaforloop, inaxyloop : Byte;
  v1, v2, v3 : Integer;
  fs : Longint;
  fss : String;
  lntemp : Integer;
  cury, curx : Byte;
  counter : Word;
  oldn : Byte;
LABEL ut, neddit, oppdit, nedditxy;
BEGIN
   Writeln;
   filename := GetParam(Addr(cmds));
   s := supertrim(filename);
   IF (filename = '') or (GetParam(Addr(s)) <> '') THEN
      Writeln('Syntax: load [filename]')
   ELSE BEGIN
      IF ( fsize(Addr(filename)) = 1024 ) THEN BEGIN
         Writeln('-> I assume the file is a raw image.');
         Writeln;
         IF finnes(Addr(filename)) THEN BEGIN
            IF bilde THEN BEGIN
               Write('Are you sure you want to replace your current image with ',filename,'? ');
               Readln(svar);
            END ELSE svar := '�';
            IF (UpCase(svar[1]) = 'Y') OR (svar = '�') THEN BEGIN
               Assign(f,filename);
               Reset(f,1);
               IF NOT bilde THEN BEGIN
                  Graphmode;
                  DefaultPalette;
                  SetPalette;
                  Tegnpalett;
                  Cursor(acticolx,acticoly,true);
                  TegnRuter;
                  TegnThumb;
               END;
               IF (FileSize(f) < 1024) THEN BEGIN
                  Close(f);
                  IF NOT bilde THEN CLS;
                  Writeln(filename,' can''t be a valid 32x32 8bpp image (to small).');
               END ELSE BEGIN
                  FOR y := 0 TO 31 DO BEGIN
                     FOR x := 0 TO 31 DO BEGIN
                        BlockRead(f,c,1);
                        P(x,y,c);
                     END;
                  END;
                  Close(f);
                  IF NOT bilde THEN CLS;
                  bilde := true;
                  Writeln;
                  Writeln('Done. ',filename,' has now been loaded.');
               END;
            END ELSE BEGIN
               Writeln;
               Writeln('No changes done.');
            END;
         END ELSE BEGIN
            Writeln('Couldn''t find ',filename,'.');
         END;
      END ELSE IF ( pos('.act',filename)>0 ) THEN BEGIN
         Writeln('ACT funkystuff...');
         Assign(f,filename);
         Reset(f,1);
         FOR x := 0 TO 255 DO BEGIN
            BlockRead(f,w,1);
            thepal[x*3] := w shr 2;
            BlockRead(f,w,1);
            thepal[x*3+1] := w shr 2;
            BlockRead(f,w,1);
            thepal[x*3+2] := w shr 2;
         END;
         Close(f);
         IF NOT palette THEN BEGIN
         	Tegnpalett;
            Cursor(acticolx,acticoly,true);
            TegnRuter;
            TegnThumb;
            DrawImage;
            END;
         palette := true;
         Writeln('ACT magic done. Hopfully it didn''t screw up any thing.');
      END ELSE IF ( fsize(Addr(filename)) = 768 ) THEN BEGIN
         Writeln('-> I assume the file is a raw palette.');
         Writeln;
         IF finnes(Addr(filename)) THEN BEGIN
            IF palette THEN BEGIN
               Write('Are you sure you want to replace your current palette with ',filename,'? ');
               Readln(svar);
               Writeln;
            END ELSE svar := '�';
            IF (UpCase(svar[1]) = 'Y') OR (svar = '�') THEN BEGIN
               Assign(f,filename);
               Reset(f,1);
               IF (FileSize(f) = 768) THEN BEGIN
                  BlockRead(f,thepal,768);
                  Close(f);
                  IF NOT palette THEN BEGIN
                     Tegnpalett;
                     Cursor(acticolx,acticoly,true);
                     TegnRuter;
                     TegnThumb;
                     DrawImage;
                  END;
                  palette := true;
                  Writeln('Done. ',filename,' has now been loaded.');
               END ELSE BEGIN
                  IF (FileSize(f) < 768) THEN
                     Writeln('This can''t be a valid palette since ',filename,' is smaller than 768 bytes.')
                  ELSE
                     Writeln('This can''t be a valid palette since ',filename,' is bigger than 768 bytes.');
                  Close(f);
               END;
            END ELSE BEGIN
               Writeln;
               Writeln('No changes done.');
            END;
         END ELSE BEGIN
            Writeln('Couldn''t find ',filename,'.');
         END;
      END ELSE IF (pos('.bip',s)>0) THEN BEGIN
         Writeln('-> I assume the file is a native Burn-image.');
         Writeln;
         IF finnes(Addr(filename)) THEN BEGIN
            Assign(f,filename);
            Reset(f,1);
            BlockRead(f,c,1);
            IF Chr(c) = 'B' THEN BEGIN
             cw := build;
             BlockRead(f,w,2);
             build := w;
             Writeln('The image was made with Burn',GetVer,GetVerS,'.');
             build := cw;
             BlockRead(f,w,2); { animdelay reserved }
             BlockRead(f,oldn,1);
             {
              1, image
              2, palette
              3, image + palette
             }
             IF (oldn <> 2) THEN BEGIN
              Writeln;
              IF bilde THEN BEGIN
                 Write('Are you sure you want to replace your current image with ',filename,'? ');
                 Readln(svar);
              END ELSE svar := '�';
              IF (UpCase(svar[1]) = 'Y') OR (svar = '�') THEN BEGIN
                 IF NOT bilde THEN BEGIN
                    Graphmode;
                    DefaultPalette;
                    SetPalette;
                    Tegnpalett;
                    Cursor(acticolx,acticoly,true);
                    TegnRuter;
                    TegnThumb;
                 END;
                 IF (FileSize(f) < 1024) THEN BEGIN
                    IF NOT bilde THEN CLS;
                    Writeln(filename,' can''t be a valid 32x32 8bpp image (to small).');
                 END ELSE BEGIN
                    FOR y := 0 TO 31 DO BEGIN
                       FOR x := 0 TO 31 DO BEGIN
                          BlockRead(f,c,1);
                          P(x,y,c);
                       END;
                    END;
                    IF NOT bilde THEN CLS;
                    bilde := true;
                    Writeln;
                    Writeln('Done. ',filename,'''s image has now been loaded.');
                 END;
              END ELSE BEGIN
                 Close(f);
                 Writeln;
                 Writeln('No changes done.');
              END;
             END;
             IF (oldn > 1) THEN BEGIN
              Writeln;
              IF palette THEN BEGIN
                 Write('Are you sure you want to replace your current palette with ',filename,'? ');
                 Readln(svar);
                 Writeln;
              END ELSE svar := '�';
              IF (UpCase(svar[1]) = 'Y') OR (svar = '�') THEN BEGIN
                 IF (FileSize(f) >= 768) THEN BEGIN
                    BlockRead(f,thepal,768);
                    IF NOT palette THEN BEGIN
                       Tegnpalett;
                       Cursor(acticolx,acticoly,true);
                       TegnRuter;
                       TegnThumb;
                       DrawImage;
                    END;
                    palette := true;
                    Writeln('Done. ',filename,'''s palette has now been loaded.');
                 END ELSE BEGIN
                    Writeln('This can''t be a valid palette since ',filename,' is smaller than 768 bytes.');
                 END;
              END ELSE BEGIN
                 Writeln;
                 Writeln('No changes done.');
              END;
             END;
             IF oldn = 1 THEN BEGIN
                Writeln;
            Writeln('Type defpal if you want to reset the palette.');
             END;
           END ELSE BEGIN
              Writeln('This wasn''t a native Burn-image after all.');
           END;
           Close(f);
         END ELSE BEGIN
            Writeln('Couldn''t find ',filename,'.');
         END;
       END ELSE IF (pos('.bsc',s)>0) OR (pos('.scr',s)>0) THEN BEGIN
         Writeln('-> I assume the file is a script.');
         Writeln;
         IF finnes(Addr(filename)) THEN BEGIN
            Assign(f,filename);
            Reset(f,1);
            fs := FileSize(f);
            Str(fs,fss);
            {1025500 er ca. st�rrelsen for 1000*1024+768+500 }
            IF filesize(f) > 1025500 THEN BEGIN
             Writeln('Script-file to large!');
                 Close(f);
  	               Exit;
  		      END;
               oldn := n;
              callarray[0] := 0; {antall jmp-punkter}
              retarray[0] := 0; {antall ret-punkter}
               inaforloop := 0;
               inaxyloop := 0;
               startpunkt := 0;
              { F�rst m� man plukke opp alle jmp-pointene }
               Write('Analyzing script file, ');
               asm
                  xor bx,bx
                  mov ah,03h
                  int 10h
                  mov cury,dh
                  mov curx,dl
               end;
               Repeat
                  Write(FilePos(f),' of ',fss,' bytes...');
                  asm
                     xor bx,bx
                     mov ah,02h
                     mov dh,cury
                     mov dl,curx
                     int 10h
                  end;
                 s := '';
                  counter := 0;
                Repeat
                     Repeat
                        IF counter = 1000 THEN BEGIN
                           Write(FilePos(f),' of ',fss,' bytes...');
                           asm
                              xor bx,bx
                              mov ah,02h
                              mov dh,cury
                              mov dl,curx
                              int 10h
                           end;
                           counter := 0;
                        END;
                        Inc(counter);
   		               BlockRead(f,c,1);
                      UNTIL (c in[10,13,39,97,98,101,103,105,109,110,123]) OR EOF(f); {grovsort}
                     IF (c = 10) OR (c = 13) OR (c = 39) THEN Break;
                     { tror ikke jeg trenger c > 31 }
                    s := s + Chr(c);
                     { for � f� beregningen ved 'begin' til � bli riktig... }
                     IF EOF(f) THEN s := s + ' ';
                UNTIL EOF(f);
                  s := supertrim(s);
                  IF (pos('begin',s)=1) OR (pos('{',s)=1) THEN BEGIN
   	               cw := FilePos(f)-Ord(s[0])-1;
                    whocares := true;
                    FOR w := 1 TO callarray[0] DO BEGIN
      					   IF callarray[w] = cw THEN BEGIN
        					   whocares := false; { jmp-point finnes fra f�r }
                          break;
                       END;
                    END;
                    IF whocares THEN BEGIN
                        Inc(callarray[0]);
                        callarray[callarray[0]] := cw;
                    END;
                 END;
                  IF s[1]+s[2]+s[3]+s[4] = 'main' THEN BEGIN
                     startpunkt := FilePos(f)-Ord(s[0])-1;
                  END;
               UNTIL EOF(f);
               IF startpunkt = 0 THEN BEGIN
                  Writeln(FilePos(f),' of ',fss,' bytes...');
                  Writeln;
                  Writeln('Error: A script must begin with "main" on a single line.');
                  Writeln;
                  n := oldn;
                  Exit;
               END ELSE Writeln(fss,' of ',fss,' bytes...');
               seek(f,startpunkt);
  	            { S� kan man hoppe rundt }
               oldca := callarray[0];
               Repeat
                  oppdit:
                  s := '';
                  Repeat
                     {$I-}
                     BlockRead(f,c,1);
                     {$I+}
                     IF IOResult <> 0 THEN BEGIN {!}
                       Writeln;
                        Writeln('------------------------------------------------------------');
                       Writeln;
                        Writeln('Script error  : Something is wrong with the script-structure.');
                        Writeln('Byte position : ',FilePos(f));
                        IF (inaforloop > 0) OR (inaxyloop > 0) THEN
                           Writeln('In a for-loop : yes')
                        ELSE
                           Writeln('In a for-loop : no');
                        Writeln;
                        Writeln('It''s most likely that a start or end of a block is missing.');
                        Writeln;
                        Writeln('------------------------------------------------------------');
                       Writeln;
                        n := oldn;
                        Exit;
                     END;
                     IF c > 31 THEN s := s + Chr(c);
                     IF (c = 10) OR (c = 13) OR (c = 39) THEN Break;
                  UNTIL EOF(f);
                  s2 := s;
                  s := supertrim(s2);
                  IF (pos('begin',s)=1) OR (pos('{',s)=1) THEN s := '''';
                  IF (pos('safe',s)=1) THEN s := '''';
                  IF (pos('echo',s)=1) THEN s := s2;
                  IF (pos('call',s)=1) THEN BEGIN
                     { pusher retadresse, hopper til jmp-point }
                Inc(retarray[0]);
                cw := FilePos(f);
                retarray[retarray[0]] := cw;
                Val(GetParam(addr(s)),w,cw);
                     {$I-}
               Seek(f,callarray[w]);
                     {$I+}
                     IF IOResult <> 0 THEN BEGIN
                       Writeln;
                        Writeln('------------------------------------------------------------');
                       Writeln;
                        Writeln('Error trying to "',s,'"');
                        Writeln('Couldn''t seek to bytepos ',callarray[w]);
                        Writeln('Stated number of "{"''s or "begin"''s : ',callarray[0]);
                        Writeln('True number of "{"''s or "begin"''s : ',oldca);
                        Writeln('Stated number of "}"''s or "end"''s : ',retarray[0]);
                        Writeln('I am in a for-loop : ',(inaforloop>0));
                        Writeln;
                        Writeln('Overflow : ',(callarray[0]>oldca));
                       Writeln;
                        Writeln('------------------------------------------------------------');
                       Writeln;
                        n := oldn;
                        Exit;
                     END;
                     s := '''';
                  END;
                  IF (pos('end',s)=1) OR (pos('}',s)=1) THEN BEGIN
                     { "popper siste retadresse" }
                     Seek(f,retarray[retarray[0]]);
                     Dec(retarray[0]);
                     {Ikke heeelt sikker p� at dette er "rette m�ten"}
                     IF (inaforloop = inaxyloop) AND (inaforloop > 0) THEN GOTO nedditxy;
                     IF inaforloop > 0 THEN GOTO neddit;
                     s := '''';
                  END;
                  IF (pos('exit',s)=1) OR (pos('break',s)=1) THEN BEGIN n := oldn; Break; END;
                  IF (pos('end',s)=1) OR (pos('quit',s)=1) THEN BEGIN
                     Writeln;
                     Writeln('The script reached a "quit" or "end" command.');
                     Writeln;
                     n := oldn;
                     Exit;
                  END;
                  IF (pos('kquit',s)=1) THEN
               IF KeyEntered THEN BEGIN
                        Writeln;
                        Writeln('A keypress from the user stopped the script.');
                        Writeln;
                        asm
                           xor ah,ah
                           int 16h
                        end;
                        n := oldn;
                        exit;
                     END ELSE s := '''';
                  IF ord(s[1]) = 39 THEN
                     whocares := true
                  ELSE BEGIN
                     oldlasterror := lasterror;
                     IF pos('for ',s) = 1 THEN BEGIN
                        IF retarray[0] = 1 THEN BEGIN
                         Writeln;
                           Writeln('------------------------------------------------------------');
                         Writeln;
                           Writeln('Script error');
                           Writeln;
                           Writeln('Due to a silly bug in Burn''s script-engine, you can''t');
                           Writeln('use for-loops in procedures. It messes up the whole');
                           Writeln('for-loop variable set - for some reason. The for-loop');
                  Writeln('implementation is generally not very extensive.');
                           Writeln;
                           Writeln('Simplified:');
                  Writeln('Don''t use for-loops in procedures. It''s bad.');
                           Writeln;
                           Writeln('Byte position : ',FilePos(f));
                         Writeln;
                         Writeln('Press space to continue, or any other key stop the script.');
                           Writeln;
                           Writeln('------------------------------------------------------------');
                         Writeln;
                         asm
                            xor ah,ah
                            int 16h
                            mov y,al { variabel-gjenbruk}
                         end;
                         IF y <> 32 THEN break;
                END;
    							p1 := GetParam(Addr(s));
                 p2 := GetParam(Addr(p1));
                 p3 := GetParam(Addr(p2));
                 p4 := GetParam(Addr(p3));
                 p1[0] := Chr(Length(p1) - Length(p2) - 1);
                 p2[0] := Chr(Length(p2) - Length(p3) - 1);
                 p3[0] := Chr(Length(p3) - Length(p4) - 1);
                 IF ((p1='') or (p2='')) or ((p3='') or (p4='')) THEN BEGIN
                         Writeln;
                           Writeln('------------------------------------------------------------');
                         Writeln;
                          Writeln('Script error  : "',s,'"');
                           Writeln('Byte position : ',FilePos(f));
                           Writeln;
                         Writeln;
                         Writeln('Press space to continue, or any other key stop the script.');
                           Writeln;
                           Writeln('------------------------------------------------------------');
                         Writeln;
                         asm
                            xor ah,ah
                            int 16h
                            mov y,al { variabel-gjenbruk}
                         end;
                         IF y <> 32 THEN break;
   						   END ELSE BEGIN
         							Val(p1,v1,cw);
         							Val(p2,v2,cw);
         							Val(p3,v3,cw);
      							FOR n := v1 TO v2 DO BEGIN
                       Inc(n,(v3-1));
                      IF ((v3-1)>0) and (n>v2) THEN BEGIN
                    	Writeln;
                    	Exit;
                    END;
         							IF ((v3-1)<0) and (n<v2) THEN BEGIN
                    	Writeln;
                    	Exit;
                    END;
                              IF pos('call ',p4) > 0 THEN BEGIN
                        Inc(retarray[0]);
                        cw := FilePos(f);
                        retarray[retarray[0]] := cw;
                        Val(GetParam(addr(p4)),w,cw);
                       Seek(f,callarray[w]);
                                 Inc(inaforloop); { Enter new forloop }
                                 GOTO oppdit;
                                 neddit:
                                 Dec(inaforloop);
                              END ELSE	BEGIN
                                 oldlasterror := lasterror;
   										prevail := command(p4);
                              END;
         	 	  	   	END;
               	END;
                     END ELSE BEGIN
     	                  IF pos('fxy ',s) = 1 THEN BEGIN
   							   p4 := GetParam(Addr(s));
                   IF (p4='') THEN BEGIN
                           Writeln;
                             Writeln('------------------------------------------------------------');
                           Writeln;
                            Writeln('Script error  : "',s,'"');
                             Writeln('Byte position : ',FilePos(f));
                             Writeln;
                           Writeln;
                           Writeln('Press space to continue, or any other key stop the script.');
                             Writeln;
                             Writeln('------------------------------------------------------------');
                           Writeln;
                           asm
                              xor ah,ah
                              int 16h
                              mov y,al { variabel-gjenbruk}
                           end;
                           IF y <> 32 THEN break;
     						   END ELSE BEGIN
        							FOR xpos := 0 TO 31 DO BEGIN
        							FOR ypos := 0 TO 31 DO BEGIN
                        IF (xpos>31) or (ypos>31) THEN BEGIN
                    		Writeln;
                    		Exit;
                    	END;
                                IF pos('call ',p4) > 0 THEN BEGIN
                          Inc(retarray[0]);
                          cw := FilePos(f);
                          retarray[retarray[0]] := cw;
                          Val(GetParam(addr(p4)),w,cw);
                    	   Seek(f,callarray[w]);
                                    inaxyloop := inaforloop;
                                   GOTO oppdit;
                                   nedditxy:            { if inaxyloop = inaforloop, hopp ned hit?? }
                                    inaxyloop := 0;
                                END ELSE	BEGIN
                                   oldlasterror := lasterror;
     										prevail := command(p4);
                                END;
                              END;
           	 	  	   	END;
                 	END;
                        END ELSE BEGIN
   							   whocares := Command(s);
                        END;
                     END;
                    IF (oldlasterror <> lasterror) THEN BEGIN
                       Writeln;
                       Writeln('------------------------------------------------------------');
                       Writeln;
                       Writeln('Script error  : "',s,'"');
                        Writeln('Byte position : ',FilePos(f));
                       Writeln;
                       Writeln;
                       Writeln('Press space to continue, or any other key stop the script.');
                        Writeln;
  	                     Writeln('------------------------------------------------------------');
                       Writeln;
                       asm
                          xor ah,ah
                          int 16h
                          mov oldlasterror,al { variabel-gjenbruk}
                       end;
                       IF oldlasterror <> 32 THEN break;
                     END;
            END;
                  IF NOT whocares THEN Writeln('You cannot quit burn through a script!');
               UNTIL EOF(f);
               Close(f);
               n := oldn;
         END ELSE BEGIN
            Writeln('Couldn''t find ',filename,'.');
         END;
      END ELSE IF (pos('.ico',s)>0) THEN BEGIN
         Writeln('Burn can''t load .ico files yet... maybe in a later version. :)');
      END ELSE IF (pos('.gif',s)>0) THEN BEGIN
         Writeln('Burn can''t load .gif files yet... maybe in a later version. :)');
      END ELSE IF (pos('.pcx',s)>0) THEN BEGIN
         Writeln('Burn can''t load .pcx files yet... maybe in a later version. :)');
      END ELSE IF (pos('.bmp',s)>0) THEN BEGIN
         Writeln('Burn can''t load .bmp files yet... maybe in a later version. :)');
      END ELSE Writeln('Syntax: load [filename]')
   END;
   Writeln;
END;
PROCEDURE cd(cmds : String);
VAR
   i : Byte;
   s : String;
BEGIN
   IF supertrim(cmds) = 'cd\' THEN s := '\' ELSE BEGIN
      FOR i := 3 TO Length(cmds) DO s[i-2] := cmds[i];
      s[0] := Chr((Length(cmds)-2));
      IF s[1] = ' ' THEN BEGIN
         FOR i := 1 TO (Length(s)-1) DO s[i] := s[i+1];
         Dec(s[0]);
      END;
   END;
   IF s = '' THEN BEGIN
      Writeln;
      GetDir(0,s);
      Writeln(s);
      Writeln;
   END ELSE BEGIN
      {$I-}
      Chdir(s);
      {$I-}
      IF IOresult <> 0 THEN
       RandomError(cmds)
      ELSE BEGIN
         Writeln;
         GetDir(0,s);
         Writeln(s);
         Writeln;
      END;
   END;
END;
PROCEDURE cdrv(cmds : String);
VAR s : String;
BEGIN
      Writeln;
      {$I-}
      Chdir(cmds[1]+cmds[2]);
      {$I-}
      IF IOresult <> 0 THEN
       RandomError(cmds)
      ELSE BEGIN
         GetDir(0,s);
         Writeln(s);
      END;
      Writeln;
END;
PROCEDURE mfor(cmds : String); Forward;
PROCEDURE Softenall;
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
        c := c + MoreMem[(yi-1) shl 5+x];
         xi := x+1;
         IF x = 31 THEN xi := 0;
        c := c + MoreMem[y shl 5+xi];
         yi := y+1;
         IF y = 31 THEN yi := 0;
        c := c + MoreMem[yi shl 5+x];
         xi := x;
         yi := y;
         IF x = 0 THEN xi := 31 ELSE Dec(xi);
         IF y = 31 THEN yi := 0 ELSE Inc(yi);
        c := c + MoreMem[y shl 5+xi];
         c := ((c div 4)+MoreMem[y shl 5+x]) shr 1;
         P(x,y,c);
     END;
   END;
END;
PROCEDURE Soften(x, y : Byte);
VAR
   c : Word;
   xi, yi : Byte;
BEGIN
   Bilde2MoreMem;
   xi := x;
   yi := y;
   c := MoreMem[yi shl 5+xi];
   IF x = 0 THEN xi := 32;
   IF y = 0 THEN yi := 32;
   c := c + MoreMem[(yi-1) shl 5+x];
   xi := x+1;
   IF x = 31 THEN xi := 0;
   c := c + MoreMem[y shl 5+xi];
   yi := y+1;
   IF y = 31 THEN yi := 0;
   c := c + MoreMem[yi shl 5+x];
   xi := x;
   yi := y;
   IF x = 0 THEN xi := 31 ELSE Dec(xi);
   IF y = 31 THEN yi := 0 ELSE Inc(yi);
   c := c + MoreMem[y shl 5+xi];
   c := ((c div 4)+MoreMem[y shl 5+x]) shr 1;
   P(x,y,c);
END;
PROCEDURE kBlur(x,y : Byte);
VAR
   xp,yp : Integer;
   they,thex,v : Integer;
   counter : Byte;
   va : Array [0..8] of Byte; { 9 stk }
   ma : Array [0..8] of Byte;
BEGIN
   { va blir verdi-array }
   counter := 0;
   FOR xp := -1 TO 1 DO FOR yp := -1 TO 1 DO BEGIN
      thex := x+xp;
      they := y+yp;
      IF thex = -1 THEN thex := 31;
      IF they = -1 THEN they := 31;
      IF thex = 32 THEN thex := 0;
      IF they = 32 THEN they := 0;
      va[counter] := MoreMem[they shl 5+thex];
      inc(counter);
   END;
   { ma blir antall-med-den-verdien-array }
   FOR xp := 0 TO 8 DO ma[xp] := 0;
   FOR xp := 0 TO 8 DO BEGIN
      FOR yp := 0 TO 8 DO BEGIN
         IF va[xp] = va[yp] THEN Inc(ma[xp]);
      END;
   END;
   { v blir den array-posisjonen med st�rst antall }
   yp := ma[0];
   v := 0;
   FOR xp := 0 TO 8 DO BEGIN
      IF ma[xp] > ma[v] THEN v := xp;
   END;
   { verdien til det elementet med st�rst antall blir brukt }
   P(x,y,va[v]);
END;
PROCEDURE kBlurAll;
VAR x,y : Byte;
BEGIN
   FOR x := 0 TO 31 DO BEGIN
      FOR y := 0 TO 31 DO BEGIN
         kBlur(x,y);
      END;
   END;
END;
FUNCTION Blur(x,y : Byte) : Boolean;
VAR
   xp,yp : Integer;
   rc,gc,bc : Word;
   they,thex,v : Integer;
LABEL top;
BEGIN
   { Dette er litt messy kode. }
   rc := 0;
   gc := 0;
   bc := 0;
   FOR xp := -1 TO 1 DO FOR yp := -1 TO 1 DO BEGIN
      thex := x+xp;
      they := y+yp;
      IF thex = -1 THEN thex := 31;
      IF they = -1 THEN they := 31;
      IF thex = 32 THEN thex := 0;
      IF they = 32 THEN they := 0;
      v := MoreMem[they shl 5+thex] * 3;
      rc := rc+ThePal[v];
      gc := gc+ThePal[v+1];
      bc := bc+ThePal[v+2];
   END;
   { div 9, shr 3, round(/9) }
   rc := rc div 9;
   gc := gc div 9;
   bc := bc div 9;
   IF rc > 63 THEN BEGIN
     rc := Round(rc * (63/rc));
     gc := Round(gc * (63/rc));
     bc := Round(bc * (63/rc));
   END;
  IF gc > 63 THEN BEGIN
     rc := Round(rc * (63/gc));
     gc := Round(gc * (63/gc));
     bc := Round(bc * (63/gc));
   END;
  IF bc > 63 THEN BEGIN
     rc := Round(rc * (63/bc));
     gc := Round(gc * (63/bc));
     bc := Round(bc * (63/bc));
   END;
top:
   v := FinnesDenneFargen(rc,gc,bc,2); {1*}
   IF (v > -1) THEN {1+}
      P(x,y,v)
   ELSE BEGIN {1-}
      v := FinnLedigFarge; {2*}
      IF v > 2 THEN BEGIN {2+}
         ThePal[v*3+0] := rc;
         ThePal[v*3+1] := gc;
         ThePal[v*3+2] := bc;
         P(x,y,v);
      END ELSE BEGIN {2-}
         FjernDuplikatFarger;
         v := FinnLedigFarge; {3*}
         IF v > 2 THEN BEGIN {3+}
            ThePal[v*3+0] := rc;
            ThePal[v*3+1] := gc;
            ThePal[v*3+2] := bc;
            P(x,y,v);
         END ELSE BEGIN {3-}
   		   blur := false;
            Exit;
         END;
      END;
   END;
END;
FUNCTION mBlur(x,y : Byte) : Boolean;
VAR
   xp,yp : Integer;
   rc,gc,bc : Word;
   they,thex,v : Integer;
   funnet : Boolean;
   m : Byte;
LABEL top;
BEGIN
   { Dette er litt messy kode. }
   rc := 0;
   gc := 0;
   bc := 0;
   FOR xp := -1 TO 1 DO FOR yp := -1 TO 1 DO BEGIN
      thex := x+xp;
      they := y+yp;
      IF thex = -1 THEN thex := 31;
      IF they = -1 THEN they := 31;
      IF thex = 32 THEN thex := 0;
      IF they = 32 THEN they := 0;
      v := MoreMem[they shl 5+thex] * 3;
      rc := rc+ThePal[v];
      gc := gc+ThePal[v+1];
      bc := bc+ThePal[v+2];
   END;
   { div 9, shr 3, round(/9) }
   rc := rc div 9;
   gc := gc div 9;
   bc := bc div 9;
   { N� er gjennomsnittet i rc, gc og bc - finn medianen, finn n�rmeste ellers }
   funnet := false;
   FOR xp := -1 TO 1 DO FOR yp := -1 TO 1 DO BEGIN
      v := MoreMem[they shl 5+thex] * 3;
      IF (ThePal[v] = rc) AND (ThePal[v+1] = gc) AND (ThePal[v+2] = bc) THEN BEGIN
         rc := ThePal[v];
         gc := ThePal[v+1];
         bc := ThePal[v+2];
         funnet := true;
      END;
   END;
   IF NOT funnet THEN BEGIN
      funnet := false;
      m := 1;
      Repeat
         FOR xp := -1 TO 1 DO FOR yp := -1 TO 1 DO BEGIN
            v := MoreMem[they shl 5+thex] * 3;
            IF (ThePal[v] < rc+m) AND (ThePal[v] > rc-m) AND
           (ThePal[v+1] < gc+m) AND (ThePal[v+1] > gc-m) AND
          (ThePal[v+2] < bc+m) AND (ThePal[v+2] > bc-m) THEN BEGIN
               rc := ThePal[v];
               gc := ThePal[v+1];
               bc := ThePal[v+2];
               funnet := true;
            END;
         END;
         inc(m);
      UNTIL funnet OR (m = 60);
   END;
   IF rc > 63 THEN BEGIN
     rc := Round(rc * (63/rc));
     gc := Round(gc * (63/rc));
     bc := Round(bc * (63/rc));
   END;
  IF gc > 63 THEN BEGIN
     rc := Round(rc * (63/gc));
     gc := Round(gc * (63/gc));
     bc := Round(bc * (63/gc));
   END;
  IF bc > 63 THEN BEGIN
     rc := Round(rc * (63/bc));
     gc := Round(gc * (63/bc));
     bc := Round(bc * (63/bc));
   END;
top:
   v := FinnesDenneFargen(rc,gc,bc,2); {1*}
   IF (v > -1) THEN {1+}
      P(x,y,v)
   ELSE BEGIN {1-}
      v := FinnLedigFarge; {2*}
      IF v > 2 THEN BEGIN {2+}
         ThePal[v*3+0] := rc;
         ThePal[v*3+1] := gc;
         ThePal[v*3+2] := bc;
         P(x,y,v);
      END ELSE BEGIN {2-}
         FjernDuplikatFarger;
         v := FinnLedigFarge; {3*}
         IF v > 2 THEN BEGIN {3+}
            ThePal[v*3+0] := rc;
            ThePal[v*3+1] := gc;
            ThePal[v*3+2] := bc;
            P(x,y,v);
         END ELSE BEGIN {3-}
   		   mBlur := false;
            Exit;
         END;
      END;
   END;
END;
PROCEDURE Blurgfx;
VAR
   x,y : Byte;
  quality : Boolean;
   curx, cury : Byte;
   savepal : Array [0..767] of Byte;
BEGIN
   ClearKb;
   quality := true;
   Bilde2MoreMem;
   Move(thepal,savepal,768);
   SkrivXY(' Truecolor',235,15,$0100,false);
   SkrivXY(' blur in',235,22,$0100,false);
   SkrivXY(' progress',235,29,$0100,false);
   FOR y := 0 TO 31 DO BEGIN
      Mem[$a000:((66+y) shl 6+(66+y) shl 8)+228] := system1;
      Mem[$a000:((66+y) shl 6+(66+y) shl 8)+229] := system2;
     FOR x := 0 TO 31 DO BEGIN
        quality := Blur(x,y);
         TegnThumb;
         IF KeyEntered THEN BEGIN
            MoreMem2Bilde;
            Move(savepal,thepal,768);
          SetPalette;
           TegnThumb;
           DrawImage;
            Exit;
         END;
      END;
      IF quality = false THEN BEGIN
         MsgBox('Not enough free colors!',70,true);
         MoreMem2Bilde;
         Move(savepal,thepal,768);
         Exit;
     END;
   END;
   palette := true;
   SetPalette;
   TegnThumb;
   DrawImage;
   {!}
END;
PROCEDURE mBlurgfx;
VAR
   x,y : Byte;
  quality : Boolean;
   curx, cury : Byte;
   savepal : Array [0..767] of Byte;
BEGIN
   ClearKb;
   quality := true;
   Bilde2MoreMem;
   Move(thepal,savepal,768);
   SkrivXY('Truecolor',235,15,$0100,false);
   SkrivXY('median blur',235,22,$0100,false);
   SkrivXY('in progress',235,29,$0100,false);
   FOR y := 0 TO 31 DO BEGIN
      Mem[$a000:((66+y) shl 6+(66+y) shl 8)+228] := system1;
      Mem[$a000:((66+y) shl 6+(66+y) shl 8)+229] := system2;
     FOR x := 0 TO 31 DO BEGIN
        quality := mBlur(x,y);
         TegnThumb;
         IF KeyEntered THEN BEGIN
            MoreMem2Bilde;
            Move(savepal,thepal,768);
          SetPalette;
           TegnThumb;
           DrawImage;
            Exit;
         END;
      END;
      IF quality = false THEN BEGIN
         MsgBox('Not enough free colors!',70,true);
         MoreMem2Bilde;
         Move(savepal,thepal,768);
         Exit;
     END;
   END;
   palette := true;
   SetPalette;
   TegnThumb;
   DrawImage;
   {!}
END;
PROCEDURE Flip(saynomore : Boolean);
VAR i,y : Byte;
BEGIN
   IF saynomore THEN
     FOR y := 0 TO 31 DO FOR i := 0 TO 15 DO Pswap(0+i,31-i,y,y)
   ELSE BEGIN
     Writeln;
     IF bilde THEN BEGIN
        FOR y := 0 TO 31 DO FOR i := 0 TO 15 DO Pswap(0+i,31-i,y,y);
        Writeln('Done. Your image has done a horizontal flip.');
     END ELSE Writeln('You must load an image before you can flip it.');
     Writeln;
   END;
END;
PROCEDURE Flop(saynomore : Boolean);
VAR i,x : Byte;
BEGIN
   IF saynomore THEN
     FOR x := 0 TO 31 DO FOR i := 0 TO 15 DO Pswap(x,x,0+i,31-i)
   ELSE BEGIN
     Writeln;
     IF bilde THEN BEGIN
        FOR x := 0 TO 31 DO FOR i := 0 TO 15 DO Pswap(x,x,0+i,31-i);
        Writeln('Done. Your image has done a vertical flop.');
     END ELSE Writeln('You must load an image before you can flop it.');
     Writeln;
   END;
END;
PROCEDURE Cycle(b : Boolean);
VAR x,y,c : Byte;
BEGIN
   IF b THEN BEGIN
      FOR x := 0 TO 31 DO FOR y := 0 TO 31 DO BEGIN
         c := G(x,y);
         Inc(c);
         P(x,y,c);
   	END;
   END ELSE BEGIN
      FOR x := 0 TO 31 DO FOR y := 0 TO 31 DO BEGIN
         c := G(x,y);
         Dec(c);
         P(x,y,c);
   	END;
   END;
   bilde := true;
END;
FUNCTION Palettemode : Word; Forward;
FUNCTION Helpmode : Word; Forward;
FUNCTION Turtlemode : Word;
VAR
   k : Word;
BEGIN
   Letter(235,10,'t',0);
   Letter(255,25,Chr(Ord('a')+(heading div 45)),0);
   IF CapsOn THEN KillCaps;
   Repeat
      Cursor(((xpos*5)+21),((ypos*4)+67),false);
      TegnThumb;
      DrawImage;
      GetK(Addr(k));
      Cursor(((xpos*5)+21),((ypos*4)+67),false);
      DrawImage;
      Turtlemode := k;
      CASE k OF
         18432: Forward(1,false);
         20480: Backward(1,false);
         19712: BEGIN
            Letter(255,25,' ',system2);
            TurnRight(false);
            Letter(255,25,Chr(Ord('a')+(heading div 45)),0);
         END;
         19200: BEGIN
            Letter(255,25,' ',system2);
            TurnLeft(false);
            Letter(255,25,Chr(Ord('a')+(heading div 45)),0);
         END;
         11386,11354: BEGIN
            Letter(255,25,' ',system2);
            TurnLeft(false);
            Forward(1,false);
            P(xpos,ypos,GetActiveColor);
            Forward(1,false);
            Letter(255,25,Chr(Ord('a')+(heading div 45)),0);
         END;
         11640,11608: BEGIN
            Letter(255,25,' ',system2);
            TurnRight(false);
            Forward(1,false);
            P(xpos,ypos,GetActiveColor);
            Forward(1,false);
            Letter(255,25,Chr(Ord('a')+(heading div 45)),0);
         END;
         13358: BEGIN
            Letter(255,25,' ',system2);
            TurnRight(false);
            Forward(1,false);
            Letter(255,25,Chr(Ord('a')+(heading div 45)),0);
         END;
         13100: BEGIN
            Letter(255,25,' ',system2);
            TurnLeft(false);
            Forward(1,false);
            Letter(255,25,Chr(Ord('a')+(heading div 45)),0);
         END;
         14624: Plot;
         3115,20011: NextColor(false);
         18989,13613: PrevColor(false);
         15616: BEGIN
            Letter(235,10,'t',system2);
            Letter(255,25,' ',system2);
            TurtleMode := PaletteMode;
            Exit;
         END;
         15104: BEGIN
            Letter(235,10,'t',system2);
            Letter(255,25,' ',system2);
            TurtleMode := HelpMode;
            Exit;
         END;
         7181,14624,15360: BEGIN
            k := 283;
         END;
      END;
      IF CapsOn THEN BEGIN
         P(xpos,ypos,GetActiveColor);
         bilde := true;
      END;
   UNTIL k = 283;
   IF CapsOn THEN KillCaps;
   Letter(235,10,'t',system2);
   Letter(255,25,' ',system2);
END;
FUNCTION Helpmode;
VAR
   savepal : Array [0..767] of Byte;
   gray, black, white, red, green, blue, yellow : Word;
   ac, nc, c, k : Word;
   menup, cpos : Byte;
LABEL
   main;
 PROCEDURE HelpClear;
 BEGIN
    CursorX := 0;
    CursorY := 1;
    MsgBox('',0,false);
 END;
 PROCEDURE Overview;
 BEGIN
    HelpClear;
    Skrivln('Burn is a small program for Dos.',green);
    Skrivln('It might work under Windows as well, if',green);
    Skrivln('it has a good day. ;)',green);
    Skrivln('',green);
    Skrivln('It was written in the same language',green);
    Skrivln('as "Fast Tracker 2", which is Pascal.',green);
    Skrivln('',green);
    Skrivln('The main purpose of Burn is to make',green);
    Skrivln('it easier for me, the author, to create',green);
    Skrivln('sprites (small pictures) for other programs',green);
    Skrivln('that I make.',green);
   Skrivln('',green);
   Skrivln('If you like Burn, I''m happy.',green);
    Skrivln('And if you don''t, I really don''t care. ;)',green);
    Skrivln('',green);
    Skrivln('',green);
    Skrivln('Press any key to get back...',red);
    wfk;
 END;
 PROCEDURE Bugs;
 BEGIN
    HelpClear;
    Skrivln('Burn is not bugfree.',red);
    Skrivln('',blue);
    Skrivln('If you find a bug, feel free to send me an',white);
    Skrivln('e-mail, and I''ll see what I can do.',white);
    Skrivln('',blue);
    Skrivln('My e-mail adress can be found by typing',white);
    Skrivln('"info" at the console.',white);
    Skrivln('',blue);
    Skrivln('',blue);
    Skrivln('Press a key...',blue);
    wfk;
 END;
 PROCEDURE BurnUse;
 BEGIN
    HelpClear;
    Skrivln('Here are a few suggested uses for Burn:',red);
    Skrivln('',green);
    Skrivln('You can use Burn to convert a raw',green);
    Skrivln('32x32 256color image w/ a palette',green);
    Skrivln('to ascii, .ico, .bmp or .pcx.',green);
    Skrivln('',green);
    Skrivln('You can use Burn to draw and have fun.',green);
    Skrivln('',green);
    Skrivln('You can use Burn to make a handcrafted',green);
    Skrivln('palette.',green);
    Skrivln('',green);
    Skrivln('You can use Burn to make a tileable backround',green);
   Skrivln('image for your favorite window-manager.',green);
    Skrivln('',green);
    Skrivln('You can use Burn to make a small animation.',green);
    Skrivln('',green);
    Skrivln('You can use Burn to draw small buttons.',green);
    Skrivln('',green);
    Skrivln('',green);
    Skrivln('You may press a key...',red);
    wfk;
 END;
 PROCEDURE WhyConsole;
 BEGIN
    HelpClear;
    Skrivln('',green);
    Skrivln('q: Why''s Burn got a console?',red);
    Skrivln('a: It''s more expert-friendly.',blue);
    Skrivln('',green);
    Skrivln('Press a key to return...',white);
    wfk;
 END;
 PROCEDURE ScriptConsole;
 BEGIN
    HelpClear;
    Skrivln('Burn has a set of console and script-commands',green);
    Skrivln('which makes it possible to do some simple,',green);
    Skrivln('file operations, and make animations, amongst',green);
    Skrivln('others',green);
    Skrivln('',green);
    Skrivln('I''m planning to make a full overview of all',green);
    Skrivln('the script and console commands here, some',green);
    Skrivln('time in the future.',green);
    Skrivln('',green);
    Skrivln('Press a key to return...',green);
    wfk;
 END;
 PROCEDURE DrawmodeOverview;
 BEGIN
    HelpClear;
    Skrivln('Drawmode overview',blue);
    Skrivln('',ac);
{            0123456789..........0123456789..........0123456789}
    Skrivln('Capslock    : Toggles the "pen down"-feature.',ac);
    Skrivln('Arrow left  :	Moves the cursor to the left.',ac);
    Skrivln('Arrow right :	Moves the cursor to the right.',ac);
    Skrivln('Arrow up    :	Moves the cursor up.',ac);
    Skrivln('Arrow down  : Moves the cursor down.',ac);
    Skrivln('Delete      : Scrolls the image to the left.',ac);
    Skrivln('Page down   :	Scrolls the image to the right.',ac);
    Skrivln('Home        : Scrolls the image up.',ac);
    Skrivln('End         : Scrolls the image down.',ac);
   Skrivln('Insert      : Flips the image in one way.',ac);
    Skrivln('Page up     : Flips the image in another way.',ac);
    Skrivln('Enter       : Enters palettemode.',ac);
    Skrivln('',ac);
    Skrivln('z	: Some sort of twirl.',ac);
    Skrivln('x	: Some weird sort of blur.',ac);
    Skrivln('c	: Quantisize the colors.',ac);
    Skrivln('v : Another weird sort of blur.',ac);
    Skrivln('b : "Normal" palette-dependant blur.',ac);
    Skrivln('n : Truecolor blur with 256 colors!',ac);
    Skrivln('    (Burn tries it''s best).',ac);
    Skrivln('d : Gradient fill. The direction of the fill',ac);
    Skrivln('    is the same as the turtle''s direction.',ac);
    Skrivln('    (See turtlemode)',ac);
    Skrivln('',ac);
    Skriv('Press any key to read more...',blue);
    wfk;
    HelpClear;
    Skrivln('m : Truecolor median blur.',ac);
    Skrivln('a : Truecolor blur on a single pixel.',ac);
    Skrivln('f : Normal fill. (Note that the fill-rutines',ac);
    Skrivln('    are far from beeing perfect, but they''re',ac);
    Skrivln('    hopefully better than nothing :) ).',ac);
    Skrivln('g : One press shows you the palette-nr.',ac);
    Skrivln('    Two presses selects that color.',ac);
    Skrivln('    color.',ac);
    Skrivln('h : Fancy non-linear gradient fill.',ac);
    Skrivln('. : Cycles all pixels one step forward.',ac);
    Skrivln(', : Cycles all pixels one step backward.',ac);
    Skrivln('k : The first press selects the first',ac);
    Skrivln('    coordinate. The second press selects',ac);
    Skrivln('    the second coord. and draws a rectangle.',ac);
    Skrivln('l : Draws a line, same way as the rectangle.',ac);
    Skrivln('o : Draws a circle, same way as above.',ac);
    Skrivln('p : Draws a circle, using the radius',ac);
    Skrivln('    instead of the edge-thingies to',ac);
    Skrivln('    base the actual shape on.',ac);
    Skrivln('+ : Next color.',ac);
   Skrivln('- : Previous color.',ac);
    Skrivln('',ac);
    Skrivln('Press a key to get back...',blue);
    wfk;
 END;
 PROCEDURE PalettemodeOverview;
 BEGIN
    HelpClear;
    Skrivln('Palettemode overview',blue);
    Skrivln('',ac);
{            0123456789..........0123456789..........0123456789}
    Skrivln('Capslock    : Toggles the "move color".',ac);
    Skrivln('Arrow left  :	Moves the cursor to the left.',ac);
    Skrivln('Arrow right :	Moves the cursor to the right.',ac);
    Skrivln('Arrow up    :	Moves the cursor up.',ac);
    Skrivln('Arrow down  : Moves the cursor down.',ac);
    Skrivln('Home        : Moves to the start of the row.',ac);
    Skrivln('End         : Moves to the end of the row.',ac);
    Skrivln('Delete      : Makes the color black.',ac);
    Skrivln('Insert      : Makes the color white.',ac);
    Skrivln('Space       : Mark''s the start/end of a',ac);
   Skrivln('              selection.',ac);
    Skrivln('Page up     : Lighten the selected color(s).',ac);
    Skrivln('Page down   : Darken the selected color(s).',ac);
    Skrivln('Enter       : Enter drawmode.',ac);
    Skrivln('',ac);
    Skrivln('r        : Increases the red-value of the',ac);
    Skrivln('           selected color(s).',ac);
    Skrivln('g        : Same as above, but with green.',ac);
    Skrivln('b        : Same as above, but with blue.',ac);
    Skrivln('e        : Decreases the red-value of the',ac);
    Skrivln('           selected color(s).',ac);
    Skrivln('f        : Same as above, but with green.',ac);
    Skrivln('v        : Same as above, but with blue.',ac);
    Skrivln('',ac);
    Skriv('Press any key to get more amazing info...',blue);
    wfk;
    HelpClear;
    Skrivln('j     : Interpolates the color-values',ac);
   Skrivln('        between the selected colors.',ac);
    Skrivln('        (gradient)',ac);
    Skrivln('m     : Makes the selected color(s)',ac);
   Skrivln('        monochrome.',ac);
    Skrivln('i     : Inverts the selected color(s).',ac);
    Skrivln('+     : Selects next color.',ac);
    Skrivln('-     : Selects previous color.',ac);
    Skrivln('c     : Copy the selected color to mem.',ac);
    Skrivln('p     : Pastes the copied color.',ac);
    Skrivln('s     : Swaps two colors (in two-steps).',ac);
    Skrivln('k     : Kills (removes) the unused',ac);
    Skrivln('        colors amongst the selected ones.',ac);
    Skrivln(',     : Sorts the selected colors by',ac);
    Skrivln('        lightness.',ac);
    Skrivln('.     : Same as above, but reversed.',ac);
    Skrivln('q     : Sorts the marked color(s), by',ac);
   Skrivln('        lightness, and removes the',ac);
    Skrivln('        unused ones.',ac);
    Skrivln('',ac);
    Skrivln('You can move a color around with ctrl+arrows.',ac);
    Skrivln('',ac);
    Skrivln('',ac);
    Skriv('Press the "anykey" to get back... :P',blue);
    wfk;
 END;
 PROCEDURE TurtlemodeOverview;
 BEGIN
    HelpClear;
    Skrivln('Turtlemode overview',blue);
    Skrivln('',ac);
    Skrivln('Arrow left  : Turns the turtle to the left.',ac);
    Skrivln('Arrow right :	Turns the turtle to the right.',ac);
    Skrivln('Arrow up    :	Moves the turtle forward.',ac);
    Skrivln('Arrow down  :	Moves the turtle backward.',ac);
    Skrivln('Space       : Plots a point.',ac);
    Skrivln('Enter       : Returns to drawmode.',ac);
    Skrivln('Capslock    : Leaves a trail of pixles',ac);
    Skrivln('              behind.',ac);
    Skrivln('',ac);
    Skrivln('z   : Turns the turtle left, and',ac);
    Skrivln('      moves it forward two steps.',ac);
    Skrivln('x   : Turns the turtle right, and',ac);
    Skrivln('      moves it forward two steps.',ac);
    Skrivln(',   : Turns the turtle left, and',ac);
    Skrivln('      moves it forward one step.',ac);
    Skrivln('.   : Turns the turtle right, and',ac);
    Skrivln('      moves it forward one step.',ac);
    Skrivln('+   : Selects next color.',ac);
    Skrivln('-   : Selects previous color.',ac);
    Skrivln('',ac);
    Skriv('Press space to return...',blue);
    wfk;
 END;
 PROCEDURE GfxmodeOverview;
 BEGIN
    HelpClear;
    ac := green;
    nc := red;
    Skrivln('',ac);
    Skriv(  'If you type',ac);
                Skriv(' "draw" ',nc);
                 Skrivln('in the console, you''ll',ac);
    Skriv('hopefully enter the so-called',ac);
                     	          Skriv(' "drawmode"',nc);
                                         Skrivln('.',ac);
    Skrivln('This is were you can plot pixels and ',ac);
   Skrivln('otherwise manipulate the image.',ac);
    Skrivln('',ac);
    Skrivln('',ac);
    Skrivln('When there''s colorful graphics on your',ac);
   Skrivln('screen, and it''s not text, it hopefully',ac);
   Skrivln('means that you''re in the graphical-interface',ac);
   Skrivln('part of burn... and that''s a good sign. =)',ac);
    Skrivln('',ac);
    Skrivln('',ac);
    Skrivln('Anyways, when you''re in som sort of',ac);
   Skrivln('graphics-mode, there is several keys',ac);
    Skriv('you can use. The so-called',ac);
   	Skriv('"global keys"',nc);
      Skrivln('.',ac);
    Skrivln('',ac);
    Skrivln('',ac);
    Skrivln('Press any key to read more...',ac);
    wfk;
    HelpClear;
    ac := green;
    nc := red;
    Skrivln('',ac);
    Skriv('Here is an overview of the global keys:',ac);
    Skrivln('',ac);
    Skrivln('',ac);
   Skriv('   F1        ',nc); Skrivln(': help mode',ac);
    Skriv('   F2        ',nc); Skrivln(': normal draw mode',ac);
    Skriv('   F3        ',nc); Skrivln(': palette mode',ac);
    Skriv('   F4        ',nc); Skrivln(': turtle mode',ac);
    Skriv('   Backspace ',nc); Skrivln(': undo',ac);
    Skrivln('',ac);
    Skrivln('',ac);
    Skrivln('Press any key to return...',ac);
    wfk;
 END;
 PROCEDURE DrawmodeInfo;
 LABEL TestMain;
 BEGIN
   TestMain:
    HelpClear;
    Inc(CursorY,2);
    ac := white;
    nc := gray;
    IF (menup > 4) OR (menup < 0) THEN menup := 0;
    Skrivln('    Go ahead, use those funky arrow-keys! ;)',blue);
    Inc(CursorY);
    cpos := CursorY;
    Repeat
      CursorY := cpos;
      IF menup = 0 THEN c := ac ELSE c := nc;
      CursorX := 6;
      Skrivln('*Graphics mode overview',c);
      IF menup = 1 THEN c := ac ELSE c := nc;
      CursorX := 6;
    Skrivln('*Drawmode overview',c);
      IF menup = 2 THEN c := ac ELSE c := nc;
      CursorX := 6;
    Skrivln('*Palettemode overview',c);
      IF menup = 3 THEN c := ac ELSE c := nc;
      CursorX := 6;
    Skrivln('*Turtlemode overview',c);
      IF menup = 4 THEN c := ac ELSE c := nc;
      CursorX := 6;
    Skrivln('*Go back',c);
      GetK(Addr(k));
      HelpMode := k;
      CASE k OF
         20480: BEGIN
                Inc(menup);
                   IF menup = 5 THEN menup := 0;
                END;
         18432: BEGIN
                   IF menup = 0 THEN menup := 5;
                   Dec(menup);
                END;
         7181, 14624: BEGIN
                  CASE menup OF
                     0: BEGIN GfxmodeOverview; GOTO TestMain; END;
                     1: BEGIN DrawmodeOverview; GOTO TestMain; END;
                     2: BEGIN PalettemodeOverview; GOTO TestMain; END;
                     3: BEGIN TurtlemodeOverview; GOTO TestMain; END;
                     4: k := 283;
                  END;
               END;
      15360, 15616, 15872: BEGIN k := 283; HelpMode := 15360; END;
      END;
    UNTIL k = 283;
    menup := 5;
 END;
 PROCEDURE WhyF;
 BEGIN
    HelpClear;
    Skrivln('It''s fun. ;)',blue);
    Skrivln('',blue);
    Skrivln('Press a key to return.',blue);
    wfk;
 END;
 PROCEDURE Lisence;
 BEGIN
    HelpClear;
    Skrivln('Burn is licenced under the Gnu Public License.',green);
    wfk;
 END;
 PROCEDURE MoreInfo;
 BEGIN
    HelpClear;
    Skrivln('Type "readme" at the console',green);
    Skrivln('to get an overview of all the',green);
    Skrivln('console commands.',green);
    Skrivln('',green);
    Skrivln('Press any key to return...',green);
    wfk;
 END;
 PROCEDURE HelpRestore;
 BEGIN
    Move(savepal,thepal,45);  {bruker bare 45 bytes}
    SetPalette;
 END;
 PROCEDURE HelpSet;
 BEGIN
    Move(thepal,savepal,45);  {bruker bare 45 bytes}
    { 0 - system1 }
    thepal[00] := $00; thepal[01] := $00; thepal[02] := $00;
    { 1 - system2 - bgcolor }
    thepal[03] := $00; thepal[04] := $00; thepal[05] := $00;
    { 2 - system3 }
    thepal[06] := $00; thepal[07] := $00; thepal[08] := $00;
    { 3 - white blurcolor }
    thepal[09] := $DA; thepal[10] := $DA; thepal[11] := $DA;
    { 4 - white textcolor }
    thepal[12] := $FF; thepal[13] := $FF; thepal[14] := $FF;
    { 5 - red blurcolor }
    thepal[15] := $DA; thepal[16] := $00; thepal[17] := $00;
    { 6 - red textcolor }
    thepal[18] := $FF; thepal[19] := $00; thepal[20] := $00;
    { 7 - green blurcolor }
    thepal[21] := $00; thepal[22] := $DA; thepal[23] := $00;
    { 8 - green textcolor }
    thepal[24] := $00; thepal[25] := $FF; thepal[26] := $00;
    { 9 - blue blurcolor }
    thepal[27] := $00; thepal[28] := $00; thepal[29] := $CA;
    { A - blue textcolor }
    thepal[30] := $00; thepal[31] := $00; thepal[32] := $F0;
    { B - yellow blurcolor }
    thepal[33] := $DA; thepal[34] := $DA; thepal[35] := $00;
    { C - yellow textcolor }
    thepal[36] := $FF; thepal[37] := $FF; thepal[38] := $00;
    { D - gray blurcolor }
    thepal[39] := $D0; thepal[40] := $D0; thepal[41] := $D0;
    { E - gray textcolor }
    thepal[42] := $E0; thepal[43] := $E0; thepal[44] := $E0;
    { Set palette }
    SetPalette;
 END;
BEGIN
   HelpSet;
   { Lettere � bruke }
   black  := $0000;  white := $0304;
  red    := $0506;  green := $0708;  blue := $090A;
  yellow := $0B0C;   gray := $0D0E;
   menup := 0;
  main:
   HelpClear;
   Skrivln('This is the help-system for '+vstring+GetVer,red);
   Inc(CursorY,2);
   ac := blue;
   Skrivln('   Use the arrow keys to navigate.',ac);
   Skrivln('   Press ENTER to select a topic.',ac);
   Skrivln('   Press ESC to go back.',ac);
   Inc(CursorY,2);
   ac := white;
   nc := gray;
   Skrivln('    Availible topics:',ac);
   Inc(CursorY);
   cpos := CursorY;
   Repeat
      CursorY := cpos;
      IF menup = 0 THEN c := ac ELSE c := nc;
      CursorX := 6;
      Skrivln('*An overview of Burn',c);
      IF menup = 1 THEN c := ac ELSE c := nc;
      CursorX := 6;
    Skrivln('*Bugs',c);
      IF menup = 2 THEN c := ac ELSE c := nc;
      CursorX := 6;
    Skrivln('*What can I use burn for?',c);
      IF menup = 3 THEN c := ac ELSE c := nc;
      CursorX := 6;
      Skrivln('*The console',c);
      IF menup = 4 THEN c := ac ELSE c := nc;
      CursorX := 6;
      Skrivln('*Script-language and console commands',c);
      IF menup = 5 THEN c := ac ELSE c := nc;
      CursorX := 6;
      Skrivln('*The graphics mode',c);
      IF menup = 6 THEN c := ac ELSE c := nc;
      CursorX := 6;
      Skrivln('*Why, why, why?',c);
      IF menup = 7 THEN c := ac ELSE c := nc;
      CursorX := 6;
      Skrivln('*Lisence info',c);
      IF menup = 8 THEN c := ac ELSE c := nc;
      CursorX := 6;
      Skrivln('*More info',c);
      IF menup = 9 THEN c := ac ELSE c := nc;
      CursorX := 6;
      Skrivln('*Exit help',c);
      GetK(Addr(k));
      HelpMode := k;
      CASE k OF
         20480: BEGIN
                Inc(menup);
                   IF menup = 10 THEN menup := 0;
                END;
         18432: BEGIN
                   IF menup = 0 THEN menup := 10;
                   Dec(menup);
                END;
         7181, 14624: BEGIN
                  CASE menup OF
                     0: BEGIN Overview; GOTO main; END;
                     1: BEGIN Bugs; GOTO main; END;
                     2: BEGIN BurnUse; GOTO main; END;
                     3: BEGIN WhyConsole; GOTO main; END;
                     4: BEGIN ScriptConsole; GOTO main; END;
                     5: BEGIN DrawmodeInfo; GOTO main; END;
                     6: BEGIN WhyF; GOTO main; END;
                     7: BEGIN Lisence; GOTO main; END;
                     8: BEGIN MoreInfo; GOTO main; END;
                     { Help me damnit, I just want to convert a .bmp to a .ico! }
                     9: k := 283;
                  END;
               END;
      15360, 15616, 15872: BEGIN k := 283; HelpMode := 15360; END;
      END;
   UNTIL k = 283;
   IF CapsOn THEN KillCaps;
   HelpRestore;
END;
FUNCTION Palettemode;
VAR
   w,k : Word;
   x,y : Word;
   pre : Word;
   tr,tg,tb : Byte;
   tn : Word;
   savepal : Array [0..767] of Byte;
 PROCEDURE GetACt;
 BEGIN
    pre := TheImage[(y shl 8)+(y shl 6)+x+321]*3;
    tr := ThePal[pre+0];
    tg := ThePal[pre+1];
    tb := ThePal[pre+2];
    tn := pre;
 END;
 PROCEDURE SetACt;
 BEGIN
    pre := TheImage[(y shl 8)+(y shl 6)+x+321];
    SwapAllColors(pre,tn div 3);
    pre := pre*3;
    ThePal[pre+0] := tr;
    ThePal[pre+1] := tg;
    ThePal[pre+2] := tb;
    TegnThumb;
    DrawImage;
    SetPalette;
    palette := true;
 END;
 PROCEDURE SwapACt;
 BEGIN
    pre := TheImage[(y shl 8)+(y shl 6)+x+321];
    SwapAllColors(pre,tn div 3);
    pre := pre*3;
    ThePal[tn+0] := ThePal[pre+0];
    ThePal[pre+0] := tr;
    ThePal[tn+1] := ThePal[pre+1];
    ThePal[pre+1] := tg;
    ThePal[tn+2] := ThePal[pre+2];
    ThePal[pre+2] := tb;
    TegnThumb;
    DrawImage;
    SetPalette;
    palette := true;
 END;
BEGIN
   x := acticolx;
   y := acticoly;
   first_done := false;
   Move(thepal,savepal,768);
   Cursor(x,y,true);
   IF CapsOn THEN KillCaps;
   GetACt;
   Repeat
      IF (k <> 0) THEN RGBinfo(x,y,0);
      Cursor(x,y,true);
      DrawImage;
      GetK(Addr(k));
      {Trenger ikke slette gammel rgbinfo hvis space }
      IF (k <> 0) XOR (k = 14624) THEN RGBinfo(x,y,system2);
      Cursor(x,y,true);
      DrawImage;
      Palettemode := k;
      CASE k OF
         19712: BEGIN
                   IF CapsOn THEN BEGIN
   			          Move(thepal,savepal,768);
   			          GetACt;
   		             Inc(x,7);
   		             IF x > 218 THEN BEGIN {7*31+1}
   		                x := 1;
   		                Inc(y,7);
     		                IF y > 50 THEN y := 1; {7*7+1}
     	             END;
                      SwapACt;
                   END ELSE BEGIN
                      Inc(x,7);
                      IF x > 218 THEN x := 1;
                   END;
                END;
         19200: BEGIN
                   IF CapsOn THEN BEGIN
   			          Move(thepal,savepal,768);
   			          GetACt;
   		             IF x < 2 THEN BEGIN
  	    	                x := 225; {7*32+1}
   		                IF y < 2 THEN y := 57; {7*8+1}
                       Dec(y,7);
   		             END;
   		             Dec(x,7);
                      SwapACt;
                   END ELSE BEGIN
                      IF x < 2 THEN x := 225;
                      Dec(x,7);
                   END;
                END;
         3115,20011: BEGIN
            Inc(x,7);
            IF x > 218 THEN BEGIN {7*31+1}
               x := 1;
               Inc(y,7);
               IF y > 50 THEN y := 1; {7*7+1}
            END;
         END;
         18989,13613: BEGIN
            IF x < 2 THEN BEGIN
               x := 225; {7*32+1}
               IF y < 2 THEN y := 57; {7*8+1}
               Dec(y,7);
            END;
            Dec(x,7);
         END;
         18432: BEGIN
                   IF CapsOn THEN BEGIN
                   Move(thepal,savepal,768);
                   GetACt;
       	          IF y < 2 THEN y := 57; {(7*8)+1}
     					 Dec(y,7);
                      SwapACt;
                   END ELSE BEGIN
   			          IF y < 2 THEN y := 57; {(7*8)+1}
   						 Dec(y,7);
                   END;
             END;
         20480: BEGIN
                   IF CapsOn THEN BEGIN
                   Move(thepal,savepal,768);
                   GetACt;
       	          Inc(y,7);
     					 IF y > 50 THEN y := 1; {7*7+1}
                      SwapACt;
                   END ELSE BEGIN
   			          Inc(y,7);
   						 IF y > 50 THEN y := 1; {7*7+1}
                   END;
                END;
         4978: BEGIN Move(thepal,savepal,768); SelectMoreRed(x,y); END;
         8807: BEGIN Move(thepal,savepal,768); SelectMoreGreen(x,y); END;
         12386: BEGIN Move(thepal,savepal,768); SelectMoreBlue(x,y); END;
         4709: BEGIN Move(thepal,savepal,768); SelectLessRed(x,y); END;
         8550: BEGIN Move(thepal,savepal,768); SelectLessGreen(x,y); END;
         12150: BEGIN Move(thepal,savepal,768); SelectLessBlue(x,y); END;
     {c} 11875: GetACt;
     {p}  6512: BEGIN Move(thepal,savepal,768); SetACt; END;
     {s}  8051: BEGIN Move(thepal,savepal,768); SwapACt; END;
  {pgup} 18688: BEGIN Move(thepal,savepal,768); SelectBright(x,y); END;
  {pgdn} 20736: BEGIN Move(thepal,savepal,768); SelectDark(x,y); END;
   {del} 21248: BEGIN
                  Move(thepal,savepal,768);
                  pre := TheImage[(y shl 8)+(y shl 6)+x+321]*3;
                  ThePal[pre+0] := 0;
                  ThePal[pre+1] := 0;
                  ThePal[pre+2] := 0;
                  SetPalette;
                  palette := true;
               END;
   {ins} 20992: BEGIN
                  Move(thepal,savepal,768);
                  pre := TheImage[(y shl 8)+(y shl 6)+x+321]*3;
                  ThePal[pre+0] := 63;
                  ThePal[pre+1] := 63;
                  ThePal[pre+2] := 63;
                  SetPalette;
                  palette := true;
               END;
 {space} 14624: SelectionOn(x,y);
     {a} 7777: BEGIN SelectionOn(1,1); x := 7*31+1; y := 7*7+1; END;
     {j}  9322: BEGIN Move(thepal,savepal,768); SelectColorGradient(x,y); END;
     {m} 12909: BEGIN Move(thepal,savepal,768); SelectMono(x,y); END;
     {i}  5993: BEGIN Move(thepal,savepal,768); SelectInvert(x,y); END;
     {k}  9579: BEGIN Move(thepal,savepal,768); SelectKill(x,y); END;
     {,} 13100: BEGIN Move(thepal,savepal,768); SelectSortLeft(x,y); END;
     {.} 13358: BEGIN Move(thepal,savepal,768); SelectSortRight(x,y); END;
     {q}  4209: BEGIN Move(thepal,savepal,768); SelectKillSortRemoveReallyDarkAll(x,y); END;
         15872: BEGIN
            Cursor(x,y,true);
            acticolx := x;
            acticoly := y;
            PaletteMode := Turtlemode;
            Exit;
         END;
         15104: BEGIN
            Cursor(x,y,true);
            acticolx := x;
            acticoly := y;
            PaletteMode := HelpMode;
            Exit;
         END;
         18176: x := 1;
         20224: x := 218; {7*31+1}
         283,7181,15360: BEGIN  { 14624 var med her f�r }
            k := 283;
            Cursor(x,y,true);
            acticolx := x;
            acticoly := y;
         END;
         3592: BEGIN
         Move(savepal,thepal,768);
            SetPalette;
         END;
      END;
   UNTIL k = 283;
   IF CapsOn THEN KillCaps;
END;
PROCEDURE Drawmode;
VAR
   k : Word;
   i,c : Word;
   s : String;
   oldk : Word;
   savepal : Array [0..767] of Byte;
   oxp, oyp : Byte;
   xp, yp : String[3];
BEGIN
   IF CapsOn THEN KillCaps;
   oxp := $FF;
   Move(thepal,savepal,768);
   Bilde2MoreMem;
   Repeat
      Cursor(((xpos*5)+21),((ypos shl 2)+67),false);
      TegnThumb;
      oldk := k;
      DrawImage;
      Str(xpos,xp);
      Str(ypos,yp);
      SkrivXY('x:'+xp+' y:'+yp,254,192,$02 shl 8+G(xpos,ypos),false);
      GetK(Addr(k));
      IF oldk = 8807 THEN FOR i := 1 TO Ord(s[0]) DO Letter(255+7*i,22,s[i],system2);
      Cursor(((xpos*5)+21),((ypos shl 2)+67),false);
      DrawImage;
      CASE k OF
         11386: BEGIN
                   Bilde2MoreMem;
                Distortall(1);    {z}
                END;
         18432: BEGIN
                Dec(ypos);
             IF ypos < 0 THEN ypos := 31;
                END;
         20480: BEGIN
                Inc(ypos);
             IF ypos > 31 THEN ypos := 0;
             END;
         19712: BEGIN
                Inc(xpos);
             IF xpos > 31 THEN xpos := 0;
             END;
         19200: BEGIN
                Dec(xpos);
             IF xpos < 0 THEN xpos := 31;
             END;
         18176: BEGIN
                   Bilde2MoreMem;
                MovePictureUp;
                END;
         20224: BEGIN
                   Bilde2MoreMem;
             MovePictureDown;
                END;
         21248: BEGIN
                   Bilde2MoreMem;
                MovePictureLeft;
                END;
         20736: BEGIN
                   Bilde2MoreMem;
             MovePictureRight;
                END;
         18688: BEGIN
                   Bilde2MoreMem;
                RotateRight(false); {z}
                END;
         20992: BEGIN
                   Bilde2MoreMem;
                Flop(true);
                END;
  	      14624: BEGIN
                Bilde2MoreMem;
             Plot;
             END;
         12909: BEGIN {m}
                   Bilde2MoreMem;
                mBlurGfx;
         	    END;
         4471: BEGIN {w}
                  Bilde2MoreMem;
                  mBlur(xpos,ypos);
               END;
         4209: BEGIN {q}
                   Bilde2MoreMem;
                   kBlur(xpos,ypos);
               END;
         5993: BEGIN
                  Bilde2MoreMem;
                  MsgBox('',60,false);
                  SkrivXY('Please press the key that',74,87,$0100,false);
                  SkrivXY('best represent the letter',74,95,$0100,false);
                  SkrivXY('you want to insert.',74,103,$0100,false);
                  GetK(Addr(oldk)); {oldk trengs ikke lenger}
                  SkrivXY(chr(oldk),xpos,ypos,GetActiveColor,true);
               END;
         8807: IF NOT (oldk = 8807) THEN BEGIN {g}
               	i := G(xpos,ypos);
                 Str(i,s);
                 FOR i := 1 TO Ord(s[0]) DO Letter(255+7*i,22,s[i],system1);
                 DrawImage;
              END ELSE BEGIN
                 SetColor(G(xpos,ypos),false);
              END;
         9836: IF oxp = $FF THEN BEGIN {l}
                  oxp := xpos;
                  oyp := ypos;
                 Str(oxp,s);
                 FOR i := 1 TO Ord(s[0]) DO Letter(255+7*i,22,s[i],system1);
                 Str(oyp,s);
                 FOR i := 1 TO Ord(s[0]) DO Letter(255+7*i,30,s[i],system1);
                 DrawImage;
              END ELSE BEGIN
                 Str(oxp,s);
                 FOR i := 1 TO Ord(s[0]) DO Letter(255+7*i,22,s[i],system2);
                 Str(oyp,s);
                 FOR i := 1 TO Ord(s[0]) DO Letter(255+7*i,30,s[i],system2);
                  Bilde2MoreMem;
                  bilde := true;
                  PLinje(oxp,oyp,xpos,ypos,GetActiveColor);
                  oxp := $FF;
              END;
         9579: IF oxp = $FF THEN BEGIN {k}
                  oxp := xpos;
                  oyp := ypos;
                 Str(oxp,s);
                 FOR i := 1 TO Ord(s[0]) DO Letter(255+7*i,22,s[i],system1);
                 Str(oyp,s);
                 FOR i := 1 TO Ord(s[0]) DO Letter(255+7*i,30,s[i],system1);
                 DrawImage;
              END ELSE BEGIN
                 Str(oxp,s);
                 FOR i := 1 TO Ord(s[0]) DO Letter(255+7*i,22,s[i],system2);
                 Str(oyp,s);
                 FOR i := 1 TO Ord(s[0]) DO Letter(255+7*i,30,s[i],system2);
                  Bilde2MoreMem;
                  bilde := true;
                  PLinje(oxp,oyp,xpos,oyp,GetActiveColor);
                  PLinje(xpos,oyp,xpos,ypos,GetActiveColor);
                  PLinje(oxp,ypos,xpos,ypos,GetActiveColor);
                  PLinje(oxp,oyp,oxp,ypos,GetActiveColor);
                  oxp := $FF;
              END;
         6255: IF oxp = $FF THEN BEGIN {o}
                  oxp := xpos;
                  oyp := ypos;
                 Str(oxp,s);
                 FOR i := 1 TO Ord(s[0]) DO Letter(255+7*i,22,s[i],system1);
                 Str(oyp,s);
                 FOR i := 1 TO Ord(s[0]) DO Letter(255+7*i,30,s[i],system1);
                 DrawImage;
              END ELSE BEGIN
                 Str(oxp,s);
                 FOR i := 1 TO Ord(s[0]) DO Letter(255+7*i,22,s[i],system2);
                 Str(oyp,s);
                 FOR i := 1 TO Ord(s[0]) DO Letter(255+7*i,30,s[i],system2);
                  Bilde2MoreMem;
                  IF xpos > oxp THEN BEGIN
                     IF ypos > oyp THEN BEGIN
                        Circle((oxp+xpos) shr 1,(oyp+ypos) shr 1,(xpos-oxp) shr 1,(ypos-oyp) shr 1,GetActiveColor);
                     END ELSE BEGIN
                        Circle((oxp+xpos) shr 1,(oyp+ypos) shr 1,(xpos-oxp) shr 1,(oyp-ypos) shr 1,GetActiveColor);
                     END;
                  END ELSE BEGIN
                     IF ypos > oyp THEN BEGIN
                        Circle((oxp+xpos) shr 1,(oyp+ypos) shr 1,(oxp-xpos) shr 1,(ypos-oyp) shr 1,GetActiveColor);
                     END ELSE BEGIN
                        Circle((oxp+xpos) shr 1,(oyp+ypos) shr 1,(oxp-xpos) shr 1,(oyp-ypos) shr 1,GetActiveColor);
                     END;
                  END;
                  oxp := $FF;
              END;
         6512: IF oxp = $FF THEN BEGIN {p}
                  oxp := xpos;
                  oyp := ypos;
                 Str(oxp,s);
                 FOR i := 1 TO Ord(s[0]) DO Letter(255+7*i,22,s[i],system1);
                 Str(oyp,s);
                 FOR i := 1 TO Ord(s[0]) DO Letter(255+7*i,30,s[i],system1);
                 DrawImage;
              END ELSE BEGIN
                 Str(oxp,s);
                 FOR i := 1 TO Ord(s[0]) DO Letter(255+7*i,22,s[i],system2);
                 Str(oyp,s);
                 FOR i := 1 TO Ord(s[0]) DO Letter(255+7*i,30,s[i],system2);
                  Bilde2MoreMem;
                  IF xpos > oxp THEN BEGIN
                     IF ypos > oyp THEN BEGIN
                        Circle(oxp,oyp,(xpos-oxp),(ypos-oyp),GetActiveColor);
                     END ELSE BEGIN
                        Circle(oxp,oyp,(xpos-oxp),(oyp-ypos),GetActiveColor);
                     END;
                  END ELSE BEGIN
                     IF ypos > oyp THEN BEGIN
                        Circle(oxp,oyp,(oxp-xpos),(ypos-oyp),GetActiveColor);
                     END ELSE BEGIN
                        Circle(oxp,oyp,(oxp-xpos),(oyp-ypos),GetActiveColor);
                     END;
                  END;
                  oxp := $FF;
              END;
         7777: BEGIN              {a}
               Bilde2MoreMem;
            Blur(xpos,ypos);
                  SetPalette;
                  palette := true;
      			END;
         8051: BEGIN
                  Bilde2MoreMem;
               Soften(xpos,ypos); {s}
               END;
         11640: BEGIN
                   Bilde2MoreMem;
                Softenall;        {x}
                END;
         11875: BEGIN
                   Bilde2MoreMem;
                   kBlurAll; {c}
                END;
         12150: BEGIN
                   Bilde2MoreMem;
                Shittyblurall(1); {v}
                END;
         12654: BEGIN
                   Bilde2MoreMem;
                Blurgfx; {n}
                END;
         12386: BEGIN
                   Bilde2MoreMem;
                Shittyblurall(0); {b}
                END;
         13100: BEGIN
                   Bilde2MoreMem;
                Cycle(false);     {,}
                END;
         13358: BEGIN
                   Bilde2MoreMem;
                Cycle(true);      {.}
                END;
         8292: BEGIN
                  Bilde2MoreMem;
               Fill(xpos,ypos,GetActiveColor,true,false);  {d}
               END;
         8550: BEGIN
                  Bilde2MoreMem;
               Fill(xpos,ypos,GetActiveColor,false,false); {f}
               END;
         9064: BEGIN
                  Bilde2MoreMem;
               Fill(xpos,ypos,GetActiveColor,true,true);   {h}
               END;
         5993: BEGIN
                  RGBinfo(((xpos*5)+21),((ypos*4)+67),0);
                  DrawImage;
                  asm
                     xor ah,ah
                     int 16h
                  end;
                  RGBinfo(((xpos*5)+21),((ypos*4)+67),system2);
                  DrawImage;
                END;
         15872: BEGIN k := Turtlemode; Move(thepal,savepal,768); END;
         15616,7181: BEGIN k := Palettemode; Move(thepal,savepal,768); END;
         15104: HelpMode; { ikke k := HelpMode for at man ikke skal g� til consolet }
         3592: BEGIN
                  MoreMem2Bilde;
                  Move(savepal,thepal,768);
                  SetPalette;
                  DrawImage;
                  KillCaps;
               END;
         3115,20011: NextColor(false);
         18989,13613: PrevColor(false);
      END;
     IF CapsOn THEN BEGIN
         Bilde2MoreMem;
         P(xpos,ypos,GetActiveColor);
  	      bilde := true;
      END;
      IF KeyEntered THEN ClearKb;
   UNTIL k = 283;
END;
PROCEDURE Draw;
VAR
   s : String[2];
BEGIN
   Graphmode;
   DefaultPalette;
   Tegnpalett;
   IF NOT bilde THEN Cursor(acticolx,acticoly,true);
   TegnRuter;
   TegnThumb;
   SetPalette;
   FadeReady;
   DrawImage;
   FadeIn(10);
   Drawmode;
   FadeOut(10);
   Cls;
   Writeln('Image   : ',bilde);
   Writeln('Palette : ',palette);
   Writeln;
END;
PROCEDURE Help;
VAR w : Word;
BEGIN
   GraphMode;
   HelpMode;
   cls;
END;
PROCEDURE new(cmds : String);
VAR
   i : Byte;
   w : Word;
   svar,s : String;
BEGIN
   Writeln;
   FOR i := 4 TO Length(cmds) DO s[i-3] := cmds[i];
   s[0] := Chr((Length(cmds)-3));
   IF s[1] = ' ' THEN BEGIN
      FOR i := 1 TO (Length(s)-1) DO s[i] := s[i+1];
      Dec(s[0]);
   END;
   IF s = '' THEN i := 0;
   IF (bilde OR palette) THEN BEGIN
      Write('Are you sure you want to clear out your current image and palette? ');
      Readln(svar);
   END ELSE svar := '�';
   IF (UpCase(svar[1]) = 'Y') OR (svar = '�') THEN BEGIN
      Val(s,i,w);
      bc := i;
      bilde := false;
      palette := false;
      xpos := initpx;
      ypos := initpy;
      acticolx := initcx;
      acticoly := initcy;
      TegnRuter;
      IF svar = '�' THEN
         Writeln('The image and palette is already cleared out.')
      ELSE BEGIN
         Writeln;
         Writeln('The image and the palette was cleared out.');
         Writeln('I''m ready for your endless scribbeling.');
      END;
   END ELSE BEGIN
      Writeln;
      Writeln('No changes done.');
   END;
   Writeln;
END;
PROCEDURE Info;
BEGIN
   Writeln;
   Writeln(vstring);
   LinjeFarge(GetCursorY-1,8,9);
   Writeln;
  Writeln(' ',GetVer,GetVerS);
   Writeln;
END;
PROCEDURE DoForward(cs : String; b : Boolean);
VAR
   l : Word;
   c : Word;
   s : String;
BEGIN
   IF cs = 'forward' THEN
      l := 1
   ELSE BEGIN
      s := Right(cs,Length(cs)-8);
      Val(s,l,c);
   END;
   IF (l < 1) or (l > 255) THEN BEGIN
      Writeln;
      Writeln('Invalid parameter. Please use a number between 1 and 255.');
      Writeln;
   END ELSE
      Forward(l,b);
END;
PROCEDURE DoBackward(cs : String; b : Boolean);
VAR
   l : Word;
   c : Word;
   s : String;
BEGIN
   IF cs = 'backward' THEN
      l := 1
   ELSE BEGIN
      s := Right(cs,Length(cs)-9);
      Val(s,l,c);
   END;
   IF (l < 1) or (l > 255) THEN BEGIN
      Writeln;
      Writeln('Invalid parameter. Please use a number between 1 and 255.');
      Writeln;
   END ELSE
      Backward(l,b);
END;
PROCEDURE ShowKey;
VAR k : Word;
BEGIN
   Writeln;
   GetK(Addr(k));
   Writeln('Dec: ',k);
   Writeln('Hex: ',h(k));
   Writeln;
END;
PROCEDURE DefPal;
VAR
   svar : String;
BEGIN
   Writeln;
   IF palette THEN BEGIN
     Write('Are you sure you want to reset your current palette? ');
     Readln(svar);
     IF (UpCase(svar[1]) = 'Y') THEN BEGIN
         palette := false;
        DefaultPalette;
        Writeln;
        Writeln('Default palette is now loaded.');
     END ELSE BEGIN
        Writeln;
        Writeln('No changes done.');
     END;
   END ELSE BEGIN
    Writeln('Your palette is already default.');
      Writeln('No changes done.');
   END;
   Writeln;
END;
PROCEDURE MonoPal;
VAR
   svar : String;
BEGIN
   Writeln;
   Write('Are you sure you want to replace your palette with a monochrome one? ');
   Readln(svar);
   IF (UpCase(svar[1]) = 'Y') THEN BEGIN
      MonoPalette;
      palette := true;
      Writeln;
      Writeln('Greyscale palette is now loaded.');
   END ELSE BEGIN
      Writeln;
      Writeln('No changes done.');
   END;
   Writeln;
END;
PROCEDURE SetColorCmd(cs : String);
VAR
   s : String;
   w, c : Word;
BEGIN
   Writeln;
   s := GetParam(Addr(cs));
   IF s = '' THEN
      Writeln('Syntax: color [colornumber]')
   ELSE BEGIN
      Val(s,w,c);
      IF (w > 255) THEN
         Writeln('Out of range.')
      ELSE
         SetColor(w,true);
   END;
   Writeln;
END;
PROCEDURE ClearImage;
VAR
   x,y  : Byte;
   svar : String;
BEGIN
   Writeln;
   IF bilde THEN BEGIN
     Write('Are you sure you want to clear your current image? ');
     Readln(svar);
     IF (UpCase(svar[1]) = 'Y') THEN BEGIN
     			FOR x := 0 TO 31 DO BEGIN
        			FOR y := 0 TO 31 DO BEGIN
           			P(x,y,bc);
        			END;
     			END;
               acticolx := initcx;
               acticoly := initcy;
               xpos := initpx;
               ypos := initpy;
               bilde := false;
               Writeln(#13,#10,'Image cleared.');
     END ELSE Writeln(#13,#10,'No changes done.');
   END ELSE Writeln('There was no image to clear.');
  Writeln;
END;
PROCEDURE DoSet(cmd : String);
VAR
   p1,p2,p3,p4 : String;
   nr,r,g,b : Word;
   c : Integer;
BEGIN
   Writeln;
   p1 := GetParam(Addr(cmd));
   p2 := GetParam(Addr(p1));
   p3 := GetParam(Addr(p2));
   p4 := GetParam(Addr(p3));
   p1[0] := Chr(Length(p1) - Length(p2) - 1);
   p2[0] := Chr(Length(p2) - Length(p3) - 1);
   p3[0] := Chr(Length(p3) - Length(p4) - 1);
   IF ((p1='') or (p2='')) or ((p3='') or (p4='')) THEN BEGIN
      Writeln('Syntax: set [colornumber][r][g][b]');
   END ELSE BEGIN
      Val(p1,nr,c);
      Val(p2,r,c);
      Val(p3,g,c);
      Val(p4,b,c);
      IF ((nr > 255) OR (r > 255)) OR ((g > 255) OR (b > 255)) THEN
         Writeln('Out of range.')
      ELSE BEGIN
         IF NOT bilde THEN BEGIN
        Graphmode;
            IF NOT palette THEN DefaultPalette;
            IF NOT palette THEN SetPalette;
            Tegnpalett;
            Cursor(acticolx,acticoly,true);
            TegnRuter;
            TegnThumb;
         END;
         ThePal[nr*3+0] := r;
         ThePal[nr*3+1] := g;
         ThePal[nr*3+2] := b;
         IF NOT bilde THEN CLS;
         palette := true;
         Writeln('Color number ',nr,' has been sat to R:',r,' G:',g,' B:',b,'.');
      END;
   END;
   Writeln;
END;
PROCEDURE Tile;
VAR
   superx,supery,x,y : Byte;
BEGIN
   Writeln;
   IF bilde THEN BEGIN
      Graphmode;
      SetPalette;
      FadeReady;
      FOR y := 0 TO 31 DO BEGIN
         FOR x := 0 TO 31 DO BEGIN
            FOR superx := 0 TO 9 DO BEGIN
               FOR supery := 0 TO 5 DO BEGIN
                  Mem[$a000:1280+(y+(supery*32))*320+(x+(superx*32))] := G(x,y);
               END;
            END;
         END;
      END;
      FadeIn(10);
      WFK;
      FadeOut(10);
      CLS;
   END ELSE Writeln('You must load an image before you can tile it.');
   Writeln;
END;
PROCEDURE center;
VAR
   superx,supery,x,y : Byte;
BEGIN
   Writeln;
   IF bilde THEN BEGIN
      Graphmode;
      SetPalette;
      FadeReady;
      superx := (320 - 32) div 2;
      supery := (200 - 32) div 2;
      FOR y := 0 TO 31 DO BEGIN
         FOR x := 0 TO 31 DO BEGIN
            Mem[$a000:1280+(y+supery)*320+(x+superx)] := G(x,y);
         END;
      END;
      FadeIn(10);
      WFK;
      FadeOut(10);
      CLS;
   END ELSE Writeln('You must load an image before you can show it.');
   Writeln;
END;
PROCEDURE Shittytxt;
VAR
   x,y : Byte;
BEGIN
   Writeln;
   IF bilde THEN BEGIN
      Write('Fast palette-based blurring in progress... ');
      Bilde2MoreMem;
      Shittyblurall(0);
      Writeln('100%');
      Writeln;
      Writeln('Done. Your image has been blurred.');
   END ELSE Writeln('You must load an image before you can blur it.');
   Writeln;
END;
PROCEDURE Shittytxt2;
VAR
   x,y : Byte;
BEGIN
   Writeln;
   IF bilde THEN BEGIN
      Write('Fast palette-based blurring in progress... ');
      Bilde2MoreMem;
      Shittyblurall(1);
      Writeln('100%');
      Writeln;
      Writeln('Done. Your image has been blurred.');
   END ELSE Writeln('You must load an image before you can blur it.');
   Writeln;
END;
PROCEDURE NewAnim(cms : String);
VAR
  s : String[12];
BEGIN
   Writeln;
   s := GetParam(Addr(cms));
   Repeat
     IF (s = '') OR (Ord(s[0]) > 5) THEN BEGIN
        Repeat
           Write('Please give your new animation a name (max 5 letters): ');
           Readln(s);
        UNTIL (Ord(s[0]) < 6);
         IF s = '' THEN BEGIN
            Writeln;
            Writeln('No name - no animation.');
            Writeln;
            exit;
         END;
     END;
     framebase := s+'-'+'00';
      s := framebase + '.BIM';
     IF finnes(addr(s)) THEN BEGIN
         Writeln;
        Writeln('An animation with that name already exist.');
         Writeln;
        s := '';
     END;
  UNTIL (s <> '');
   command('new');
   IF NOT (Pos('script',s) > 0) THEN BEGIN
   	command('draw');
     Writeln('Type "saveframe" to add an image to the animation.');
   END;
   Writeln;
END;
PROCEDURE JmpFrame;
VAR
   s,s2 : String[12];
   animfind : SearchRec;
   max : Byte;
   v : Byte;
  c : Word;
   fb : String[12];
BEGIN
   Writeln;
   IF NOT (framebase = 'int19h') THEN BEGIN
      fb := framebase;
   	Writeln('Animation name   : ',Copy(framebase,1,Ord(framebase[0])-3));
     Dec(framebase[0],3);
     max := 0;
     FindFirst(framebase+'???.BIM', AnyFile, animfind);
     WHILE DosError = 0 DO BEGIN
        s := animfind.Name;
    	   s := copy(s,Ord(s[0])-5,2);
       	Val(s,v,c);
        IF (v > max) THEN max := v;
    	   FindNext(animfind);
     END;
     IF (max = 0) AND (s <> '00') THEN BEGIN
         Writeln('Frames found     : 0');
         Writeln;
         Writeln('Unable to jump...');
         Writeln('support for nonexistant frames not implemented yet. ;)');
         Writeln;
         framebase := fb;
       exit;
     END;
      IF max = 0 THEN BEGIN
         Writeln('Can''t jump - there''s only one image available.');
         Writeln;
         framebase := fb;
         exit;
      END;
     Writeln('Number of frames : ',max+1);
     Writeln;
     IF bilde THEN BEGIN
        Writeln('Jumping to another frame involves overwriting the current image.');
        command('new');
     END ELSE Writeln;
           IF bilde THEN BEGIn
              framebase := fb;
              exit;
           END;
     Repeat
              Writeln;
        Write('Choose a frame (0..',max,') : ');
        Readln(s2);
              IF s2 = '' then s2 := '0';
        Val(s2,v,c);
           UNTIL (Ord(s2[0]) < 3) AND (v <= max);
     IF s2[0] = #1 THEN s2 := '0'+s2;
     s := framebase + '-';
     s := s + s2;
     framebase := s;
     command('load '+s+'.bim');
     Str(v,s2);
     IF (v <= 99) AND (v >= 0) THEN BEGIN
        Writeln;
        Writeln('Current frame : ',s2);
        Writeln;
     END ELSE BEGIN
        Writeln;
        Writeln('Invalid frame-number.');
        Writeln;
     END;
   END ELSE BEGIN
     Writeln('You must load an animation with "loadanim" or create a new');
    Writeln('one with "newanim" before you can jump to a specific frame.');
      Writeln;
   END;
   Writeln;
END;
PROCEDURE SaveFrame;
VAR
  s : String[2];
   v : Byte;
   c : Word;
   fn : String[12];
BEGIN
   Writeln;
   IF NOT (framebase = 'int19h') THEN BEGIN
    	Writeln('Animation name   : ',Copy(framebase,1,Ord(framebase[0])-3));
      s := Copy(framebase,Ord(framebase[0])-1,2);
      Val(s,v,c);
     Writeln('Frame number     : ',v,' (counting from 0)');
      fn := framebase+'.bim';
      Writeln('Filename         : ',framebase,'.bim');
      IF finnes(Addr(fn)) THEN BEGIN
         command('save image '+framebase+'.bim');
         Writeln('An image with that filename already exist.');
         exit;
      END ELSE command('save image '+framebase+'.bim');
      s := copy(framebase,Length(framebase)-1,2);
      delete(framebase,Length(framebase)-1,2);
      Val(s,v,c);
      Inc(v);
      IF v < 100 THEN BEGIN
         Str(v,s);
         IF s[0] = #1 THEN s := '0'+s;
         framebase := framebase + s;
      END ELSE BEGIN
         CLS;
         framebase := framebase + '99';
   		Writeln('Last frame reached! (99)');
         Writeln;
         Exit;
      END;
   END ELSE BEGIN
     Writeln('You must load an animation with "loadanim" or create a new');
    Writeln('one with "newanim" before you can start adding frames with');
      Writeln('"saveframe".');
      Writeln;
   END;
END;
PROCEDURE SaveFrameAs;
VAR
  s : String[2];
   v : Byte;
   c : Word;
   fn : String[12];
BEGIN
   Writeln;
   Repeat
      Write('Please enter the desired framebase: ');
      Readln(framebase);
   UNTIL Ord(framebase[0]) < 8;
   Writeln;
  	Writeln('Animation name   : ',Copy(framebase,1,Ord(framebase[0])-3));
   s := Copy(framebase,Ord(framebase[0])-1,2);
   Val(s,v,c);
   Writeln('Frame number     : ',v,' (counting from 0)');
   fn := framebase+'.bim';
   Writeln('Filename         : ',framebase,'.bim');
   IF finnes(Addr(fn)) THEN BEGIN
      command('save image '+framebase+'.bim');
      Writeln('An image with that filename already exist.');
      exit;
   END ELSE command('save image '+framebase+'.bim');
   s := copy(framebase,Length(framebase)-1,2);
   delete(framebase,Length(framebase)-1,2);
   Val(s,v,c);
   Inc(v);
   IF v < 100 THEN BEGIN
      Str(v,s);
      IF s[0] = #1 THEN s := '0'+s;
      framebase := framebase + s;
   END ELSE BEGIN
      CLS;
      framebase := framebase + '99';
    Writeln('Last frame reached! (99)');
      Writeln;
      Exit;
   END;
END;
PROCEDURE ShowAnim;
VAR
   superx,supery,x,y : Byte;
   s : String[12];
   fb : String[8];
   animfind : SearchRec;
   framenr : Byte;
   max : Byte;
   v : Byte;
   c : Word;
   f : File;
   vent : Word;
BEGIN
   Writeln;
   IF NOT (framebase = 'int19h') THEN BEGIN
      fb := framebase;
     framebase := Copy(framebase,1,Ord(framebase[0])-3);
    Writeln('Animation name : ',framebase);
     max := 0;
    FindFirst(framebase+'???.BIM', AnyFile, animfind);
  	   WHILE DosError = 0 DO BEGIN
    	   s := animfind.Name;
        s := copy(s,Ord(s[0])-5,2);
        Val(s,v,c);
        IF (v > max) THEN max := v;
        FindNext(animfind);
     END;
     IF (max = 0) AND (s <> '00') THEN BEGIN
        Writeln('Frames found   : 0');
         Writeln;
         Writeln('I couldn''t find your animation.');
         Writeln('Make sure that all frames in the animation has');
         Writeln('the extension ".bim".');
        Writeln;
         framebase := fb;
       exit;
     END;
     Writeln('Frames found   : ',max+1);
     Writeln;
     Writeln('Notice that if you''ve used several different palettes during');
     Writeln('the making of this animation, it might look... uhm... a bit strange.');
     Writeln;
     Repeat
              Writeln('Press ENTER to skip...');
              Write('Delay between frames (0-10000 ms): ');
              Readln(s);
              Val(s,vent,c);
              IF s = '' THEN vent := 41;
           UNTIL ((vent >= 0) AND (vent < 10001));
     Graphmode;
     SetPalette;
     Repeat
        FOR v := 0 TO max DO BEGIN
           Str(v,s);
           IF s[0] = #1 THEN s := '0'+s;
            {$I-}
           Assign(f,framebase+'-'+s+'.bim');
           Reset(f,1);
            {$I+}
            IF IOResult <> 0 THEN BEGIN
               Cls;
          Writeln('Error: "',framebase,'-',s,'.bim" was not found.');
               Writeln('Unable to show the animation.');
               Writeln;
             framebase := fb;
           exit;
            END;
            asm {antiflicker}
            mov dx,3dah
            @l1:
            in al,dx
            and al,8
            jnz @l1
            @l2:
            in al,dx
            and al,8
            jz @l2
            end;
           FOR y := 0 TO 31 DO FOR x := 0 TO 31 DO BEGIN
               BlockRead(f,MemL[$a000:(y shl 8)+(y shl 6)+x+28304],4);
               Inc(x,3);
           END;
           Close(f);
           IF KeyEntered THEN break;
 			   WaitPeriod(vent);
        END;
     UNTIL KeyEntered;
      framebase := fb;
     CLS;
      IF vent > 0 THEN
         Writeln('Calculated FPS: ',1000 div vent)
      ELSE
         Writeln('Calculated FPS: N/A');
      WFK;
   END ELSE BEGIN
     Writeln('You must load an animation with "loadanim" or create a new');
    Writeln('one with "newanim" before you can view it with "showanim".');
   END;
   Writeln;
END;
PROCEDURE SaveAnimScript(cms : String);
VAR
   s : String;
   t : Text;
BEGIN
   Writeln;
   IF NOT (framebase = 'int19h') THEN BEGIN
      Writeln('I''m now going to create a nice script for you that');
      Writeln('shows your animation when executed.');
      Writeln;
      s := Copy(framebase,1,Ord(framebase[0])-3);
      Writeln('I will name the script ',s,'.bsc.');
      IF finnes(addr(s)) THEN BEGIN
         Writeln;
       Writeln('Ooups... that file already exists.');
         s := Chr(Random(200)+32);
         s := s + Chr(Random(200)+32) + Chr(Random(200)+32);
         s := s + Chr(Random(200)+32) + Chr(Random(200)+32);
      Writeln('I guess you won''t let me name the script ',s,'.bsc, so please');
         Write('type in a sensible, max 5-letter long, name: ');
         Readln(s);
         s := supertrim(s);
      END;
      IF Ord(s[0]) > 5 THEN BEGIN
         Writeln;
       Writeln('Name to long!');
         Exit;
      END;
      IF NOT (right(s,4) = '.bsc') THEN s := s + '.bsc';
      IF finnes(addr(s)) THEN BEGIN
         Writeln;
       Writeln('File already exist!');
         Writeln;
         Exit;
      END;
      Assign(t,s);
      s := Copy(framebase,1,Ord(framebase[0])-3);
      Rewrite(t);
      s := s + '.bpl';
      IF palette THEN save('save pal '+s);
      Writeln(t,'main');
      IF finnes(addr(s)) THEN Writeln(t,'load ',s);
      Dec(s[0],4);
      Writeln(t,'loadanim ',s);
      Writeln(t,'cls');
      Writeln(t,'showanim');
      Writeln(t,'cls');
      Writeln(t,'echo Have a nice day! :)');
      Close(t);
   END ELSE BEGIN
     Writeln('You must load an animation with "loadanim" or create a new');
    Writeln('one with "newanim" before I can create a nice anim-script.');
   END;
   Writeln;
END;
PROCEDURE LoadAnim(cms : String);
VAR
  s : String[12];
BEGIN
   Writeln;
   s := GetParam(Addr(cms));
   Repeat
     IF (s = '') OR (Ord(s[0]) > 5) THEN BEGIN
        Repeat
            IF bilde THEN BEGIN
           Writeln('Note that loading an animation will clear out your current image.');
               Writeln;
            END;
           Write('Type in the name of the animation you want to load: ');
           Readln(s);
        UNTIL (Ord(s[0]) < 6);
         IF s = '' THEN BEGIN
            Writeln;
            Writeln('You must type in a name in order to load an animaton.');
            Writeln;
            exit;
         END;
     END;
     framebase := s+'-'+'00';
      s := framebase + '.BIM';
     IF NOT finnes(addr(s)) THEN BEGIN
         Writeln;
        Writeln('Couldn''t find an animation with that name.');
         Writeln;
        s := '';
     END;
  UNTIL (s <> '');
   bilde := false;
   command('jmpframe');
END;
PROCEDURE spin(cms : String);
VAR
   s : String;
   c : Integer;
   w : Word;
BEGIN
   Writeln;
   s := GetParam(Addr(cms));
   IF s = '' THEN
      Writeln('Syntax: distort [force]')
   ELSE BEGIN
      Val(s,w,c);
      IF (w > 255) THEN
         Writeln('Out of range.')
      ELSE IF bilde THEN BEGIN
         distortall(w);
         Writeln('Done. Your image has been distorted with a force of ',w,'.');
      END ELSE Writeln('You must load an image before you can distort it.');
   END;
   Writeln;
END;
PROCEDURE Blurtxt;
VAR
   x,y : Byte;
   quality : Boolean;
   curx, cury : Byte;
   savepal : Array [0..767] of Byte;
BEGIN
   Writeln;
   IF bilde THEN BEGIN
      Write('Truecolor blur with only 256 colors in progress... ');
      Bilde2MoreMem;
      Move(thepal,savepal,768);
      asm
         xor bx,bx
         mov ah,03h
         int 10h
         mov cury,dh
         mov curx,dl
      end;
      FOR y := 0 TO 31 DO BEGIN
         Write(y*3,'%');
         FOR x := 0 TO 31 DO BEGIN
            quality := Blur(x,y);
            IF (quality = false) OR KeyEntered THEN BEGIN
               Writeln;
               MoreMem2Bilde;
               Move(savepal,thepal,768);
           IF quality THEN BEGIN
                  asm
                     xor ah,ah
                     int 16h
                  end;
             Writeln('Key pressed.')
          END ELSE
             Writeln('Not enough free colors!');
               Writeln;
               Exit;
            END;
         END;
         asm
            xor bx,bx
            mov ah,02h
            mov dh,cury
            mov dl,curx
            int 10h
         end;
      END;
      palette := true;
      Writeln('100%');
      Writeln;
      Writeln('Done. Your image has been blurred.');
   END ELSE Writeln('You must load an image before you can blur it.');
   Writeln;
END;
PROCEDURE DefTurtle;
BEGIN
   Writeln;
   heading := 0;
   xpos := initpx;
   ypos := initpy;
   Writeln('Turtle heading    : 0');
   Writeln('Turtle x position : ',xpos);
   Writeln('Turtle y position : ',ypos);
   Writeln;
END;
PROCEDURE Framename;
BEGIN
   Writeln;
   IF framebase = 'int19h' THEN BEGIN
      Writeln('No animation loaded.');
      Writeln;
      Writeln('Type "newanim" or "loadanim" to make or load');
      Writeln('an animation.');
   END ELSE
      Writeln('This frame''s name is ',framebase,'.');
   Writeln;
END;
PROCEDURE NormPal;
VAR
   svar : String;
BEGIN
   Writeln;
     Write('Are you sure you want to load the default VGA palette? ');
     Readln(svar);
     IF (UpCase(svar[1]) = 'Y') THEN BEGIN
       Graphmode;
       GetPalette;
       palette := true;
       CLS;
        Writeln;
        Writeln('Default VGA palette is now loaded.');
     END ELSE BEGIN
        Writeln;
        Writeln('No changes done.');
     END;
   Writeln;
END;
PROCEDURE num(s : String);
VAR
   w,c : Word;
   t : String;
BEGIN
   Writeln;
   s := GetParam(Addr(s));
   IF s = '' THEN BEGIN
      Writeln('Syntax: num (a decimal or hexadecimal number)');
      Writeln;
      Exit;
   END;
   s := ntrim(s);
   Val(s,w,c);
   IF (w > $ffff) OR (Ord(s[0]) > 5) THEN BEGIN
      Writeln('Out of range');
      Writeln;
      Exit;
   END;
   Writeln('dec -> hex : ',h(w),'h');
   Writeln('hex -> dec : ',d(s));
   Writeln;
END;
FUNCTION command(cmd : String) : Boolean;
VAR
  s : String;
BEGIN
   command := true;
   IF Ord(cmd[0]) = 0 then exit;
   IF cmd = 'draw' then begin draw; exit; end;
   IF cmd = 'cls' then begin txtcls; exit; end;
   IF cmd = 'tile' then begin tile; exit; end;
   IF cmd = 'show' then begin center; exit; end;
   IF cmd = 'help' then begin help; exit; end;
   IF cmd = 'readme' then begin save('save readme screen'); exit; end;
   IF cmd = 'ascii' then begin save('save ascii screen'); exit; end;
   IF cmd = 'info' then begin save('save info screen'); exit; end;
   IF cmd = 'showkey' then begin showkey; exit; end;
   IF cmd = 'ver' then begin info; exit; end;
   IF cmd = 'defpal' then begin defpal; exit; end;
   IF cmd = 'monopal' then begin monopal; exit; end;
   IF cmd = 'vgapal' then begin normpal; exit; end;
   IF cmd = 'defturtle' then begin defturtle; exit; end;
   IF cmd = 'framename' then begin framename; exit; end;
   IF cmd = 'saveframe' then begin saveframe; exit; end;
   IF cmd = 'saveas' then begin saveframeas; exit; end;
   IF cmd = 'jmpframe' then begin jmpframe; exit; end;
   IF cmd = 'showanim' then begin showanim; exit; end;
   IF (pos('distort ',cmd)=1) or (cmd='distort') then begin spin(cmd); exit; end;
   IF (pos('newanim ',cmd)=1) or (cmd='newanim') then begin newanim(cmd); exit; end;
   IF (pos('loadanim ',cmd)=1) or (cmd='loadanim') then begin loadanim(cmd); exit; end;
   IF cmd='makeanimscript' then begin saveanimscript(cmd); exit; end;
   IF (pos('num ',cmd)=1) or (cmd='num') then begin num(cmd); exit; end;
   IF ((cmd = 'quit') or (cmd = 'exit')) or ((cmd = 'bye') or (cmd = 'logout')) then begin command := false; exit; end;
   IF cmd[2] = ':' then begin cdrv(cmd); exit; end;
   IF (pos('new ',cmd)=1) or (cmd='new') then begin new(cmd); exit; end;
   IF (pos('cd ',cmd)=1) or (cmd='cd') then begin cd(cmd); exit; end;
   IF (pos('cd',cmd)=1) then begin cd('cd '+Right(cmd,ord(cmd[0])-2)); exit; end;
   IF cmd = 'nextcolor' then begin NextColor(true); exit; end;
   IF cmd = 'prevcolor' then begin PrevColor(true); exit; end;
   IF cmd = 'turnleft' then begin TurnLeft(true); exit; end;
   IF cmd = 'turnright' then begin TurnRight(true); exit; end;
   IF cmd = 'clear' then begin ClearImage; exit; end;
   IF cmd = 'plot' then begin DrawPlot; exit; end;
   IF cmd = 'fastblur' then begin shittytxt; exit; end;
   IF cmd = 'fastdist' then begin shittytxt2; exit; end;
   IF cmd = 'blur' then begin blurtxt; exit; end;
   IF cmd = 'flip' then begin flip(false); exit; end;
   IF cmd = 'flop' then begin flop(false); exit; end;
   IF cmd = 'floipp' then begin RotateRight(true); exit; end;
   IF cmd = 'wfk' then begin wfk; exit; end;
   IF cmd = 'noanim' then begin framebase := 'int19h'; Writeln(chr(10),'Done.',chr(10)); exit; end;
   IF cmd = 'k' then begin dir('dir *.bsc'); exit; end;
   IF cmd = 'halt' then begin asm mov ax,0003h; int 10h; end; halt(0); end;
   IF (pos('heading ',cmd)=1) or (cmd='heading') then begin setvar(cmd,Addr(heading),'HEADING',true,1,360); exit; end;
   IF (pos('n ',cmd)=1) or (cmd='n') then begin setvar(cmd,Addr(n),'N',false,0,0); exit; end;
   IF (pos('xpos ',cmd)=1) or (cmd='xpos') then begin setvar(cmd,Addr(xpos),'XPOS',true,0,31); exit; end;
   IF (pos('ypos ',cmd)=1) or (cmd='ypos') then begin setvar(cmd,Addr(ypos),'YPOS',true,0,31); exit; end;
   IF (pos('bc ',cmd)=1) or (cmd='bc') then begin setvar(cmd,Addr(bc),'BC',true,0,255); exit; end;
   IF (pos('color ',cmd)=1) or (cmd='color') then begin SetColorCmd(cmd); exit; end;
   IF (pos('set ',cmd)=1) or (cmd='set') then begin doset(cmd); exit; end;
   IF (pos('for ',cmd)=1) or (cmd='for') then begin mfor(cmd); exit; end;
   IF (pos('fxy ',cmd)=1) or (cmd='fxy') then begin xyfor(cmd); exit; end;
   IF (pos('forward ',cmd)=1) or (cmd='forward') then begin doForward(cmd,true); exit; end;
   IF (pos('backward ',cmd)=1) or (cmd='backward') then begin doBackward(cmd,true); exit; end;
   IF (pos('dir ',cmd)=1) or (cmd='dir') then begin dir(cmd); exit; end;
   IF (pos('ls ',cmd)=1) or (cmd='ls') then begin dir(cmd); exit; end;
   IF (pos('type ',cmd)=1) or (cmd='type') then begin typefile(cmd); exit; end;
   IF (pos('cat ',cmd)=1) or (cmd='cat') then begin typefile(cmd); exit; end;
   IF (pos('del ',cmd)=1) or (cmd='del') then begin remove(cmd); exit; end;
   IF (pos('echo ',cmd)=1) or (cmd='echo') then begin echo(cmd); exit; end;
   IF (pos('save ',cmd)=1) or (cmd='save') then begin save(cmd); exit; end;
   IF (pos('load ',cmd)=1) or (cmd='load') then begin load(cmd); exit; end;
   IF (pos('rem',cmd)=1) then exit;
   s := cmd + '.BSC';
   IF finnes(addr(s)) THEN load('load '+s) ELSE randomerror(cmd);
   command := true;
END;
PROCEDURE xyfor;
VAR
   p1 : String;
   prevail : Boolean;
   c : Integer;
BEGIN
   Writeln;
   p1 := GetParam(Addr(cmds));
   IF (p1='')THEN BEGIN
      Writeln('Syntax: fxy [command]');
   END ELSE BEGIN
      FOR xpos := 0 TO 31 DO BEGIN
      FOR ypos := 0 TO 31 DO BEGIN
       prevail := command(p1);
      END;
      END;
   END;
   Writeln;
END;
PROCEDURE mfor;
VAR
   p1,p2,p3,p4 : String;
   prevail : Boolean;
   v1,v2,v3 : Integer;
   c : Integer;
BEGIN
   Writeln;
   p1 := GetParam(Addr(cmds));
   p2 := GetParam(Addr(p1));
   p3 := GetParam(Addr(p2));
   p4 := GetParam(Addr(p3));
   p1[0] := Chr(Length(p1) - Length(p2) - 1);
   p2[0] := Chr(Length(p2) - Length(p3) - 1);
   p3[0] := Chr(Length(p3) - Length(p4) - 1);
   IF (p1='') OR (p2='') OR (p3='') THEN BEGIN
      Writeln('Syntax: for [startvalue][endvalue](step)[command]');
   END ELSE BEGIN
      Val(p1,v1,c);
      Val(p2,v2,c);
      Val(p3,v3,c);
      FOR n := v1 TO v2 DO BEGIN
         Inc(n,(v3-1));
         IF ((v3-1)>0) and (n>v2) THEN BEGIN Writeln; Exit; END;
         IF ((v3-1)<0) and (n<v2) THEN BEGIN Writeln; Exit; END;
       prevail := command(p4);
      END;
   END;
   Writeln;
END;
PROCEDURE Console;
VAR
   cmd : String;
   b : Byte;
LABEL slutt,again;
BEGIN
   { burde skrive mine egne taste-registrerings-rutiner }
   cls;
   IF build = 0 THEN
     Writeln(vstring,' - missing datafile version (burn.dat)')
   ELSE
      Writeln(vstring,GetVer,GetVerS,prov);
   LinjeFarge(GetCursorY-1,8,9);
   Writeln;
   Writeln('Press space to enter drawmode.');
   LinjeFarge(GetCursorY-1,11,15);
   LinjeFarge(GetCursorY-1,6,7);
   Writeln('Press enter (or any key) to use the console.');
   LinjeFarge(GetCursorY-1,11,15);
   LinjeFarge(GetCursorY-1,6,7);
   cmd := 'autoexec.bsc';
   IF finnes(addr(cmd)) THEN command('load autoexec.bsc');
   asm
      xor ah,ah
      int 16h
      mov b,al
   end;
   IF b = 32 THEN command('draw');
   {IF b = ord('h') THEN command('help');}
   Writeln('Type help to get some.');
   LinjeFarge(GetCursorY-1,9,15);
   LinjeFarge(GetCursorY-1,5,7);
   Writeln;
   Write(prompt);
   { Her kan man sette tids-intensive load-ting som ikke tar FOR lang tid }
again:
   KillCaps;
   Readln(cmd);
   IF command(cmd) = false then goto slutt;
   Write(prompt);
   goto again;
slutt:
   Writeln;
   IF palette OR bilde THEN BEGIN
      Write('Are you sure you want to quit Burn? ');
      Readln(cmd);
      IF UpCase(cmd[1]) <> 'Y' THEN BEGIN Writeln; Write(prompt); goto again; END;
   END;
END;
PROCEDURE Splash;
VAR
   fn : String;
   b,b2 : Byte;
   w : Word;
   f : File;
   i : Integer;
BEGIN
   fn := 'burn.dat';
   IF finnes(addr(fn)) THEN BEGIN
      GraphMode;
      Assign(f,fn);
      Reset(f,1);
      BlockRead(f,build,2);
      w := 0;
      Repeat
         BlockRead(f,b,1);
         IF (b AND 192)=192 THEN BEGIN
            Dec(b,192);
            BlockRead(f,b2,1);
            FillChar(theimage[w],b,b2);
            Inc(w,b);
         END ELSE BEGIN
            theimage[w] := b;
            inc(w);
         END;
      UNTIL w = 52800; {165*320+1}
      Seek(f,FilePos(f)+1);
      BlockRead(f,thepal,768);
      FOR w := 0 TO 768 DO thepal[w] := thepal[w] shr 2;
      Close(f);
      ClearKb;
      nofade := false;
      FadeReady;
      DrawImage;
      FadeIn(10);
      WhiteFadeReady;
      w := 0;
      Repeat
         Inc(w);
         IF NOT KeyEntered THEN WaitPeriod(10);
      UNTIL (w = 2000);
      FadeIn(6);
      FadeOut(6);
      ClearKb;
      Cls;
   END;
END;
BEGIN
   n := 0;
   build := 0;
   heading := 0;
   cursorx := 0;
   cursory := 0;
   acticolx := initcx;
   acticoly := initcy;
   xpos := initpx;
   ypos := initpy;
   framebase := 'int19h';
   bc := 0;
   lasterror := 10;
   bilde := false;
   palette := false;
   cbreak := false;
   {checkbreak := false;} { crt constant }
   Randomize;
   nofade := false;
   Splash;
   nofade := true;
   Console;
   Textmode;
   ClearKb;
END.
