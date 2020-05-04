unit uMainFormScene01;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZVectorMath, BZMath, BZColors, BZGraphic, BZBitmap,
  BZCadencer, BZStopWatch, BZParallelThread, BZAnimationTool, BZScreenMode,
  BZBitmapFont, {%H-}BZBitmapIO,
  BZSound, BZOpenALManager, BZSoundSample, BZSoundFileModplug;

Const
  cMaxLogoFadeStep : Byte = 128;
  cMaxStars : Integer = 499;
  cStarFieldZFactor : Integer = 1;
  cAStep = 1;
  cDivs = 4800;
  cConeRadiusTop : Single = 48;

type

  TStarRec = record
    x,y,z : Integer;
  end;

  TStarFieldMode = (sfmVertical, sfmHorizontal, sfm3D, sfmWormHole, sfmWormHoleMoving, sfmPlane, sfmPlaneUp,sfmPerspectivePlane, sfmDoublePlane, sfmSinusPlane, sfmDoubleSinusPlane, sfmPlaneDown);

  TTunnelRec = packed record
    a,l : Single;
  end;

  TParams = Array[0..0] of Double;
  { TMainForm }

  TMainForm = class(TForm)
    BZScreenMode : TBZScreenMode;
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormShow(Sender : TObject);
    procedure BZScreenModeAfterRestoreVideoMode(Sender : TObject);
    procedure BZScreenModeBeforeChangeVideoMode(Sender : TObject; ScreenWidth, ScreenHeight, ScreenDepth, ScreenFrequency : Integer; VideoModeIndex : TBZScreenModeResolution);
    procedure FormKeyPress(Sender : TObject; var Key : char);
  private

    FFullScreen : Boolean;     // Mode plein ecran
    FShowFPS : Boolean; // Affiche FPS
    FShowDebugInfo : Boolean; // Affiche Infos

    FCadencer : TBZCadencer;
    FStopWatch : TBZStopWatch;

    FSoundManager : TBZSoundOpenALManager;
    FSoundLibrary : TBZSoundLibrary;
    FSoundSources : TBZSoundSources;
    FSoundSource  : TBZSoundSource;

    FDisplayBuffer : TBZBitmap;         // Tampon d'affichage
    FBackBuffer    : TBZBitmap;         // Tampon secondaire
    FTempBuffer    : TBZBitmap;         // Tampon temporaire au besoin
    FWormTextureMap : TBZBitmap;  //Max 16x16
    FImg01, FImg02, FImg03, FImg04 : TBZBitmap; // Pour stocker les différentes images

    FBmpFontA, FBmpFontB : TBZBitmapFont; // Police de caractères bitmap

    FCurrentScene, FCurrentScenePart : Byte;   // Scene en cours. Partie de la scene

    FCurrentStep : Integer;
    FDeltaHeightAf,FDelta1, FDelta2, FDeltaA, FDeltaB : Single;
    FDeltaWidthA, FDeltaHeightA,
    FDeltaWidthB, FDeltaHeightB  : Integer; // Increments dimensions

    FFloatStep : Single;

    FDeltaPosA, FDeltaPosB, FDeltaPosC, FDeltaPosD : TBZVector2f;     // Increment position virgule flottante
    FiDeltaPosA, FiDeltaPosB, FiDeltaPosC, FiDeltaPosD : TBZVector2i; // Increment position

    FMasterAlpha, FDeltaAlpha, FTempAlphaA, FTempAlphaB, FTempAlphaC : Byte;    // Valeur Alpha et Increment
    FStartDelay : Boolean;
    FRect : TBZRect;
    FTempFloatRectA, FTempFloatRectB, FFloatRect, FFloatRectB, FFloatRectC, FFloatRectD : TBZFloatRect;

    FRate : Integer;
    FCurrentRate : Integer;

    // StarFields
    FStarsPos : array[0..499] of TStarRec;
    FStarsSpeed : array[0..499] of Word;
    FFrameCounter : DWord;
    FStarFieldMode : TStarFieldMode;
    FAngleB,FAngle, FBaseLine, FThresOld, FDeltaAngle, FDeltaBaseLine : Integer;
    FZoomFactor : Single;

    // WormHoles
    FSintab:array[0..449] of integer;
    FStab,FCtab:array[0..255] of integer;

    FWormStep, FWormX, FWormY, FWormXSt, FWormYSt : Integer;
    FWormColor : TBZColor;
    FWormColorDelta : Byte;

    FWormBBuffer : PByte;
    FWormBSinLUT, FWormBCosLUT, FWormBLUT : Array[0..cDivs-1] of single;

    // Planar tunnel mapping
    FAddx, FAddY, FMoveX, FMoveY : Integer;
    {$CODEALIGN RECORDMIN=16}
    FScreenResolution : TBZVector2i;
    FScreenInvResolution : TBZVector2f;
    FScreenAspectRatio : TBZVector2f;
    FTextureSize : TBZVector2f;
    FTextureMaxSize : TBZVector2i;
    FAddMove : TBZVector2i;
    FIncMove : TBZVector2i;
    //FiChannel0Resolution  : TBZVector2i;
    {$CODEALIGN RECORDMIN=4}
    FMaxLinePix : Integer;
    FTunnelParams : TParams;
    FTunnelLUT : Array[0..2047, 0..1535] of TTunnelRec;
    FMinConeSize : Integer;

    // Interference
    FCircleBufferA : TBZBitmap;
    FCircleBufferB : TBZBitmap;


    procedure InitTunnelMapping;
    procedure RasterizeTunnelMappingLineProc(Sender: TObject; Index: Integer ; Data : Pointer);
    procedure DoRenderTunnelMapping(NewTime : Double);
    //=====================================
    procedure RenderCircleInterference(DeltaTime : Double);


    procedure GenerateCircleMap(Bmp : TBZBitmap; aWidth : Byte; aColor : TBZColor);


    function ComputeCenterPos(Source : TBZBitmap; Dest : TBZBitmap) : TBZVector2i;

    procedure FadeInOut(Source : TBZBitmap; Dest : TBZBitmap; vCurrentStep, vMaxStep : Byte; DstX, DstY : Integer);
    procedure Rouleau(Src, Dst : TBZBitmap; CurY, Ep : Integer);

    procedure DoWormHolePixel;
    procedure DoWormHoleB(ToTemp:Boolean);

    procedure InitStarsField3D;
    Procedure RenderStarsField3D(Angle : Integer; BaseLine : Integer; ThresOld : Integer);

    procedure CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
  public
    procedure InitEngine;
    procedure InitScene;
    procedure InitScene_01;



    procedure DoneEngine;
    procedure DoneScene;
    procedure DoneScene_01;


    procedure RenderScene(deltaTime, newTime : Double);

    procedure RenderScene_01(deltaTime, newTime : Double);
    procedure RenderScene_02(deltaTime, newTime : Double);
    procedure RenderScene_03(deltaTime, newTime : Double);
    procedure RenderScene_04(deltaTime, newTime : Double);
    procedure RenderScene_05(deltaTime, newTime : Double);
    procedure RenderScene_06(deltaTime, newTime : Double);
    procedure RenderScene_07(deltaTime, newTime : Double);

  end;

var
  MainForm : TMainForm;

implementation

{$R *.lfm}

//Uses
//  BZLogger;
{ TMainForm }

procedure TMainForm.FormCreate(Sender : TObject);
begin
  InitEngine;
  InitScene;
end;

procedure TMainForm.FormDestroy(Sender : TObject);
begin
  DoneScene;
  DoneEngine;
end;

procedure TMainForm.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  //Screen.Cursor := crDefault;
  FSoundManager.Sources.Items[0].Playing := False;
  FSoundManager.Active := False;
  FCadencer.Enabled := False;
  FStopWatch.Stop;
  BZScreenMode.RestoreVideoMode;
  //GlobalLogger.LogNotice('Total Time = '+FStopWatch.GetValueAsTime);
  DoneScene_01;
  CanClose := True;
end;

procedure TMainForm.FormShow(Sender : TObject);
begin
  InitScene_01;

  //BZScreenMode.ChangeVideoMode;
  Screen.Cursor := crNone;
  FStopWatch.Start;
  FCadencer.Enabled := True;
end;

procedure TMainForm.BZScreenModeAfterRestoreVideoMode(Sender : TObject);
begin
  BZScreenMode.ShowMouseCursor;
end;

procedure TMainForm.BZScreenModeBeforeChangeVideoMode(Sender : TObject; ScreenWidth, ScreenHeight, ScreenDepth, ScreenFrequency : Integer; VideoModeIndex : TBZScreenModeResolution);
begin
  BZScreenMode.HideMouseCursor;
end;

procedure TMainForm.FormKeyPress(Sender : TObject; var Key : char);
begin
  if key=#27 then close;
end;

function TMainForm.ComputeCenterPos(Source : TBZBitmap; Dest : TBZBitmap) : TBZVector2i;
Var
  CenterDestPos, CenterSourcePos : TBZVector2i;
begin
  CenterSourcePos.Create(Source.CenterX + 1, Source.CenterY + 1);
  CenterDestPos.Create(Dest.CenterX + 1, Dest.CenterY + 1);
  Result := CenterDestPos - CenterSourcePos;
end;

procedure TMainForm.Rouleau(Src, Dst : TBZBitmap; CurY, Ep : Integer);
var
  yr,i, j:integer;
  Roulod: array of Word;
begin
  SetLength(Roulod,Ep);
  yr := 0;
  j := Ep div 8;
  for i:=1 to ep do
  begin
    roulod[i]:= yr + Ep;

    if (i<J) then Inc(yr,8);
    if (i>J) and (i<=J*2) then Inc(yr,6);
    if (i>J*2) and (i<=J*3) then Inc(yr,4);
    if (i>J*3) and (i<=J*4) then Inc(yr,2);
    if (i>J*4) and (i<=J*5) then Inc(yr,4);
    if (i>J*5) and (i<=J*6) then Inc(yr,6);
    if (i>J*6) and (i<=J*7) then Inc(yr,8);


  end;
  //for i:= Ep downto 1 do
  for i:= 1 to Ep do
  begin
    yr:=CurY+roulod[i];
    //if yr < Src.MaxHeight then
    //begin
      Dst.CopyHorizontalBand(Src,yr,1,CurY+(Ep-i));
    //end;
  end;
  Dst.CopyHorizontalBand(Src,CurY,1,CurY);
  SetLength(Roulod,0);
  Roulod := nil;
end;

procedure TMainForm.DoWormHolePixel;
Var
  i, j : Integer;

  procedure PutPolarPixel(xo, yo, r, a : Integer; c : TBZColor);
  var
    x,y:word;
    Radius : Byte;
  begin
    x := FDisplayBuffer.CenterX + xo +(r * FSintab[90+a]) div 54; //108;
    y := FDisplayBuffer.CenterY + yo +(r * FSintab[a]) div 64;//128;
    Radius := (FWormColorDelta div 96) + 1;
    if (Radius <= 10) and (Radius >= 1) then
    With FBackBuffer.Canvas do
    begin
       Pen.Style := ssClear;
       Brush.Color := C;
       Brush.Style := bsSolid;
       Circle(x,y,Radius);
     end;
  end;

begin

  J:=5;
  FWormColorDelta:=5;
  FWormStep:=1;
  while (j<(FBackBuffer.Height-5)) do
  begin
    FWormColor := BZColor(FWormColorDelta,FWormColorDelta,FWormColorDelta);//FPalette[ColorDelta];
    i:=0;
    while i<360 do
    begin
      PutPolarPixel(FCtab[(FWormX+(FBackBuffer.Height-j)) mod 255],FStab[(FWormY+(FBackBuffer.Height-j)) mod 255],j,i,FWormColor);
      inc(i,cAStep);
    end;
    inc(j,FWormStep);
    if (j mod 2)=0 then
    begin
       inc(FWormStep);
       inc(FWormColorDelta,10);
       //if ColorDelta>254 then ColorDelta:=8;
    end;
  end;
  FWormX:=FWormXst+FWormX mod 255;
  FWormY:=FWormYst+FWormY mod 255;
end;

procedure TMainForm.DoWormHoleB(ToTemp:Boolean);
var
  y: Integer;
  OutColor : TBZColor;
  p : PByte;
  DstLine: PBZColor;
begin

  y := 0;
  if ToTemp then DstLine := FTempBuffer.GetScanLine(0)
  else DstLine := FBackBuffer.GetScanLine(0);
  P := FWormBBuffer;
  While (y<=FBackBuffer.MaxSize) do
  begin
    OutColor := FWormTextureMap.getPixelOffset(p^);
    DstLine^ := OutColor;
    inc(p);
    inc(DstLine);
    inc(y);
  end;

  FWormTextureMap.ShiftLeft;
  FWormTextureMap.ShiftUp;
end;

procedure TMainForm.FadeInOut(Source : TBZBitmap; Dest : TBZBitmap; vCurrentStep, vMaxStep : Byte; DstX, DstY : Integer);
Var
  SrcPtr, DstPtr : PBZColor;
  x,y, DestY : Integer;
  FadeColor, BackColor : TBZColor;
begin
   DestY := DstY;
   For y := 0 to Source.MaxHeight do
   begin
     SrcPtr := Source.GetScanLine(y);
     DstPtr := Dest.GetPixelPtr(DstX, DestY);
     For x := 0 to Source.MaxWidth do
     begin
       if SrcPtr^.Alpha > 0 then
       begin
         FadeColor.Red := ClampByte((SrcPtr^.Red * vCurrentStep) div vMaxStep);
         FadeColor.Green := ClampByte((SrcPtr^.Green * vCurrentStep) div vMaxStep);
         FadeColor.Blue := ClampByte((SrcPtr^.Blue * vCurrentStep) div vMaxStep);
         FadeColor.Alpha := ClampByte((SrcPtr^.Alpha * vCurrentStep) div vMaxStep);
         BackColor := DstPtr^;
         //DstPtr^ := SrcPtr^.Blend(FadeColor, FadeColor.Alpha);
         DstPtr^:= BackColor.Blend(FadeColor, ((255*vCurrentStep) div vMaxStep));
       end;
       Inc(SrcPtr);
       Inc(DstPtr);
     end;
     Inc(DestY);
   end;
end;

procedure TMainForm.InitStarsField3D;
Var
  i : Word;
begin
  Randomize;
  For i:=0 to cMaxStars do
  begin
     Case FStarFieldMode of
       sfmVertical :;
       sfmHorizontal :;
       sfm3D, sfmWormHole, sfmWormHoleMoving :
       begin
         // 3D Hole
         FStarsPos[i].x := Random((FDisplayBuffer.CenterX)) - ((FDisplayBuffer.CenterX) shr 1);
         FStarsPos[i].y := Random((FDisplayBuffer.CenterY)) - ((FDisplayBuffer.CenterY) shr 1);
         FStarsPos[i].z := Random(100) + cStarFieldZFactor;
       end;
       sfmPlane, sfmDoublePlane, sfmSinusPlane, sfmDoubleSinusPlane, sfmPlaneDown :
       begin
         // 3D Plane
         FStarsPos[i].x := Random((FDisplayBuffer.Width-20)) - (FDisplayBuffer.CenterX-20);
         FStarsPos[i].y := FBaseLine; //(FDisplayBuffer.Height-10);
         FStarsPos[i].z := Random(100) + cStarFieldZFactor;  //100;
       end;
       sfmPerspectivePlane :
       begin
          // 3D Plane up to down perspective
        FStarsPos[i].x := 20 + (Random((FDisplayBuffer.CenterX)) - (FDisplayBuffer.CenterX shr 1)-20);
        FStarsPos[i].y := FBaseLine; // Copris entre -100 et 100. 0 = milieu de l'ecran
        FStarsPos[i].z := Random(100) + cStarFieldZFactor; // 100
       end;
     end;
     FStarsSpeed[i] := Random(2)+1;
  end;

end;

procedure TMainForm.RenderStarsField3D(Angle : Integer; BaseLine : Integer; ThresOld : Integer);
Const
  Dist = 200;
  cStarsDensity = 180;
Var
  x,y, i, j, ColLevel, radius, dst : Integer;
  Col : TBZColor;

  procedure NewStarA(num : Integer; aBaseLine : Integer);
  Var a : Single;
  begin
    Case FStarFieldMode of
      sfm3D :
      begin
        // 3D  Center
        FStarsPos[Num].x := Random((FDisplayBuffer.CenterX)) - ((FDisplayBuffer.CenterX) shr 1);
        FStarsPos[Num].y := Random((FDisplayBuffer.CenterY)) - ((FDisplayBuffer.CenterY) shr 1);
        FStarsPos[Num].z := Random(100) + cStarFieldZFactor; // 100
        // 3D Center 2
        //FStarsPos[Num].x := Random((FDisplayBuffer.Width)) - Dist;
        //FStarsPos[Num].y := Random((FDisplayBuffer.Height)) - Dist;
      end;
      sfmWormHole :
      begin
        // 3d WormHole fixe
        a:=random*6.283;
        FStarsPos[Num].X:=trunc(50*cos(a));
        FStarsPos[Num].Y:=trunc(50*sin(a));
        FStarsPos[Num].z := Random(100) + cStarFieldZFactor; // 100
      end;
      sfmWormHoleMoving :
      begin
        // 3D Moving WormHole
        a:=random*6.283;
        FStarsPos[Num].X:=trunc(50*cos(a)+30*cos(FCurrentStep/256));
        FStarsPos[Num].Y:=trunc(50*sin(a)+30*sin(FCurrentStep/64));
        FStarsPos[Num].z := Random(100) + cStarFieldZFactor; // 100
      end;
      sfmPlane :
      begin
        // 3D Plane Down to top full line
        FStarsPos[Num].x := Random((FDisplayBuffer.Width-20)) - (FDisplayBuffer.CenterX-20);
        FStarsPos[Num].y := aBaseLine;
        FStarsPos[Num].z := Random(100) + cStarFieldZFactor; // 100
      end;
      sfmPerspectivePlane :
      begin
        // 3D Plane up to down perspective
        FStarsPos[Num].x := 20 + (Random((FDisplayBuffer.CenterX)) - (FDisplayBuffer.CenterX shr 1)-20);
        FStarsPos[Num].y := aBaseLine; // Copris entre -100 et 100. 0 = milieu de l'ecran
        FStarsPos[Num].z := Random(100) + cStarFieldZFactor; // 100
      end;
      sfmPlaneUp :
      begin
        // 3D Plane Up
        FStarsPos[Num].x := (Random((FDisplayBuffer.Width-20) - BaseLine * 4) - ((FDisplayBuffer.CenterX -20 ) - (aBaseLine * 2)));
        FStarsPos[Num].y := aBaseLine; // Copris entre -100 et 100. 0 = milieu de l'ecran
        FStarsPos[Num].z := Random((BaseLine Shr 1)-10) + ((BaseLine Shr 1)-10) + 100;
      end;
      sfmDoublePlane :
      begin
        // 3D Double Plane
        FStarsPos[Num].x := Random((FDisplayBuffer.Width-20)) - (FDisplayBuffer.CenterX-20);
        FStarsPos[Num].y:= ((FStarsPos[Num].x*Angle) div FDisplayBuffer.CenterY) + 20;
        FStarsPos[Num].z := Random(100) + cStarFieldZFactor; // 100
      end;
      sfmSinusPlane, sfmDoubleSinusPlane :
      begin
        // 3D Sinus Plane
        FStarsPos[Num].x := Random((FDisplayBuffer.Width-20)) - (FDisplayBuffer.CenterX-20);
        FStarsPos[Num].y:=trunc(aBaseLine*Sin((Angle/cStarsDensity))*Cos((FStarsPos[Num].x/cStarsDensity)+1)+aBaseline); //85 = Haut
        FStarsPos[Num].z := Random(100) + cStarFieldZFactor; // 100
      end;
      sfmPlaneDown :
      begin

      end;

    end;
  end;

  procedure NewStarB(num : Integer; aBaseLine : Integer);
  begin
    Case FStarFieldMode of
      sfmPerspectivePlane :
      begin
        // 3D Plane up to down perspective
        FStarsPos[Num].x := 20 + (Random((FDisplayBuffer.CenterX-20)) - (FDisplayBuffer.CenterX shr 1)-20);
        FStarsPos[Num].y := aBaseLine; // Copris entre -100 et 100. 0 = milieu de l'ecran
        FStarsPos[Num].z := Random(100) + cStarFieldZFactor; // 100
      end;
      sfmPlaneUp :
      begin
        // 3D Plane Up
        FStarsPos[Num].x := (Random((FDisplayBuffer.Width-20) - BaseLine * 4) - ((FDisplayBuffer.CenterX -20 ) - (aBaseLine * 2)));
        FStarsPos[Num].y := aBaseLine; // Copris entre -100 et 100. 0 = milieu de l'ecran
        FStarsPos[Num].z := Random((BaseLine Shr 1)-10) + ((BaseLine Shr 1)-10) + 100;
      end;
      sfmDoublePlane :
      begin
        // 3D Double Plane
        FStarsPos[Num].x := Random((FDisplayBuffer.Width-20)) - (FDisplayBuffer.CenterX-20);
        FStarsPos[Num].y:= ((FStarsPos[Num].x*Angle) div FDisplayBuffer.CenterY) - 20;
        FStarsPos[Num].z := Random(100) + cStarFieldZFactor; // 100
      end;
      sfmDoubleSinusPlane :
      begin
        // 3D Sinus Plane
        FStarsPos[Num].x := Random((FDisplayBuffer.Width-20)) - (FDisplayBuffer.CenterX-20);
        FStarsPos[Num].y:=trunc(-aBaseLine*Sin((FFrameCounter/cStarsDensity))*Cos((FStarsPos[Num].x/cStarsDensity)+1)-aBaseLine); //85 = Haut
        FStarsPos[Num].z := Random(100) + cStarFieldZFactor; // 100
      end;

      sfmPlaneDown :
      begin

      end;

    end;
  end;

begin
  //for j:= 0 to 31 do
  //begin
  for I := 0 to cMaxStars do
  begin
    //inc(FStarsPos[I].Z,FStarsSpeed[I]);
    FStarsPos[I].Z := FStarsPos[I].Z + FStarsSpeed[I];
    //if FStarsPos[I].Z > 400 then NewStarA(I, BaseLine);
    //FStarsSpeed[I] := 1; //(1+(FStarsPos[I].Z div 25))*(1+(5-(abs(FStarsPos[I].X*FStarsPos[I].Y) div 50)));
    dst := Dist  - FStarsPos[I].Z;
    if (Dst <> 0) then
    begin
      x := Dist*FStarsPos[I].X div Dst;
      y := Dist*FStarsPos[I].Y div Dst;
    end
    else NewStarA(I, BaseLine);

    if (FStarFieldMode = sfmDoublePlane) or (FStarFieldMode = sfmDoubleSinusPlane) then
    begin
      // DoublePlane
      if (X < -FBackBuffer.CenterX) or (Y < -FBackBuffer.CenterY)  then
      begin
        NewStarA(I, BaseLine);
      end;
      if (X > FBackBuffer.CenterX) or (Y > FBackBuffer.CenterY)  then
      begin
        NewStarB(I, BaseLine);
      end;
      X := X + FBackBuffer.CenterX;
      Y := Y + FBackBuffer.CenterY;
      ColLevel :=  FStarsPos[I].Z;
      if ColLevel < 8 then ColLevel := 8;
      Col.Create(ColLevel,ColLevel,ColLevel);
      Radius := ColLevel div 32;
      if (Radius <= 8) and (Radius > 0) then
      begin
        With FBackBuffer.Canvas do
        begin
          //Antialias := False;
          Pen.Style := ssClear;
          Brush.Color := Col;
          Brush.Style := bsSolid;
          Circle(x,y,Radius);
        end;
      end;
      //FBackBuffer.SetPixel(x,y,col);
    end
    else if (FStarFieldMode = sfmPlaneUp) then
    begin
      // PlaneUp
      if (X < -FBackBuffer.CenterX) or (X > FBackBuffer.CenterX) or
         (Y < -FBackBuffer.CenterY) or (Y > FBackBuffer.CenterY) then NewStarA(I, BaseLine);

      if (X > -FBackBuffer.CenterX) or (X < FBackBuffer.CenterX) or
         (Y > -FBackBuffer.CenterY) or (Y < FBackBuffer.CenterY) then
      begin
        X := X + FBackBuffer.CenterX;
        Y := Y + FBackBuffer.CenterY;
        ColLevel :=  FStarsPos[I].Z;
        if ColLevel < 8 then ColLevel := 8;
        Col.Create(ColLevel,ColLevel,ColLevel);
        Radius := ColLevel div 32;
        if (Radius <= 8) and (Radius > 0) then
        begin
          With FBackBuffer.Canvas do
          begin
            Pen.Style := ssClear;
            Brush.Color := Col;
            Brush.Style := bsSolid;
            Circle(x,y,Radius);
          end;
        end;
        //FBackBuffer.SetPixel(x,y,col);
      end;
    end
    else if (FStarFieldMode = sfmPlaneDown) then
    begin
      // Descente
      //if (xe>-159) and (xe<159) and (ye>-99) and (ye<99)
      //                  then AffichePixel(seg(video^),xe,ye,z[i])
      //                  else if i<maxsame then Nouvelle(i)
      //                                    else begin
      //                                              z[i]:=z[i]+2;
      //                                              y[i]:=y[i]+1;
      //                                         end;
    end
    else
    begin
      X := X + FBackBuffer.CenterX;
      Y := Y + FBackBuffer.CenterY;
      ColLevel :=  FStarsPos[I].Z;
      if ColLevel < 8 then ColLevel := 8;
      Col.Create(ColLevel,ColLevel,ColLevel);
      Radius := ColLevel div 32;
      if (Radius <= 8) and (Radius > 0) then
      begin
        With FBackBuffer.Canvas do
        begin
          Pen.Style := ssClear;
          Brush.Color := Col;
          Brush.Style := bsSolid;
          Circle(x,y,Radius);
        end;
      end;
      //FBackBuffer.SetPixel(x,y,col);
    end;
  end;

  //end;
end;

procedure TMainForm.CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
begin
  if (FCurrentScene = 1) then
  begin
    if FStartDelay then
    begin
      Sleep(1000);
      FStartDelay := False;
    end;
  end;
  RenderScene(DeltaTime, NewTime);
  Caption:='Old School is back : '+Format('%.*f FPS', [3, FStopWatch.getFPS]);
  FDisplayBuffer.FastCopy(FBackBuffer);
  FDisplayBuffer.DrawToCanvas(Canvas, ClientRect);
  if FCurrentScene = 0 then Inc(FCurrentScene);
end;

procedure TMainForm.InitEngine;
begin
  FDisplayBuffer := TBZBitmap.Create(Width, Height);
  FDisplayBuffer.Clear(clrBlack);
  FBackBuffer := TBZBitmap.Create(Width,Height);
  FDisplayBuffer.Clear(clrTransparent);
  FTempBuffer := TBZBitmap.Create;

  FCadencer := TBZCadencer.Create(Self);
  FCadencer.Enabled := False;
  FCadencer.OnProgress := @CadencerProgress;

  FStopWatch := TBZStopWatch.Create(self);

  FSoundManager :=  TBZSoundOpenALManager.Create(Self);
  FSoundManager.Cadencer := FCadencer;
  FSoundLibrary := TBZSoundLibrary.Create(self);
  FSoundManager.Sources.Add;
  FSoundManager.Sources.Items[0].SoundLibrary := FSoundLibrary;

  DoubleBuffered := True;
end;

procedure TMainForm.InitScene;
Var
  i,j,x,y : Integer;
  z, cx,cy,xx,yy: Single;
  p : PByte;
begin
  FImg01 := TBZBitmap.Create;
  FImg02 := TBZBitmap.Create;
  FImg03 := TBZBitmap.Create;
  FImg04 := TBZBitmap.Create;

  FCurrentScene := 1;
  FCurrentScenePart := 1;
  FCurrentStep := 0;
  FStartDelay := True;

  FWormTextureMap := TBZBitmap.Create;
  FWormTextureMap.LoadFromFile('data\images\wormhole3.bmp');

    For i := 0 to cDivs-1 do
    begin
      FWormBLUT[i] := c2PI*i / cDivs;
      FWormBSinLUT[i] := System.Sin(FWormBLUT[i]);
      FWormBCosLUT[i] := System.Cos(FWormBLUT[i]);
    end;

    FWormBBuffer := nil;
    GetMem(FWormBBuffer, FBackBuffer.Width * FBackBuffer.Height);

    for j:=1 to cDivs do
    begin
      z  := -1.0 + (System.ln(2.0 * j / cDivs));
  		cx := (FBackBuffer.Width*j /cDivs);
      cy := (FBackBuffer.Height*j /cDivs);
      for i:=0 to cDivs-1 do
      begin
        xx := cx * FWormBCosLUT[i];
        yy := cy * FWormBSinLUT[i];
        yy := yy - (45 * z);
        x := Round(xx + FBackBuffer.Width div 2);
        y := Round(yy + ((FBackBuffer.Height div 2) div 2));
        if ((x >= 0) and (x < FDisplayBuffer.Width) And (y >= 0) And (y < FBackBuffer.Height)) then
        begin
          P := PByte(FWormBBuffer + (y * FBackBuffer.Width) + x);
          P^ := ((Round(i / 10) Mod FWormTextureMap.Width) + (FWormTextureMap.Width * (Round(j / 9) mod FWormTextureMap.Height))) and 255;
        end;
      end;
    end;

    InitTunnelMapping;

    FCircleBufferA := TBZBitmap.Create(Width * 2, Height * 2);
      //FCircleBufferB.Clear(clrTransparent);
      FCircleBufferA.Clear(BZColor(0, 0, 128));
      GenerateCircleMap(FCircleBufferA, 64, BZColor(255, 0, 128));    //BZColor(255, 255, 128));

      FCircleBufferB := TBZBitmap.Create(Width * 2, Height * 2);
      FCircleBufferB.Clear(clrTransparent);
      GenerateCircleMap(FCircleBufferB, 48,  BZColor(255, 255, 128));
end;

procedure TMainForm.DoneScene;
begin
  FreeAndNil(FCircleBufferA);
  FreeAndNil(FCircleBufferB);
  FreeAndNil(FWormTextureMap);
  FreeMem(FWormBBuffer);
  FWormBBuffer := nil;
  FreeAndNil(FImg04);
  FreeAndNil(FImg03);
  FreeAndNil(FImg02);
  FreeAndNil(FImg01);
end;

procedure TMainForm.InitScene_01;
begin
  FImg01.LoadFromFile('data\images\logo_fpc_BW.png');
  FImg02.LoadFromFile('data\images\logo_fpc.png');

  FBmpFontA := TBZBitmapFont.Create('data\images\fonts\blue_font.png',28,28);
  FBmpFontA.Alphabet := '!"#$%&''()*/+-=.:<>{}[]\^?@0123456789   ABCDEFGHIJKL|MNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
  FBmpFontA.VerticalLowerCaseGapSize := 2;
  FBmpFontA.HorizontalLowerCaseGapSize := -8;
  FBmpFontA.HorizontalGapSize := -10;
  FBmpFontA.VerticalGapSize := 0;
  FBmpFontA.SpaceOffset := 0;

  FBmpFontB := TBZBitmapFont.Create('data/images/fonts/KnightHawks_fontb.png',32,25);
  FBmpFontB.Alphabet := ' !"  % `() +,-./0123456789:; = ?''ABCDEFGHIJKLMNOPQRSTUVWXYZ    _     ';
  FBmpFontB.VerticalLowerCaseGapSize := 2;
  FBmpFontB.HorizontalLowerCaseGapSize := 0;
  FBmpFontB.HorizontalGapSize := 0;
  FBmpFontB.VerticalGapSize := 0;
  FBmpFontB.SpaceOffset := 0;

  FiDeltaPosA := ComputeCenterPos(FImg01,FDisplayBuffer);
end;

procedure TMainForm.DoneScene_01;
begin
  FreeAndNil(FBmpFontB);
  FreeAndNil(FBmpFontA);
end;

procedure TMainForm.DoneEngine;
begin
  FreeAndNil(FSoundManager);
  FreeAndNil(FSoundLibrary);
  FreeAndNil(FStopWatch);
  FreeAndNil(FCadencer);
  FreeAndNil(FTempBuffer);
  FreeAndNil(FBackBuffer);
  FreeAndNil(FDisplayBuffer);
end;

procedure TMainForm.RenderScene_01(deltaTime, newTime : Double);
Var
  BackColor : TBZColor;
  V : Byte;
  MaxStep : Byte;
begin
  Case FCurrentScenePart of
    1 :
    begin
      v := (clrWhite.Red * FCurrentStep) div cMaxLogoFadeStep;
      BackColor.Create(v,v,v,v);
      FbackBuffer.Clear(BackColor);
      Inc(FCurrentStep);
      if (FCurrentStep > cMaxLogoFadeStep) then
      begin
        Inc(FCurrentScenePart);
        Dec(FCurrentStep);
      end;
    end;
    2 :
    begin
      v := (clrWhite.Red * FCurrentStep) div cMaxLogoFadeStep;
      BackColor.Create(v,v,v,v);
      FbackBuffer.Clear(BackColor);
      FadeInOut(FImg01, FbackBuffer, cMaxLogoFadeStep-FCurrentStep, cMaxLogoFadeStep, FiDeltaPosA.X,FiDeltaPosA.Y-80);
      Dec(FCurrentStep);
      if (FCurrentStep<0) then
      begin
        Inc(FCurrentScenePart);
        FCurrentStep := 1;
        FBmpFontA.DrawMode := dmSet;
        FBmpFontA.AlphaMode := amAlphaBlend;
        FBmpFontA.MasterAlpha := 0;
        FbackBuffer.Clear(clrBlack);

        With FbackBuffer.Canvas do
        begin
          DrawMode.AlphaMode := amAlphaBlend;
          DrawMode.MasterAlpha := ClampByte(FCurrentStep * 4);
          Pen.Style := ssSolid;
          Pen.Color := clrWhite;
          Pen.Width := 5;
          Brush.Style := bsGradient;
          Brush.Gradient.Kind := gkVertical;
          Brush.Gradient.ColorSteps.AddColorStop(clrBlue,0.0);
          Brush.Gradient.ColorSteps.AddColorStop(clrSilver,1.0);
          //Rectangle(2,2,FTempBuffer.Width-2,FTempBuffer.Height-2);
          Rectangle(FiDeltaPosA.X-6,FiDeltaPosA.Y-86, FiDeltaPosA.X + FImg02.Width + 6, FiDeltaPosA.Y-80+FImg02.Height+2);
        end;
        //FbackBuffer.PutImageStretch(,ClampByte(FCurrentStep * 4));
        FbackBuffer.PutImage(FImg01, FiDeltaPosA.X,FiDeltaPosA.Y-80,255,dmSet,amAlphaCheck);
      end;
    end;
    3 :
    begin
      MaxStep :=  64;
      if (FCurrentStep > 23) then FbackBuffer.Clear(clrBlack);

      With FbackBuffer.Canvas do
      begin
        DrawMode.AlphaMode := amAlphaBlend;
        DrawMode.MasterAlpha := ClampByte(FCurrentStep * 4);
        //Pen.Style := ssSolid;
        //Pen.Color := clrWhite;
        //Pen.Width := 5;
        //Brush.Style := bsGradient;
        //Brush.Gradient.Kind := gkVertical;
        //Brush.Gradient.ColorSteps.AddColorStop(clrBlue,0.0);
        //Brush.Gradient.ColorSteps.AddColorStop(clrSilver,1.0);
        //Rectangle(2,2,FTempBuffer.Width-2,FTempBuffer.Height-2);
        Rectangle(FiDeltaPosA.X-6,FiDeltaPosA.Y-86, FiDeltaPosA.X + FImg02.Width + 6, FiDeltaPosA.Y-80+FImg02.Height+2);
      end;
      //FbackBuffer.PutImageStretch(,ClampByte(FCurrentStep * 4));
      FbackBuffer.PutImage(FImg01, FiDeltaPosA.X,FiDeltaPosA.Y-80,255,dmSet,amAlphaCheck);
      FadeInOut(FImg02, FBackBuffer, FCurrentStep, MaxStep, FiDeltaPosA.X,FiDeltaPosA.Y-80);

      if (FCurrentStep > 23) then FBmpFontA.MasterAlpha := ClampByte((FCurrentStep-24) * 6);
      //FBmpFontA.TextOut(FbackBuffer,512 - 200 , FiDeltaPosA.Y+FImg02.Height-70, 'Free Pascal Compiler');
      Case FCurrentStep of
        24..31  : FBmpFontA.TextOut(FbackBuffer,512 - ((3*20) shr 1)  , FiDeltaPosA.Y+FImg02.Height-70, 'FPC');
        32  : FBmpFontA.TextOut(FbackBuffer,512 - ((4*20) shr 1)  , FiDeltaPosA.Y+FImg02.Height-70, 'F PC');
        33  : FBmpFontA.TextOut(FbackBuffer,512 - ((5*20) shr 1)  , FiDeltaPosA.Y+FImg02.Height-70, 'F P C');
        34  : FBmpFontA.TextOut(FbackBuffer,512 - ((6*20) shr 1)  , FiDeltaPosA.Y+FImg02.Height-70, 'Fr P C');
        35  : FBmpFontA.TextOut(FbackBuffer,512 - ((7*20) shr 1)  , FiDeltaPosA.Y+FImg02.Height-70, 'Fre P C');
        36  : FBmpFontA.TextOut(FbackBuffer,512 - ((8*20) shr 1)  , FiDeltaPosA.Y+FImg02.Height-70, 'Free P C');
        37  : FBmpFontA.TextOut(FbackBuffer,512 - ((9*20) shr 1) , FiDeltaPosA.Y+FImg02.Height-70, 'Free Pa C');
        38  : FBmpFontA.TextOut(FbackBuffer,512 - ((10*20) shr 1) , FiDeltaPosA.Y+FImg02.Height-70, 'Free Pasc C');
        39  : FBmpFontA.TextOut(FbackBuffer,512 - ((11*20) shr 1) , FiDeltaPosA.Y+FImg02.Height-70, 'Free Pasca C');
        40  : FBmpFontA.TextOut(FbackBuffer,512 - ((12*20) shr 1) , FiDeltaPosA.Y+FImg02.Height-70, 'Free Pascal C');
        41  : FBmpFontA.TextOut(FbackBuffer,512 - ((13*20) shr 1) , FiDeltaPosA.Y+FImg02.Height-70, 'Free Pascal Co');
        42  : FBmpFontA.TextOut(FbackBuffer,512 - ((14*20) shr 1) , FiDeltaPosA.Y+FImg02.Height-70, 'Free Pascal Com');
        43  : FBmpFontA.TextOut(FbackBuffer,512 - ((15*20) shr 1) , FiDeltaPosA.Y+FImg02.Height-70, 'Free Pascal Comp');
        44  : FBmpFontA.TextOut(FbackBuffer,512 - ((16*20) shr 1) , FiDeltaPosA.Y+FImg02.Height-70, 'Free Pascal Compi');
        45  : FBmpFontA.TextOut(FbackBuffer,512 - ((17*20) shr 1) , FiDeltaPosA.Y+FImg02.Height-70, 'Free Pascal Compil');
        46  : FBmpFontA.TextOut(FbackBuffer,512 - ((18*20) shr 1) , FiDeltaPosA.Y+FImg02.Height-70, 'Free Pascal Compile');
        47  : FBmpFontA.TextOut(FbackBuffer,512 - ((19*20) shr 1) , FiDeltaPosA.Y+FImg02.Height-70, 'Free Pascal Compiler');
        48..64 :
        begin
          //FBmpFontA.MasterAlpha := ClampByte(FCurrentStep * 4);
          if (FCurrentStep > 63) then FBmpFontA.MasterAlpha := 255;
          FBmpFontA.TextOut(FbackBuffer,512 - 200 , FiDeltaPosA.Y+FImg02.Height-70, 'Free Pascal Compiler');
        end;
      end;

      inc(FCurrentStep);

      if (FCurrentStep>MaxStep) then
      begin
        Inc(FCurrentScenePart);
        FTempBuffer.SetSize(width, Height);
        FTempBuffer.FastCopy(FBackBuffer);
        FCurrentStep := 32;
      end;
    end;
    4 :
    begin
      FBackBuffer.Clear(clrBlack);
      MaxStep := 32;
      FadeInOut(FTempBuffer, FBackBuffer, FCurrentStep, MaxStep,0,0);
      Dec(FCurrentStep);
      if (FCurrentStep < 0) then
      begin
        Inc(FCurrentScene);
        FCurrentStep := 0;
        FCurrentScenePart := 1;
        FImg01.LoadFromFile('data\images\logo_lazarus_part01.png');
        FImg02.LoadFromFile('data\images\logo_lazarus_part02.png');
        FImg03.LoadFromFile('data\images\logo_lazarus_part03.png');
        FImg04.LoadFromFile('data\images\logo_lazarus_paw.png');
        //FImg01.PreMultiplyAlpha;
        FFloatRect.Create(FBackBuffer.CenterX - 31, FBackBuffer.CenterY - 82, FBackBuffer.CenterX-30, FBackBuffer.CenterY - 81);
        FDeltaWidthA := 1; //FImg01.Width div 128;
        FDeltaHeightAf := 0.5;// (FImg01.Height div 2) div 39;

        FiDeltaPosA := ComputeCenterPos(FImg01,FBackBuffer);
        FiDeltaPosA.Y := FiDeltaPosA.Y - 80;
        FiDeltaPosA.X := FiDeltaPosA.X - 30;

        FiDeltaPosB := ComputeCenterPos(FImg03,FBackBuffer);
        FiDeltaPosB.Y := FBackBuffer.Height;
        //FiDeltaPosB.X := FiDeltaPosB.X;

        FiDeltaPosC.Create(FiDeltaPosA.X + FImg01.Width + 5,-200);

        FTempAlphaA := 0;
        FTempAlphaB := 0;
        FTempAlphaC := 0;
      end;
    end;
  end;
end;

procedure TMainForm.RenderScene_02(deltaTime, newTime : Double);
Var
  MaxStep : Word;
  Alpha : Byte;
begin
  Case FCurrentScenePart of
    1 :
    begin
      MaxStep := 650;
      FBackBuffer.Clear(clrBlack);
      FBackBuffer.PutImage(FImg04,40,40,FtempAlphaC,dmSet,amAlphaBlend);
      if FTempAlphaC < 64 then Inc(FTempAlphaC);

      if FCurrentStep < 512 then Alpha := ClampByte(FCurrentStep div 2) else Alpha := 255;

      FBackBuffer.PutImageStretch(FImg01, FFloatRect.ASRect, Alpha);
      if (FFloatRect.Left > FiDeltaPosA.X ) then
      begin
        FFloatRect.Left := FFloatRect.Left - FDeltaWidthA;
        FFloatRect.Right := FFloatRect.Right + FDeltaWidthA;
      end;

      FFloatRect.Top := FFloatRect.Top + FDeltaHeightAf;
      FFloatRect.Bottom := FFloatRect.Bottom - FDeltaHeightAf;
      if (FFloatRect.Top < FiDeltaPosA.Y) or (FFloatRect.Top > (FiDeltaPosA.Y+FImg01.Height)) then FDeltaHeightAf := -FDeltaHeightAf;


      if (FCurrentStep > 420) then
      begin
        if FTempAlphaA < 255 then FTempAlphaA := FTempAlphaA + 1;
        FBackBuffer.PutImage(FImg02, FiDeltaPosC.X, FiDeltaPosC.Y, FTempAlphaA);
        if (FiDeltaPosC.Y<FiDeltaPosA.Y) then FiDeltaPosC.Y := FiDeltaPosC.Y + 2;

        if FTempAlphaB < 255 then FTempAlphaB := FTempAlphaB + 1;
        FBackBuffer.PutImage(FImg03, FiDeltaPosB.X, FiDeltaPosB.Y, FTempAlphaB);
        if (FiDeltaPosB.Y>420) then Dec(FiDeltaPosB.Y,2);
      end;

      Inc(FCurrentStep);
      if (FCurrentStep > MaxStep) then
      begin
       // Close;
        Inc(FCurrentScenePart);
      end;
    end;
    2 :
    begin
      //Sleep(300);
      Inc(FCurrentScene);
      FCurrentStep := 0;
      FCurrentScenePart := 1;
      FImg01.LoadFromFile('data\images\ready.png');
      FImg02.LoadFromFile('data\images\to.png');
      FImg03.LoadFromFile('data\images\demo.png');
      FImg04.LoadFromFile('data\images\nographicdriver.png');

      FFloatRect.Left := 335;
      FFloatRect.Top := 362;
      FFloatRect.Right := FFloatRect.Left + FImg02.Width;
      FFloatRect.Bottom := FFloatRect.Top + FImg02.Height;

      FFloatRectB.Left := FFloatRect.Left + FImg02.CenterX;
      FFloatRectB.Top := FFloatRect.Top + FImg02.CenterY;
      FFloatRectB.Right := FFloatRect.Right - FImg02.CenterX;
      FFloatRectB.Bottom := FFloatRect.Bottom - FImg02.CenterY;

      FFloatRectC := FFLoatRectB;

      FiDeltaPosA.X := 340;
      FiDeltaPosA.Y := -100;
      FTempAlphaA := 0;
      FiDeltaPosB.X := 1290;
      FiDeltaPosB.Y := 350;
      FTempAlphaB := 0;
      FTempAlphaC := 0;

      FCadencer.Enabled := False;
      FSoundLibrary.Samples.Clear;
      FSoundLibrary.Samples.AddFile('data\sounds\anarchy.mod','Music1');  //4m40   //'data\sounds\back2b.xm' --> 3m12
      FSoundManager.Sources.Items[0].SoundName := 'Music1';
      FSoundManager.Sources.Items[0].Volume:= 0;
      FSoundManager.Active := True;
      FBackBuffer.Clear(clrBlack);
      FCadencer.Enabled := True;
    end;
  end;
end;

procedure TMainForm.RenderScene_03(deltaTime, newTime : Double);
Var
  MaxTime : Single;
  MaxStep : Word;
begin

  Case FCurrentScenePart of
    1 :
    begin
      FBackBuffer.Clear(clrBlack);
      MaxStep := 1000;

      if (FCurrentStep>200) then
      begin
        if Not(FSoundManager.Sources.Items[0].Playing) then FSoundManager.Sources.Items[0].Playing := True;
        if FSoundManager.Sources.Items[0].Volume<255 then
        begin
          FSoundManager.Sources.Items[0].Volume := ClampByte(Round(Tweener(0,255,FTempAlphaC,512,amInOut, atCubic)+0.5));
          inc(FTempAlphaC);
        end;
      end;

      if  (FCurrentStep < 340 ) then
      begin
        if (FiDeltaPosA.Y<340) then FiDeltaPosA.Y := FiDeltaPosA.Y + 1;
        if FTempAlphaA < 255 then FTempAlphaA := FTempAlphaA + 1;
      end;
      FBackBuffer.PutImage(FImg01,FiDeltaPosA.X,FiDeltaPosA.Y, FTempAlphaA);

      if (FCurrentStep > 235 ) then
      begin
        if (FiDeltaPosB.X>425) then FiDeltaPosB.X := FiDeltaPosB.X - 2;
        if FTempAlphaB < 255 then FTempAlphaB := FTempAlphaB + 1;
        FBackBuffer.PutImage(FImg03,FiDeltaPosB.X,FiDeltaPosB.Y,FTempAlphaB);
      end;

      if (FCurrentStep > 335) then
      begin
        MaxTime := 335;
        if (FCurrentStep<670) then
        begin
          FFloatRectC.Left := Tweener(FFloatRectB.Left,FFloatRect.Left, FCurrentStep-335,MaxStep, amIn, atBounce);
          FFloatRectC.Top := Tweener(FFloatRectB.Top,FFloatRect.Top, FCurrentStep-335,MaxTime, amIn, atBounce);
          FFloatRectC.Right := Tweener(FFloatRectB.Right,FFloatRect.Right, FCurrentStep-335,MaxTime, amIn, atBounce);
          FFloatRectC.Bottom := Tweener(FFloatRectB.Bottom,FFloatRect.Bottom, FCurrentStep-335,MaxTime, amIn, atBounce);
          FBackBuffer.PutImageStretch(FImg02, FFloatRectC.ASRect,255);
        end
        else
        begin
          FBackBuffer.PutImageStretch(FImg02, FFloatRect.ASRect,255);
        end;
      end;

      if (FCurrentStep>665) then FBackBuffer.PutImage(FImg04,340,450);

      Inc(FCurrentStep);
      if (FCurrentStep > MaxStep) then
      begin
        Inc(FCurrentScenePart);
        FCurrentStep := 0;
        FiDeltaPosA.X := FBackBuffer.CenterX;
        FiDeltaPosA.Y := FBackBuffer.CenterY;
        FiDeltaPosB.X := FBackBuffer.CenterX;
        FiDeltaPosB.Y := FBackBuffer.CenterY;
        FImg01.LoadFromFile('data\images\everyone.png');
        FImg02.LoadFromFile('data\images\thisdoor.png');
        FImg03.LoadFromFile('data\images\someby.png');
        FTempAlphaA := 2;
      end;
    end;
    2 :
    begin
      MaxStep := 400 ;
      if (FCurrentStep<128) then
      begin
         With FBackBuffer.Canvas do
         begin
           Pen.Width := 1;
           Pen.Color := clrWhite;
           Line(FiDeltaPosA.X, FiDeltaPosA.Y,FiDeltaPosB.X,FiDeltaPosB.Y);
         end;
         FiDeltaPosB.X := FiDeltaPosB.X + 4;
         FiDeltaPosA.X := FiDeltaPosA.X - 4;
      end
      else
      begin
        if (FCurrentStep < 320) then
        begin
          With FBackBuffer.Canvas do
          begin
            Pen.Style := ssSolid;
            Pen.Color := clrWhite;
            Line(FiDeltaPosA.X, FiDeltaPosA.Y,FiDeltaPosB.X,FiDeltaPosA.Y);
            Line(FiDeltaPosA.X, FiDeltaPosB.Y,FiDeltaPosB.X,FiDeltaPosB.Y);
            Pen.Style := ssClear;
            Brush.Style := bsSolid;
            Brush.Color := clrBlack;
            Rectangle(FiDeltaPosA.X, FiDeltaPosA.Y+1,FiDeltaPosB.X, FiDeltaPosB.Y-1);
          end;
          FiDeltaPosB.Y := FiDeltaPosB.Y + 2;
          FiDeltaPosA.Y := FiDeltaPosA.Y - 2;
        end;

        if (FCurrentStep > 140) then
        begin
          if FCurrentStep = 141 then
          begin
            FiDeltaPosC.x := 0;
            FiDeltaPosC.y := FBackBuffer.CenterY;
            FTempAlphaA := 2;
          end;
          if (FCurrentStep < 183) then
          begin
            FBackBuffer.CopyHorizontalBand(FImg01, FImg01.CenterY - (FTempAlphaA shr 1) ,FTempAlphaA,FiDeltaPosC.Y);
            FTempAlphaA := FTempAlphaA + 2;
            FiDeltaPosC.y := FiDeltaPosC.y - 2;
          end;
          if (FCurrentStep <= 220) then FBackBuffer.PutImage(Fimg01,0,FiDeltaPosC.Y);
        end;

        if (FCurrentStep > 220) then
        begin
          if FCurrentStep = 221 then
          begin
            FiDeltaPosC.x := 0;
            FiDeltaPosC.y := FBackBuffer.CenterY-1;
            FTempAlphaA := 2;
          end;
          if (FCurrentStep < 263) then
          begin
            FBackBuffer.CopyHorizontalBand(FImg02, FImg02.CenterY - (FTempAlphaA shr 1) ,FTempAlphaA,FiDeltaPosC.Y);
            FTempAlphaA := FTempAlphaA + 2;
            FiDeltaPosC.y := FiDeltaPosC.y - 2;
          end;
          if (FCurrentStep <= 290) then FBackBuffer.PutImage(Fimg02,0,FiDeltaPosC.Y);
        end;

        if (FCurrentStep > 290) then
        begin
          if FCurrentStep = 291 then
          begin
            FiDeltaPosC.x := 0;
            FiDeltaPosC.y := FBackBuffer.CenterY-1;
            FTempAlphaA := 2;
          end;
          if (FCurrentStep < 333) then
          begin
            FBackBuffer.CopyHorizontalBand(FImg03, FImg03.CenterY - (FTempAlphaA shr 1) ,FTempAlphaA,FiDeltaPosC.Y);
            FTempAlphaA := FTempAlphaA + 2;
            FiDeltaPosC.y := FiDeltaPosC.y - 2;
          end;
          if (FCurrentStep <= 365) then FBackBuffer.PutImage(Fimg03,0,FiDeltaPosC.Y);
        end;

      end;
      Inc(FCurrentStep);
      if (FCurrentStep > MaxStep) then
      begin
        Inc(FCurrentScene);
        FBackBuffer.Clear(clrBlack);
        FCurrentStep := 0;
        FCurrentScenePart := 1;
        FImg01.LoadFromFile('data\images\logo-jhb-02.png');
        FImg02.LoadFromFile('data\images\present.png');
        FiDeltaPosA := ComputeCenterPos(FImg01, FBackBuffer);
        FiDeltaPosB := ComputeCenterPos(FImg02, FBackBuffer);
        //GlobalLogger.logNotice('DeltaPosA = '+FiDeltaPosA.ToString);
      end;
    end;
  end;
end;

procedure TMainForm.RenderScene_04(deltaTime, newTime : Double);
Var
  MaxStep : Word;
begin

  Case FCurrentScenePart of
    1 :
    begin
      MaxStep := 250;
      //FBackBuffer.Clear(clrBlack);
      if (FCurrentStep<255) then FadeInOut(FImg01, FBackBuffer, FCurrentStep, 255, FiDeltaPosA.X,FiDeltaPosA.Y-120);
      if (FCurrentStep>=255) then
      begin
        FBackBuffer.PutImage(FImg01,FiDeltaPosA.X,FiDeltaPosA.Y-120);
      end;
      if (FCurrentStep>200) then  FBackBuffer.PutImage(FImg02,FiDeltaPosB.X,(FiDeltaPosA.Y-80) + Fimg01.Height);

      Inc(FCurrentStep);
      if (FCurrentStep > MaxStep) then
      begin
        Inc(FCurrentScenePart);
        FCurrentStep := 0;
      end;
    end;
    2 :
    begin
      //FBackBuffer.Clear(clrBlack);
      Inc(FCurrentScene);
      FCurrentStep := 0;
      FCurrentScenePart := 1;
      FImg01.LoadFromFile('data\images\graffiti-wall.jpg');
      FImg02.LoadFromFile('data\images\Graff-OldSchool.png');
      FImg03.LoadFromFile('data\images\Graff-IsBack.png');
      // Destination Img 01
      FFloatRect.Create(87, 200, 937, 524);
      // Debut Img 01
      FFloatRectB.Create(-200,-200,1224,938);

      // Destination Img 02
      FFloatRectC.Create(553, 424, 943,604);
      // Debut Img 02
      FFloatRectD.Create(-227,64,1700,964);

      FTempAlphaA := 0;
      FTempAlphaB := 0;
      FDeltaA := 1;
      FDeltaB := 1;
      FDelta1 := 255 / 230;
      FDelta2 := 255 / 300;

      FTempFloatRectA := FFloatRectB;
      FTempFloatRectB := FFloatRectD;
    end;
  end;

end;

procedure TMainForm.RenderScene_05(deltaTime, newTime : Double);
Var
  MaxStep : Word;
begin

  Case FCurrentScenePart of
    1 :
    begin
      MaxStep := 985;
      if (FCurrentStep<768) then Rouleau(FImg01, FBackBuffer, FCurrentStep, 64);

      if (FCurrentStep>770) and (FCurrentStep<961) then
      begin
        FTempFloatRectA.left   := Tweener(FTempFloatRectA.left,   FFloatRect.Left,   FCurrentStep - 770,200, amOut, atSine);
        FTempFloatRectA.Top    := Tweener(FTempFloatRectA.Top,    FFloatRect.Top,    FCurrentStep - 770,200, amOut, atSine);
        FTempFloatRectA.Right  := Tweener(FTempFloatRectA.Right,  FFloatRect.Right,  FCurrentStep - 770,200, amOut, atSine);
        FTempFloatRectA.Bottom := Tweener(FTempFloatRectA.Bottom, FFloatRect.Bottom, FCurrentStep - 770,200, amOut, atSine);
        //if FTempAlphaA<255 then FTempAlphaA := Round(Tweener(0 , 255, FCurrentStep - 770,330, etQuadOut));
        FBackBuffer.PutImageStretch(FImg02,  FTempFloatRectA.ASRect, Trunc(FDeltaA));
        if (FDeltaA< 255) then FDeltaA := FDeltaA + FDelta1;
      end;

      if (FCurrentStep>790) and (FCurrentStep<970) then
      begin
        FTempFloatRectB.left   := Tweener(FTempFloatRectB.left,   FFloatRectC.Left,   FCurrentStep - 790,200, amOut, atSine);
        FTempFloatRectB.Top    := Tweener(FTempFloatRectB.Top,    FFloatRectC.Top,    FCurrentStep - 790,200, amOut, atSine);
        FTempFloatRectB.Right  := Tweener(FTempFloatRectB.Right,  FFloatRectC.Right,  FCurrentStep - 790,200, amOut, atSine);
        FTempFloatRectB.Bottom := Tweener(FTempFloatRectB.Bottom, FFloatRectC.Bottom, FCurrentStep - 790,200, amOut, atSine);
        FBackBuffer.PutImageStretch(FImg03,  FTempFloatRectB.ASRect,  Trunc(FDeltaB));
        if (FDeltaB<192) then  FDeltaB := FDeltaB + FDelta2;
      end;

      Inc(FCurrentStep);
      if (FCurrentStep > MaxStep) then
      begin
        FTempBuffer.SetSize(width, Height);
        FTempBuffer.FastCopy(FBackBuffer);

        Inc(FCurrentScene);
        FCurrentScenePart:=1;
        FCurrentStep := 0;

        FStarFieldMode :=  sfmPerspectivePlane;
        FAngleB := 360;
        FBaseLine := 100;
        FThresOld := cMaxStars - 300;
        FDeltaAngle := 1;
        FDeltaBaseLine := 100;
        InitStarsField3D;

        FImg01.LoadFromFile('data\images\module.png');
        FiDeltaPosA.Create(-(FImg01.Width + 40), FBackBuffer.MaxHeight-FImg01.CenterY-120);
        FAngle := 0;
        FZoomFactor := 0.2;
        FDeltaHeightA := -32;
        FDeltaHeightB := -32;
        FFloatStep := (1.1*cPi)/FBackBuffer.Height;
        FDeltaAlpha := 0;

        FBmpFontA.LoadFromFile('data\images\fonts\charset_3.png',56,80);
        FBmpFontA.Alphabet := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.,;:?!-#"''&()[]`/\µ@°+=*$£€<>^§%';
        FBmpFontA.SpaceOffset := -12;
        //GlobalLogger.LogNotice('Start Scene 06 = '+FStopWatch.GetValueAsTime);
        //GlobalLogger.LogNotice('Music Len in Bytes = '+ FSoundManager.Sources.Items[0].Sample.Data.LengthInBytes.ToString);
        //GlobalLogger.LogNotice('Current Music Time pos in Byte = '+FSoundManager.Sources.Items[0].TimePositionInByte.ToString);
      end;
    end;
  end;
end;

procedure TMainForm.InitTunnelMapping;
Var
  i,j,cx,cy: Integer;
  a,l : Single;
  idh,idw,dyy, dxx,xx, yy,uv_x, uv_y,aspectratio :Single;


begin
   aspectratio := (FBackBuffer.Width*2) / (FBackBuffer.Height*2);
   cx := FBackBuffer.Width-1;
   cy := FBackBuffer.Height-1;
   idh := 1 / FBackBuffer.Height;
   idw := 1 / (FBackBuffer.Width);
   //for j := 0 to (FDisplayBuffer.Height*2)-1 do
   for j := -cy to cy do
   begin
     yy := (j * idh);
     yy := (yy + yy) * aspectratio;
     dyy := yy * yy;
   //  for i := 0 to (FDisplayBuffer.Width*2)-1 do
   for i := -cx to cx do
     begin
       xx := (i * idw);
       xx := ((xx + xx)) * aspectratio ;
       dxx := xx * xx;

       A := BZMath.ArcTan2(yy,xx)+ i*(0.002*1.8); //j/100) ;
       L := System.Sqrt(dxx + dyy);// +0.08*0.18); //
       uv_X := (A * cInv2Pi);// + i*(0.001*1.1) ;
       uv_Y := ((0.5/ L));// + i*0.001001; //*1.1) ;

        //uv_x := 2*xx;
        //uv_y := 2*abs(yy);
        //uv_x := Fract((uv_x/uv_y));
        uv_y := 2*uv_y; //*uv_y;

       FTunnelLUT[cx+i,cy+j].a := uv_x;
       FTunnelLUT[cx+i,cy+j].l := uv_y

     end;
   end;

  // Initalisation des données de la scene
  FScreenResolution.Create(FBackBuffer.Width,FBackBuffer.Height);
  FScreenInvResolution.Create(1,1);
  FScreenInvResolution := FScreenInvResolution / FScreenResolution;
  FScreenAspectRatio.Create(FScreenResolution.x  / FScreenResolution.y,1.0);
  FTextureSize.Create(256, 256);

  FAddMove.Create(32,48);
  FIncMove.Create(40,60);
  FMaxLinePix := FBackBuffer.MaxWidth; //(FDisplayBuffer.Width shr 2)-1;
  FMinConeSize := 40;
end;

procedure TMainForm.RasterizeTunnelMappingLineProc(Sender : TObject; Index : Integer; Data : Pointer);
var
  k,p,i : Integer;
  //mc : Single;
  mc : Byte;
  PixPtr : PBZColor;
  OutColor : TBZColor;
  {$CODEALIGN VARMIN=16}
    uv : TBZFloatPoint;
    ts : TBZPoint;
  {$CODEALIGN VARMIN=4}
begin
  PixPtr := FBackBuffer.GetScanLine(Index);
  p:= Index + FMoveY;

  i := 0;
  while i<FMaxLinePix do
  begin
      k := i + FMoveX;
      uv.Create(FTunnelLUT[k,p].a,FTunnelLUT[k,p].l);
      mc := Round((cConeRadiusTop-uv.Y));
      if (mc>FMinConeSize) and (mc<49) then
      begin
        uv := uv * FTextureSize;
        ts := uv.Round;
        ts := ts + FAddMove;
        ts.X := ts.X and (FTextureMaxSize.X);
        ts.Y := ts.Y and (FTextureMaxSize.Y);

        OutColor := FImg02.GetPixel(ts.x, ts.y);
        if (OutColor.Alpha>92) then
        begin
          PixPtr^:= OutColor;
        end;
      end;
      Inc(PixPtr);

      inc(i);
  end;
end;

procedure TMainForm.DoRenderTunnelMapping(NewTime : Double);
// Coeur du rendu de la scene
Var

 fpx,fpy :Single;

begin

   //PixPtr := FDisplayBuffer.GetScanLine(0);
   //for j := 0 to FDisplayBuffer.MaxHeight do
   //begin
   //  for i := 0 to FDisplayBuffer.MaxWidth do
   //  begin
   //    uv_X := FTunnelLUT[i,j].A;
   //    uv_Y := FTunnelLUT[i,j].L + NewTime;
   //    iuv_x := Round(uv_x * FTextureMap.Width) mod FTextureMap.Width;
   //    iuv_y := Round(uv_y * FTextureMap.Height) mod FTextureMap.Height;
   //    PixPtr^ := FTextureMap.GetPixel(iuv_x, iuv_y);
   //    Inc(PixPtr);
   //  end;
   //end;

  FTunnelParams[0] := newTime;

  fpx := (Cos((10*(newTime/160))*cPI));
  fpy := (Sin(cPi+(4*(newTime/16))*cPI));

  FMoveX := Round(511 + 256*fpx);
  FMoveY := Round(383 + 190*fpy) ;
  //FMoveX := 128;
// if (FMoveX and 2)=0 then
  //FBackBufferBuffer.ColorFilter.AdjustBrightness(0.7);

  ParallelFor(0,FBackBuffer.MaxHeight,@RasterizeTunnelMappingLineProc, Pointer(FTunnelParams));
  FIncMove.Create(Round(1*fpx),10);
  FAddMove := FAddMove + FIncMove;
  //FAddMove.x := Round(128+);
  //FAddX := FAddX + 1; //128;
  //FAddX :=  128;
  //FAddY := FAddY + 10;
 // FdisplayBuffer.Clear(clrBlack);
  //FBitmapRasterizer.Rasterize;

end;

procedure TMainForm.RenderScene_06(deltaTime, newTime : Double);
Var
  MaxStep : Word;
  v : Byte;
  BackColor : TBZColor;
  i,k : Integer;

  procedure DrawText(Str : String; xStart,yStart : Integer);
  Var
    a : Single;
    x,y, i : Integer;
  begin
    a := 0;
    for x := 0 to length( str)-1 do
    begin
      for i:=0 to 6 do
      begin
        a := ((newtime*700)+((x*80)-(i*40)))*FFloatStep;
        y :=  Round(YStart + cos(a)*25);
        FBmpFontB.TextOut(FBackBuffer, xStart + (x*FBmpFontB.CarWidth)  , y, Str[x+1]);
      end;
    end;
  end;

  procedure DrawSpiralScrollText(dx,dy : Integer; Rings,ScaleFactor,a,b, dg : Single; StartChar : Integer);
  Var
    r,x,y,degrees, spiralEnd, theta : Single;
    i,s,d : Integer;
    SpiralScrollText : String;
  begin
    SpiralScrollText := '                        .....This hole is a pit of doom, it''s time to more colored real-time FX !!! Let''s go back to the futur !!!                        ';
    degrees := DegtoRadian(dg);
    //Spiralend := 360 * 2 * 10 * degrees; //;
    SpiralEnd := Rings * c2Pi;
    //a := 3;
    //b := 4;
    theta := -Degrees * 20;
    //theta := SpiralEnd;

    d := StartChar + 20;
    //while Theta < SpiralEnd do
    For i :=  d downto StartChar do
    begin
      r := a + b * Theta;//  + (Degrees*StartChar);
      x := dx + (r * System.cos(theta)*ScaleFactor);
      y := dy - (r * System.sin(theta)*ScaleFactor);
      if d <= Length(SpiralScrollText) then FBmpFontA.TextOut(FBackBuffer,Round(x) , Round(y), SpiralScrollText[i]);
      Theta := Theta + (Degrees);
    end;
  end;

begin
  Case FCurrentScenePart of
    1 :
    begin
      MaxStep := 5100;
      FBackBuffer.Clear(clrBlack);
      if (FCurrentStep<4800) then FBackBuffer.Clear(clrBlack)
      else if (FCurrentStep>=4800) and (FCurrentStep<5068)  then
      begin
        if ((FCurrentStep Mod 100) = 0) then FBackBuffer.ColorFilter.AdjustBrightness(0.45);
      end;

      if (FCurrentStep>16) then RenderStarsField3D(FAngleB, FBaseLine, FThresOld);
      if (FCurrentStep<=32) then FadeInOut(FTempBuffer, FBackBuffer, 32-FCurrentStep, 32,0,0);

      if (FCurrentStep>40) and (FCurrentStep<=800) then
      begin
        FBackBuffer.PutImageRotateAndScale(Fimg01,FiDeltaPosA.X, FiDeltaPosA.y,FAngle mod 360,FZoomFactor,Fimg01.CenterX, Fimg01.CenterY);
        if (FCurrentStep<530) then
        begin
          Inc(FiDeltaPosA.X,2);
          FiDeltaPosA.y := FiDeltaPosA.y + Round(Sin(NewTime+100));
          if (FCurrentStep>350) then if FZoomFactor<1.0 then FZoomFactor := FZoomFactor+0.02;
        end;
        Inc(FAngle);
        //Inc(FAngleB);
        if (FCurrentStep>600)  then
        begin
          if FZoomFactor>0.65 then FZoomFactor := FZoomFactor-0.01;
          if (FiDeltaPosA.y<FBackBuffer.CenterY-FImg01.CenterY-80) then Dec(FiDeltaPosA.y,3);
        end;
      end;

      if (FCurrentStep>560) then
      begin
        if (FCurrentStep<=750) then DrawText('ENTER INTO THE MODULE !', 180, FDeltaHeightA);
        if (FCurrentStep< 700) then inc(FDeltaHeightA);
        if (FCurrentStep> 750) then
        begin
          if FDeltaHeightA> -32 then
          begin
            DrawText('ENTER INTO THE MODULE !', 180, FDeltaHeightA);
            Dec(FDeltaHeightA);
          end;
          if (FCurrentStep<=1050) then
          begin
            DrawText('PREPARE TO TRAVEL IN SPACE-TIME',10, FDeltaHeightB);
            if FDeltaHeightB<350 then Inc(FDeltaHeightB);
          end;
        end;
      end;

      if (FCurrentStep>800) and (FCurrentStep<1000) then
      begin
        if (FZoomFactor>0) then FBackBuffer.PutImageRotateAndScale(Fimg01,FiDeltaPosA.X, FiDeltaPosA.y,FAngle mod 360,FZoomFactor,Fimg01.CenterX, Fimg01.CenterY);
        if (FCurrentStep>800) then If FBaseLine>0 then Dec(FBaseLine);
        if (FZoomFactor>0)  and (FCurrentStep>950) then
        begin
          FZoomFactor := FZoomFactor - 0.02;
          Dec(FiDeltaPosA.y);
        end;
        Inc(FAngle);
      end;

      if FCurrentStep = 1200 then
      begin
        FStarFieldMode :=  sfm3D;
        FBaseLine := 85;
      end;

      if (FCurrentStep >= 1200) then
      begin
        FAngleB := FAngleB + FDeltaAngle;
        if (FAngleB > 720) or (FAngleB < -720) then FDeltaAngle := - FDeltaAngle;
      end;

      if FCurrentStep = 2100 then
      begin
        FStarFieldMode :=  sfmDoubleSinusPlane;
      end;

      if FCurrentStep = 3100 then
      begin
        FStarFieldMode :=  sfmDoublePlane;
      end;



      if FCurrentStep = 4100 then
      begin
        FStarFieldMode :=  sfmWormHoleMoving;
      end;

      if FCurrentStep > 5068 then
      begin
        v := (clrWhite.Red * FDeltaAlpha) div 32;
        BackColor.Create(v,v,v,v);
        FbackBuffer.Clear(BackColor);
        inc(FDeltaAlpha);
      end;

      Inc(FCurrentStep);
      if (FCurrentStep > MaxStep) then
      begin
        Inc(FCurrentScenePart);
        FCurrentStep := 0;
        for i:=0 to 255 do
        begin
           FCtab[i]:=round(System.Cos(cPI*i/128)*195);
           FStab[i]:=round(System.Sin(cPI*i/128)*95);

        end;
        for i:=0 to 449 do FSintab[i]:=round(System.Sin(c2PI*i/360)*255);

        FWormX := 30;
        FWormY := 20;
        FWormXSt:=2;
        FWormYSt:=3;
        FTempAlphaA := 0;
        FThresOld := 0;
      end;
    end;
    2 :
    begin
      MaxStep := 1700;

      if FDeltaAlpha>0 then
      begin
        v := (clrWhite.Red * FDeltaAlpha) div 32;
        BackColor.Create(v,v,v,v);
        FbackBuffer.Clear(BackColor);
        Dec(FDeltaAlpha);
      end else FBackBuffer.Clear(clrBlack);

      if FCurrentStep < 200 then DoWormHolePixel;

      if FCurrentStep >= 200 then
      begin
          DoWormHoleB(False);
          if FCurrentStep>350 then
          begin
            if FThresOld<156 then
            begin
              DrawSpiralScrollText(460,300,0,4,6.55,28,12,FThresOld);
              if ((FCurrentStep mod 7) = 0) then inc(FThresOld);
            end;
          end;
          k := 0;
          if FCurrentStep < 1000 then while (k<2550000) do Inc(k);
        end;

      Inc(FCurrentStep);
      if (FCurrentStep > MaxStep) then
      begin
        Inc(FCurrentScenePart);
        Fimg02.LoadFromFile('data\images\wormhole4.png');
        FTextureMaxSize.Create(FImg02.MaxWidth, FImg02.MaxHeight);
        FDeltaAlpha := 0;
        FCurrentStep := 0;
      end;
    end;
    3 :
    begin
      MaxStep := 700;
      if FCurrentStep < 300 then
      begin
         FbackBuffer.ColorFilter.AdjustBrightness(0.65);
      end
      else FBackBuffer.Clear(BZColor(22,26,20));
      DoRenderTunnelMapping(newTime);
      if FCurrentStep > 400 then
      begin
        //if ((FCurrentStep div 4) = 0) then
        if (FMinConeSize<48) then inc(FMinConeSize);

        if (FCurrentStep > 448) then
        begin
          if FDeltaAlpha<255 then
          begin
            v := (clrWhite.Red * FDeltaAlpha) div 32;
            BackColor.Create(v,v,v,v);
            FbackBuffer.Clear(BackColor);
            Inc(FDeltaAlpha);
          end;
        end;
      end;
      Inc(FCurrentStep);
      if (FCurrentStep > MaxStep) then
      begin
        FCurrentScenePart:=1;
        Inc(FCurrentScene);
        FCurrentStep := 0;
        FImg01.LoadFromFile('data\images\Austinpowers-king-oberon.png');
      end;
    end;
  end;
end;

procedure TMainForm.GenerateCircleMap(Bmp : TBZBitmap; aWidth : Byte; aColor : TBZColor);
Var
  x,y : Integer;
  Radius : Single;
  modulo : Integer;
begin
  for y :=-Bmp.CenterY to Bmp.CenterY do
  begin
    for x := -Bmp.CenterX to Bmp.CenterX do
    begin
       Radius := System.Sqrt(x*x+y*y);
       Modulo := Round(Abs(Radius)) mod aWidth;
       if Modulo > (aWidth div 2) then
         Bmp.SetPixel(Bmp.CenterX + X, Bmp.CenterY + Y, aColor)
    end;
  end;
  Bmp.BlurFilter.GaussianSplitBlur(2);
end;

procedure TMainForm.RenderCircleInterference(DeltaTime : Double);
// Coeur du rendu de la scene
var
  x1,y1,x2,y2,cx,cy : Integer;
  v: Single;
begin
 // FDisplayBuffer.Clear(clrBlack);

  cx := FBackBuffer.CenterX;
  cy := FBackBuffer.CenterY;

	x1 := cX + Round(cX * BZMath.Cos(DeltaTime));
	y1 := cY + Round(cY * BZMath.Sin(DeltaTime * 1.3));

  v:= DeltaTime + 1.33;
  x2 := cX + Round(cX * BZMath.Cos(-v));
  y2 := cY + Round(cY * BZMath.Sin(v * 2.3));

  FBackBuffer.PutImage(FCircleBufferA,x1,y1,FBackBuffer.Width, FBackBuffer.Height,0,0,dmSet,amNone);
  FBackBuffer.PutImage(FCircleBufferB,x2,y2,FBackBuffer.Width, FBackBuffer.Height,0,0,dmCombine,amNone,255,cmXOR);
end;

procedure TMainForm.RenderScene_07(deltaTime, newTime : Double);
Var
  BackColor : TBZColor;
  V : Byte;
  MaxStep : Word;
begin

  Case FCurrentScenePart of
    1 :
    begin
      MaxStep := 1200;
      if FDeltaAlpha>0 then
      begin
        v := (clrWhite.Red * FDeltaAlpha) div 32;
        BackColor.Create(v,v,v,v);
        FbackBuffer.Clear(BackColor);
        Dec(FDeltaAlpha);
      end;
      RenderCircleInterference(newTime);


      //Austim  Scroll down to top

      //ScrollText DCYP : ....It's nickedick, isn't it ?   Now it's the time of disco !   Take your mojo with you !.....



      Inc(FCurrentStep);
      if (FCurrentStep > MaxStep) then
      begin
        Inc(FCurrentScenePart);
        Inc(FCurrentScene);
        FCurrentStep := 0;
      end;
    end;
    2 :; //ShadeBlobs
    3:// Anim Twist Big Brother is watching you ---> 1984 wasn't.. Morph text 1984 to 2019 ---> Truth, is the new hate speech
  end;
end;

//procedure TMainForm.RenderScene_08(deltaTime, newTime : Double);
//1) Coopers and keifrens + ScrollText sin-z
//2) MetaBalls
//3) Plasma Rasta far hi !!!

//procedure TMainForm.RenderScene_09(deltaTime, newTime : Double);
//1) Lens
//2) RotoZoom
//3) Fire + circular scroll text

//procedure TMainForm.RenderScene_10(deltaTime, newTime : Double);
//1) Image : vari_tikru : apparation blocks aleatoires
//2) Greetings part --> DropWave FX + Logo Group deformé scroll de droite a gauche, gauche droite <->
//3) Credits --> 2D Bump fx
//4) BZScene image + circular deformation

procedure TMainForm.RenderScene(deltaTime, newTime : Double);
begin
  //ShowMessage('Run Scene : '+ FCurrentScene.ToString);
  Case FCurrentScene of
    1 : RenderScene_01(DeltaTime, NewTime);
    2 : RenderScene_02(DeltaTime, NewTime);
    3 : RenderScene_03(DeltaTime, NewTime);
    4 : RenderScene_04(DeltaTime, NewTime);
    5 : RenderScene_05(DeltaTime, NewTime);
    6 : RenderScene_06(DeltaTime, NewTime);
    7 : RenderScene_07(DeltaTime, NewTime);
  end;
end;

end.

