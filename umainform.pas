Unit umainform;

{$mode objfpc}{$H+}

{$DEFINE DEBUG}

Interface

Uses
  Classes, Sysutils, Fileutil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Spin,
  BZCadencer, BZSound, BZOpenALManager,
  BZSoundSample, BZSoundFileWAV,  BZSoundFileMP3 , BZSoundFileOGG , BZSoundFileModplug, uinfoform;

Type

  { TMainForm }

  TMainForm = Class(Tform)
    Button1 : Tbutton;
    Button2 : Tbutton;
    Button3 : Tbutton;
    Button4: TButton;
    Button5: TButton;
    chkSoundEnv : Tcheckbox;
    chkDirectFilter : Tcheckbox;
    ComboBox1: TComboBox;
    Combobox2 : Tcombobox;
    FloatSpinEdit1: TFloatSpinEdit;
    FloatSpinEdit2: TFloatSpinEdit;
    FloatSpinEdit3: TFloatSpinEdit;
    Groupbox1 : Tgroupbox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1 : Tlabel;
    Label11: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label2 : Tlabel;
    Label3 : Tlabel;
    Label4 : Tlabel;
    Label5 : Tlabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lblDuration : Tlabel;
    lblNbBits : Tlabel;
    lblNbChannels : Tlabel;
    lblFrequency : Tlabel;
    lblFileName : Tlabel;
    OpenDialog : Topendialog;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    TrackBar1: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    Procedure Button1click(Sender : Tobject);
    Procedure Button2click(Sender : Tobject);
    Procedure Button3click(Sender : Tobject);
    Procedure Button4click(Sender : Tobject);
    Procedure Button5click(Sender : Tobject);
    Procedure Chkdirectfilterclick(Sender : Tobject);
    Procedure Chksoundenvclick(Sender : Tobject);
    Procedure Combobox2change(Sender : Tobject);
    Procedure Formcreate(Sender : Tobject);
    Procedure Formdestroy(Sender : Tobject);
    Procedure Formshow(Sender : Tobject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
  Private

  Public
    SoundManager : TBZSoundOpenALManager;
    SoundLibrary : TBZSoundLibrary;
    SoundFXLibrary : TBZSoundFXLibrary;
    SoundSources : TBZSoundSources;
    SoundSource  : TBZSoundSource;
    Cadencer : TBZCadencer;
    ReverbFX : TBZReverbSoundEffect;
  End;

Var
  MainForm : TMainForm;

Implementation

{$R *.lfm}

{ TMainForm }

Procedure TMainForm.Formcreate(Sender : Tobject);
Begin
 // SoundManager := TBZOpenAL.Create(Self);
  //Cadencer := TBZCadencer.Create(self);
  //Cadencer.Enabled := False;
  //SoundManager.Cadencer := Cadencer;
  //SoundLibrary := TBZSoundLibrary.Create(self);

  //SoundManager.Sources.Add;
  //SoundManager.Sources.Items[0].SoundLibrary := SoundLibrary;

  //vActiveSoundManager := SoundManager
End;

Procedure TMainForm.Button2click(Sender : Tobject);
Begin

  SoundManager.Sources.Items[TButton(Sender).Tag].Playing := not(SoundManager.Sources.Items[TButton(Sender).Tag].Playing);
  Cadencer.Enabled := SoundManager.Sources.Items[TButton(Sender).Tag].Playing;
  //Timer1.Enabled:=SoundManager.Sources.Items[TButton(Sender).Tag].Playing;
  //SoundManager.UpdateSources;
End;

Procedure TMainForm.Button1click(Sender : Tobject);
Begin

  if OpenDialog.Execute then
  begin
     Cadencer.Enabled := False;
    SoundLibrary.Samples.Clear;
    SoundLibrary.Samples.AddFile(OpenDialog.FileName,ExtractFileName(OpenDialog.FileName));
    SoundManager.Sources.Items[0].SoundName := ExtractFileName(OpenDialog.FileName);
    //SoundManager.Sources.Items[0].UseEnvironnment := true;

    lblFileName.Caption := SoundManager.Sources.Items[0].SoundName;
    lblFrequency.Caption := InttoStr(SoundManager.Sources.Items[0].Sample.Data.Frequency)+' Mhz';
    lblNbChannels.Caption := InttoStr(SoundManager.Sources.Items[0].Sample.Data.NbChannels);
    lblNbBits.Caption := InttoStr(SoundManager.Sources.Items[0].Sample.Data.BitsPerSample);
    lblDuration.Caption := FloatToStr(SoundManager.Sources.Items[0].Sample.Data.LengthInSec)+' Sec';
    ProgressBar1.Max := SoundManager.Sources.Items[0].Sample.Data.LengthInBytes;
    //ProgressBar1.Position :=ProgressBar1.Max div 2;
    //showmessage(InttoStr(SoundManager.Sources.Items[0].Sample.LengthInBytes));
    Timer1.Enabled:=True;
    //InttoStr(SoundManager.Sources.Items[0].Sample.Data.getSoundDataSize);
    //
   // SoundManager.Active := True;
  End;
End;

Procedure TMainForm.Button3click(Sender : Tobject);
Begin
  SoundManager.Sources.Items[TButton(Sender).Tag].Pause := not(SoundManager.Sources.Items[TButton(Sender).Tag].Pause);
End;

Procedure TMainForm.Button4click(Sender : Tobject);
Begin
  SMInfosForm.Show;
End;

Procedure TMainForm.Button5click(Sender : Tobject);
Begin
  SoundManager.Sources.Items[0].Pitch:=1.0;
  TrackBar4.Position:= 100;
End;

Procedure TMainForm.Chkdirectfilterclick(Sender : Tobject);
Begin
  SoundManager.Sources.Items[0].DirectFilterActivated := Chkdirectfilter.Checked;
End;

Procedure TMainForm.Chksoundenvclick(Sender : Tobject);
Begin
  SoundManager.Sources.Items[0].AuxSlots.Items[0].Activated := ChkSoundEnv.Checked ;
End;

Procedure TMainForm.Combobox2change(Sender : Tobject);
Begin
  Case Combobox2.ItemIndex of
    0 : SoundManager.Environment := seGeneric;
    1 : SoundManager.Environment := seGeneric;
    2 : SoundManager.Environment := sePaddedCell;
    3 : SoundManager.Environment := seRoom;
    4 : SoundManager.Environment := seBathroom;
    5 : SoundManager.Environment := seLivingRoom;
    6 : SoundManager.Environment := seStoneroom;
    7 : SoundManager.Environment := seAuditorium;
    8 : SoundManager.Environment := seConcertHall;
    9 : SoundManager.Environment := seCave;
    10 : SoundManager.Environment := seArena;
    11 : SoundManager.Environment := seHangar;
    12 : SoundManager.Environment := seCarpetedHallway;
    13 : SoundManager.Environment := seHallway;
    14 : SoundManager.Environment := seStoneCorridor;
    15 : SoundManager.Environment := seAlley;
    16 : SoundManager.Environment := seForest;
    17 : SoundManager.Environment := seCity;
    18 : SoundManager.Environment := seMountains;
    19 : SoundManager.Environment := seQuarry;
    20 : SoundManager.Environment := sePlain;
    21 : SoundManager.Environment := seParkingLot;
    22 : SoundManager.Environment := seSewerPipe;
    23 : SoundManager.Environment := seUnderWater;
    24 : SoundManager.Environment := seDrugged;
    25 : SoundManager.Environment := seDizzy;
    26 : SoundManager.Environment := sePsychotic;
  End;

End;

Procedure TMainForm.Formdestroy(Sender : Tobject);
Begin
 Cadencer.Enabled := False;
 FreeAndNil(SoundManager);
 FreeAndNil(SoundLibrary);
 FreeAndNil(Cadencer);
End;

Procedure TMainForm.Formshow(Sender : Tobject);
Begin
  SoundManager :=  TBZSoundOpenALManager.Create(Self);
  Cadencer := TBZCadencer.Create(self);
  Cadencer.Enabled := False;
  SoundManager.Cadencer := Cadencer;
  SoundLibrary := TBZSoundLibrary.Create(self);
  SoundFXLibrary := TBZSoundFXLibrary.Create(self);

  SoundManager.Sources.Add;
  SoundManager.Sources.Items[0].SoundLibrary := SoundLibrary;
  SoundManager.Sources.Items[0].SoundFXLibrary := SoundFXLibrary;
  //ShowMessage('Lib FX Count : '+IntToStr(SoundFXLibrary.FX.Count));
  with SoundFXLibrary.FX.AddFilterLowPass('Custom_LP_Filter') Do
  begin
    Gain := 0.3;
    GainHF := 0.3;
  end;
  //ShowMessage('Lib FX Count : '+IntToStr(SoundFXLibrary.FX.Count));
  SoundManager.Sources.Items[0].DirectFilter := 'Custom_LP_Filter';
  SoundManager.Sources.Items[0].DirectFilterActivated := true;

  with SoundFXLibrary.FX.AddEffectEAXReverb('EAX_Reverb_Test') Do
  begin
    preset := seConcertHall;
  end;

  With SoundManager.Sources.Items[0].AuxSlots.Add do
  begin
    SoundFXLibrary := Self.SoundFXLibrary;
    Name := 'EAX_Reverb_Test';
    Activated := False;
  end;

(*  SoundManager.Sources.Items[0].DirectFilteringType := ftLowPass;
  TBZLowPassSoundFilter(SoundManager.Sources.Items[0].DirectFilter).Gain:=0.5;
  TBZLowPassSoundFilter(SoundManager.Sources.Items[0].DirectFilter).GainHF:=0.5;

  ReverbFX := TBZReverbSoundEffect.Create(SoundManager.Sources.Items[0].Effects);
  SoundManager.Sources.Items[0].Effects.AddEffect(ReverbFX);
  SoundManager.Sources.Items[0].ActiveEffects:=true; *)

  OpenDialog.Filter := 'Tous les fichier audio|*.wav;*.ogg;*.mp3;*.mod;*.s3m;*.xm;*.it|Microsoft WAV File Format|*.wav|OGG Vorbis File Format|*.ogg|MP3 File Format|*.mp3|Module Tracker File Format|*.mod;*.s3m;*.xm;*.it';
  Cadencer.Enabled := True;
  SoundManager.Active := True;
  SMInfosForm.Memo1.Lines.Add(SoundManager.getInformations);
End;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
//  if SoundManager.Sources.Items[0].Playing then
 // alGetSourcef(SoundManager.Sources.Items[0].ManagerTag, AL_SEC_OFFSET, tp);
//  ProgressBar1.Position := round(tp*SoundManager.Sources.Items[0].Sample.Data.Sampling.Frequency);
  ProgressBar1.Position:=SoundManager.Sources.Items[0].TimePositionInByte;

end;

procedure TMainForm.TrackBar1Change(Sender: TObject);
begin
  SoundManager.Sources.Items[TTrackBar(Sender).Tag].Volume:=TTrackBar(Sender).Position / 100;
end;

procedure TMainForm.TrackBar3Change(Sender: TObject);
begin
  SoundManager.MasterVolume:= TrackBar3.position / 100;
end;

procedure TMainForm.TrackBar4Change(Sender: TObject);
begin
  SoundManager.Sources.Items[TTrackBar(Sender).Tag].Pitch:=TTrackBar(Sender).Position / 100;
end;

End.

