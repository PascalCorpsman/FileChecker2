(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of FileChecker2                                          *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit12;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm12 }

  TForm12 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ListBox1: TListBox;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
  private
    findex: integer;
  public
    Procedure Init(index: integer);

  End;

Var
  Form12: TForm12;

Implementation

{$R *.lfm}

Uses ufilechecker;

{ TForm12 }

Procedure TForm12.FormCreate(Sender: TObject);
Begin
  caption := 'Categories';
  edit1.text := '';
End;

Procedure TForm12.Button1Click(Sender: TObject);
Begin
  ListBox1.Items.Add(edit1.text);
End;

Procedure TForm12.Button2Click(Sender: TObject);
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    ListBox1.Items.Delete(ListBox1.ItemIndex);
  End;
End;

Procedure TForm12.Button4Click(Sender: TObject);
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    setlength(DataBase[findex].Categories, high(DataBase[findex].Categories) + 2);
    DataBase[findex].Categories[high(DataBase[findex].Categories)] := ListBox1.Items[ListBox1.ItemIndex];
    Init(findex);
    DBChanged := true;
  End;
End;

Procedure TForm12.Button5Click(Sender: TObject);
Var
  s: String;
  i, j: Integer;
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    s := ListBox1.Items[ListBox1.ItemIndex];
    For i := 0 To high(DataBase[findex].Categories) Do Begin
      If DataBase[findex].Categories[i] = s Then Begin
        For j := i To high(DataBase[findex].Categories) - 1 Do Begin
          DataBase[findex].Categories[j] := DataBase[findex].Categories[j + 1];
        End;
        SetLength(DataBase[findex].Categories, high(DataBase[findex].Categories));
        DBChanged := true;
        Init(findex);
        exit;
      End;
    End;
  End;
End;

Procedure TForm12.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Var
  i: Integer;
Begin
  Categories.Clear;
  For i := 0 To ListBox1.Items.Count - 1 Do Begin
    Categories.Add(ListBox1.Items[i]);
  End;
End;

Procedure TForm12.Init(index: integer);
Var
  i: Integer;
Begin
  findex := index;
  label4.caption := DataBase[index].Filename;
  label6.caption := DataBase[index].Categories.Join(', ');
  ListBox1.Clear;
  For i := 0 To Categories.Count - 1 Do Begin
    ListBox1.Items.Add(Categories[i]);
  End;
End;

End.

