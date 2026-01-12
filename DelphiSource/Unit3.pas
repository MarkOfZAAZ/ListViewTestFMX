unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.Controls.Presentation, FMX.StdCtrls, ListViewZaaz, FMX.DialogService,
  FMX.Objects;

type
  TForm3 = class(TForm)
    ListView1: TListViewZaaz;
    StyleBook2: TStyleBook;
    StyleBook1: TStyleBook;
    imgEdit: TImage;
    imgPdf: TImage;
    procedure FormCreate(Sender: TObject);
    procedure ListView1ItemClickEx(const Sender: TObject; ItemIndex: Integer;
      const LocalClickPos: TPointF; const ItemObject: TListItemDrawable);
    procedure ListView1DeletingItem(Sender: TObject; AIndex: Integer;
      var ACanDelete: Boolean);
  private
    { Private declarations }
    procedure PopulateList;
    procedure EditItem(const AIndex: Integer);
    procedure ConfirmDelete(const AIndex: integer);
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.ConfirmDelete(const AIndex: integer);
begin
  // this is where we ask the user to confirm the delete
  if (AIndex < 0) or (AIndex >= ListView1.Items.Count) then
    Exit;

  var JobNo := ListView1.Items[AIndex].Tag;

  TDialogService.MessageDialog(
    Format('Delete Job "%d" ?', [JobNo]),
    TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
    TMsgDlgBtn.mbNo,
    0,
    procedure(const AResult: TModalResult)
    begin
      if AResult = mrYes then
      begin
        // Manual delete after confirmation
        if (AIndex >= 0) and (AIndex < ListView1.Items.Count) then
          ListView1.Items.Delete(AIndex);
      end;
      ListView1.Selected := nil;
      ListView1.SetFocus;
    end
  );
end;

procedure TForm3.EditItem(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= ListView1.Items.Count) then
    Exit;
  ShowMessage('Edit item: ' + ListView1.Items[AIndex].Text);
end;

procedure TForm3.FormCreate(Sender: TObject);
begin

  PopulateList;
end;

procedure TForm3.ListView1DeletingItem(Sender: TObject; AIndex: Integer;
  var ACanDelete: Boolean);
begin
  // Always cancel automatic deletion
  ACanDelete := False;
  ConfirmDelete(AIndex);
end;

procedure TForm3.ListView1ItemClickEx(const Sender: TObject; ItemIndex: Integer;
  const LocalClickPos: TPointF; const ItemObject: TListItemDrawable);
begin
  // Trigger only when accessory (>) is tapped
  if ItemObject is TListItemAccessory then
  begin
    ShowMessage(
      'Accessory clicked for ' + ListView1.Items[ItemIndex].Text
    );
  end;
end;

procedure TForm3.PopulateList;
var
  I: Integer;
begin
  ListView1.BeginUpdate;
  try
    ListView1.Items.Clear;
    for I := 1 to 150 do
    begin
      with ListView1.Items.Add do
      begin
        Tag := I;
        Data['lvoJobno'] := Format('%.*d', [5, I]);
        Data['lvoCustomer'] := Format('Customer %d', [I]);
        Data['lvoJobtype'] := Format('Job type %d', [I]);
        Data['lvoJobdate'] := DateToStr(Now);
        Data['lvoPdf'] := imgPdf.Bitmap;
        Data['lvoEdit'] := imgEdit.Bitmap;
      end;
    end;
  finally
    ListView1.EndUpdate;
  end;
end;

end.
