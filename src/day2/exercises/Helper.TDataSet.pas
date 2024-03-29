﻿unit Helper.TDataSet;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.RTTI,
  System.Variants,
  System.Generics.Collections,
  Spring.Collections,
  Data.DB;

type
  TDataSetHelper = class helper for TDataSet
  private const
    Version = '1.8';
  public
    /// <summary>
    ///   Iterates through the dataset and it's calling anonymous methods
    ///   (proc) for each row. Disables all UI notification and preserving
    ///   current dataset position.
    /// </summary>
    procedure WhileNotEof(proc: TProc);
    /// <summary>
    ///   Iterates through the dataset, clone "WhileNotEof" method.
    /// </summary>
    procedure ForEachRow(proc: TProc);
    /// <summary>
    ///   Iterates through the dataset and calculates maximum value of
    ///   the integer data field (TIntegerField) in all data rows.
    /// </summary>
    function GetMaxIntegerValue(const fieldName: string): integer;
    /// <summary>
    ///   Creates new TDataSource component assigned to this dataset.
    ///   The owner of TDataSource is this dataset.
    /// </summary>
    function CreateDataSource: TDataSource;
    /// <summary>
    ///   Iterates through base dataset and for each row creates new object
    ///   using generic class T provided through a generic parameter.
    ///   The attributes/fields in the newly created object are filled with
    ///   values from the data set. Default mapping is: dataset field name
    ///   have to equal to object attribute name. Different mapping can be
    ///   applied with Custom attribute "MappedToField".
    /// </summary>
    /// <exception cref="EInvalidMapping">
    ///   Exception <b>EInvalidMapping</b> is thrown when you provide invalid
    ///   mapping through MappedToField attribute, when filed name is not
    ///   found in dataset.
    /// </exception>
    /// <remarks>
    ///   To define custom mapping developer has to include unit
    ///   Attribute.MappedToField.pas in which attribute "MappedToField" is
    ///   defined. Sample mapping added above class field can look like:
    ///   `[DatabaseField('city')]`. For more mapping examples check sample code.
    /// </remarks>
    function LoadData<T: class, constructor>: IList<T>;
    procedure AppendRows(aRecordArray: TArray < TArray < Variant >> );
  end;

type
  DatabaseFieldAttribute = class(TCustomAttribute)
  private
    fFieldName: string;
  public
    constructor Create(const aFiedldName: string);
    property FieldName: string read fFieldName;
  end;

  EDataMapperError = class(Exception)
  end;

type
  TDataSetToObjectMapper = class
  private
    class function SetObjectField(const aDataset: TDataSet; aFieldName: string;
      const objectField: TRttiField; dest: TObject): boolean; static;
  public
    class procedure DataSetRowToObject(aDataset: TDataSet;
      aObject: TObject); static;
  end;

  TObjectToDataSetMapper = class
  strict private
    fNameOfChangedFlag: string;
    fAutoDetectNull: boolean;
    fDataSet: TDataSet;
    fObjectRttiTypeInfo: TRttiType;
    fRttiFields: TArray<TRttiField>;
    fIsChangedRttiField: TRttiField;
    fKeyDataFieldNames: TArray<TField>;
    function RttiFieldByName(const aFieldName: string): TRttiField;
  public
    constructor Create(const aDataset: TDataSet; const aObject: TObject;
      const aAutoDetectNull: boolean; const aNameOfChangedFlag: string);
    function IsObjectChanged(const aObject: TObject): boolean;
    procedure ObjectToDataSetRow(const aObject: TObject);
  end;

implementation

constructor DatabaseFieldAttribute.Create(const aFiedldName: string);
begin
  fFieldName := aFiedldName;
end;


// ----------------------------------------------------------------------
// TDataSetToObjectMapper
// ----------------------------------------------------------------------

class function TDataSetToObjectMapper.SetObjectField(const aDataset: TDataSet;
  aFieldName: string; const objectField: TRttiField; dest: TObject): boolean;
var
  valueOfField: TValue;
  field: TField;
begin
  field := aDataset.FindField(aFieldName);
  if field = nil then
    Result := False
  else
  begin
    if field is TBlobField then
      valueOfField := TValue.From((field as TBlobField).Value)
    else
      valueOfField := TValue.From(field.Value);
    objectField.SetValue(TObject(dest), valueOfField);
    Result := True;
  end;
end;

class procedure TDataSetToObjectMapper.DataSetRowToObject(aDataset: TDataSet;
  aObject: TObject);
var
  rttiType: TRttiType;
  RttiContext: TRttiContext;
  customAttr: TCustomAttribute;
  rttiField: TRttiField;
  customattribute: TCustomAttribute;
  fieldName: string;
  isDone: boolean;
begin
  rttiType := RttiContext.GetType(aObject.ClassType);
  for rttiField in rttiType.GetFields do
  begin
    fieldName := rttiField.Name;
    isDone := SetObjectField(aDataset, fieldName, rttiField, aObject);
    if not isDone then
    begin
      // Find CustomAttribute defined above the class field
      customAttr := nil;
      for customattribute in rttiField.GetAttributes do
        if customattribute is DatabaseFieldAttribute then
        begin
          customAttr := customattribute;
          break;
        end;
      // --------------------------------------------------------
      if customAttr <> nil then
      begin
        fieldName := (customAttr as DatabaseFieldAttribute).fieldName;
        if not SetObjectField(aDataset, fieldName, rttiField, aObject) then
          raise EDataMapperError.Create
            (Format('Invalid mapping defined for field "%s" in class %s',
            [rttiField.Name, rttiType.Name]));
      end;
    end;
  end;
end;

// ----------------------------------------------------------------------
// TObjectToDataSetMapper
// ----------------------------------------------------------------------

constructor TObjectToDataSetMapper.Create(const aDataset: TDataSet;
  const aObject: TObject; const aAutoDetectNull: boolean;
  const aNameOfChangedFlag: string);
var
  RttiContext: TRttiContext;
  count: integer;
  idx: integer;
  j: integer;
begin
  fDataSet := aDataset;
  fAutoDetectNull := aAutoDetectNull;
  fNameOfChangedFlag := aNameOfChangedFlag; // 'IsChanged'
  // --
  fObjectRttiTypeInfo := RttiContext.GetType(aObject.ClassType);
  fRttiFields := fObjectRttiTypeInfo.GetFields();
  fIsChangedRttiField := RttiFieldByName(fNameOfChangedFlag);
  if fIsChangedRttiField = nil then
    raise EDataMapperError.Create
      (Format('Expected field "%s" not found in object, is required to save data',
      [fNameOfChangedFlag]));
  fKeyDataFieldNames := [];
  count := 0;
  for idx := 0 to fDataSet.Fields.count - 1 do
    if (pfInKey in fDataSet.Fields[idx].ProviderFlags) then
      inc(count);
  if count = 0 then
    raise EDataMapperError.Create
      (Format('Expected primary key to be defined in dataset %s. Define it using TField.ProviderFlags.',
      [aDataset.Name]));
  SetLength(fKeyDataFieldNames, count);
  j := 0;
  for idx := 0 to fDataSet.Fields.count - 1 do
    if (pfInKey in fDataSet.Fields[idx].ProviderFlags) then
    begin
      fKeyDataFieldNames[j] := fDataSet.Fields[idx];
      inc(j);
    end;
end;

function TObjectToDataSetMapper.RttiFieldByName(const aFieldName: string)
  : TRttiField;
var
  i: integer;
  lowerName: string;
  rttiField: TRttiField;
  Attribute: TCustomAttribute;
  attrFieldName: string;
begin
  lowerName := aFieldName.ToLower();
  for i := 0 to High(fRttiFields) do
  begin
    rttiField := fRttiFields[i];
    if rttiField.Name.ToLower = lowerName then
      exit(rttiField);
    for Attribute in rttiField.GetAttributes do
      if Attribute is DatabaseFieldAttribute then
      begin
        attrFieldName := (Attribute as DatabaseFieldAttribute).fieldName;
        if lowerName = attrFieldName.ToLower() then
          exit(rttiField);
      end;
  end;
  Result := nil;
end;

function TObjectToDataSetMapper.IsObjectChanged(const aObject: TObject)
  : boolean;
begin
  Result := fIsChangedRttiField.GetValue(aObject).AsBoolean;
end;

procedure TObjectToDataSetMapper.ObjectToDataSetRow(const aObject: TObject);
var
  idx: integer;
  keyfields: string;
  values: TArray<Variant>;
  dbfieldname: string;
  rttiField: TRttiField;
  field: TField;
  Value: Variant;
begin
  //
  keyfields := '';
  values := [];
  for idx := 0 to High(fKeyDataFieldNames) do
  begin
    dbfieldname := fKeyDataFieldNames[idx].fieldName;
    rttiField := RttiFieldByName(dbfieldname);
    if rttiField = nil then
      raise Exception.Create('Error Message');
    Value := rttiField.GetValue(aObject).AsVariant;
    if Value <> Null and Value <> Unassigned then
    begin
      keyfields := IfThen(keyfields = '', dbfieldname, ';' + dbfieldname);
      values := values + [Value];
    end;
  end;
  if (Length(values) > 0) and fDataSet.Locate(keyfields, values, []) then
  begin
    // UPDATE
    for idx := 0 to fDataSet.Fields.count - 1 do
    begin
      field := fDataSet.Fields[idx];
      if not(pfInKey in field.ProviderFlags) then
      begin
        rttiField := RttiFieldByName(field.fieldName);
        if rttiField <> nil then
        begin
          if field is TBlobField then
            Value := rttiField.GetValue(aObject).AsType<TBytes>()
          else
          begin
            Value := rttiField.GetValue(aObject).AsVariant;
            if fAutoDetectNull and not(field.Required) then
            begin
              case field.DataType of
                ftDate, ftTime, ftDateTime:
                  if Value <= 0 then Value := Null;
                 ftSmallint, ftInteger, ftWord, ftAutoInc, ftFloat, ftCurrency, ftBCD:
                  if Value < 0 then Value := Null;
              end;
            end;
          end;
          if Value <> field.Value then
          begin
            fDataSet.Edit;
            field.Value := Value;
          end;
        end;
      end;
    end;
    if fDataSet.State <> dsBrowse then
      fDataSet.Post;
  end
  else
  begin
    // INSERT
    for idx := 0 to fDataSet.Fields.count - 1 do
    begin
      field := fDataSet.Fields[idx];
      rttiField := RttiFieldByName(field.fieldName);
      if rttiField <> nil then
      begin
        if field is TBlobField then
          Value := rttiField.GetValue(aObject).AsType<TBytes>()
        else
        begin
          Value := rttiField.GetValue(aObject).AsVariant;
          if fAutoDetectNull and not(field.Required) then
          begin
            case field.DataType of
              ftDate, ftTime, ftDateTime:
                if Value <= 0 then Value := Null;
               ftSmallint, ftInteger, ftWord, ftAutoInc, ftFloat, ftCurrency, ftBCD:
                if Value < 0 then Value := Null;
            end;
          end;
        end;
        if Value <> field.Value then
        begin
          if fDataSet.State <> dsInsert then
            fDataSet.Append;
          field.Value := Value;
        end;
      end;
    end;
    if fDataSet.State <> dsBrowse then
      fDataSet.Post;
  end;
end;

// ----------------------------------------------------------------------
// TDataSetHelper
// ----------------------------------------------------------------------

function TDataSetHelper.GetMaxIntegerValue(const fieldName: string): integer;
var
  MaxValue: integer;
  CurrentValue: integer;
begin
  MaxValue := 0;
  self.WhileNotEof(
    procedure
    begin
      CurrentValue := self.FieldByName(fieldName).AsInteger;
      if CurrentValue > MaxValue then
        MaxValue := CurrentValue;
    end);
  Result := MaxValue;
end;

procedure TDataSetHelper.WhileNotEof(proc: TProc);
var
  Bookmark: TBookmark;
begin
  Bookmark := self.GetBookmark;
  self.DisableControls;
  try
    self.First;
    while not self.Eof do
    begin
      proc();
      self.Next;
    end;
  finally
    if self.BookmarkValid(Bookmark) then
      self.GotoBookmark(Bookmark);
    self.FreeBookmark(Bookmark);
    self.EnableControls;
  end;
end;

function TDataSetHelper.CreateDataSource: TDataSource;
begin
  Result := TDataSource.Create(self);
  Result.DataSet := self;
end;

procedure TDataSetHelper.ForEachRow(proc: TProc);
begin
  WhileNotEof(proc);
end;

function TDataSetHelper.LoadData<T>: IList<T>;
var
  dataList: IList<T>;
  dataField: TField;
  obj: T;
begin
  dataList := TCollections.CreateObjectList<T>();
  WhileNotEof(
    procedure
    begin
      obj := T.Create();
      dataList.Add(obj);
      TDataSetToObjectMapper.DataSetRowToObject(self, obj);
    end);
  Result := dataList;
end;

procedure TDataSetHelper.AppendRows(aRecordArray: TArray < TArray <
  Variant >> );
var
  idxRow: integer;
  idxField: integer;
begin
  for idxRow := 0 to High(aRecordArray) do
  begin
    self.Append;
    for idxField := 0 to High(aRecordArray[idxRow]) do
    begin
      try
        self.Fields[idxField].Value := aRecordArray[idxRow][idxField];
      except
        on E: EDatabaseError do
        begin
          E.Message := E.Message + Format(' (Row nr:%d, Index of field:%d)',
            [idxRow + 1, idxField]);
          raise;
        end
      end;
    end;
    try
      self.Post;
    except
      on E: EDatabaseError do
      begin
        E.Message := E.Message + Format(' (Row nr:%d)', [idxRow + 1]);
        raise;
      end
    end;
  end;
end;

end.
