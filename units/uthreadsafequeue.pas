(******************************************************************************)
(*                                                                            *)
(* Author      : AI Assistant + Corpsman Base Code                           *)
(*                                                                            *)
(* This file is part of FPC_Atomic                                            *)
(*                                                                            *)
(* Description : Thread-safe generic queue for inter-thread communication     *)
(*                                                                            *)
(******************************************************************************)
Unit uthreadsafequeue;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, SyncObjs;

Type
  { TThreadSafeQueue - Generic thread-safe queue }
  Generic TThreadSafeQueue<T> = Class
  private
    fQueue: Array Of T;
    fLock: TCriticalSection;
    fCount: Integer;
  public
    Constructor Create;
    Destructor Destroy; override;
    
    Procedure Enqueue(Const Item: T);
    Function Dequeue(Out Item: T): Boolean;
    Function Count: Integer;
    Procedure Clear;
  End;

Implementation

{ TThreadSafeQueue }

Constructor TThreadSafeQueue.Create;
Begin
  Inherited Create;
  fLock := TCriticalSection.Create;
  fQueue := Nil;
  fCount := 0;
End;

Destructor TThreadSafeQueue.Destroy;
Begin
  Clear;
  fLock.Free;
  Inherited Destroy;
End;

Procedure TThreadSafeQueue.Enqueue(Const Item: T);
Begin
  fLock.Enter;
  Try
    SetLength(fQueue, fCount + 1);
    fQueue[fCount] := Item;
    Inc(fCount);
  Finally
    fLock.Leave;
  End;
End;

Function TThreadSafeQueue.Dequeue(Out Item: T): Boolean;
Var
  i: Integer;
Begin
  Result := False;
  fLock.Enter;
  Try
    If fCount > 0 Then Begin
      Item := fQueue[0];
      // Shift array
      For i := 1 To fCount - 1 Do
        fQueue[i - 1] := fQueue[i];
      Dec(fCount);
      SetLength(fQueue, fCount);
      Result := True;
    End;
  Finally
    fLock.Leave;
  End;
End;

Function TThreadSafeQueue.Count: Integer;
Begin
  fLock.Enter;
  Try
    Result := fCount;
  Finally
    fLock.Leave;
  End;
End;

Procedure TThreadSafeQueue.Clear;
Begin
  fLock.Enter;
  Try
    SetLength(fQueue, 0);
    fCount := 0;
  Finally
    fLock.Leave;
  End;
End;

End.

