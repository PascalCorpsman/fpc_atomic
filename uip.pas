(******************************************************************************)
(* uip.pas                                                         ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Unit to get all Adapternames and their assosiated            *)
(*               IP-Addresses                                                 *)
(*                                                                            *)
(* Inspired by : https://forum.lazarus.freepascal.org/index.php?topic=24506.0 *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*                                                                            *)
(******************************************************************************)

Unit uip;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Type
  TIPAddress = Array[0..3] Of Byte;

  TNetworkAdapter = Record
    IpAddress: String; // xxx.xxx.xxx.xxx -> IPV4
    SubnetMask: String; // xxx.xxx.xxx.xxx -> IPV4
    AdapterName: String;
  End;

  TNetworkAdapterList = Array Of TNetworkAdapter;

  (*
   * Call this to get all local Adapternames and their IPV-4 addresses
   *)
Function GetLocalIPs(): TNetworkAdapterList;

(*
 * Calculates from a given Address and Subnet Mask the Broadcast address for the net
 * e.g. 192.168.168.1, 255.255.255.0 -> 192.168.168.255
 * !! Attention !!
 * this works only with IPV-4 addresses
 *)
Function CalculateBroadCastAddressFromAddress(Address, SubnetMask: String): String;

(*
 * Converts a IPV-4 String into TIPAddress, also doing some checks!
 *)
Function StrToIPAddress(Value: String): TIPAddress;

Function IsNetworkPresent(): Boolean;

Implementation

Uses
  process, StrUtils;

Function StrToIPAddress(Value: String): TIPAddress;
Var
  t: TStringArray;
  i: Integer;
Begin
  t := value.Split('.');
  If length(t) <> 4 Then Raise Exception.Create('Invalid ip:' + Value);
  For i := 0 To 3 Do Begin
    If Not (strtointdef(t[i], -1) In [0..255]) Then Raise Exception.Create('Invalid ip:' + Value);
    result[i] := strtointdef(t[i], -1);
  End;
End;

Function IsNetworkPresent: Boolean;
Var
  blub: TNetworkAdapterList;
Begin
  blub := GetLocalIPs();
  result := assigned(blub);
  If length(blub) = 1 Then Begin
    If blub[0].IpAddress = '127.0.0.1' Then result := false;
  End;
  setlength(blub, 0);
End;

Function GetLocalIPs: TNetworkAdapterList;
Var
  sl: TStringList;
  responce, tmp: String;
{$IFDEF Windows}
  j,
{$ENDIF}
  i: Integer;
Begin
  result := Nil;
  sl := TStringList.Create;
{$IFDEF Linux}
  RunCommand('ifconfig', [], responce, [poWaitOnExit]);
{$ENDIF}
{$IFDEF Windows}
  RunCommand('ipconfig', [], responce, [poNoConsole, poWaitOnExit]);
{$ENDIF}
  sl.Text := responce;
  For i := 1 To sl.Count - 1 Do Begin
{$IFDEF Linux}
    If pos('inet ', sl[i]) <> 0 Then Begin
      setlength(result, high(result) + 2);

      tmp := copy(sl[i], pos('inet ', sl[i]) + length('inet '), length(sl[i]));
      result[high(result)].IpAddress := copy(tmp, 1, pos(' ', tmp) - 1);

      tmp := copy(sl[i], pos('netmask ', sl[i]) + length('netmask '), length(sl[i])) + ' ';
      result[high(result)].SubnetMask := copy(tmp, 1, pos(' ', tmp) - 1);

      // TODO: Maybe implement a more "Robust" method to get the adapter name ??
      result[high(result)].AdapterName := copy(sl[i - 1], 1, pos(':', sl[i - 1]) - 1);
    End;
{$ENDIF}
{$IFDEF Windows}
    If pos('IPv4-', sl[i]) <> 0 Then Begin
      setlength(result, high(result) + 2);

      tmp := copy(sl[i], pos(':', sl[i]) + 1, length(sl[i]));
      result[high(result)].IpAddress := trim(tmp);

      // TODO: Maybe implement a more "Robust" method to get the subnet mask ??
      tmp := copy(sl[i + 1], pos(':', sl[i]) + 1, length(sl[i]));
      result[high(result)].SubnetMask := tmp;

      result[high(result)].AdapterName := 'Could not resolve adapter name';
      For j := i Downto 1 Do Begin
        If trim(sl[j]) = '' Then Begin
          tmp := sl[j - 1];
          tmp := copy(tmp, 1, pos(':', tmp) - 1);
          If tmp <> '' Then Begin
            result[high(result)].AdapterName := tmp;
          End;
          break;
        End;
      End;
    End;
{$ENDIF}
  End;
  sl.free;
End;

Function CalculateBroadCastAddressFromAddress(Address, SubnetMask: String): String;
Var
  BroadcastAddr, Addr, Mask: TIPAddress;
  M: String;
  b: Boolean;
  i: Integer;
Begin
  Addr := StrToIPAddress(Address);
  Mask := StrToIPAddress(SubnetMask);
  // Check if subnet mask is valid
  m := inttobin(mask[0], 8) + inttobin(mask[1], 8) + inttobin(mask[2], 8) + inttobin(mask[3], 8);
  If m[1] <> '1' Then Raise Exception.Create('Invalid subnet mask:' + SubnetMask);
  b := true;
  For i := 1 To length(m) Do Begin
    If b Then Begin
      If m[i] <> '1' Then b := false;
    End
    Else Begin
      If m[i] <> '0' Then Begin
        Raise Exception.Create('Invalid subnet mask:' + SubnetMask);
      End;
    End;
  End;
  // Calculate the Broadcast address
  For i := 0 To 3 Do Begin
    BroadcastAddr[i] := Addr[i] Or (Not Mask[i]);
  End;
  result := format('%d.%d.%d.%d', [BroadcastAddr[0], BroadcastAddr[1], BroadcastAddr[2], BroadcastAddr[3]]);
End;

End.

