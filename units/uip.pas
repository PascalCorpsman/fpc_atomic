(******************************************************************************)
(* uip.pas                                                         ??.??.???? *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
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
(*               0.02 - Add Darwin support by Pavel Zverina                   *)
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
  process, StrUtils
{$IFDEF Windows}
  , math
{$ENDIF}
  ;

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
  j, k: Integer;
{$ENDIF}
  i: Integer;
Begin
  result := Nil;
  sl := TStringList.Create;
{$IFDEF Linux}
  RunCommand('ifconfig', [], responce, [poWaitOnExit]);
{$ENDIF}
{$IFDEF Darwin}
  // macOS uses ifconfig similar to Linux
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
{$IFDEF Darwin}
    // macOS ifconfig format: "inet 192.168.1.100 netmask 0xffffff00 broadcast 192.168.1.255"
    // or: "	inet 192.168.0.72 netmask 0xffffff00 broadcast 192.168.0.255"
    If (pos('inet ', sl[i]) <> 0) And (pos('inet6 ', sl[i]) = 0) Then Begin
      // Extract IP address (skip "inet " and get the first word)
      tmp := trim(sl[i]);
      tmp := copy(tmp, pos('inet ', tmp) + length('inet '), length(tmp));
      tmp := copy(tmp, 1, pos(' ', tmp) - 1);
      // Skip loopback interface (127.0.0.1)
      If (tmp <> '127.0.0.1') And (tmp <> '') Then Begin
        setlength(result, high(result) + 2);
        result[high(result)].IpAddress := tmp;

        // Extract netmask (can be in hex format like 0xffffff00 or decimal)
        tmp := sl[i];
        If pos('netmask ', tmp) <> 0 Then Begin
          tmp := copy(tmp, pos('netmask ', tmp) + length('netmask '), length(tmp));
          tmp := copy(tmp, 1, pos(' ', tmp) - 1);
          // If netmask is in hex format (0x...), we could convert it, but for now just store as is
          // Most common: 0xffffff00 = 255.255.255.0
          result[high(result)].SubnetMask := tmp;
        End
        Else Begin
          result[high(result)].SubnetMask := '255.255.255.0'; // Default fallback
        End;

        // Get adapter name from previous line (format: "en0: flags=..." or "en0:")
        If i > 0 Then Begin
          tmp := trim(sl[i - 1]);
          If pos(':', tmp) <> 0 Then Begin
            tmp := copy(tmp, 1, pos(':', tmp) - 1);
            result[high(result)].AdapterName := trim(tmp);
          End
          Else Begin
            result[high(result)].AdapterName := 'unknown';
          End;
        End
        Else Begin
          result[high(result)].AdapterName := 'unknown';
        End;
      End;
    End;
{$ENDIF}
{$IFDEF Windows}
    // Windows ipconfig format can be:
    // "   IPv4 Address. . . . . . . . . . . : 192.168.1.100"
    // or localized versions like "   IPv4-Adresse  . . . . . . . . . : 192.168.1.100"
    // Look for "IPv4" followed by "Address" or "Adresse" (German) or similar
    If (pos('IPv4', sl[i]) <> 0) And ((pos('Address', sl[i]) <> 0) Or (pos('Adresse', sl[i]) <> 0)) Then Begin
      setlength(result, high(result) + 2);

      // Extract IP address - it's after the last colon
      tmp := sl[i];
      j := length(tmp);
      // Find last colon
      While (j > 0) And (tmp[j] <> ':') Do
        dec(j);
      If j > 0 Then Begin
        tmp := copy(tmp, j + 1, length(tmp));
        result[high(result)].IpAddress := trim(tmp);
        // Skip loopback interface
        If result[high(result)].IpAddress = '127.0.0.1' Then Begin
          setlength(result, high(result));
          Continue;
        End;
      End
      Else Begin
        setlength(result, high(result));
        Continue;
      End;

      // Try to find subnet mask in next few lines
      result[high(result)].SubnetMask := '255.255.255.0'; // Default fallback
      For j := i + 1 To min(i + 5, sl.Count - 1) Do Begin
        If (pos('Subnet Mask', sl[j]) <> 0) Or (pos('Subnetmaske', sl[j]) <> 0) Then Begin
          tmp := sl[j];
          k := length(tmp);
          While (k > 0) And (tmp[k] <> ':') Do
            dec(k);
          If k > 0 Then Begin
            tmp := copy(tmp, k + 1, length(tmp));
            result[high(result)].SubnetMask := trim(tmp);
          End;
          break;
        End;
      End;

      // Find adapter name - look backwards for adapter name (usually before "IPv4" line)
      result[high(result)].AdapterName := 'unknown';
      For j := i Downto max(1, i - 10) Do Begin
        If trim(sl[j]) = '' Then Begin
          If j > 0 Then Begin
            tmp := trim(sl[j - 1]);
            // Adapter name is usually on a line ending with ":"
            If (pos(':', tmp) <> 0) And (pos('IPv4', tmp) = 0) Then Begin
              tmp := copy(tmp, 1, pos(':', tmp) - 1);
              If tmp <> '' Then Begin
                result[high(result)].AdapterName := trim(tmp);
              End;
              break;
            End;
          End;
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

