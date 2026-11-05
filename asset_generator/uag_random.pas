(******************************************************************************)
(*                                                                            *)
(* This file is part of FPC_Atomic                                            *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
(*
 * Deterministic random numbers for the asset generator.
 *
 * Do not use Random / RandSeed of the RTL: its algorithm may change between
 * compiler versions and then a stored seed would produce a different image.
 *)
Unit uag_random;

{$MODE ObjFPC}{$H+}
{$MODESWITCH advancedrecords}
{$Q-}{$R-} // the hash and PCG rely on unsigned overflow

Interface

Type

  { TAGRandom: PCG32 (XSH RR), one stream per area / purpose }

  TAGRandom = Record
    State: QWord;
    Incr: QWord;
    Procedure Init(Seed: QWord; Stream: QWord = 54);
    Function Next: Cardinal;
    Function NextUnit: Double; // [0, 1)
    Function Range(lo, hi: Double): Double; // [lo, hi)
    Function RangeI(lo, hi: Integer): Integer; // [lo, hi], both included
    Function Chance(p: Double): Boolean;
  End;

Function FNV1a64(Const s: String; Basis: QWord = QWord($CBF29CE484222325)): QWord;
Function DeriveSeed(Seed: QWord; Const Name: String): QWord; // Seed for a sub area, e.g. DeriveSeed(Main, 'atomic/p3')
Function HashUnit(Seed: QWord; Const Key: String): Double; // [0, 1), depends only on Seed and Key (not on call order)
Function HashUnitI(Seed: QWord; Const Key: String; Index: Integer): Double;
Function NewRandomSeed: QWord; // From the clock, only for "roll the dice", never inside generators

Implementation

Uses SysUtils;

Function Mix64(z: QWord): QWord; // SplitMix64 finalizer
Begin
  z := (z Xor (z Shr 30)) * QWord($BF58476D1CE4E5B9);
  z := (z Xor (z Shr 27)) * QWord($94D049BB133111EB);
  result := z Xor (z Shr 31);
End;

Function FNV1a64(Const s: String; Basis: QWord): QWord;
Var
  i: Integer;
Begin
  result := Basis;
  For i := 1 To length(s) Do Begin
    result := result Xor ord(s[i]);
    result := result * QWord($100000001B3);
  End;
End;

Function DeriveSeed(Seed: QWord; Const Name: String): QWord;
Begin
  result := Mix64(FNV1a64(Name, Mix64(Seed Xor QWord($9E3779B97F4A7C15))));
End;

Function UnitFromBits(v: QWord): Double;
Begin
  // 53 random bits -> [0, 1), exact in Double
  result := (v Shr 11) / 9007199254740992.0;
End;

Function HashUnit(Seed: QWord; Const Key: String): Double;
Begin
  result := UnitFromBits(DeriveSeed(Seed, Key));
End;

Function HashUnitI(Seed: QWord; Const Key: String; Index: Integer): Double;
Begin
  result := UnitFromBits(DeriveSeed(Seed, Key + '#' + IntToStr(Index)));
End;

Function NewRandomSeed: QWord;
Begin
  result := Mix64(QWord(GetTickCount64) Xor (QWord(Trunc(Now * 86400000)) Shl 20)) Mod 1000000000;
End;

{ TAGRandom }

Procedure TAGRandom.Init(Seed: QWord; Stream: QWord);
Begin
  State := 0;
  Incr := (Stream Shl 1) Or 1;
  Next;
  State := State + Mix64(Seed);
  Next;
End;

Function TAGRandom.Next: Cardinal;
Var
  old: QWord;
  xorshifted, rot: Cardinal;
Begin
  old := State;
  State := old * QWord(6364136223846793005) + Incr;
  xorshifted := Cardinal(((old Shr 18) Xor old) Shr 27);
  rot := Cardinal(old Shr 59);
  result := (xorshifted Shr rot) Or (xorshifted Shl ((-Integer(rot)) And 31));
End;

Function TAGRandom.NextUnit: Double;
Var
  hi, lo: QWord;
Begin
  hi := Next;
  lo := Next;
  result := UnitFromBits((hi Shl 32) Or lo);
End;

Function TAGRandom.Range(lo, hi: Double): Double;
Begin
  result := lo + (hi - lo) * NextUnit;
End;

Function TAGRandom.RangeI(lo, hi: Integer): Integer;
Begin
  If hi <= lo Then exit(lo);
  result := lo + Integer(Next Mod Cardinal(hi - lo + 1));
End;

Function TAGRandom.Chance(p: Double): Boolean;
Begin
  result := NextUnit < p;
End;

End.
