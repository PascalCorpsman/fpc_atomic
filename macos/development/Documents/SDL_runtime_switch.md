# SDL2 runtime vs. link-time loading

Tento projekt je aktuálně nastavený na **runtime** načítání `libSDL2.dylib` (viz `{$DEFINE SDL_RUNTIME_LOADING}` v `sdl2_cfg.inc`). Následující postup popisuje, jak přepnout na link-time variantu a jak se vrátit zpět.

## A. Přepnutí na link-time linkování SDL2

1. **Změna konfigurace `sdl2_cfg.inc`**
   - `client/sdl2_cfg.inc`
   - `launcher/sdl2_cfg.inc`
   V obou souborech zakomentuj nebo smaž direktivu `{$DEFINE SDL_RUNTIME_LOADING}`.

2. **Obnovení linkování v `sdl2.pas`**
   - V `units/sdl2_for_pascal/sdl2.pas` vyhledej bloky
     ```pascal
     {$IFDEF DARWIN}
       SDL_LibName = 'libSDL2.dylib';
       {$IFDEF FPC}
         {$IFNDEF SDL_RUNTIME_LOADING}
           {$LINKLIB libSDL2}
         {$ENDIF}
       {$ENDIF}
     {$ENDIF}
     ```
     Pokud chceš linkovat vždy, odstraň podmínku `{$IFNDEF SDL_RUNTIME_LOADING}` (případně ji zakomentuj).

3. **Přidání knihovny do linkování**
   - V `.lpi` souborech (`client`, `launcher`, `server`, `ai`) přidej k `CustomOptions` přepínač `-k/absolutní/cesta/libSDL2.dylib` (nebo `-F`/`-framework` při použití `SDL2.framework`).
   - Příklad pro klienta:
     ```xml
     <CustomOptions Value="-dClient -k-L.../macos/lib/aarch64 -k.../libbass.dylib -k.../libSDL2.dylib -k-ld_classic -k-rpath -k@executable_path/../lib"/>
     ```

4. **Umístění knihovny**
   - Pokud linkuješ proti lokální kopii, ujisti se, že `libSDL2.dylib` je ve `macos/lib/<arch>/` nebo nastav správnou absolutní cestu v `CustomOptions`.

5. **Rebuild**
   - Překompiluj projekty (`lazbuild --build-mode=macos_arm64 ...`).
   - Ověř pomocí `otool -L`, že binárky mají referenci na SDL2.

## B. Návrat zpět k runtime načítání (výchozí stav)

1. **Obnov `sdl2_cfg.inc`**
   - Znovu přidej `{$DEFINE SDL_RUNTIME_LOADING}` do `client/sdl2_cfg.inc` i `launcher/sdl2_cfg.inc`.

2. **Podmíněné linkování v `sdl2.pas`**
   - Ujisti se, že blok linklib v `sdl2.pas` je obalen podmínkou `{$IFNDEF SDL_RUNTIME_LOADING}` (tak, jak je nyní v repozitáři).

3. **Odeber SDL2 z `CustomOptions`**
   - V `.lpi` souborech smaž `-k .../libSDL2.dylib` (zůstane jen BASS a `-k-rpath`).

4. **Rebuild**
   - `lazbuild --build-mode=macos_arm64 ...`
   - Spouštěcí skripty (`macos/tools/run_*.command`) nastaví `DYLD_LIBRARY_PATH`, takže `libSDL2.dylib` se načte z `macos/lib/<arch>/`.

> **Poznámka:** Bez ohledu na zvolený režim by měl balíček distribuovat `libSDL2.dylib`, aby uživatel nemusel nic doinstalovávat.

