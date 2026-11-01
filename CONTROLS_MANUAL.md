# Controls: keyboard, gamecontroller and joystick

[Deutsche Version](CONTROLS_MANUAL_DE.md)

This manual explains how to set up the controls for the two players on one PC (launcher: **Controls...**, in the game: options menu, **Define keyboard layouts**). What each key does in the game is described in the [manual](MANUAL.md#in-game-key-commands).

- [The dialog Configure controls](#the-dialog-configure-controls)
- [Gamecontroller or joystick?](#gamecontroller-or-joystick)
- [Setting up a controller](#setting-up-a-controller)
- [The options](#the-options)
- [Plugging controllers in and out during the game](#plugging-controllers-in-and-out-during-the-game)
- [FAQ](#faq)

## The dialog Configure controls

The dialog has one column for **Player 1** and one for **Player 2**. At the top you choose what each player plays with:

- **Keyboard**: Below are the six keys (up, down, left, right, first and second action). Click **Grab** and press the new key. A key that is used twice (also between the two players) is shown in red.
- **Controller**: If a controller is already set up, its name is shown here. If none is set up yet, clicking it starts the [wizard](#setting-up-a-controller) right away. If you cancel it, the player stays on the keyboard.

For a controller the dialog shows:

| Display | Meaning |
|---|---|
| Status: connected (green) | The controller is plugged in and used in the game. |
| Status: not connected (red) | The controller is not plugged in right now. As soon as it is plugged in, it works again, also during the game. |
| Status: please set up again (red) | The gamecontroller was set up with an older version whose button assignment may be wrong. Just click **Set up controller...**. |
| Same controller as player X (red) | Both players use the same controller. This is allowed, but one controller then moves both players at the same time. |
| First / second action, directions | Which buttons and axes are set up. |

Shortcuts in the dialog:

| Shortcut | Effect |
|---|---|
| Alt+1 / Alt+2 | jumps to player 1 or player 2 |
| Alt + underlined letter | triggers the button / option, the letters are chosen automatically for the language and the controller name |
| Arrow keys | switch between keyboard and controller |
| Esc | Cancel |

**default** sets both players back to the default keyboard.

## Gamecontroller or joystick?

SDL2 knows most controllers (Xbox, PlayStation, Switch and many others) as **gamecontroller**. Then SDL knows which button is A, B, LB, ..., where the D-Pad and the left stick are, and what the rest position looks like.

| | Gamecontroller (recommended) | Joystick |
|---|---|---|
| For | all controllers known to SDL | older or exotic devices that SDL does not know as gamecontroller |
| Setup | 2 steps: first and second action | 5 steps: rest position, both actions, up, left |
| Directions | D-Pad, optional the left analog stick | the axes that were set up, optional the hat (D-Pad) |
| Button names | A, B, X, Y, LB, RB, ... | Button 1, Button 2, ... |

If the device is offered as gamecontroller, use that. Joystick is meant for devices where this is not possible, or if you want to choose the axes yourself.

## Setting up a controller

The wizard starts with **Set up controller...** or when switching to controller for the first time. It guides you step by step: at the top it says what to do, below is a checklist of all steps and their results (✓ done, ▶ current, ○ open). The current line blinks while the wizard waits for the controller.

1. **Choose device**: Select the controller and whether it is used as gamecontroller or as joystick. Identical controllers are numbered (#2, #3). If you just plugged in a controller, use **Refresh devices**.
2. **Rest position** (joystick only): Let go of all sticks and buttons, then **Next**.
3. **First action**: Press the button you want to drop bombs with. The wizard then continues by itself.
4. **Second action**: Press a different button.
5. **Up** (joystick only): Push the stick up and hold it for half a second. If you use the D-Pad (hat) here, the player is controlled with the D-Pad only and the next step is skipped.
6. **Left** (joystick only): Push the stick to the left and hold it for a moment.
7. **Done**: Apply with **Finish** or the first action you just set up.

**< Back** always goes back exactly one step, nothing else is lost. In the first step it leads back to the device selection. **Cancel** (or Esc) keeps the previous setting.

The **live view** at the bottom is only for testing: it shows the axes, the pressed buttons and the direction the game would detect.

## The options

The options are set per player and can be changed in the dialog at any time without setting up the controller again. Options that do not apply to the device type are grayed out.

| Option | Default | Applies to | Meaning |
|---|---|---|---|
| Use analog stick | on | gamecontroller | In addition to the D-Pad, the left analog stick controls the player. |
| Use hat (D-Pad) | on | joystick | In addition to the axes, the first hat (the D-Pad of many joysticks) controls the player. |
| Hysteresis (no flickering) | on | stick and axes | see below |
| Diagonal: stronger axis wins | on | stick and axes | see below |

### Hysteresis

A stick counts as pressed in a direction when it is moved at least 25 % out of the center. If you hold it exactly at this limit, the value jitters above and below it: the player then gets "pressed" and "released" in very quick succession, it stutters, and each time a message is sent to the server.

With hysteresis a direction is pressed at 25 %, but only released when the stick is back below about 14 %. In between the last state is kept and the player walks smoothly. For the D-Pad the option makes no difference. You only need to turn it off if you want the player to stop at the slightest release of the stick.

### Diagonal: stronger axis wins

The player can only walk in one direction at a time. If you hold the stick diagonally, two directions are "pressed", e.g. down and right.

- **Off** (the old behavior): the direction that crossed the 25 % limit last wins. If you push the stick sideways a little early when turning, the player turns at once and stops at the wall in front of the crossing.
- **On**: only the axis that is moved further counts. You keep walking until the stick points more sideways than down, and only then turn. So that the direction does not jump back and forth at exactly 45°, the current axis keeps the direction until the other one is 25 % stronger.

The option does not apply to the D-Pad and the hat, there is no "stronger" direction there. It is grayed out if no stick or axis is used.

## Plugging controllers in and out during the game

If a controller is unplugged (or a wireless controller goes to standby), the player releases all keys at once and stops. When it is plugged in again, it continues to work without a restart. The controller is recognized by its name and, for identical controllers, by their order.

## FAQ

**The status says "please set up again", although everything worked.**
Older versions stored the raw button numbers for gamecontrollers. On Xbox controllers A, B, X and Y matched by chance, the shoulder buttons did not (e.g. LB became BACK). Click **Set up controller...** once and it is correct.

**My controller does not show up.**
Plug in the controller and click **Refresh devices** in the wizard. If the SDL2 library is missing, the dialog shows "SDL2 not available", then `SDL2.dll` (Windows) or `libSDL2` (Linux) has to be installed.

**Two identical controllers: which one is player 1?**
The order in which the operating system reports them. In the wizard they are numbered (#1, #2), in the dialog the number is shown after the name.

**The D-Pad is not accepted as first action.**
On a gamecontroller the D-Pad can not be used for actions, it is only used for the directions.

**Where is this stored?**
In `fpc_atomic.cfg` in the config folder (launcher: **Config folder**), keys `UseSDL`, `SDL_...`, for player 2 with a `2` at the end.
