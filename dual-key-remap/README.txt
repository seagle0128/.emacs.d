# Windows Dual Key Remap: Capslock to Ctrl/Escape

Remap any key to any other two keys on Windows. Most commonly used to remap CapsLock to Escape when pressed alone and Ctrl when pressed with other keys. Written in C to run at the lowest Windows API level for speed and reliability 🔥.

## Features

- Fast with minimal cpu footprint
- Does not swallow or leak inputs
- Handles edge cases (correct key timings!)

Other solutions can leak inputs, get stuck, or do not handle key timing edge cases. Dual Key Remap runs at the lowest Windows API level making it both fast and reliable with an absolutely tiny cpu footprint.

## Installation

1) Download and unzip the latest [release](https://github.com/ililim/dual-key-remap/releases).
2) Put both 'dual-key-remap.exe' and 'config.txt' in a permament directory of your choice. (e.g. `C:\Program Files\dual-key-remap`).
3) Create a shortcut to 'dual-key-remap.exe' in your startup directory (e.g. `C:\Users\%USERNAME%\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup\dual-key-remap.lnk`).
4) Optionally edit config.txt (see below) and run 'dual-key-remap.exe'. 🥳 Your chosen keys are now remapped!

To uninstall, terminate the script from the task manager and remove the startup shortcut.

## Configuration

With the default configuration Dual Key Remap will remap CapsLock to Escape when pressed alone and Ctrl when pressed with other keys. To change this simply edit config.txt and adjust the key values. You can refer to keys by their names as described in the [wiki](https://github.com/ililim/dual-key-remap/wiki/Using-config.txt#key-names).

## Tips and Tricks

Below are a few optional advanced tips for configuring your system and using Dual Key Remap. They assume you are using it to rebind CapsLock to Ctrl/Escape, but if you are rebinding other keys they might still be helpful to you.

### Shortcuts that require Escape

Certain applications might require shortcuts such as Shift+Escape. These can still be launched with Dual Key Remap. To launch these, simply hold the modifier keys (e.g. Shift) **first** and then press the CapsLock key last. Dual Key Remap will send the Shift+Escape key combination as expected!

The reason this works is because Dual Key Remap decides which key to send depending on whether any other keys where pressed _after_ CapsLock was held down, so tapping CapsLock as the last part of a key sequence will always send Escape.

### Administrator access

If launched normally Dual Key Remap will not be able to rebind your key inputs while you're viewing escalated/administrator applications (e.g. Task Manager). To make your rebindings work in those contexts make sure to run Dual Key Remap as administrator. You can also create an [elevated shorcut](https://winaero.com/create-elevated-shortcut-to-skip-uac-prompt-in-windows-10/) for Dual Key Remap.

### Rebind keys during login / before Windows loads

Dual Key Remap won't rebind any keys until the application is running, which means that on the login screen (and until Windows has fully loaded) your keys aren't rebound yet. This is not a problem for most people, but if it bothers you there are some additional steps you could take to improve your experience.

By using an utility like [Sharp Keys](https://github.com/randyrants/sharpkeys) you can rebind keys in the windows registry. While this method only supports simple key-to-key rebinding, these rebindings persist in the Windows login screen. So if you want to use CapsLock as Ctrl/Escape you could do the following:
- Rebind CapsLock to Escape in Sharp Keys.
- Rebind Escape to Ctrl/Escape in Dual Key Remap.
- Your CapsLock key will work as Escape during login and early startup, and once Windows is fully loaded the dual key functionality will activate!

## Contributing

### Building dual-key-remap.exe

To build dual-key-remap you will need a way to compile c programs. You can follow [Microsoft's documentation](https://msdn.microsoft.com/en-us/library/bb384838.aspx) to get the required tooling setup.

Once that's all setup you should be able to use to the Visual C++ developer command prompt to run the following commands:

Run the tests:

```
cl .\test.c && .\test.exe
```

Build the application:

```
cl .\dual-key-remap.c /link user32.lib shell32.lib /SUBSYSTEM:WINDOWS /ENTRY:mainCRTStartup
```
