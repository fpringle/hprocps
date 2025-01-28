# Structure of the `procps-bindings` library

## HSC modules

- `System/Proc/Bindings/Tab/C.hsc`
- `System/Proc/Bindings/C.hsc`
- `System/Proc/Bindings/Flags.hsc`

**All** of the weird C/CPP/HSC stuff goes in here.
Anything that doesn't need the weird stuff should go somewhere else.

## Internal modules

- `System/Proc/Bindings/Tab/Internal.hs`
- `System/Proc/Bindings/C/Utils.hs`

Pure Hask, internal, potentially unsafe, subject to change.

## Exposed modules

- `System/Proc/Bindings.hs`
- `System/Proc/Bindings/Tab.hs`
- `System/Proc/Bindings/Tab/Config.hs`
- `System/Proc/Bindings/Error.hs`
