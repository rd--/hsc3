keystate keynum minval maxval lag

Report the status of a particular key.  A key is either pressed,
or not pressed.

The keycode 38 is the A key on my keyboard.  Under X the xev(1)
command is useful in determining your keyboard layout.

> sinosc AR 800 0 * (keystate KR 38 0 0.1 0.5)
