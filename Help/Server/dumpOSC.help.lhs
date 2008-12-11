/dumpOSC                                  Display incoming OSC messages

PrintLevel (int) - code

Turns on and off printing of the contents of incoming Open Sound
Control messages. This is useful when debugging your command stream.

The values for the code are as follows:

NoPrinter   (0) - turn dumping OFF.
TextPrinter (1) - print the parsed contents of the message.
HexPrinter  (2) - print the contents in hexadecimal.
AllPrinter  (3) - print both the parsed and hexadecimal representations of the contents.
