
      _ __ ___   ___ _ __ ___
     | '_ ` _ \ / _ \ '__/ __|
     | | | | | |  __/ | | (__ _
     |_| |_| |_|\___|_|  \___(_)

    The Modern Ethereal Relay Chat daemon.

merc is an IRC daemon written using the `asyncio` framework in Python 3.3. It
aims to be a modern implementation of the IRC protocol with relatively good
performance, ease-of-use, and extensibility.

# Features

 * Full UTF-8 support.

 * Unicode case-mapping, e.g. the channel name `#straße` is mapped to
   `#STRASSE`.

 * Unicode nickname support.

 * No `identd` support.

 * ... and hopefully more things to come!

# Running

 1. Install all the required dependencies:

        pip3 install -r requirements.txt

 2. Run the server.

        python3 -m merc
