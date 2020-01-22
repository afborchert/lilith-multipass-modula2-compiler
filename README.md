# Historic ETH Zürich Modula-2 Multipass Compiler for the Lilith architecture

The ETH Zürich Modula-2 Multipass Compiler for the Lilith architecture
was developed by a team consisting of Leo Geissmann, Christian Jacobi,
Urs Ammann, Svend Erik Knudsen, and others under the direction of
Niklaus Wirth at the

Institut für Informatik
ETH-Zürich
CH-8092 Zürich

These are, however, not the original sources as shipped by ETH Zürich
(unfortunately, no machine-readable copy of them survived at our site)
but those we adapted to our needs such that we could run them on our
Lilith emulator on a Perkin-Elmer 3220 machine running UNIX Edition VII.

Please note that this software is not free (free as freedom) in a
sense comparable to the GNU General Public License. See the file
_License-Agreement_ how the original software was licensed to us,
and see _LICENSE_ for some further notes and explanations.

You'll find the sources in the _src_ subdirectory along with the
binaries C18.Base, C18.Init, C18.Lister, C18.Pass1, C18.Pass2,
C18.Pass3, C18.Pass4, and C18.Symfile. These binaries were generated
by exactly these sources in combination with mcl, the M-Code linker
(to be found in a separate package) and they can be run on our
ancient Lilith emulator (to be found in another package), i.e. this
compiler is delivered in a steady-state.

The script _mc_ allows to compile Modula-2 sources using this
compiler, running on the Lilith emulator, into M-Code which
can likewise be linked using mcl and run on the Lilith emulator.

See https://github.com/afborchert/lilith for more infos.

Andreas F. Borchert
