# This file is part of BishBosh.
#
# BishBosh is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# BishBosh is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with BishBosh.  If not, see <http://www.gnu.org/licenses/>.

.PHONY: hlint test prof cabalCheck sdist findOmissions xboard duel haddock graphmod

PACKAGE_NAME	= bishbosh
SHELL		= /bin/bash
GHC_OPTIONS	= --ghc-options='-j'

# Check for lint.
hlint:
	@$@	--cpp-define 'USE_PARALLEL'\
		--cpp-define 'USE_POLYPARSE=1'\
		--cpp-define 'USE_UNBOXED_ARRAYS'\
		--ignore 'Use tuple-section'\
		src-lib/ +RTS -N -RTS || true
	@$@	--cpp-define 'USE_HXTRELAXNG'\
		--cpp-define 'USE_PARALLEL'\
		--cpp-define 'USE_UNBOXED_ARRAYS'\
		--cpp-define 'USE_UNIX'\
		--cpp-define 'MOVE_NOTATION=S'\
		--ignore 'Reduce duplication'\
		src-exe/ +RTS -N -RTS || true
	@$@	--cpp-define 'USE_PARALLEL'\
		--cpp-define 'USE_POLYPARSE=1'\
		--cpp-define 'USE_SELECT=1'\
		--ignore 'Use tuple-section'\
		src-test/ +RTS -N -RTS

# Compile with various CPP-flags & run the test-suites.
test:
	@for FLAG in -narrownumbers -polyparse -hxtrelaxng narrownumbers -threaded unboxedarrays; do\
		echo $${FLAG};\
		stack '$@' --flag="$(PACKAGE_NAME):$${FLAG}" $(GHC_OPTIONS) || break;\
	done

# Profile.
prof:
	@stack install --library-profiling --executable-profiling $(GHC_OPTIONS)
	@$(PACKAGE_NAME) -i 'config/Raw/$(PACKAGE_NAME)_$@.xml' +RTS -p -N2 -RTS

# Check the cabal-file.
cabalCheck:
	@cabal check

# Package for upload to Hackage.
sdist:
	@cabal '$@'

# Find source-files missing from the distribution.
findOmissions: sdist
	@diff <(find src-* -type f \( -name '*.hs' -a ! -name 'Setup.hs' \) | sed 's!^\./!!' | sort) <(tar -ztf dist*/sdist/$(PACKAGE_NAME)-*.tar.gz | grep '\.hs$$' | sed 's!^$(PACKAGE_NAME)-[0-9.]*/!!' | sort)

# Install.
$$HOME/.local/bin/$(PACKAGE_NAME):
	@stack install $(GHC_OPTIONS)

# Run the installed application as an xboard-engine.
xboard: $$HOME/.local/bin/$(PACKAGE_NAME)
	@$@ -fcp '$(PACKAGE_NAME) -i "config/CECP/$(PACKAGE_NAME)_black.xml" +RTS -N2 -RTS'

# Install locally.
$$HOME/.local/bin/duel:
	@stack install $(GHC_OPTIONS)

# Start a battle.
duel: $$HOME/.local/bin/duel
	@$@ --verbosity='Verbose' --nGames=128 --readTimeout=30 -i 'config/Raw/bishbosh_$@_white.xml' -i 'config/Raw/bishbosh_$@_black.xml'

# Build the source-code documentation.
haddock:
	@stack '$@' --no-$@-deps $(GHC_OPTIONS)

# Show module-dependency graph.
graphmod:
	@$@ --graph-dim='40,24' -i 'src-exe' -i 'src-lib' Main | tred | dot -Tsvg | display	# CAVEAT: doesn't include 'Duel'.

