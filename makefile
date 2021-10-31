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

.PHONY: cabalCheck duel findOmissions graphmod haddock hlint prof profN2 randomTest sdist test xboard

PACKAGE_NAME	= bishbosh
SHELL		= /bin/bash
GHC_OPTIONS	= --ghc-options='-j'
BIN_DIR		= $$HOME/.local/bin/

$(BIN_DIR)/graphmod $(BIN_DIR)/hlint:
	@[ -x $@ ] || stack install $(suffix @) $(GHC_OPTIONS)

# Display the module-dependency graph.
graphmod: $(BIN_DIR)/graphmod
	@$@ --graph-dim='40,24' -i 'src-lib' -i 'src-exe' Main | tred | dot -Tsvg | display

# Groom sourcecode.
hlint: $(BIN_DIR)/hlint
	@$@ -j	--no-exit-code\
		--cpp-define 'USE_NEWTYPE_WRAPPERS'\
		--cpp-define 'USE_PARALLEL'\
		--cpp-define 'USE_POLYPARSE=L'\
		--cpp-define 'USE_UNBOXED_ARRAYS'\
		--ignore 'Use tuple-section'\
		src-lib/ +RTS -N -RTS
	@$@ -j	--no-exit-code\
		--cpp-define 'MOVE_NOTATION=S'\
		--cpp-define 'USE_HXTRELAXNG'\
		--cpp-define 'USE_UNBOXED_ARRAYS'\
		--cpp-define 'USE_UNIX'\
		--ignore 'Reduce duplication'\
		src-exe/ +RTS -N -RTS
	@$@ -j	--cpp-define 'USE_NEWTYPE_WRAPPERS'\
		--cpp-define 'USE_POLYPARSE=L'\
		--cpp-define 'USE_SEARCH'\
		--ignore 'Use tuple-section'\
		src-test/ +RTS -N -RTS

# Serially compile with various CPP-flags & run the test-suites.
test:
	@for FLAG in -polyparse newtypewrappers narrownumbers unboxedarrays -hxtrelaxng -threaded precision; do\
		echo $${FLAG};\
		stack '$@' --flag="$(PACKAGE_NAME):$${FLAG}" $(GHC_OPTIONS) || break;\
	done

# Compile with random CPP-flags & run the test-suites.
randomTest:
	@FLAGS=$$(shuf --echo -- hxtrelaxng -hxtrelaxng narrownumbers -narrownumbers newtypewrappers -newtypewrappers polyparse -polyparse precision -precision threaded -threaded unboxedarrays -unboxedarrays | head --lines=3 | sed -e 's/\(.*\)/--flag=$(PACKAGE_NAME):\1/');\
	echo $${FLAGS};\
	stack test $${FLAGS} $(GHC_OPTIONS)

# Profile a single-threaded build, to access entry-counts.
prof:
	@stack install --profile --flag='bishbosh:-threaded' $(GHC_OPTIONS);
	sleep 16;	# Let the test-machine reach a quiescent state.
	@$(PACKAGE_NAME) -i 'config/Raw/$(PACKAGE_NAME)_$@.xml' +RTS -p -RTS

# Profile.
profN2:
	@stack install --profile $(GHC_OPTIONS);
	@$(PACKAGE_NAME) -i 'config/Raw/$(PACKAGE_NAME)_prof.xml' +RTS -p -N2 -RTS

# Install this product.
$(BIN_DIR)/$(PACKAGE_NAME) $(BIN_DIR)/duel:
	@stack install $(GHC_OPTIONS)

# Run the installed application as an xboard-engine.
xboard: $(BIN_DIR)/$(PACKAGE_NAME)
	@$@ -fcp '$(PACKAGE_NAME) -i "config/CECP/$(PACKAGE_NAME)_black.xml" +RTS -N2 -RTS'

# Start a battle.
duel: $(BIN_DIR)/duel
	@$@ --nGames=32 --verbosity='Verbose' -i 'config/Raw/$(PACKAGE_NAME)_$@_white.xml' -i 'config/Raw/$(PACKAGE_NAME)_$@_black.xml'

# Build the source-code documentation.
haddock:
	@stack '$@' --no-$@-deps $(GHC_OPTIONS)

# Package for upload to Hackage.
sdist:
	@cabal '$@'

# Check the cabal-file.
cabalCheck:
	@cabal check

# Find source-files missing from the distribution.
findOmissions: sdist
	@diff <(find src-* -type f -name '*.hs' | sed 's!^\./!!' | sort) <(tar -ztf `find dist* -mtime 0 -name '$(PACKAGE_NAME)-*.tar.gz' -print` | grep '\.hs$$' | grep -v 'Setup.hs' | sed 's!^$(PACKAGE_NAME)-[0-9.]*/!!' | sort)

