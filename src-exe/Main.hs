{-# LANGUAGE CPP #-}
{-
	Copyright (C) 2018 Dr. Alistair Ward

	This file is part of BishBosh.

	BishBosh is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	BishBosh is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with BishBosh.  If not, see <http://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]

	* Reads the configuration by parsing the command-line arguments.

	* Plays a chess-game as configured.
-}

module Main(main) where

import			BishBosh.Data.Float()
-- import		BishBosh.Data.Ratio()	-- Required if either criterionWeight or criterionValue is Rational.
import			Control.Arrow((&&&), (***))
import			Control.Category((>>>))
import qualified	BishBosh.ContextualNotation.PGN			as ContextualNotation.PGN
import qualified	BishBosh.ContextualNotation.PGNDatabase		as ContextualNotation.PGNDatabase
import qualified	BishBosh.ContextualNotation.QualifiedMoveForest	as ContextualNotation.QualifiedMoveForest
import qualified	BishBosh.Data.Exception				as Data.Exception
import qualified	BishBosh.Input.CommandLineOption		as Input.CommandLineOption
import qualified	BishBosh.Input.IOOptions			as Input.IOOptions
import qualified	BishBosh.Input.Options				as Input.Options
import qualified	BishBosh.Input.PGNOptions			as Input.PGNOptions
import qualified	BishBosh.Input.UIOptions			as Input.UIOptions
import qualified	BishBosh.Input.Verbosity			as Input.Verbosity
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.Play					as Play
import qualified	BishBosh.Property.Empty				as Property.Empty
import qualified	BishBosh.Property.Null				as Property.Null
import qualified	BishBosh.Property.ShowFloat			as Property.ShowFloat
import qualified	BishBosh.State.PlayState			as State.PlayState
import qualified	BishBosh.State.TurnsByLogicalColour		as State.TurnsByLogicalColour
import qualified	BishBosh.Text.Show				as Text.Show
import qualified	BishBosh.Text.ShowList				as Text.ShowList
import qualified	BishBosh.Types					as T
import qualified	Control.Exception
import qualified	Control.Monad
import qualified	Data.Default
import qualified	Data.List
import qualified	Data.Maybe
import qualified	Data.Version
import qualified	Paths_bishbosh					as Paths	-- Either local stub, or package-instance auto-generated by 'Setup build'.
import qualified	System.Console.GetOpt				as G
import qualified	System.Directory
import qualified	System.Environment
import qualified	System.Exit
import qualified	System.FilePath
import qualified	System.Info
import qualified	System.IO
import qualified	System.Random
import qualified	Text.Printf
import qualified	Text.XML.HXT.Core				as HXT
import qualified	ToolShed.Data.List
import qualified	ToolShed.System.File
import			System.FilePath((</>), (<.>))

#ifdef USE_NARROW_NUMBERS
import			BishBosh.Data.Integral()	-- HXT.XmlPickler.
#endif

#ifdef USE_HXTRELAXNG
import qualified	Text.XML.HXT.RelaxNG
#endif

#ifdef TOOL_VERSION_ghc
import qualified	GHC.Conc
#endif

#ifdef USE_UNIX
import qualified	BishBosh.Concurrent.SignalHandlers	as Concurrent.SignalHandlers
#endif

type Row	= T.Y
type Column	= T.X		-- N.B. there's no requirement to be the same type as Row.
type Options	= Input.Options.Options Column T.CriterionWeight T.PieceSquareValue T.RankValue Row T.X T.Y

writeXMLToFile
	:: HXT.XmlPickler pickleable
	=> System.FilePath.FilePath	-- ^ Destination.
	-> String			-- ^ Root-element.
	-> System.FilePath.FilePath	-- ^ DTD file-path.
	-> pickleable			-- ^ The pickleable.
	-> IO ()
writeXMLToFile destinationFilePath rootElementTag dtdFilePath x	= System.IO.withFile destinationFilePath System.IO.WriteMode (
	`System.IO.hPutStrLn` (
		showString "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE " . showString rootElementTag . showString " SYSTEM " . shows dtdFilePath $ showString ">\n" (
			HXT.showPickled [
				HXT.withIndent HXT.yes	-- CAVEAT: corresponding input-option 'HXT.withRemoveWS' may subsequently be required.
			] x
		)
	)
 )

-- | Entry-point.
main :: IO ()
main	= do
	progName	<- System.Environment.getProgName
	args		<- System.Environment.getArgs
	dataDir		<- fmap System.FilePath.normalise Paths.getDataDir	-- Defined using 'cabal configure --datadir="<dir>" --datasubdir="<dir>"'.
	appUserDataDir	<- System.Directory.getAppUserDataDirectory progName	-- CAVEAT: on Unix/Gnu-Linux, requires the '$HOME' environment-variable to have been defined.
	executablePath	<- fmap System.FilePath.normalise System.Environment.getExecutablePath

	System.Directory.doesDirectoryExist appUserDataDir >>= (
		`Control.Monad.unless` Control.Exception.catch (
			System.Directory.createDirectory appUserDataDir
		) (
			\e -> do
				System.IO.hPutStrLn System.IO.stderr $ shows (e :: Control.Exception.SomeException) "."

				System.Exit.exitFailure
		)
	 )

	let
		author :: String
		author	= "Dr. Alistair Ward"

		inputConfigFilePathFlag :: Input.CommandLineOption.Flag
		inputConfigFilePathFlag	= "inputConfigFilePath"
{-
	Define all permissible command-line options.
	This specification is fairly complicated:
		Each command-line option is categorised as one of:
			an "IO-action", which is subsequently sequenced with other IO-actions, before exiting;
			an "Options-mutator", which if specified, defines a function used to transform 'Input.CommandLineOption.CommandLineOption'.

		Additionally, the command-line options are recorded as they're processed, to enable display of the command-line in the results;
		if one merely displays 'args', then security-sensitive details may be revealed, & it's difficult to find & remove such options, since the associated flag may be truncated to a minimum unique string.
-}
		optDescrList :: [
			G.OptDescr (
				Maybe (String {-flag-}, Maybe String {-optional value-}),	-- Record command-line specifications.
				Input.CommandLineOption.CommandLineOption Options		-- Defines the action to take on receipt of a command-line option.
			) -- Pair.
		 ]
		optDescrList	= [
			G.Option "?"	["help"] (
				G.NoArg (
					Nothing {-N/A-},
					Input.CommandLineOption.mkIOAction printHelp
				)
			) $ showString infoString "print this help, & then exit.",
			G.Option "v"	["version"] (
				G.NoArg (
					Nothing {-N/A-},
					Input.CommandLineOption.mkIOAction printVersion
				)
			) $ showString infoString "print version-information, & then exit.",
			G.Option "i"	[inputConfigFilePathFlag] (
				G.ReqArg (
					Just . (,) inputConfigFilePathFlag . Just &&& Input.CommandLineOption.mkConfigLocationParameter
				) $ mkTypeString filePathString
			) $ showString inputString "define the path to an XML-file from which the configuration can be read.",
			G.Option "r"	[Input.Options.randomSeedTag] (
				G.OptArg (
					Just . (,) Input.Options.randomSeedTag &&& Input.CommandLineOption.mkOptionsMutator . setRandomSeed
				) $ mkTypeString intString
			) . showString executionString . showString "seed the pseudo-random number-generator with the specified integer, to produce a repeatable sequence; if this option is unspecified then the seed is unpredictable, but if only its argument is unspecified then the seed defaults to '" $ shows defaultRandomSeed "'.",
			G.Option "o"	[Input.IOOptions.outputConfigFilePathTag] (
				G.ReqArg (
					Just . (,) Input.IOOptions.outputConfigFilePathTag . Just &&& Input.CommandLineOption.mkOptionsMutator . setOutputConfigFilePath
				) $ mkTypeString filePathString
			) $ showString outputString "define the path to a file into which the unprocessed configuration, formatted in XML, should be written.",
			G.Option ""	[Input.UIOptions.printMoveTreeTag] (
				G.ReqArg (
					Just . (,) Input.UIOptions.printMoveTreeTag . Just &&& Input.CommandLineOption.mkOptionsMutator . setPrintMoveTree
				) $ mkTypeString intString
			) $ showString outputString "draw the tree of all possible moves in the configured notation, each annotated by a fitness-evaluation for the resulting position, truncated to the specified number of plies.",
			G.Option ""	["checkPickler"] (
				G.NoArg (
					Nothing {-N/A-},
					Input.CommandLineOption.mkIOAction checkPickler
				)
			) $ showString outputString "check the XML-pickler used to read & write configuration-files, & then exit.",
			G.Option ""	["generateDTD"] (
				G.NoArg (
					Nothing {-N/A-},
					Input.CommandLineOption.mkIOAction generateDTD
				)
			) $ showString outputString "generate a rough Document Type Definition (DTD), defining the XML-format of configuration-files, & then exit. CAVEAT: it must be manually amended to identify 'IMPLIED', 'ID', & 'IDREF' attributes.",
			G.Option ""	[Input.Verbosity.tag] (
				G.ReqArg (
					Just . (,) Input.Verbosity.tag . Just &&& Input.CommandLineOption.mkOptionsMutator . setVerbosity
				) . mkTypeString $ Text.ShowList.capitaliseInitial Input.Verbosity.tag
			) . showString outputString . showString "define the log-level; default '" $ shows (Input.UIOptions.getVerbosity defaultUIOptions) "'."
		 ] where
			mkTypeString :: ShowS
			mkTypeString s	= showChar '<' $ showString s ">"

			intString, filePathString, executionString, infoString, inputString, outputString :: String
			intString	= "Int"
			filePathString	= "File-path"
			executionString	= "Execution: "
			infoString	= "Info: "
			inputString	= "Input: "
			outputString	= "Output: "

			defaultUIOptions :: Input.UIOptions.UIOptions Row Column
			defaultUIOptions	= Data.Default.def

			defaultRandomSeed :: Input.Options.RandomSeed
			defaultRandomSeed	= 0

-- Unary options-mutators.
			setPrintMoveTree, setVerbosity, setOutputConfigFilePath	:: String -> Options -> Options
			setPrintMoveTree	= Input.Options.setMaybePrintMoveTree . Just . Input.CommandLineOption.readArg
			setVerbosity		= Input.Options.setVerbosity . Input.CommandLineOption.readArg
			setOutputConfigFilePath	= Input.Options.setMaybeOutputConfigFilePath . Just

			setRandomSeed :: Maybe String -> Options -> Options
			setRandomSeed	= Input.Options.setMaybeRandomSeed . Just . Data.Maybe.maybe defaultRandomSeed Input.CommandLineOption.readBoundedIntegral

-- Nullary I/O-actions.
			printHelp, printVersion :: IO ()
			printHelp	= Control.Monad.void . Text.Printf.printf "Usage:\t%s\nEBNF argument-format:\n\t%-9s = %s;\n\t%-9s = %s;\n\t%-9s = %s;\n" (
				G.usageInfo progName optDescrList
			 ) filePathString (
				showString fileNameTag . showString " ('" . showChar System.FilePath.pathSeparator . showString "' " $ showString fileNameTag ")*"
			 ) intString "[0-9]+" (
				Text.ShowList.capitaliseInitial Input.Verbosity.tag
			 ) $ ToolShed.Data.List.showListWith ('(', '|', ')') Input.Verbosity.range " (* Case-sensitive *)" where
				fileNameTag :: String
				fileNameTag	= "File-name"

			printVersion	= putStrLn (
				showString progName . showChar '-' . showsVersion Paths.version . showString "\n\nCompiled by " . showString System.Info.compilerName . showChar '-' . showsVersion System.Info.compilerVersion . showString ".\n\nCopyright (C) 2018 " . showString author . showString ".\nThis program comes with ABSOLUTELY NO WARRANTY.\nThis is free software, and you are welcome to redistribute it under certain conditions.\n\nWritten by " $ showString author "."
			 ) >> System.Exit.exitSuccess where
				showsVersion :: Data.Version.Version -> ShowS
				showsVersion	= foldr (.) id . Data.List.intersperse (showChar '.') . map shows . Data.Version.versionBranch

			checkPickler, generateDTD :: IO ()
			checkPickler	= Control.Monad.void . HXT.runX $ HXT.constA (Data.Default.def :: Options) >>> HXT.checkPickler HXT.xpickle

			generateDTD	= Control.Monad.void . HXT.runX $ HXT.constA (undefined :: Options) >>> HXT.xpickleWriteDTD HXT.xpickle [
				HXT.withTrace 0 {-valid values in closed interval [0, 4]-}
			 ] "-" {-stdout-}	-- CAVEAT: this DTD requires manual correction of defaulted attributes, which are erroneously defined as 'REQUIRED' rather than 'IMPLIED'.
#ifdef USE_UNIX
	Concurrent.SignalHandlers.handleSignals
#endif

-- Use the list of possible options, to process the actual list of arguments.
	case G.getOpt G.RequireOrder optDescrList args of
		(commandLineOptions, [{-non-options-}], [{-errors-}])
			| not $ null ioActions				-> sequence_ ioActions >> System.Exit.exitSuccess
			| Just configFilePath <- maybeConfigFilePath	-> let
{-
	The strategy is to either parse XML for the input-options, then over-ride them with each of the command-line options-mutators,
	but regrettably "verbosity" (a command-line option) is needed while doing so.
	This is resolved by reading the "verbosity" from the command-line first.
-}
					preVerbosity :: Input.Verbosity.Verbosity
					preVerbosity
						| null verbosityArgs	= Data.Default.def
						| otherwise		= Input.CommandLineOption.readArg $ head verbosityArgs
						where
							verbosityArgs :: [String]
							verbosityArgs	= Input.CommandLineOption.getArgs (
								map (showString Input.CommandLineOption.longFlagPrefix) . drop 4 {-those prefixes ambiguous with "version"-} $ Data.List.inits Input.Verbosity.tag	-- Generate all unique abbreviations.
							 ) args

				in do
					fileExists	<- System.Directory.doesFileExist configFilePath

					if not fileExists
						then do
							cwd	<- System.Directory.getCurrentDirectory

							Control.Exception.throwIO . Data.Exception.mkSearchFailure . showString "no such file " . shows configFilePath $ (
								if preVerbosity == maxBound && System.FilePath.isRelative configFilePath
									then showString "; CWD=" . shows cwd
									else id
							 ) "."	-- This error would be trapped by HXT.xunpickleDocument, but it doesn't set the exit-status.
						else let
							configDir :: System.FilePath.FilePath
							configDir	= dataDir </> "config"

							inputSysConfig :: HXT.SysConfigList
							inputSysConfig	= [
								HXT.withRemoveWS HXT.yes,						-- Remove white-space, e.g. any indentation which might have been introduced by 'HXT.withIndent'.
								HXT.withStrictInput HXT.yes,						-- Read the input file strictly (cf. lazily), this ensures file-closure even if not completely read.
#ifdef USE_HXTRELAXNG
								Text.XML.HXT.RelaxNG.withRelaxNG $ configDir </> progName <.> "rng"	-- Validate against the referenced RelaxNG schema.
#else
								HXT.withValidate HXT.yes						-- Validate against any DTD referenced from the specified XML-file.
#endif
							 ]

							maximumTraceLevel :: Int
							maximumTraceLevel	= 2	-- CAVEAT: HXT trace-levels 3 & 4 are too verbose.

							processInputOptions :: Options -> IO Options
							processInputOptions options	= let
								options' :: Options
								options'	= (
									\o -> (
										if Data.Maybe.isNothing . Input.IOOptions.getMaybePersistence $ Input.Options.getIOOptions o
											then Input.Options.setMaybePersistence $ Just (
												appUserDataDir </> System.FilePath.takeBaseName configFilePath <.> "txt",
												True	-- Automatic.
											)
											else id
									) o
								 ) $ foldr ($) options optionsMutators	-- Sequentially mutate the configuration, using each of the options-mutators specified on the command-line.

								ioOptions				= Input.Options.getIOOptions options'
								(maybeOutputConfigFilePath, uiOptions)	= Input.IOOptions.getMaybeOutputConfigFilePath &&& Input.IOOptions.getUIOptions $ ioOptions
								verbosity				= Input.UIOptions.getVerbosity uiOptions
								(showsInfoPrefix, showsWarningPrefix)	= const (
									Text.Show.showsInfoPrefix,
									Text.Show.showsWarningPrefix
								 ) `either` const (
									Text.ShowList.showsInfoPrefix,
									Text.ShowList.showsWarningPrefix
								 ) $ Input.UIOptions.getEitherNativeUIOrCECPOptions uiOptions
							 in do
#ifdef TOOL_VERSION_ghc
								numProcessors	<- GHC.Conc.getNumProcessors

								Control.Monad.when (verbosity /= minBound && numProcessors > 1 && GHC.Conc.numCapabilities == 1) . System.IO.hPutStrLn System.IO.stderr . showsWarningPrefix . showString "this application typically benefits from " . shows numProcessors . showString " CPU-cores; try '" . showString progName . showChar ' ' $ showString (unwords args) " +RTS -N'."
#endif
								Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . showsInfoPrefix . showString Input.Options.tag . Text.ShowList.showsAssociation $ Property.ShowFloat.showsFloatToN (Input.UIOptions.getNDecimalDigits uiOptions) options' "."

								Control.Monad.when (Data.Maybe.isJust maybeOutputConfigFilePath) $ let
									Just outputConfigFilePath	= maybeOutputConfigFilePath
								 in Control.Exception.catch (
									writeXMLToFile outputConfigFilePath Input.Options.tag (
										configDir </> progName <.> "dtd"
									) options {-the original-}
								 ) $ \e -> Control.Monad.unless (verbosity == minBound) . System.IO.hPutStrLn System.IO.stderr . showsWarningPrefix . showString "failed to write the configuration in XML, to " . showString Input.IOOptions.outputConfigFilePathTag . Text.ShowList.showsAssociation . shows outputConfigFilePath . showString "; " $ shows (e :: Control.Exception.SomeException) "."	-- This error isn't fatal, so continue.

								randomGen	<- Data.Maybe.maybe (
									do
										Control.Monad.when (verbosity > Data.Default.def) . System.IO.hPutStrLn System.IO.stderr $ showsWarningPrefix "seeding the pseudo-random number-generator from the operating-system; the result will not typically be repeatable."

										System.Random.getStdGen
								 ) (
									return {-to IO-monad-} . System.Random.mkStdGen	-- Seed the pseudo-random number-generator with the specified integer.
								 ) $ Input.Options.getMaybeRandomSeed options'

								playState	<- uncurry (Play.play randomGen) . (,) options' =<< (	-- Artificially construct a pair, to reduce the strictness & thus allow the UI to be rendered while PGN-databases are evaluated in the background.
									\qualifiedMoveForest -> do
										Control.Monad.when (verbosity == maxBound && not (Property.Null.isNull qualifiedMoveForest)) $ mapM_ (
											System.IO.hPutStrLn System.IO.stderr . showsInfoPrefix . ($ ".")
										 ) [
											showString "resulting tree contains (nGames, nMoves)" . Text.ShowList.showsAssociation . shows (ContextualNotation.QualifiedMoveForest.count qualifiedMoveForest),	-- CAVEAT: application stalls during this call.

											showString "the minimum number of pieces remaining in any of the archived games" . Text.ShowList.showsAssociation . shows (ContextualNotation.QualifiedMoveForest.findMinimumPieces qualifiedMoveForest)
										 ]

										return {-to IO-monad-} qualifiedMoveForest
								 ) =<< Control.Monad.foldM (
									\qualifiedMoveForest pgnOptions -> let
										(pgnDatabaseFilePath, minimumPlies)	= Input.PGNOptions.getDatabaseFilePath &&& Input.PGNOptions.getMinimumPlies $ pgnOptions
										searchPath				= Data.List.nub $ [
											".",	-- CWD.
											dataDir	-- The installation-directory.
										 ] ++ Data.List.unfoldr (
											\filePath -> let
												directory	= System.FilePath.takeDirectory filePath
											in if directory `elem` map return {-to List-monad-} ['.', System.FilePath.pathSeparator]
												then Nothing
												else Just (directory, directory)
										 ) executablePath
									in either (
										Control.Exception.throwIO . Data.Exception.mkParseFailure
									) (
										(
											\pgnDatabase -> do
												Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . showsInfoPrefix . showString "read PGN-database" . Text.ShowList.showsAssociation . shows pgnDatabaseFilePath . showString " containing (nGames, nMoves)" . Text.ShowList.showsAssociation $ shows (
													Data.List.foldl' (
														\acc pgn -> let
															acc'@(nGames, nPlies)	= succ *** (
																+ State.TurnsByLogicalColour.getNPlies (Model.Game.getTurnsByLogicalColour $ ContextualNotation.PGN.getGame pgn)
															 ) $ acc
														in nGames `seq` nPlies `seq` acc'
													) (0 :: Model.Game.NGames, 0) pgnDatabase
												 ) "."	-- CAVEAT: this stalls rendering of a native UI.

												return {-to IO-monad-} $ ContextualNotation.QualifiedMoveForest.mergePGNDatabase pgnDatabase qualifiedMoveForest
										) . if minimumPlies == 0
											then id
											else filter $ (>= minimumPlies) . State.TurnsByLogicalColour.getNPlies . Model.Game.getTurnsByLogicalColour . ContextualNotation.PGN.getGame
									) =<< Data.Maybe.maybe (
										Control.Exception.throwIO . Data.Exception.mkSearchFailure . showString "failed to locate " . shows pgnDatabaseFilePath . showString " in " $ shows searchPath "."
									) (
										\absolutePGNDatabaseFilePath -> ContextualNotation.PGNDatabase.parseIO absolutePGNDatabaseFilePath (
											Input.PGNOptions.getIsStrictlySequential pgnOptions
										) (
											Input.PGNOptions.getValidateMoves pgnOptions
										) (
											Input.PGNOptions.getTextEncoding pgnOptions
										) (
											Input.PGNOptions.getIdentificationTags pgnOptions
										)
									) . Data.Maybe.listToMaybe =<< ToolShed.System.File.locate pgnDatabaseFilePath searchPath
								 ) Property.Empty.empty {-QualifiedMoveForest-} (Input.IOOptions.getPGNOptionsList ioOptions)

								let
									maybeApplicationTerminationReason	= State.PlayState.getMaybeApplicationTerminationReason (playState :: State.PlayState.PlayState Column T.CriterionValue T.CriterionWeight T.PieceSquareValue T.PositionHash T.RankValue Row T.WeightedMean T.X T.Y)
								 in Control.Monad.when (
									verbosity /= minBound && Data.Maybe.isJust maybeApplicationTerminationReason
								 ) . System.IO.hPutStrLn System.IO.stderr . showsInfoPrefix . showString "application terminated " $ shows (Data.Maybe.fromJust maybeApplicationTerminationReason) "."

								return {-to IO-monad-} options'
						in Control.Monad.void {-discard the state returned by processInputOptions-} . HXT.runX $ HXT.setTraceLevel (fromEnum preVerbosity `min` maximumTraceLevel) >>> HXT.xunpickleDocument HXT.xpickle inputSysConfig configFilePath >>> HXT.traceMsg (fromEnum preVerbosity `min` maximumTraceLevel) (showString Input.Options.tag " parsed") >>> HXT.arrIO processInputOptions
			| otherwise	-> Control.Exception.throwIO . Data.Exception.mkInsufficientData $ shows (Input.CommandLineOption.longFlagPrefix ++ inputConfigFilePathFlag) " must be specified."
			where
				ioActions		:: [IO ()]
				optionsMutators		:: [Options -> Options]
				maybeConfigFilePath	:: Maybe String
				(ioActions, optionsMutators, maybeConfigFilePath)	= Input.CommandLineOption.partition3 $ map snd commandLineOptions	-- Categorise the specified command-line options.
		(_, nonOptions, [])	-> Control.Exception.throwIO . Data.Exception.mkRedundantData . showString "unexpected command-line arguments; " $ shows nonOptions "."
		(_, _, errors)		-> Control.Exception.throwIO . Data.Exception.mkParseFailure $ concat errors

