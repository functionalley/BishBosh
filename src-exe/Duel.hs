{-# LANGUAGE LambdaCase #-}
{-
	Copyright (C) 2021 Dr. Alistair Ward

	This file is part of Duel.

	Duel is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Duel is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Duel.  If not, see <http://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]	Coordinates a duel between two independently configured instances of 'bishbosh'; which can be used to evaluate relative performance.

-}

module Main(main)	where

import			Control.Arrow((&&&))
import qualified	BishBosh.Data.Exception				as Data.Exception
import qualified	BishBosh.Input.CategorisedCommandLineOptions	as Input.CategorisedCommandLineOptions
import qualified	BishBosh.Input.CommandLineOption		as Input.CommandLineOption
import qualified	BishBosh.Input.Verbosity			as Input.Verbosity
import qualified	BishBosh.Text.Case				as Text.Case
import qualified	Control.Exception
import qualified	Data.Default
import qualified	Data.List
import qualified	Data.Version
import qualified	Duel.Data.Options				as Data.Options
import qualified	Duel.Process.Intermediary			as Process.Intermediary
import qualified	Paths_bishbosh					as Paths	-- Either local stub, or package-instance auto-generated by 'Setup build'.
import qualified	System.Console.GetOpt				as G
import qualified	System.Environment
import qualified	System.Exit
import qualified	System.Info

-- | The application's entry-point.
main :: IO ()
main	= do
	progName	<- System.Environment.getProgName

	let
		optDescrList :: [
			G.OptDescr (
				Maybe (Input.CommandLineOption.Flag, Maybe String {-optional value-}),	-- Record command-line specifications.
				Input.CommandLineOption.CommandLineOption Data.Options.Options		-- Defines the action to take on receipt of a command-line option.
			) -- Pair.
		 ]
		optDescrList	= [
			G.Option "?"	["help"] (
				G.NoArg (
					Nothing {-N/A-},
					Input.CommandLineOption.mkIOAction printHelp
				)
			) "print this help, & then exit.",
			G.Option "v"	["version"] (
				G.NoArg (
					Nothing {-N/A-},
					Input.CommandLineOption.mkIOAction printVersion
				)
			) "print version-information, & then exit.",
			G.Option ""	[Input.Verbosity.tag] (
				G.ReqArg (
					Just . (,) Input.Verbosity.tag . Just &&& Input.CommandLineOption.mkOptionsMutator . Data.Options.setVerbosity . Input.CommandLineOption.readArg
				) . mkTypeSpecification $ Text.Case.toUpperInitial Input.Verbosity.tag
			) . showString "define the log-level; default '" $ shows (Data.Default.def :: Input.Verbosity.Verbosity) "'.",
			let
				flag	= "nDecimalDigits"
			in G.Option "d"	[flag] (
				G.ReqArg (
					Just . (,) flag . Just &&& Input.CommandLineOption.mkOptionsMutator . Data.Options.setNDecimalDigits . Input.CommandLineOption.readBoundedIntegral
				) $ mkTypeSpecification "Int"
			) . showString "define the precision with which to display floating-point numbers; default " $ shows (Data.Options.getNDecimalDigits Data.Default.def) ".",
			let
				flag	= "nGames"
			in G.Option "n"	[flag] (
				G.ReqArg (
					Just . (,) flag . Just &&& Input.CommandLineOption.mkOptionsMutator . Data.Options.setNGames . Input.CommandLineOption.readBoundedIntegral
				) $ mkTypeSpecification "Int"
			) . showString "define the number of games to play; default " $ shows (Data.Options.getNGames Data.Default.def) ".",
			let
				flag	= "readTimeout"
			in G.Option "t"	[flag] (
				G.ReqArg (
					Just . (,) flag . Just &&& Input.CommandLineOption.mkOptionsMutator . Data.Options.setReadTimeout . Input.CommandLineOption.readBoundedIntegral
				) $ mkTypeSpecification "Seconds"
			) . showString "define the read-timeout; default " $ shows (Data.Options.getReadTimeout Data.Default.def) " s.",
			let
				flag	= "verifyConfiguration"
			in G.Option ""	[flag] (
				G.NoArg (
					Nothing {-N/A-},
					Input.CommandLineOption.mkOptionsMutator $ Data.Options.setVerifyConfiguration True
				)
			) "verify the mutual compatibility of the two configuration-files before forwarding each to a forked instance of 'bishbosh'.",
			let
				flag	= "appendInputConfigFilePath"
			in G.Option "i"	[flag] (
				G.ReqArg (
					Just . (,) flag . Just &&& Input.CommandLineOption.mkOptionsMutator . Data.Options.appendInputConfigFilePath
				) $ mkTypeSpecification "File-path"
			) "define the path to an XML-file, to forward to a forked instance of 'bishbosh'; specify White's first, then Black's."
		 ] where
			author :: String
			author	= "Dr. Alistair Ward"

			printHelp, printVersion :: Input.CategorisedCommandLineOptions.IOAction
			printHelp	= putStrLn . showString "Usage: " $ G.usageInfo progName optDescrList
			printVersion	= putStrLn . showString progName . showChar '-' . showsVersion Paths.version . showString "\n\nCompiled by " . showString System.Info.compilerName . showChar '-' . showsVersion System.Info.compilerVersion . showString ".\n\nCopyright (C) 2021 " . showString author . showString ".\nThis program comes with ABSOLUTELY NO WARRANTY.\nThis is free software, and you are welcome to redistribute it under certain conditions.\n\nWritten by " $ showString author "." where
				showsVersion :: Data.Version.Version -> ShowS
				showsVersion	= foldr (.) id . Data.List.intersperse (showChar '.') . map shows . Data.Version.versionBranch

			mkTypeSpecification :: ShowS
			mkTypeSpecification s	= showChar '<' $ showString s ">"

	System.Environment.getArgs >>= (
		\case
			(commandLineOptions, [{-non-options-}], [{-errors-}])
				| not $ null ioActions	-> sequence_ ioActions >> System.Exit.exitSuccess
				| otherwise		-> Process.Intermediary.initialise . foldr ($) Data.Default.def $ Input.CategorisedCommandLineOptions.getOptionsMutators categorisedCommandLineOptions	-- Sequentially mutate the configuration, using each of the options-mutators specified on the command-line.
				where
					categorisedCommandLineOptions		= Input.CommandLineOption.categorise $ map snd commandLineOptions
					ioActions				= Input.CategorisedCommandLineOptions.getIOActions categorisedCommandLineOptions
			(_, nonOptions, [])	-> Control.Exception.throwIO . Data.Exception.mkRedundantData . showString "unexpected command-line arguments; " $ shows nonOptions "."
			(_, _, errors)		-> Control.Exception.throwIO . Data.Exception.mkParseFailure $ concat errors
	 ) . G.getOpt G.RequireOrder optDescrList

