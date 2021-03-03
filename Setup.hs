import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import System.FilePath ((</>))
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose, installOrdinaryFile)
import Distribution.Simple.Setup (installDest, InstallFlags (installVerbosity), CopyFlags (copyDest, copyVerbosity))
import Distribution.Simple.Flag
import Distribution.Verbosity

-- Custom preprocessor for GF files

-- Currently has problems with stack due to:
-- https://github.com/commercialhaskell/stack/issues/3491

-- TODO: Fix the above issue

main :: IO ()
main = defaultMainWithHooks userHooks

userHooks :: UserHooks
userHooks = simpleUserHooks {
    hookedPreProcessors = gfPPSuffix : knownSuffixHandlers
    -- postCopy = \args insF pDesc lbi -> do
    --     print $ buildDir lbi
    --     let cdest = fromFlag $ copyDest insF
    --     let dDir = datadir $ absoluteComponentInstallDirs pDesc lbi (localUnitId lbi) cdest
    --     let verbosity = fromFlag $ copyVerbosity insF
    --     let src = buildDir lbi </> "Prop.pgf"
    --     let dst = dDir </> "Prop.pgf"
    --     print dDir
    --     createDirectoryIfMissingVerbose verbosity True dDir
    --     installOrdinaryFile verbosity src dst
    --     return ()
}

gfPPSuffix :: PPSuffixHandler
gfPPSuffix = ("gf" , gfPP)

gfPP :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
gfPP bi lbi clbi = PreProcessor {
    platformIndependent = True,
    runPreProcessor = \(inDir,inFile) (outDir,outFile) verbosity -> do
        -- putStrLn $ "hello world! " ++ show ((inDir,inFile), (outDir,outFile), verbosity)
        let args =
                [ "-make"
                , "-f", "haskell"
                , "--haskell=gadt"
                , "--haskell=lexical"
                , "--lexical=Noun,Adj,Adj2,Verb,Verb2"
                , "--output-dir=" ++ outDir
                , "--gfo-dir=/tmp"
                , inDir </> inFile
                ]
        print args
        (gfProg, _) <- requireProgram verbosity gfProgram (withPrograms lbi)
        -- runDbProgram verbosity gfProgram (withPrograms lbi) args
        runProgram verbosity gfProg args
        putStrLn "Done running"
}

gfProgram :: Program
gfProgram = simpleProgram "gf"

