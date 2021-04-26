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
}

gfPPSuffix :: PPSuffixHandler
gfPPSuffix = ("gf" , gfPP)

gfPP :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
gfPP bi lbi clbi = PreProcessor {
    platformIndependent = True,
    runPreProcessor = \(inDir,inFile) (outDir,outFile) verbosity -> do
        -- putStrLn $ "hello world! " ++ show ((inDir,inFile), (outDir,outFile), verbosity)
        let lexical = case inFile of 
                        "Answer.gf" -> "Atom"
                        "Prop.gf" -> "Noun,Noun2,Adj,Adj2,Verb,Verb2"
                        "Questions.gf" -> "Atom,Name"
                        _ -> "N,V,A,N2,N3,V2,A2,VA,V2V,VV,V3,VS,V2A,V2S,V2Q,Adv,AdV,AdA,AdN,ACard,CAdv,Conj,Interj,PN,Prep,Pron,Quant,Det,Card,Text,Predet,Subj"
            concrete = case inFile of 
                         "ParsePredicates.gf" -> [inDir </> "ParsePredicatesEng.gf"]
                         _ -> []
        let args =
                [ "-make"
                , "-f", "haskell"
                , "--haskell=gadt"
                , "--haskell=lexical"
                , "--lexical=" ++ lexical
                , "--output-dir=" ++ outDir
                , inDir </> inFile
                ] ++ concrete
                
        print args
        (gfProg, _) <- requireProgram verbosity gfProgram (withPrograms lbi)
        -- runDbProgram verbosity gfProgram (withPrograms lbi) args
        runProgram verbosity gfProg args
        putStrLn "Done running"
}

gfProgram :: Program
gfProgram = simpleProgram "gf"

