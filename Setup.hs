import           Data.Maybe
import           Distribution.PackageDescription    hiding (Flag)
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils
import           System.Directory

main = defaultMainWithHooks simpleUserHooks
  {
    preConf = makeExtLib
  , confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
  , postConf = disablePostConfHooks -- seems crucial to not reset extraLibDir
  , preBuild = updateLibDirs
  , postCopy = copyExtLib
  , postClean = cleanExtLib
  }

makeExtLib :: Args -> ConfigFlags -> IO HookedBuildInfo
makeExtLib _ flags = do
    let verbosity = fromFlag $ configVerbosity flags
    rawSystemExit verbosity "env"
        ["make", "--directory=ext_lib"]
    return emptyHookedBuildInfo

updateLibDirs :: Args -> BuildFlags -> IO HookedBuildInfo
updateLibDirs _ _ = do
    dir <- getCurrentDirectory
    putStrLn $ "NOTE: preBuild: putting " ++ dir ++ "/ext_lib/lib into extraLibDirs"
    let extlibDir = dir ++ "/ext_lib/lib"
        bi = emptyBuildInfo { extraLibDirs = [ extlibDir ] }
    return (Just bi, [])

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
    let packageDescription = localPkgDescr localBuildInfo
        lib = fromJust $ library packageDescription
        libBuild = libBuildInfo lib
        libPref = libdir $ absoluteInstallDirs packageDescription localBuildInfo NoCopyDest
    putStrLn $ "NOTE: confHook: adding " ++ libPref ++ " to extraLibDirs"
    return localBuildInfo {
        localPkgDescr = packageDescription {
            library = Just $ lib {
                libBuildInfo = libBuild {
                    extraLibDirs = libPref : extraLibDirs libBuild
                }
            }
        }
    }

disablePostConfHooks :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
disablePostConfHooks args flags pd lbi = return ()

copyExtLib :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyExtLib _ flags pkg_descr lbi = do
    let libPref = libdir . absoluteInstallDirs pkg_descr lbi
                . fromFlag . copyDest
                $ flags
        config = configFlags lbi
        verb = fromFlag $ configVerbosity config
    putStrLn $ "NOTE: postcopy: installing ext_lib/lib/libext.a to " ++ libPref ++ "/libext.a"
    installExecutableFile verb "ext_lib/lib/libext.a" (libPref ++ "/libext.a")

cleanExtLib :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
cleanExtLib _ flags _ _ =
    let verbosity = fromFlag $ cleanVerbosity flags
    in rawSystemExit verbosity "env" ["make", "--directory=ext_lib", "clean"]
