data RecordProduct a b =
    RecordProduct { pfirst :: a, psecond :: b }
    deriving (Eq, Show)

data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
    Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
    , lang :: ProgLang }
    deriving (Eq, Show)
    
nineToFive :: Programmer
nineToFive = Programmer { os = Mac , lang = Haskell }
-- We can reorder stuff
-- when we use record syntax
feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda, os = GnuPlusLinux }

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]
allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = x, lang = y} 
                    | x <- allOperatingSystems, y <- allLanguages]

