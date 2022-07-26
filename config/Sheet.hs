import Data.Function (on)
import Data.List (transpose)
import qualified Data.Map.Strict as Map

-- Used for western music theory

-- Sheet datatype
data Sheet = Sheet {key :: Pattern Note, mode :: Pattern String, numerals :: Pattern Int, sndDom :: Pattern Bool, dim :: Pattern Bool, aug :: Pattern Bool, triSub :: Pattern Bool}

sheet = Sheet {key = "c", mode = "major", numerals = "1", sndDom = "f", dim = "f", aug = "f", triSub = "f"}

progWith limit sheet pt  = note ((limit) $ scale (mode sheet) ( (|+|) ((|+|) (numerals sheet) (-2)) pt) |+ (key sheet))
sndDomProgWith op sheet pt = (note (scale "major" (pt |+ 3)) |- note 7 ) |+ (note $ (op) $ scale "major" ((numerals sheet) |- 1) |+ (key sheet) |+ "7")
dimProg sheet pt = note (scale "diminished" (pt |- 1)) |+ note (key sheet) |+ note (scale (mode sheet) (numerals sheet)) |- note 3
augProg sheet pt = note (scale "augmented" (pt |- 1)) |+ note (key sheet) |+ note (scale (mode sheet) ((numerals sheet) |- 1))
progWithCondition limit sheet pt = (sew (triSub sheet) (note (-6)) (note 0)) +| (sew (sndDom sheet) (sndDomProgWith limit sheet pt) $ sew (dim sheet) (dimProg sheet pt) $ sew (aug sheet) (augProg sheet pt) $ progWith limit sheet pt)
prog = progWithCondition id

-- Used for using parts and segments
stacker = Map.fromList
(!) = (Map.!)
allowedAndNeededKeys = ["lead", "bass", "key", "drums", "pad", "clock", "pod", "arp"]
addSilence pt = map (\x -> if (Map.member x (pt)) then (x, pt ! x) else (x, silence)) allowedAndNeededKeys
filledWithSilence parts = map (\x -> addSilence x) parts
transformBy y [x] = [(show y, snd x)]
transformBy y (x:xs) = (show y, snd x) : (transformBy (y + 1) xs)
transform x = transformBy 1 x
transformStacker parts = stacker $ map (\x -> (fst $ head x, transform x)) (transpose $ filledWithSilence parts)
transformBy _ [] = []

-- Permanent filter transition
lpfBy x d t = ( cutoff (15000 - (15000 - d) * (rotR t $ slow x envL)))
lpf' d t = lpfBy 4 d t
hpfBy x s d t = ( hpf (s + d * (rotR t $ slow x envL)))
hpf' d t = hpfBy 4 0 d t

-- Timesignature
mapfxBy _ _ [] = []
mapfxBy y c [x] = [(show y, (# cps (c * x)))]
mapfxBy y c (x:xs) = (show y, (# cps (c * x)) ) : (mapfxBy (y + 1) c xs)
mapfx = mapfxBy 1