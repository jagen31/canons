{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Arrows #-}

module Main where
import Euterpea hiding (a, b, c, d, e, f, g, transpose, invert)
import HSoM
import FRP.UISF.AuxFunctions (unique, SEvent)
import Data.Function (on)
import Data.List hiding (transpose)
import Data.Maybe
import Data.Bifunctor
import Control.Monad.State

-- retain accidentals
data MyPitchClass s = MyPitchClass { pclass :: s, accidental :: Int } deriving (Eq, Functor, Show)
pc = MyPitchClass

type MyPitch = (MyPitchClass PitchClass, Int)

c,d,e,f,g,a,b :: Octave -> Dur -> Music MyPitch

c o d = note d (pc C 0, o)
d o d' = note d' (pc D 0, o)
e o d = note d (pc E 0, o) 
f o d = note d (pc F 0, o) 
g o d = note d (pc G 0, o) 
a o d = note d (pc A 0, o) 
b o d = note d (pc B 0, o) 

-- map to semitones
-- these should really be actual maps
-- this might not be the best use of the field called "accidental"
basis :: [MyPitchClass PitchClass]
basis = [pc C 0, pc D 2, pc E 4, pc F 5, pc G 7, pc A 9, pc B 11]

toIntervals :: [MyPitchClass PitchClass] -> [MyPitchClass PitchClass]
toIntervals ps@((MyPitchClass _ fstA):_) = go ps
  where 
    go ((MyPitchClass p a):[]) = [pc p $ mod (fstA - a) 12]
    go ((MyPitchClass p a):(p2@(MyPitchClass _ a')):rest) = (pc p $ a' - a):(go (p2:rest))

bIntervals = toIntervals basis

encodeFrom :: (Eq a) => [MyPitchClass a] -> a -> Int
encodeFrom b c = fromJust $ findIndex ((==) c . pclass) b

resolveFrom :: (Eq a) => [MyPitchClass a] -> a -> Int
resolveFrom b c = (accidental . fromJust) $ find ((==) c . pclass) b

decodeFrom :: [MyPitchClass a] -> Int -> a
decodeFrom b = pclass . (b !!)

encode = encodeFrom basis
resolve = resolveFrom basis
decode = decodeFrom basis

distanceUp, distanceDown :: Int -> Int -> Int -> Int
distanceUp n1 n2 m = (n2 + m - n1) `mod` m
distanceDown n1 n2 m = m - distanceUp n1 n2 m

-- transpose within given key
-- very ugly- has to check if the transposition crossed C to determine whether
-- to add/subtract an additional octave due to encoding of octaves.
-- ordering is key -> transpose -> pitch
transposeD :: [MyPitchClass PitchClass] -> (Int, Int) -> MyPitch -> MyPitch
transposeD k (i', o') (MyPitchClass sym a, o) = 
  let i = encodeFrom k sym
      c = encodeFrom k C
      l = length k
      m = (i + i') `mod` l
      -- this really can be improved
      cross = if i' > 0 && c /= i && (distanceUp i c l) <= i' then 1 else 
        if i' < 0 && (c == i || (distanceDown i c l) < -i') then -1 else 0
      p = (decodeFrom k m) in
        -- + new accidental in key, + original accidental, - old accidental in key
        (pc p $ (resolveFrom k p) + a - (resolveFrom k sym), o + o' + cross)

toSemitones :: MyPitch -> Int
toSemitones (MyPitchClass p a, o) = (resolve p) + a + 12 * (o + 1)

-- not ideal
diffOctaves :: MyPitch -> MyPitch -> Int
diffOctaves p1 p2 = (toSemitones p1 - toSemitones p2) `div` 12

invertD :: [MyPitchClass PitchClass] -> MyPitch -> MyPitch -> MyPitch
invertD k mp'@(MyPitchClass p' a', o') mp@(MyPitchClass p a, o) = 
  transposeD k (diff, o'') (transposeD k (diff, o'') mp)
      where
        diff = (encodeFrom k p') - (encodeFrom k p)
        o'' = diffOctaves mp' mp

majorScale :: [Int]
majorScale = [2, 2, 1, 2, 2, 2]

makeKey :: MyPitchClass PitchClass -> [Int] -> [MyPitchClass PitchClass]
makeKey p scale = scanl go p scale
  where 
    go (MyPitchClass c a) n = MyPitchClass (decode (mod (encode c + 1) (length basis))) (n - resolveFrom bIntervals c)

gMajor = makeKey (pc G 0) majorScale

applyKey :: [MyPitchClass PitchClass] -> MyPitchClass PitchClass -> MyPitchClass PitchClass
applyKey k (MyPitchClass p a) = pc p (a + (resolveFrom k p))

cantus :: Int -> Music MyPitch
cantus o = bimap (applyKey gMajor) id <$> notes
  where o' = o + 1
        notes = line $ map ($qn) [g o', f o', e o', d o', b o, c o', d o', g o]

crab :: Music a -> Music a
crab m = m :=: (retro m)

canon1 = times 2 $ crab (cantus 2)
canon2 = invertD gMajor (pc B 0, 3) <$> cantus 3

canons = [canon1, canon2]

selectCanon :: SEvent Int -> BufferOperation MidiMessage
selectCanon ap = if isJust ap 
  then AppendToBuffer $ musicToMsgs' defParams ((pitch . toSemitones) <$> canons !! (fromJust ap)) 
  else NoBOp

ui0 :: UISF () ()
ui0 = proc _ -> do
  devid <- selectOutput -< ()
  ap <- radio ["canon 1", "canon 2"] 0 >>> unique -< ()
  midiOutB -< (devid, selectCanon ap)
  returnA -< ()

main :: IO ()
main = runMUI' ui0
