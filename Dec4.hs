module Dec4 (solve, taskA, taskB) where
import Common
import Data.Function (on)
import Data.List (sortBy)

data Event
  = BeginShift ID
  | FallAsleep
  | WakeUp
    deriving (Show, Eq)

isBeginShift :: Event -> Bool
isBeginShift (BeginShift _) = True
isBeginShift _              = False

type Timestamp = Int
type LogRecord = (Timestamp, Event)
type Nap = [Minute]
type Minute = Int
type ID = Int

data Guard = Guard {guardId :: ID, nap :: Nap} deriving Show
data Shift = Shift {shiftGuardId :: ID, shiftNap :: Nap} deriving Show

minute :: Timestamp -> Int
minute = (`rem` 60)

timestamp :: Parser Char Timestamp
timestamp = do
  thisChar '['
  year <- int
  month <- thisChar '-' *> int
  day <- thisChar '-' *> int
  hour <- thisChar ' ' *> int
  minute <- thisChar ':' *> int
  thisChar ']'
  pure $ sum [year*12*31*3600, month*31*3600, day*3600, hour*60, minute]

event :: Parser Char Event
event = msum
  [ string "falls asleep" *> eof *> pure FallAsleep
  , string "wakes up" *> eof *> pure WakeUp
  , BeginShift <$> (string "Guard #" *> int <* string " begins shift" <* eof)
  ]

logRecord :: Parser Char LogRecord
logRecord = pair " " timestamp event

parseLog :: String -> [LogRecord]
parseLog = sortBy (compare `on` fst) . map (parse_ logRecord) . lines

parseShifts :: [LogRecord] -> [Shift]
parseShifts = parse_ (many shift)
  where
    nap = do
      (t0, _) <- char ((== FallAsleep) . snd)
      (t1, _) <- char ((== WakeUp) . snd)
      pure [minute t0 .. minute t1-1]
    shift = do
      (_, BeginShift n) <- char (isBeginShift . snd)
      naps <- many nap
      pure $ Shift n (concat naps)

-- | Merge all shifts into a single big one per guard.
groupShifts :: [Shift] -> [Guard]
groupShifts = map merge . soupBy (compare `on` shiftGuardId)
  where merge (Shift n ms:xs) = Guard n (ms ++ concatMap shiftNap xs)

longest :: [a] -> [a] -> Ordering
longest = compare `on` length

taskA :: String -> Int
taskA inp = guardId guard * zzzminute guard
  where
    laziestGuard = greatestBy_ (longest `on` nap)
    guard = laziestGuard $ groupShifts $ parseShifts $ parseLog inp
    zzzminute = head . greatestBy_ longest . soup . nap

taskB :: String -> Int
taskB inp = guardId worstMinuteGuard * worstMinute worstMinuteGuard
  where
    guards = groupShifts $ parseShifts $ parseLog inp
    timesAsleepOnSameMinute = maybe 0 length . greatestBy longest . soup . nap
    worstMinuteGuard = greatestBy_ (compare `on` timesAsleepOnSameMinute) guards
    worstMinute = head . greatestBy_ longest . soup . nap
