module MonadExamples where

-- List or []
-- twiceWhenEven [1..3] = [1, 4, 4, 0]
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  -- bind applies the functon for each x in xs, then concatinates
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]


-- Maybe
data Cow = Cow {
    name :: String,
    age :: Int,
    weight :: Int
  } deriving (Eq, Show)

validateName :: String -> Maybe String
validateName "" = Nothing
validateName str = Just str

validatePositive :: Int -> Maybe Int
validatePositive n
  | n >= 0 = Just n
  | otherwise = Nothing

bessIfUnderUnder500 :: Cow -> Maybe Cow
bessIfUnderUnder500 cow =
  let w = weight cow
      n = name cow
   in if n == "Bess" && w > 499
        then Nothing
        else Just cow

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case validateName name' of
    Nothing -> Nothing
    Just nammy ->
      case validatePositive age' of
        Nothing -> Nothing
        Just agey ->
          case validatePositive weight' of
            Nothing -> Nothing
            Just weighty ->
              bessIfUnderUnder500
                (Cow nammy agey weighty) -- yuck

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  validName <- validateName name'
  validAge <- validatePositive age'
  validWeight <- validatePositive weight'
  bessIfUnderUnder500 (Cow validName validAge validWeight)

-- Either

type Founded = Int
type Coders = Int

data SoftwareShop = Shop {
    founded     :: Founded,
    programmers :: Coders
  } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Founded -> Either FoundedError Founded
validateFounded n
  | n < 0     = Left $ NegativeYears n
  | n > 500  = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Coders -> Either FoundedError Coders
validateCoders n
  | n < 0     = Left $ NegativeCoders n
  | n > 5000  = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Founded -> Coders -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ TooManyCodersForYears founded coders
    else Right $ Shop { founded = founded, programmers = programmers }
