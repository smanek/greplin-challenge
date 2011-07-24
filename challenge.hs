import Data.Char
import qualified List as List

slice xs (i+1) k = take (k-i) $ drop i xs

findPalindromes :: String -> [String]
findPalindromes str = [substring |    
                       x <- [1..(length str)],
                       y <- [(x+1)..(length str)],
                       let substring = slice str x y,
                       substring == reverse substring]
                      
--removes punction, whitespace, and makes letters lowercase
cleanString :: String -> String
cleanString (x : xs)
  | isSpace x = cleanString xs
  | isPunctuation x = cleanString xs
  | otherwise = toLower x : cleanString xs
cleanString _ = []                

maxBy :: (Ord a) => [t] -> (t -> a) -> Maybe t
maxBy (x : xs) fn = Just (fst (maxByHelper xs fn (x, fn x)))
    where maxByHelper :: (Ord a) => [t] -> (t -> a) -> (t, a) -> (t, a)
          maxByHelper (x : xs) fn (maxVal, maxMetric)
              | fn x > maxMetric = maxByHelper xs fn (x, fn x)
              | fn x <= maxMetric = maxByHelper xs fn (maxVal, maxMetric)
          maxByHelper _ _ res = res
maxBy _ _ = Nothing                            

longestPalindrome :: String -> Maybe String
longestPalindrome str =  maxBy (findPalindromes str) length

level1 :: Maybe String
level1 = longestPalindrome corpus 
    where corpus = cleanString "FourscoreandsevenyearsagoourfaathersbroughtforthonthiscontainentanewnationconceivedinzLibertyanddedicatedtothepropositionthatallmenarecreatedequalNowweareengagedinagreahtcivilwartestingwhetherthatnaptionoranynartionsoconceivedandsodedicatedcanlongendureWeareqmetonagreatbattlefiemldoftzhatwarWehavecometodedicpateaportionofthatfieldasafinalrestingplaceforthosewhoheregavetheirlivesthatthatnationmightliveItisaltogetherfangandproperthatweshoulddothisButinalargersensewecannotdedicatewecannotconsecratewecannothallowthisgroundThebravelmenlivinganddeadwhostruggledherehaveconsecrateditfaraboveourpoorponwertoaddordetractTgheworldadswfilllittlenotlenorlongrememberwhatwesayherebutitcanneverforgetwhattheydidhereItisforusthelivingrathertobededicatedheretotheulnfinishedworkwhichtheywhofoughtherehavethusfarsonoblyadvancedItisratherforustobeherededicatedtothegreattdafskremainingbeforeusthatfromthesehonoreddeadwetakeincreaseddevotiontothatcauseforwhichtheygavethelastpfullmeasureofdevotionthatweherehighlyresolvethatthesedeadshallnothavediedinvainthatthisnationunsderGodshallhaveanewbirthoffreedomandthatgovernmentofthepeoplebythepeopleforthepeopleshallnotperishfromtheearth"
          
fib :: [Integer]
fib = 1 : 2 : zipWith (+) fib (tail fib)

isPrime :: Integer -> Bool
isPrime p = p > 1 && 
            (all (\n -> p `mod` n /= 0 ) $ takeWhile (\n -> n*n <= p) [2..])
            
primeFibGreaterThan :: Integer -> Integer
primeFibGreaterThan min = head $ dropWhile 
                          (\x -> x < min || not (isPrime x))
                          fib
primeFactors n = factor n primes
  where factor n (p:ps) | p*p > n = [n]
                        | n `mod` p /= 0 = factor n ps
                        | otherwise = p : factor (n `div` p) (p:ps)
        primes = 2 : filter ((==1) . length . primeFactors) [3,5..]
        
level2 :: Integer
level2 = sum $ List.nub $ primeFactors (1 + primeFibGreaterThan 227000)

subsets :: [Int] -> [[Int]]
subsets [] = [[]]
subsets (x:xs) = map (x:) y ++ y
                  where y = subsets xs

acceptableSubsetP :: [Int] -> Bool
acceptableSubsetP [] = False
acceptableSubsetP lst = sum f == sum l
    where (f, l) = splitAt ((length lst) - 1) lst
          
acceptableSubsets :: [Int] -> [[Int]]
acceptableSubsets lst = filter acceptableSubsetP (subsets lst)

level3 :: Int
level3 = length $ acceptableSubsets dt
         where dt = [3, 4, 9, 14, 15, 19, 28, 37, 47, 50, 54, 56, 59, 61, 70, 73, 78, 81, 92, 95, 97, 99]