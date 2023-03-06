import Data.List
import System.Random
import System.IO.Unsafe

users = ["user1","user2","user3","user4"]
items = ["item1","item2","item3","item4","item5","item6"]
purchasesHistory = [("user1",[["item1","item2","item3"],["item1","item2","item4"]]) , ("user2",[["item2","item5"],["item4","item5"]]) , ("user3",[["item3","item2"]]) , ("user4",[])]

{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

randomZeroToX :: Int -> Int
randomZeroToX x = unsafePerformIO (getStdRandom (randomR (0, x)))

{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

createEmptyFreqList :: [a] -> [(a, [b])]
createEmptyFreqList [] = []
createEmptyFreqList (x:xs) = (x,[]):createEmptyFreqList xs

{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

getAllUsersStats :: [(String, [[String]])] -> [(String, [(String, [(String, Int)])])]
getAllUsersStats list = getAllUsersStatsH users list

getAllUsersStatsH [] _ = []
getAllUsersStatsH (x:xs) list = (x,(fillOneUserStats x (createEmptyFreqList items) list)):getAllUsersStatsH xs list
 

fillOneUserStats _ [] _ = []
fillOneUserStats user ((item,empty):rest) list = (item,empty++(stat user item list)):fillOneUserStats user rest list

stat user item list = (freqList item items (concat (getMatch item (getPurchasesHistoryOfUser user list))))


getPurchasesHistoryOfUser user ((user1,x):xs) 
                                             | user == user1 = x
                                             | otherwise = getPurchasesHistoryOfUser user xs											 

getMatch _ [] = []
getMatch item (x:xs) 
                    | elem item x = x:getMatch item xs
                    | otherwise = getMatch item xs
		
freq _ [] = 0
freq item (x:xs) = if item == x then 1+(freq item xs) else freq item xs
		
freqList _ [] _ = []					
freqList itemOut (x:xs) list
                        | (x /= itemOut) && (elem x list) = (x,freq x list):freqList itemOut xs list
                        | otherwise = freqList itemOut xs list						
						
{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

purchasesIntersection :: Eq a => [(a,[(a,Int)])] -> [(a,[(a,[(a,Int)])])] -> [[(a,[(a,Int)])]]
purchasesIntersection _ [] = []			
purchasesIntersection list ((_,list1):rest) = (userIntersection list1 list) : purchasesIntersection list rest

userIntersection [] _ = []
userIntersection ((item,list):rest) list1 
                                         | (addTuples list (getItemList item list1)) == [] = userIntersection rest list1
										 | otherwise = (item,(addTuples list (getItemList item list1))) : userIntersection rest list1

getItemList item ((item1,list):rest) = if item == item1 then list else getItemList item rest

addTuples list1 list2 = (addTuples1 list1 list2) ++ (addTuples2 list2 list1)

addTuples1 _ [] = []
addTuples1 [] _ = []
addTuples1 ((item,num):rest) list
                                | not(elem (item,num) list) = (item,num):addTuples1 rest list
								| otherwise = (item,(num+getFreq item list)):addTuples1 rest list

addTuples2 _ [] = []
addTuples2 [] _ = []
addTuples2 ((item,num):rest) list
                                | not(elem (item,num) list) = (item,num):addTuples2 rest list
								| otherwise = addTuples2 rest list

getFreq _ [] = 0
getFreq item ((item1,num):rest) = if item == item1 then num+getFreq item rest else getFreq item rest

{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}			

freqListItems :: String -> [(String, Int)]
freqListItems user = freqListItemsH items (getPurchasesHistoryOfUser user purchasesHistory)

freqListItemsH [] _ = []
freqListItemsH (item:rest) list 
                          | (freq1 item list) /= 0 = (item,(freq1 item list)) : freqListItemsH rest list
                          | otherwise = freqListItemsH rest list						  

freq1 _ [] = 0
freq1 item (x:xs)
                 | elem item x = (freq1H item x) + freq1 item xs
				 | otherwise = freq1 item xs

freq1H _ [] = 0
freq1H item (x:xs)
                  | item /= x = 1 + freq1H item xs
                  | otherwise = freq1H item xs				  
				  
{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

freqListCart :: String ->[String] -> [(String, Int)]
freqListCart user list = freqListCartH items (getPurchasesHistoryOfUser user purchasesHistory) list

freqListCartH [] _ _ = []
freqListCartH (item:rest) list list1
                                     | (freq2 item list list1) /= 0 = (item,(freq2 item list list1)) : freqListCartH rest list list1
                                     | otherwise = freqListCartH rest list list1						  

freq2 _ [] _ = 0
freq2 item (x:xs) list1
                       | elem item x = (freq2H item x list1) + freq2 item xs list1
				       | otherwise = freq2 item xs list1

freq2H _ [] _ = 0
freq2H item (x:xs) list1
                        | (item /= x) && (elem x list1) = 1 + freq2H item xs list1
                        | otherwise = freq2H item xs list1				  
						
{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

freqListCartAndItems :: String -> [String] -> [(String, Int)]
freqListCartAndItems user list = addTuples3 (freqListItems user) (freqListCart user list)						

addTuples3 list1 list2 = (addTuples4 list1 list2) ++ (addTuples5 list2 list1)

addTuples4 [] _ = []
addTuples4 ((item,num):rest) list
                                | not(itemExist item list) = (item,num):addTuples4 rest list
								| otherwise = (item,(num+getFreq item list)):addTuples4 rest list


addTuples5 [] _ = []
addTuples5 ((item,num):rest) list
                                | not(itemExist item list) = (item,num):addTuples5 rest list
								| otherwise = addTuples5 rest list
									
itemExist _ [] = False
itemExist item ((item1,num):rest) = if item == item1 then True else itemExist item rest							

{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

freqListUsers :: String -> [(String, Int)]
freqListUsers user = addAllTuples (purchasesIntersection (getUserStat user (getAllUsersStats purchasesHistory)) (getOtherUsersStats user (getAllUsersStats purchasesHistory)))

getUserStat user ((user1,list):rest) = if user == user1 then list else getUserStat user rest	

getOtherUsersStats _ [] = []
getOtherUsersStats userOut ((user,list):rest) = if userOut /= user then (user,list):getOtherUsersStats userOut rest else getOtherUsersStats userOut rest

addAllTuples [] = []
addAllTuples (list:[]) = addTuplesInList list
addAllTuples (list:rest) = addTuples3 (addTuplesInList list) (addAllTuples rest)

addTuplesInList [] = []
addTuplesInList ((item,list):[]) = list
addTuplesInList ((item,list):(item1,list1):rest) = addTuples3 (addTuples3 list list1) (addTuplesInList rest)
 
{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

recommendEmptyCart :: String -> String
recommendEmptyCart user = if (freqListItems user) == [] then [] else getRandomItem (freqListItems user)

getRandomItem list = (expandedListConcat list) !! (randomZeroToX ((length (expandedListConcat list)) - 1))

expandedListConcat list = concat (itemsExpandedList list)

itemsExpandedList [] = []
itemsExpandedList (tuple:rest) = ((itemRepeat (tuple)):(itemsExpandedList rest))

itemRepeat (item,0) = []
itemRepeat (item,num) = item:itemRepeat (item,num-1)
 
{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

recommendBasedOnItemsInCart :: String -> [String] -> String
recommendBasedOnItemsInCart user list = if (freqListCartAndItems user list) == [] then [] else getRandomItem (freqListCartAndItems user list)

{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

recommendBasedOnUsers :: String -> String
recommendBasedOnUsers user = if (freqListUsers user) == [] then [] else getRandomItem (freqListUsers user)

{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

recommend :: String -> [String] -> String
recommend user cart
                   | (length (getRecommendation user cart)) == 0 = items !! (randomZeroToX ((length items) - 1))
				   | (length (getRecommendation user cart)) == 1 = (getRecommendation user cart) !! 0
				   | (length (getRecommendation user cart)) == 2 = (getRecommendation user cart) !! (randomZeroToX 1)

getRecommendation user cart
                           | (recommendBasedOnItemsInCart user cart) == [] && (recommendBasedOnUsers user) == [] = []
						   | (recommendBasedOnItemsInCart user cart) /= [] && (recommendBasedOnUsers user) == [] = ((recommendBasedOnItemsInCart user cart):[])
						   | (recommendBasedOnItemsInCart user cart) == [] && (recommendBasedOnUsers user) /= [] = ((recommendBasedOnUsers user):[])
						   | otherwise = ((recommendBasedOnItemsInCart user cart):(recommendBasedOnUsers user):[])

{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

