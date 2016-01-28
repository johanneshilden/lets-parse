{-# LANGUAGE OverloadedStrings #-}
module Fri.Json.Tests where

import Data.Attoparsec.Text
import Data.Text                  ( Text, pack, unpack )
import Fri.Json.Parser

import qualified Data.Map.Strict as H

parseFail parser input =
    case parseOnly parser input of
      Left  _ -> putStrLn $ "Done. Assert failure on \"" ++ unpack input ++ "\""
      Right _ -> error $ "Error! Expected failure on pattern \"" ++ unpack input ++ "\""

parseMatch parser input = 
    case parseOnly parser input of
      Left  _ -> error $ "Error! Expected match on pattern \"" ++ unpack input ++ "\""
      Right _ -> putStrLn $ "Done. Assert match on \"" ++ unpack input ++ "\""

--------------------------------------------------------------------------------

runTests = do
    testStrings
    testNumbers
    testBoolean
    testNull
    testObjects
    testArrays
    testInput
    testResults
    testSampleData

testStrings = do
    --
    parseMatch jsonString "\"hello\""
    parseMatch jsonString "\"\""
    --
    parseFail  jsonString "xxx"

testNumbers = do
    --
    parseMatch jsonNumber "0.123213"
    parseMatch jsonNumber "-0.123213"
    parseMatch jsonNumber "-23213"
    parseMatch jsonNumber "23213"
    parseMatch jsonNumber "3.1415"
    parseMatch jsonNumber "3.00"
    parseMatch jsonNumber "1e-005"        -- Yeeah!
    --
    parseFail  jsonNumber ".123213"
    parseFail  jsonNumber "-.123213"
    parseFail  jsonNumber "+123"          -- Not valid according to http://jsonlint.com/

testBoolean = do
    --
    parseMatch jsonBoolean "true"
    parseMatch jsonBoolean "false"
    --
    parseFail  jsonBoolean "lagom"
    parseFail  jsonBoolean "TRUE"
    parseFail  jsonBoolean "fAlse"

testNull = do
    --
    parseMatch jsonNull "null"
    --
    parseFail  jsonNull "bull"

testObjects = do
    --
    parseMatch jsonObject "{}"
    parseMatch jsonObject "{\"key\":\"value\"}"
    parseMatch jsonObject "{\"key\":  \"value\"}"
    parseMatch jsonObject "{\"key\": 1.235}"
    parseMatch jsonObject "{\"key\": 1.235   }"
    parseMatch jsonObject "{  \"key\": 1.235   }"
    --
    parseFail  jsonObject "{\"key\":banana}"
    parseFail  jsonObject "{\"key\": 1.235.123}"

testArrays = do
    --
    parseMatch jsonArray "[]"
    parseMatch jsonArray "[1,2,3,4]"
    parseMatch jsonArray "[1,2,\"hello\",4]"
    parseMatch jsonArray "[1,2,\"hello\"  ,4]"
    parseMatch jsonArray "[  1, 2,    \"hello\"  ,4]"
    parseMatch jsonArray "[  1, 2,    \"hello\"  ,4  ]"
    parseMatch jsonArray "[  1, 2,    \"hello\"  ,4 , null  ]"
    parseMatch jsonArray "[false,  null, false, true, 1, {}, 123 ]"
    parseMatch jsonArray "[{  \"key\": 1.235   }, false, {\"key\":  \"value\"} , {\"key\": 1.235   }, null, false, true, 1, {}, 123 ]"
    parseMatch jsonArray "[ true,  false, false,  true  ]"
    --
    parseFail  jsonArray "[1,2,3,4,]"
    parseFail  jsonArray "[;]"
    parseFail  jsonArray "[.123]"
    parseFail  jsonArray "[ true,  false, blue,  false,  true  ]"

testInput = do
    --
    parseMatch json "[{  \"key\": 1.235   }, false, {\"key\":  \"value\"} , {\"key\": 1.235   }, null, false, true, 1, {}, 123 ]"
    parseMatch json "[]"
    parseMatch json "null"
    parseMatch json "  [{  \"key\": 1.235   }, false, {\"key\":  \"value\"} , {\"key\": 1.235   }, null, false, true, 1, {}, 123 ]"
    parseMatch json "  []"
    parseMatch json "  null"
    --
    parseFail  json "xxx"

assertKey :: Text -> Dictionary -> Json -> IO ()
assertKey key map match =
  case H.lookup key map of
    Just val ->
        if match == val
            then putStrLn $ "Ok. Key \"" ++ unpack key ++ "\" matches value \"" ++ show val ++ "\"."
            else error $ "Key \"" ++ unpack key ++ "\" = \"" ++ show val ++ "\" does not match expected value \"" ++ show match ++ "\"."
    _ -> error $ "Key \"" ++ unpack key ++ "\" not found."

testResults = do
    let result = parse json "  {\"first\": 1.5 , \"second\" : true , \"third\": null, \"fourth\" : \"some_value\"   }"
    case result of
      Done _ (Object map) -> do 
        assertKey "first"  map (Number 1.5)
        assertKey "second" map (Boolean True)
        assertKey "third"  map (Null)
        assertKey "fourth" map (String "some_value")
      _ -> error "Unexpected result."
    let result' = parse json "  {\"false\": false , \"true\"  : true }"
    case result' of
      Done _ (Object map) -> do 
        assertKey "true"  map (Boolean True)
        assertKey "false" map (Boolean False)
      _ -> error "Unexpected result."

testSampleData = do
    let Done _ result = parse json sampleData
    print "Sample data ok."

sampleData = "[ { \"_id\": \"56a9ebf24ce839c01c7e1999\", \"index\": 0, \"guid\": \"e18c208e-bad4-42a9-9b09-748102bf55b1\", \"isActive\": true, \"balance\": \"$3,414.61\", \"picture\": \"http://placehold.it/32x32\", \"age\": 35, \"eyeColor\": \"green\", \"name\": \"Pitts Bates\", \"gender\": \"male\", \"company\": \"BITENDREX\", \"email\": \"pittsbates@bitendrex.com\", \"phone\": \"+1 (904) 545-2277\", \"address\": \"688 Linden Boulevard, Wanamie, New Mexico, 8333\", \"about\": \"Aute nulla cupidatat qui duis officia elit tempor id. Velit incididunt adipisicing sunt do irure. Occaecat excepteur commodo ea laborum officia elit esse laboris velit ipsum sit occaecat anim minim. Esse dolore ex eu ipsum eu. Magna proident proident in nisi sint mollit. Ullamco duis aliquip culpa ut. Lorem consectetur ad sunt excepteur id esse consequat dolor consequat ut labore elit adipisicing laboris.\r\n\", \"registered\": \"2015-06-24T10:08:31 -03:00\", \"latitude\": -39.805345, \"longitude\": 158.535469, \"tags\": [ \"ullamco\", \"aute\", \"nulla\", \"Lorem\", \"cupidatat\", \"nisi\", \"sit\" ], \"friends\": [ { \"id\": 0, \"name\": \"Bertie Martinez\" }, { \"id\": 1, \"name\": \"Zimmerman Butler\" }, { \"id\": 2, \"name\": \"Contreras Bender\" } ], \"greeting\": \"Hello, Pitts Bates! You have 10 unread messages.\", \"favoriteFruit\": \"apple\" }, { \"_id\": \"56a9ebf27dd7baff02acb6c3\", \"index\": 1, \"guid\": \"92f57fbc-09c8-4304-a774-0f396d881b72\", \"isActive\": false, \"balance\": \"$3,844.11\", \"picture\": \"http://placehold.it/32x32\", \"age\": 40, \"eyeColor\": \"green\", \"name\": \"Marsha Mccarty\", \"gender\": \"female\", \"company\": \"EARTHPURE\", \"email\": \"marshamccarty@earthpure.com\", \"phone\": \"+1 (860) 588-3953\", \"address\": \"530 Irving Place, Hoagland, Alaska, 5605\", \"about\": \"Qui Lorem labore officia consectetur cupidatat nostrud. Ullamco anim aute id in dolor irure fugiat ipsum qui qui excepteur eiusmod amet. Laboris magna adipisicing minim fugiat qui incididunt fugiat magna id. Culpa enim excepteur magna eiusmod velit eiusmod elit velit eu. Officia id dolore laboris ad officia anim aliquip qui nulla velit. Proident ea nisi in eiusmod Lorem laborum ullamco duis nulla culpa deserunt sunt adipisicing culpa.\r\n\", \"registered\": \"2014-12-17T11:37:33 -03:00\", \"latitude\": -18.872026, \"longitude\": 10.994166, \"tags\": [ \"Lorem\", \"consequat\", \"labore\", \"velit\", \"cillum\", \"velit\", \"laborum\" ], \"friends\": [ { \"id\": 0, \"name\": \"Fisher Clements\" }, { \"id\": 1, \"name\": \"Phelps Trevino\" }, { \"id\": 2, \"name\": \"Cooley Bridges\" } ], \"greeting\": \"Hello, Marsha Mccarty! You have 3 unread messages.\", \"favoriteFruit\": \"banana\" }, { \"_id\": \"56a9ebf2afcd1473dc2aae0a\", \"index\": 2, \"guid\": \"74096d97-8bfa-4f1b-a796-4b01d8a661c2\", \"isActive\": true, \"balance\": \"$1,482.72\", \"picture\": \"http://placehold.it/32x32\", \"age\": 25, \"eyeColor\": \"green\", \"name\": \"Daphne Larsen\", \"gender\": \"female\", \"company\": \"MOTOVATE\", \"email\": \"daphnelarsen@motovate.com\", \"phone\": \"+1 (926) 562-2238\", \"address\": \"123 Schenck Court, Deercroft, Oklahoma, 4648\", \"about\": \"Esse Lorem officia sint esse veniam reprehenderit aute mollit nisi adipisicing. Mollit labore consectetur elit occaecat nostrud consectetur. Do laboris cupidatat consectetur tempor. Aliqua labore consequat et reprehenderit velit laborum dolor. Est sunt sit commodo Lorem. Pariatur ut duis mollit eiusmod nostrud ipsum esse occaecat dolore cillum. Eiusmod dolor minim aliqua ad labore pariatur officia ipsum.\r\n\", \"registered\": \"2016-01-24T07:02:54 -03:00\", \"latitude\": 23.244008, \"longitude\": -70.242739, \"tags\": [ \"voluptate\", \"ipsum\", \"amet\", \"sint\", \"non\", \"et\", \"dolore\" ], \"friends\": [ { \"id\": 0, \"name\": \"Trujillo Tillman\" }, { \"id\": 1, \"name\": \"Estella Hartman\" }, { \"id\": 2, \"name\": \"Ester Donovan\" } ], \"greeting\": \"Hello, Daphne Larsen! You have 3 unread messages.\", \"favoriteFruit\": \"banana\" }, { \"_id\": \"56a9ebf29b4f7b0b59dbfe00\", \"index\": 3, \"guid\": \"4830d6da-e2c3-40c2-871f-ac3e5ffa5bf7\", \"isActive\": true, \"balance\": \"$1,342.68\", \"picture\": \"http://placehold.it/32x32\", \"age\": 34, \"eyeColor\": \"green\", \"name\": \"Cohen Valencia\", \"gender\": \"male\", \"company\": \"KIDSTOCK\", \"email\": \"cohenvalencia@kidstock.com\", \"phone\": \"+1 (882) 545-3790\", \"address\": \"528 Cass Place, Brecon, Texas, 1695\", \"about\": \"Eiusmod exercitation nisi consequat esse nisi amet veniam mollit veniam ea elit do. Sunt culpa ex in cupidatat sit do commodo ipsum eiusmod tempor consequat. Minim proident laborum minim aliquip nostrud eiusmod laboris.\r\n\", \"registered\": \"2014-03-02T01:52:07 -03:00\", \"latitude\": 48.826724, \"longitude\": 93.074794, \"tags\": [ \"in\", \"dolor\", \"duis\", \"tempor\", \"cillum\", \"laborum\", \"et\" ], \"friends\": [ { \"id\": 0, \"name\": \"Alyson Strickland\" }, { \"id\": 1, \"name\": \"Miles Graves\" }, { \"id\": 2, \"name\": \"Dorothy Acevedo\" } ], \"greeting\": \"Hello, Cohen Valencia! You have 5 unread messages.\", \"favoriteFruit\": \"strawberry\" }, { \"_id\": \"56a9ebf293a438e8ac0df97b\", \"index\": 4, \"guid\": \"87f4639e-89eb-4096-aaec-a657b66008bc\", \"isActive\": true, \"balance\": \"$2,014.81\", \"picture\": \"http://placehold.it/32x32\", \"age\": 40, \"eyeColor\": \"green\", \"name\": \"Moore Duncan\", \"gender\": \"male\", \"company\": \"PLEXIA\", \"email\": \"mooreduncan@plexia.com\", \"phone\": \"+1 (803) 550-2282\", \"address\": \"801 Wilson Avenue, Efland, Hawaii, 1977\", \"about\": \"Culpa laboris deserunt duis commodo do in cillum aliqua. Enim id velit commodo dolor dolore occaecat exercitation cillum est est. Adipisicing anim anim ex proident. Labore in consequat sunt consectetur nisi elit culpa ad occaecat eiusmod eiusmod. Ullamco consectetur incididunt aute veniam eiusmod et quis sit sunt nostrud laborum duis exercitation. Nostrud aliquip nisi consectetur eu dolore et.\r\n\", \"registered\": \"2014-09-26T01:00:11 -03:00\", \"latitude\": -30.988582, \"longitude\": 167.066759, \"tags\": [ \"culpa\", \"ex\", \"commodo\", \"occaecat\", \"adipisicing\", \"dolore\", \"fugiat\" ], \"friends\": [ { \"id\": 0, \"name\": \"Earnestine White\" }, { \"id\": 1, \"name\": \"Mollie Cunningham\" }, { \"id\": 2, \"name\": \"Cathryn Holman\" } ], \"greeting\": \"Hello, Moore Duncan! You have 8 unread messages.\", \"favoriteFruit\": \"strawberry\" }, { \"_id\": \"56a9ebf24defae3a6f0b6a33\", \"index\": 5, \"guid\": \"54e0e11e-83ae-4a99-86d3-91cfc7f5f324\", \"isActive\": false, \"balance\": \"$3,689.40\", \"picture\": \"http://placehold.it/32x32\", \"age\": 22, \"eyeColor\": \"brown\", \"name\": \"Kennedy Bright\", \"gender\": \"male\", \"company\": \"OCTOCORE\", \"email\": \"kennedybright@octocore.com\", \"phone\": \"+1 (816) 600-3651\", \"address\": \"390 Ryerson Street, Richmond, Washington, 4638\", \"about\": \"Ut elit in ea excepteur aliqua pariatur ex Lorem incididunt eiusmod pariatur ut laboris occaecat. Qui eu ad voluptate et ex esse esse nisi non Lorem laborum exercitation est tempor. Magna amet occaecat exercitation duis fugiat sit enim ex id veniam.\r\n\", \"registered\": \"2014-04-15T10:28:47 -03:00\", \"latitude\": 30.292798, \"longitude\": -74.907212, \"tags\": [ \"ex\", \"enim\", \"fugiat\", \"occaecat\", \"deserunt\", \"enim\", \"non\" ], \"friends\": [ { \"id\": 0, \"name\": \"Jennifer Mcdowell\" }, { \"id\": 1, \"name\": \"Sallie Mclaughlin\" }, { \"id\": 2, \"name\": \"Conley Foley\" } ], \"greeting\": \"Hello, Kennedy Bright! You have 1 unread messages.\", \"favoriteFruit\": \"apple\" } ]" 

