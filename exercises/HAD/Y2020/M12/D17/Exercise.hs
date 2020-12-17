{-# LANGUAGE OverloadedStrings #-}

module Y2020.M12.D17.Exercise where

{-- if you look at how we upload capitals at ...

import Y2020.M12.D10.Solution (Capital, Capital(Capital))
import qualified Y2020.M12.D11.Solution as Caps

... you see the work of uploading Capital lat/long data to the graph-store
is accomplished, but today, we want to dowload the capitals with the
lat/longs, given the capital's country. Let's do that.

... I think we may need to create a new Capital-type, including the lat/long,
to download it from the graph-store ... OR we use the CountryInfo-type, so
we can populate a map with the Keyhole Markup Language/KML.

An example of us populating a map with KML is at 

Y2020.M11.D23.Solution.nato

... I think this example needs to be used as Data.XHTML.KML-documentation.

But, one way to look at today's #haskell exercise is a rewrite of the
nato-function to work with data from the graph store ... and to display
any alliance, just not NATO. Today, we want to display the Five Power
Defence Arrangements, and we can do that with

Y2020.M11.D23.Solution.kmlifyAlliance

we just need to populate the CountryInfoMap and the AllianceMap to do so.

We have our work cut out for us.
--}

import Data.Aeson

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Text as T

import Graph.Query
import Graph.JSON.Cypher
import Graph.JSON.Cypher.Read.Rows (TableRow)
import qualified Graph.JSON.Cypher.Read.Rows as RR

import Data.Aeson.WikiDatum
import Data.XHTML.KML

import Y2020.M10.D30.Solution hiding (name)     -- for Alliance
import Y2020.M11.D17.Solution                   -- for CountryInfo and ..-Map
import Y2020.M11.D23.Solution                   -- KMLificiation
import Y2020.M12.D15.Solution                   -- for capital

-- So, where we left off yesterday:

import Y2020.M12.D16.Solution

-- ... was to import the countries of a given alliance. Let's add capitals.
-- To do this, we need to ingest the capitals from the JSON returned

instance FromJSON Capital where
   parseJSON = undefined

-- We also need a query to extract the capital for a given country:

capitalOfQuery :: Country -> Cypher
capitalOfQuery (Country c _ _) =
   T.concat ["MATCH (:Country { name: \"", c, "\" })-[:CAPITAL]->(c:Capital) ",
             "RETURN c"]

capitalOf :: Endpoint -> Country -> IO Capital
capitalOf url country = undefined

{--
First things first. Let's get our alliance and its countries:

>>> graphEndpoint
...
>>> let url = it
>>> getGraphResponse url [allianceCountriesQuery fpda]
"{\"results\":[{\"columns\":[\"c\"],\"data\":...
>>> let resp = it
>>> (RR.justRows resp) :: [TableRow [Country]]
[TR {row = [Country {country = "United Kingdom", ...}]}]

So we have a list of countries, but, for an Alliance, we need the Set of
countries, the create our Alliance
--}

fetchAlliance :: Endpoint -> Name -> IO (Maybe Alliance)
fetchAlliance url allianceName = undefined

-- The below two functions may be useful to getting to the above fetch-function.

fetchAllianceNames :: Endpoint -> Name -> IO [AllianceNames]
fetchAllianceNames url allianceName = undefined

fetchAllianceCountries :: Endpoint -> Name -> IO (Set Country)
fetchAllianceCountries url allianceName = undefined

-- work to getting to the above 2 fetchers are in Y2020.M12.D16.Solution

{--
>>> fetchAlliance url fpda
Just (Alliance {name = "Five Power Defence Arrangements", aliases = fromList [],                countries = fromList ["Australia","Malaysia","New Zealand",
                                      "Singapore","United Kingdom"]})

Okay. Great. Now, let's get the capitals of the countries:
--}

fetchCapital :: Endpoint -> Country -> IO (Maybe Capital)
fetchCapital url (Country c _ _) = undefined

-- with the country and the capital you can build a CountryInfo value, and
-- from there, a CountryInfoMap

countryInfoFor :: Country -> Capital -> CountryInfo
countryInfoFor = undefined

-- from thence:

fetchCountryInfoMap :: Endpoint -> Set Country -> IO CountryInfoMap
fetchCountryInfoMap = undefined

{-- BONUS -------------------------------------------------------

Now that we have an alliance, we can build (a very simple) AllianceMap,
and we have the CountryInfoMap. This means we can display an alliance and
its countries.

Using kmlifyAlliances, on a global-viewer, show the Five Power Defence 
Arrangements.
--}
