{-# LANGUAGE OverloadedStrings #-}

module RecordExamples where

import Data.Text ( Text )

data Vehicle
    = Aircraft
        { vaColor :: Text
        , vaWings :: Int
        }
    | Boat
        { vbColor :: Text 
        , vbSails :: Int
        }
    | Car 
        { vcColor  :: Text
        , vcWheels :: Int
        }
    deriving (Eq, Show)

-- Can this vehicle fly in the air?
-- Even though we only want to match on the data constructor, we have to write
-- an `_` for each field in the record
vehicleCanFlyExplicit :: Vehicle -> Bool
vehicleCanFlyExplicit (Aircraft _ _) = True
vehicleCanFlyExplicit _              = False

-- Same function as above, with a simpler syntax
-- Note how the curly braces indicate matching on the data constructor, but 
-- nothing after
vehicleCanFly :: Vehicle -> Bool
vehicleCanFly Aircraft{} = True
vehicleCanFly _          = False

-- What color is this vehicle?
-- Note how the contents of a field are given a name, which is then used 
-- in the function definition
vehicleColor :: Vehicle -> Text
vehicleColor Aircraft { vaColor = color } = "A " <> color <> " aircraft"
vehicleColor Boat     { vbColor = color } = "A " <> color <> " boat"
vehicleColor Car      { vcColor = color } = "A " <> color <> " car"

-- What kind of flying vehicle is this?
-- Note how more than one record field is used
flyingVehicleType :: Vehicle -> Text
flyingVehicleType Aircraft 
    { vaColor = color, vaWings = wingQty }
    | wingQty == 0 = colorText <> "Zeppelin"
    | wingQty == 1 = colorText <> "Stealth Plane"
    | wingQty == 2 = colorText <> "Airplane"
    | wingQty  > 2 = colorText <> "Helicopter"
    | otherwise    = colorText <> "UFO"
  where
    colorText = "A " <> color <> " "
flyingVehicleType _ = "This vehicle can't fly" 
