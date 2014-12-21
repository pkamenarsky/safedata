{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.SafeData.Instances where

import Data.SafeData.SafeData

import           Control.Applicative
import           Control.Monad
import qualified Data.Array as Array
import qualified Data.Array.Unboxed as UArray
import qualified Data.Array.IArray as IArray
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as Foldable
import           Data.Fixed (HasResolution, Fixed)
import qualified Data.Foldable as F
import           Data.Int
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import           Data.Ix
import           Data.Monoid
import qualified Data.Map as Map
import           Data.Ratio (Ratio, (%), numerator, denominator)
import qualified Data.Sequence as Sequence
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Time.Calendar (Day(..))
import           Data.Time.Clock (DiffTime, NominalDiffTime, UniversalTime(..), UTCTime(..), picosecondsToDiffTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import           Data.Time.Clock.TAI (AbsoluteTime, taiEpoch, addAbsoluteTime, diffAbsoluteTime)
import           Data.Time.LocalTime (LocalTime(..), TimeOfDay(..), TimeZone(..), ZonedTime(..), timeToTimeOfDay, timeOfDayToTime)
import qualified Data.Tree as Tree
#if MIN_VERSION_base(4,7,0)
import           Data.Typeable hiding (Proxy)
#else
import           Data.Typeable
#endif
import           Data.Word
import           System.Time (ClockTime(..), TimeDiff(..), CalendarTime(..), Month(..))
import qualified System.Time as OT
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

instance SafeData a => SafeData [a] where
  putCopy = putCopies; getCopy = getCopies; errorTypeName = typeName1

{-
instance SafeData a => SafeData (Prim a) where
  kind = primitive
  getCopy = contain $
            do e <- unsafeUnPack getCopy
               return $ Prim e
  putCopy (Prim e)
    = contain $ unsafeUnPack (putCopy e)
-}

instance SafeData a => SafeData (Maybe a) where
    getCopy (Array [])  = contain Nothing
    getCopy (Array [v]) = contain $ Just $ safeGet v
    getCopy _           = error "Maybe:getCopy: expecting Array with one element"

    putCopy (Just a)    = contain $ Array [safePut a]
    putCopy Nothing     = contain $ Array []

    errorTypeName = typeName1

instance (SafeData a, SafeData b) => SafeData (Either a b) where
    getCopy (Array [])  = error "Either:getCopy: expecting Array with 2 elements"
    getCopy (Array [BValue True, v])
                        = contain $ Right $ safeGet v
    getCopy (Array [BValue False, v])
                        = contain $ Left $ safeGet v
    getCopy _           = error "Maybe:getCopy: expecting Array with 2 elements"

    putCopy (Right a)   = contain $ Array [BValue True, safePut a]
    putCopy (Left a)    = contain $ Array [BValue False, safePut a]

    errorTypeName = typeName2

instance (SafeData a, Ord a) => SafeData (Set.Set a) where
    getCopy (Array vs) = contain $ Set.fromDistinctAscList $ map safeGet vs
    getCopy _          = error "Set:getCopy: expecting Array"
    putCopy            = contain . safePut . Set.toAscList
    errorTypeName      = typeName1

instance (SafeData a, SafeData b, Ord a) => SafeData (Map.Map a b) where
    getCopy (Array vs) = contain $ Map.fromDistinctAscList $ map safeGet vs
    getCopy _          = error "Map:getCopy: expecting Array"
    putCopy            = contain . safePut . Map.toAscList
    errorTypeName      = typeName2

instance (SafeData a) => SafeData (IntMap.IntMap a) where
    getCopy (Array vs) = contain $ IntMap.fromDistinctAscList $ map safeGet vs
    getCopy _          = error "IntMap:getCopy: expecting Array"
    putCopy            = contain . safePut . IntMap.toAscList
    errorTypeName      = typeName1

instance SafeData IntSet.IntSet where
    getCopy (Array vs) = contain $ IntSet.fromDistinctAscList $ map safeGet vs
    getCopy _          = error "IntSet:getCopy: expecting Array"
    putCopy            = contain . safePut . IntSet.toAscList
    errorTypeName      = typeName

instance (SafeData a) => SafeData (Sequence.Seq a) where
    getCopy (Array vs) = contain $ Sequence.fromList $ map safeGet vs
    getCopy _          = error "Sequence:getCopy: expecting Array"
    putCopy            = contain . safePut . Foldable.toList
    errorTypeName      = typeName1

instance (SafeData a) => SafeData (Tree.Tree a) where
    getCopy (Array [a, b])       = contain $ Tree.Node (safeGet a) (safeGet b)
    getCopy _                    = error "Tree:getCopy: expecting Array with 2 elements"
    putCopy (Tree.Node root sub) = contain $ Array [safePut root, safePut sub]
    errorTypeName                = typeName1

iarray_getCopy :: (Ix i, SafeData e, SafeData i, IArray.IArray a e) => Value -> Contained (a i e)
iarray_getCopy (Array [l, h, vs]) = contain $ IArray.listArray (safeGet l, safeGet h) (safeGet vs)
iarray_getCopy _                  = error "iarray_getCopy: expecting Array with 3 elements"
{-# INLINE iarray_getCopy #-}

iarray_putCopy :: (Ix i, SafeData e, SafeData i, IArray.IArray a e) => a i e -> Contained Value
iarray_putCopy arr = contain $ Array [safePut l, safePut h, safePut (IArray.elems arr)]
  where (l, h) = IArray.bounds arr
{-# INLINE iarray_putCopy #-}

instance (Ix i, SafeData e, SafeData i) => SafeData (Array.Array i e) where
    getCopy = iarray_getCopy
    putCopy = iarray_putCopy
    errorTypeName = typeName2

instance (IArray.IArray UArray.UArray e, Ix i, SafeData e, SafeData i) => SafeData (UArray.UArray i e) where
    getCopy = iarray_getCopy
    putCopy = iarray_putCopy
    errorTypeName = typeName2


instance (SafeData a, SafeData b) => SafeData (a,b) where
    getCopy (Array [a, b]) = contain (safeGet a, safeGet b)
    getCopy _              = error "(,):getCopy: expecting Array with 2 elements"
    putCopy (a, b)         = contain $ Array [safePut a, safePut b]
    errorTypeName          = typeName2
instance (SafeData a, SafeData b, SafeData c) => SafeData (a,b,c) where
    getCopy (Array [a, b, c]) = contain (safeGet a, safeGet b, safeGet c)
    getCopy _              = error "(,):getCopy: expecting Array with 3 elements"
    putCopy (a, b, c)      = contain $ Array [safePut a, safePut b, safePut c]
instance (SafeData a, SafeData b, SafeData c, SafeData d) => SafeData (a,b,c,d) where
    getCopy (Array [a, b, c, d])
                           = contain (safeGet a, safeGet b, safeGet c, safeGet d)
    getCopy _              = error "(,):getCopy: expecting Array with 4 elements"
    putCopy (a, b, c, d)   = contain $ Array [safePut a, safePut b, safePut c, safePut d]
instance (SafeData a, SafeData b, SafeData c, SafeData d, SafeData e) => SafeData (a,b,c,d,e) where
    getCopy (Array [a, b, c, d, e])
                           = contain (safeGet a, safeGet b, safeGet c, safeGet d, safeGet e)
    getCopy _              = error "(,):getCopy: expecting Array with 5 elements"
    putCopy (a, b, c, d, e)
                           = contain $ Array [safePut a, safePut b, safePut c, safePut d, safePut e]
instance (SafeData a, SafeData b, SafeData c, SafeData d, SafeData e, SafeData f) => SafeData (a,b,c,d,e,f) where
    getCopy (Array [a, b, c, d, e, f])
                           = contain (safeGet a, safeGet b, safeGet c, safeGet d, safeGet e, safeGet f)
    getCopy _              = error "(,):getCopy: expecting Array with 6 elements"
    putCopy (a, b, c, d, e, f)
                           = contain $ Array [safePut a, safePut b, safePut c, safePut d, safePut e, safePut f]
instance (SafeData a, SafeData b, SafeData c, SafeData d, SafeData e, SafeData f, SafeData g) => SafeData (a,b,c,d,e,f,g) where
    getCopy (Array [a, b, c, d, e, f, g])
                           = contain (safeGet a, safeGet b, safeGet c, safeGet d, safeGet e, safeGet f, safeGet g)
    getCopy _              = error "(,):getCopy: expecting Array with 7 elements"
    putCopy (a, b, c, d, e, f, g)
                           = contain $ Array [safePut a, safePut b, safePut c, safePut d, safePut e, safePut f, safePut g]

instance SafeData Bool where
  kind = primitive; getCopy (BValue v) = contain v; getCopy _ = error "getCopy: Bool expected"; putCopy = contain . BValue; errorTypeName = typeName
instance SafeData Char where
  kind = primitive; getCopy (CValue v) = contain v; getCopy _ = error "getCopy: Char expected"; getCopies (SValue v) = contain v; getCopies _ = error "getCopies: SValue expected"; putCopy = contain . CValue; putCopies = contain . SValue; errorTypeName = typeName
instance SafeData Double where
  kind = primitive; getCopy (DValue v) = contain v; getCopy _ = error "getCopy: Double expected"; putCopy = contain . DValue; errorTypeName = typeName
instance SafeData Float where
  kind = primitive; getCopy (FValue v) = contain v; getCopy _ = error "getCopy: Float expected"; putCopy = contain . FValue; errorTypeName = typeName
instance SafeData Int where
  kind = primitive; getCopy (IValue v) = contain v; getCopy _ = error "getCopy: Int expected"; putCopy = contain . IValue; errorTypeName = typeName
instance SafeData Int8 where
  kind = primitive; getCopy (I8Value v) = contain v; getCopy _ = error "getCopy: Int8 expected"; putCopy = contain . I8Value; errorTypeName = typeName
instance SafeData Int16 where
  kind = primitive; getCopy (I16Value v) = contain v; getCopy _ = error "getCopy: Int16 expected"; putCopy = contain . I16Value; errorTypeName = typeName
instance SafeData Int32 where
  kind = primitive; getCopy (I32Value v) = contain v; getCopy _ = error "getCopy: Int32 expected"; putCopy = contain . I32Value; errorTypeName = typeName
instance SafeData Int64 where
  kind = primitive; getCopy (I64Value v) = contain v; getCopy _ = error "getCopy: Int64 expected"; putCopy = contain . I64Value; errorTypeName = typeName
instance SafeData Integer where
  kind = primitive; getCopy (BIValue v) = contain v; getCopy _ = error "getCopy: Integer expected"; putCopy = contain . BIValue; errorTypeName = typeName
instance SafeData Ordering where
  kind = primitive; getCopy (OrdValue v) = contain v; getCopy _ = error "getCopy: Ordering expected"; putCopy = contain . OrdValue; errorTypeName = typeName
instance SafeData Word where
  kind = primitive; getCopy (WValue v) = contain v; getCopy _ = error "getCopy: Word expected"; putCopy = contain . WValue; errorTypeName = typeName
instance SafeData Word8 where
  kind = primitive; getCopy (W8Value v) = contain v; getCopy _ = error "getCopy: Word8 expected"; putCopy = contain . W8Value; errorTypeName = typeName
instance SafeData Word16 where
  kind = primitive; getCopy (W16Value v) = contain v; getCopy _ = error "getCopy: Word16 expected"; putCopy = contain . W16Value; errorTypeName = typeName
instance SafeData Word32 where
  kind = primitive; getCopy (W32Value v) = contain v; getCopy _ = error "getCopy: Word32 expected"; putCopy = contain . W32Value; errorTypeName = typeName
instance SafeData Word64 where
  kind = primitive; getCopy (W64Value v) = contain v; getCopy _ = error "getCopy: Word64 expected"; putCopy = contain . W64Value; errorTypeName = typeName
instance SafeData () where
  kind = primitive; getCopy (UValue v) = contain v; getCopy _ = error "getCopy: () expected"; putCopy = contain . UValue; errorTypeName = typeName
instance SafeData B.ByteString where
  kind = primitive; getCopy (BSValue v) = contain v; getCopy _ = error "getCopy: ByteString expected"; putCopy = contain . BSValue; errorTypeName = typeName
instance SafeData L.ByteString where
  kind = primitive; getCopy (BSLValue v) = contain v; getCopy _ = error "getCopy: ByteString expected"; putCopy = contain . BSLValue; errorTypeName = typeName

instance (Integral a, SafeData a) => SafeData (Ratio a) where
    getCopy (Array [a, b]) = contain (safeGet a % safeGet b)
    getCopy _              = error "Ratio:getCopy: expecting Array with 2 elements"
    putCopy v              = contain $ Array [safePut $ numerator v, safePut $ denominator v]
    errorTypeName          = typeName1
instance (HasResolution a, Fractional (Fixed a)) => SafeData (Fixed a) where
    getCopy (Array [a, b]) = contain $ fromRational (safeGet a % safeGet b)
    getCopy _              = error "Fixed:getCopy: expecting Array with 1 element"
    putCopy                = contain . safePut . toRational
    errorTypeName          = typeName1


--  instances for 'text' library

instance SafeData T.Text where
    kind = base
    getCopy = contain . T.decodeUtf8 . safeGet
    putCopy = contain . safePut . T.encodeUtf8
    errorTypeName = typeName

instance SafeData TL.Text where
    kind = base
    getCopy = contain . TL.decodeUtf8 . safeGet
    putCopy = contain . safePut . TL.encodeUtf8
    errorTypeName = typeName


-- instances for 'time' library

instance SafeData Day where
    kind                 = base
    getCopy              = contain . ModifiedJulianDay . safeGet
    putCopy              = contain . safePut . toModifiedJulianDay
    errorTypeName        = typeName

instance SafeData DiffTime where
    kind                 = primitive
    getCopy (UTCValue v) = contain $ picosecondsToDiffTime v
    getCopy _            = error "DiffTime:getCopy: expecting UTCValue"
    putCopy              = contain . UTCValue . truncate
    errorTypeName        = typeName

instance SafeData UniversalTime where
    kind                 = primitive
    getCopy (UTCValue v) = contain $ ModJulianDate $ fromIntegral v
    getCopy _            = error "UniversalTime:getCopy: expecting UTCValue"
    putCopy              = contain . UTCValue . truncate . getModJulianDate
    errorTypeName        = typeName

instance SafeData UTCTime where
    kind                 = primitive
    getCopy (UTCValue v) = contain $ posixSecondsToUTCTime $ fromIntegral v
    getCopy _            = error "UTCTime:getCopy: expecting UTCValue"
    putCopy              = contain . UTCValue . truncate . utcTimeToPOSIXSeconds
    errorTypeName        = typeName

instance SafeData NominalDiffTime where
    kind                 = primitive
    getCopy (UTCValue v) = contain $ fromIntegral v
    getCopy _            = error "NominalDiffTime:getCopy: expecting UTCValue"
    putCopy              = contain . UTCValue . truncate
    errorTypeName        = typeName

instance SafeData TimeOfDay where
    kind                 = primitive
    getCopy (UTCValue v) = contain $ timeToTimeOfDay $ picosecondsToDiffTime v
    getCopy _            = error "TimeOfDay:getCopy: expecting UTCValue"
    putCopy              = contain . UTCValue . truncate . timeOfDayToTime
    errorTypeName        = typeName

instance SafeData TimeZone where
    kind = base
    getCopy (Array [mins, BValue summerOnly, SValue zoneName])
                         = contain $ TimeZone (safeGet mins) summerOnly zoneName
    getCopy _            = error "TimeZone:getCopy: expecting Array with 3 elements"
    putCopy t            = contain $ Array [ safePut $ timeZoneMinutes t
                                           , safePut $ timeZoneSummerOnly t
                                           , safePut $ timeZoneName t
                                           ]
    errorTypeName        = typeName

instance SafeData LocalTime where
    kind                   = base
    getCopy (Array [day, tod])
                           = contain $ LocalTime (safeGet day) (safeGet tod)
    getCopy _              = error "LocalTime:getCopy: expecting Array with 2 elements"
    putCopy t              = contain $ Array [safePut $ localDay t, safePut $ localTimeOfDay t]
    errorTypeName          = typeName

instance SafeData ZonedTime where
    kind                   = base
    getCopy (Array [lt, tz])
                           = contain $ ZonedTime (safeGet lt) (safeGet tz)
    getCopy _              = error "ZonedTime:getCopy: expecting Array with 2 elements"
    putCopy t              = contain $ Array [safePut $ zonedTimeToLocalTime t, safePut $ zonedTimeZone t]
    errorTypeName          = typeName

instance SafeData AbsoluteTime where
  getCopy = contain . toAbsoluteTime . safeGet
    where
      toAbsoluteTime :: DiffTime -> AbsoluteTime
      toAbsoluteTime dt = addAbsoluteTime dt taiEpoch
  putCopy = contain . safePut . fromAbsoluteTime
    where
      fromAbsoluteTime :: AbsoluteTime -> DiffTime
      fromAbsoluteTime at = diffAbsoluteTime at taiEpoch
  errorTypeName = typeName

{-
-- instances for old-time

instance SafeData ClockTime where
    kind = base
    getCopy = contain $ do secs <- safeGet
                           pico <- safeGet
                           return (TOD secs pico)
    putCopy (TOD secs pico) =
              contain $ do safePut secs
                           safePut pico

instance SafeData TimeDiff where
    kind = base
    getCopy   = contain $ do year    <- get
                             month   <- get
                             day     <- get
                             hour    <- get
                             mins    <- get
                             sec     <- get
                             pico    <- get
                             return (TimeDiff year month day hour mins sec pico)
    putCopy t = contain $ do put (tdYear t)
                             put (tdMonth t)
                             put (tdDay t)
                             put (tdHour t)
                             put (tdMin t)
                             put (tdSec t)
                             put (tdPicosec t)

instance SafeData OT.Day where
    kind = base ; getCopy = contain $ toEnum <$> get ; putCopy = contain . put . fromEnum

instance SafeData Month where
    kind = base ; getCopy = contain $ toEnum <$> get ; putCopy = contain . put . fromEnum


instance SafeData CalendarTime where
    kind = base
    getCopy   = contain $ do year   <- get
                             month  <- safeGet
                             day    <- get
                             hour   <- get
                             mins   <- get
                             sec    <- get
                             pico   <- get
                             wday   <- safeGet
                             yday   <- get
                             tzname <- safeGet
                             tz     <- get
                             dst    <- get
                             return (CalendarTime year month day hour mins sec pico wday yday tzname tz dst)
    putCopy t = contain $ do put     (ctYear t)
                             safePut (ctMonth t)
                             put     (ctDay t)
                             put     (ctHour t)
                             put     (ctMin t)
                             put     (ctSec t)
                             put     (ctPicosec t)
                             safePut (ctWDay t)
                             put     (ctYDay t)
                             safePut (ctTZName t)
                             put     (ctTZ t)
                             put     (ctIsDST t)
-}

getGenericVector :: (SafeData a, VG.Vector v a) => Value -> Contained (v a)
getGenericVector = contain . VG.fromList . safeGet

putGenericVector :: (SafeData a, VG.Vector v a) => v a -> Contained Value
putGenericVector = contain . safePut . VG.toList

instance SafeData a => SafeData (V.Vector a) where
    getCopy = getGenericVector
    putCopy = putGenericVector

instance (SafeData a, VP.Prim a) => SafeData (VP.Vector a) where
    getCopy = getGenericVector
    putCopy = putGenericVector

instance (SafeData a, VS.Storable a) => SafeData (VS.Vector a) where
    getCopy = getGenericVector
    putCopy = putGenericVector

instance (SafeData a, VU.Unbox a) => SafeData (VU.Vector a) where
    getCopy = getGenericVector
    putCopy = putGenericVector

typeName :: Typeable a => Proxy a -> String
typeName proxy = show (typeOf (undefined `asProxyType` proxy))

typeName1 :: (Typeable1 c) => Proxy (c a) -> String
typeName1 proxy = show (typeOf1 (undefined `asProxyType` proxy))

typeName2 :: (Typeable2 c) => Proxy (c a b) -> String
typeName2 proxy = show (typeOf2 (undefined `asProxyType` proxy))

