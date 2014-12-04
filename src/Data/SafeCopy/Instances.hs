{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.SafeCopy.Instances where

import Data.SafeCopy.SafeCopy

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
import           Data.Time.Clock (DiffTime, NominalDiffTime, UniversalTime(..), UTCTime(..))
import           Data.Time.Clock.TAI (AbsoluteTime, taiEpoch, addAbsoluteTime, diffAbsoluteTime)
import           Data.Time.LocalTime (LocalTime(..), TimeOfDay(..), TimeZone(..), ZonedTime(..))
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

instance SafeCopy a => SafeCopy [a] where
  putCopy = putCopies; getCopy = getCopies; errorTypeName = typeName1

{-
instance SafeCopy a => SafeCopy (Prim a) where
  kind = primitive
  getCopy = contain $
            do e <- unsafeUnPack getCopy
               return $ Prim e
  putCopy (Prim e)
    = contain $ unsafeUnPack (putCopy e)
-}

instance SafeCopy a => SafeCopy (Maybe a) where
    getCopy (Array [])  = contain Nothing
    getCopy (Array [v]) = contain $ Just $ safeGet v
    getCopy _           = error "Maybe:getCopy: expecting Array with one element"

    putCopy (Just a)    = contain $ Array [safePut a]
    putCopy Nothing     = contain $ Array []

    errorTypeName = typeName1

instance (SafeCopy a, Ord a) => SafeCopy (Set.Set a) where
    getCopy (Array vs) = contain $ Set.fromDistinctAscList $ map safeGet vs
    getCopy _          = error "Set:getCopy: expecting Array"
    putCopy            = contain . safePut . Set.toAscList
    errorTypeName      = typeName1

instance (SafeCopy a, SafeCopy b, Ord a) => SafeCopy (Map.Map a b) where
    getCopy (Array vs) = contain $ Map.fromDistinctAscList $ map safeGet vs
    getCopy _          = error "Map:getCopy: expecting Array"
    putCopy            = contain . safePut . Map.toAscList
    errorTypeName      = typeName2

instance (SafeCopy a) => SafeCopy (IntMap.IntMap a) where
    getCopy (Array vs) = contain $ IntMap.fromDistinctAscList $ map safeGet vs
    getCopy _          = error "IntMap:getCopy: expecting Array"
    putCopy            = contain . safePut . IntMap.toAscList
    errorTypeName      = typeName1

instance SafeCopy IntSet.IntSet where
    getCopy (Array vs) = contain $ IntSet.fromDistinctAscList $ map safeGet vs
    getCopy _          = error "IntSet:getCopy: expecting Array"
    putCopy            = contain . safePut . IntSet.toAscList
    errorTypeName      = typeName

instance (SafeCopy a) => SafeCopy (Sequence.Seq a) where
    getCopy (Array vs) = contain $ Sequence.fromList $ map safeGet vs
    getCopy _          = error "Sequence:getCopy: expecting Array"
    putCopy            = contain . safePut . Foldable.toList
    errorTypeName      = typeName1

instance (SafeCopy a) => SafeCopy (Tree.Tree a) where
    getCopy (Array [a, b])       = contain $ Tree.Node (safeGet a) (safeGet b)
    getCopy _                    = error "Tree:getCopy: expecting Array with 2 elements"
    putCopy (Tree.Node root sub) = contain $ Array [safePut root, safePut sub]
    errorTypeName                = typeName1

iarray_getCopy :: (Ix i, SafeCopy e, SafeCopy i, IArray.IArray a e) => Value -> Contained (a i e)
iarray_getCopy (Array [l, h, vs]) = contain $ IArray.listArray (safeGet l, safeGet h) (safeGet vs)
iarray_getCopy _                  = error "iarray_getCopy: expecting Array with 3 elements"
{-# INLINE iarray_getCopy #-}

iarray_putCopy :: (Ix i, SafeCopy e, SafeCopy i, IArray.IArray a e) => a i e -> Contained Value
iarray_putCopy arr = contain $ Array [safePut l, safePut h, safePut (IArray.elems arr)]
  where (l, h) = IArray.bounds arr
{-# INLINE iarray_putCopy #-}

instance (Ix i, SafeCopy e, SafeCopy i) => SafeCopy (Array.Array i e) where
    getCopy = iarray_getCopy
    putCopy = iarray_putCopy
    errorTypeName = typeName2

instance (IArray.IArray UArray.UArray e, Ix i, SafeCopy e, SafeCopy i) => SafeCopy (UArray.UArray i e) where
    getCopy = iarray_getCopy
    putCopy = iarray_putCopy
    errorTypeName = typeName2


instance (SafeCopy a, SafeCopy b) => SafeCopy (a,b) where
    getCopy (Array [a, b]) = contain (safeGet a, safeGet b)
    getCopy _              = error "(,):getCopy: expecting Array with 2 elements"
    putCopy (a, b)         = contain $ Array [safePut a, safePut b]
    errorTypeName          = typeName2
{-
instance (SafeCopy a, SafeCopy b, SafeCopy c) => SafeCopy (a,b,c) where
    getCopy = contain $ liftM3 (,,) safeGet safeGet safeGet
    putCopy (a,b,c) = contain $ safePut a >> safePut b >> safePut c
instance (SafeCopy a, SafeCopy b, SafeCopy c, SafeCopy d) => SafeCopy (a,b,c,d) where
    getCopy = contain $ liftM4 (,,,) safeGet safeGet safeGet safeGet
    putCopy (a,b,c,d) = contain $ safePut a >> safePut b >> safePut c >> safePut d
instance (SafeCopy a, SafeCopy b, SafeCopy c, SafeCopy d, SafeCopy e) =>
         SafeCopy (a,b,c,d,e) where
    getCopy = contain $ liftM5 (,,,,) safeGet safeGet safeGet safeGet safeGet
    putCopy (a,b,c,d,e) = contain $ safePut a >> safePut b >> safePut c >> safePut d >> safePut e
instance (SafeCopy a, SafeCopy b, SafeCopy c, SafeCopy d, SafeCopy e, SafeCopy f) =>
         SafeCopy (a,b,c,d,e,f) where
    getCopy = contain $ (,,,,,) <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet
    putCopy (a,b,c,d,e,f) = contain $ safePut a >> safePut b >> safePut c >> safePut d >>
                                      safePut e >> safePut f
instance (SafeCopy a, SafeCopy b, SafeCopy c, SafeCopy d, SafeCopy e, SafeCopy f, SafeCopy g) =>
         SafeCopy (a,b,c,d,e,f,g) where
    getCopy = contain $ (,,,,,,) <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*>
                                     safeGet <*> safeGet <*> safeGet
    putCopy (a,b,c,d,e,f,g) = contain $ safePut a >> safePut b >> safePut c >> safePut d >>
                                        safePut e >> safePut f >> safePut g
-}

instance SafeCopy Bool where
  kind = primitive; getCopy (BValue v) = contain v; getCopy _ = error "getCopy: Bool expected"; putCopy = contain . BValue; errorTypeName = typeName
instance SafeCopy Char where
  kind = primitive; getCopy (CValue v) = contain v; getCopy _ = error "getCopy: Char expected"; getCopies (SValue v) = contain v; getCopies _ = error "getCopies: SValue expected"; putCopy = contain . CValue; putCopies = contain . SValue; errorTypeName = typeName
instance SafeCopy Double where
  kind = primitive; getCopy (DValue v) = contain v; getCopy _ = error "getCopy: Double expected"; putCopy = contain . DValue; errorTypeName = typeName
instance SafeCopy Float where
  kind = primitive; getCopy (FValue v) = contain v; getCopy _ = error "getCopy: Float expected"; putCopy = contain . FValue; errorTypeName = typeName
instance SafeCopy Int where
  kind = primitive; getCopy (IValue v) = contain v; getCopy _ = error "getCopy: Int expected"; putCopy = contain . IValue; errorTypeName = typeName
instance SafeCopy Int8 where
  kind = primitive; getCopy (I8Value v) = contain v; getCopy _ = error "getCopy: Int8 expected"; putCopy = contain . I8Value; errorTypeName = typeName
instance SafeCopy Int16 where
  kind = primitive; getCopy (I16Value v) = contain v; getCopy _ = error "getCopy: Int16 expected"; putCopy = contain . I16Value; errorTypeName = typeName
instance SafeCopy Int32 where
  kind = primitive; getCopy (I32Value v) = contain v; getCopy _ = error "getCopy: Int32 expected"; putCopy = contain . I32Value; errorTypeName = typeName
instance SafeCopy Int64 where
  kind = primitive; getCopy (I64Value v) = contain v; getCopy _ = error "getCopy: Int64 expected"; putCopy = contain . I64Value; errorTypeName = typeName
instance SafeCopy Integer where
  kind = primitive; getCopy (BIValue v) = contain v; getCopy _ = error "getCopy: Integer expected"; putCopy = contain . BIValue; errorTypeName = typeName
instance SafeCopy Ordering where
  kind = primitive; getCopy (OrdValue v) = contain v; getCopy _ = error "getCopy: Ordering expected"; putCopy = contain . OrdValue; errorTypeName = typeName
instance SafeCopy Word where
  kind = primitive; getCopy (WValue v) = contain v; getCopy _ = error "getCopy: Word expected"; putCopy = contain . WValue; errorTypeName = typeName
instance SafeCopy Word8 where
  kind = primitive; getCopy (W8Value v) = contain v; getCopy _ = error "getCopy: Word8 expected"; putCopy = contain . W8Value; errorTypeName = typeName
instance SafeCopy Word16 where
  kind = primitive; getCopy (W16Value v) = contain v; getCopy _ = error "getCopy: Word16 expected"; putCopy = contain . W16Value; errorTypeName = typeName
instance SafeCopy Word32 where
  kind = primitive; getCopy (W32Value v) = contain v; getCopy _ = error "getCopy: Word32 expected"; putCopy = contain . W32Value; errorTypeName = typeName
instance SafeCopy Word64 where
  kind = primitive; getCopy (W64Value v) = contain v; getCopy _ = error "getCopy: Word64 expected"; putCopy = contain . W64Value; errorTypeName = typeName
instance SafeCopy () where
  kind = primitive; getCopy (UValue v) = contain v; getCopy _ = error "getCopy: () expected"; putCopy = contain . UValue; errorTypeName = typeName
instance SafeCopy B.ByteString where
  kind = primitive; getCopy (BSValue v) = contain v; getCopy _ = error "getCopy: ByteString expected"; putCopy = contain . BSValue; errorTypeName = typeName
instance SafeCopy L.ByteString where
  kind = primitive; getCopy (BSLValue v) = contain v; getCopy _ = error "getCopy: ByteString expected"; putCopy = contain . BSLValue; errorTypeName = typeName

{-
instance (Integral a, SafeCopy a) => SafeCopy (Ratio a) where
    getCopy   = contain $ do n <- safeGet
                             d <- safeGet
                             return (n % d)
    putCopy r = contain $ do safePut (numerator   r)
                             safePut (denominator r)
    errorTypeName = typeName1
instance (HasResolution a, Fractional (Fixed a)) => SafeCopy (Fixed a) where
    getCopy   = contain $ fromRational <$> safeGet
    putCopy   = contain . safePut . toRational
    errorTypeName = typeName1

instance SafeCopy () where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
instance SafeCopy Bool where
    getCopy = contain get; putCopy = contain . put; errorTypeName = typeName
instance (SafeCopy a, SafeCopy b) => SafeCopy (Either a b) where
    getCopy = contain $ do n <- get
                           if n then liftM Right safeGet
                                else liftM Left safeGet
    putCopy (Right a) = contain $ put True >> safePut a
    putCopy (Left a) = contain $ put False >> safePut a

    errorTypeName = typeName2

--  instances for 'text' library

instance SafeCopy T.Text where
    kind = base
    getCopy = contain $ T.decodeUtf8 <$> safeGet
    putCopy = contain . safePut . T.encodeUtf8
    errorTypeName = typeName

instance SafeCopy TL.Text where
    kind = base
    getCopy = contain $ TL.decodeUtf8 <$> safeGet
    putCopy = contain . safePut . TL.encodeUtf8
    errorTypeName = typeName

-- instances for 'time' library

instance SafeCopy Day where
    kind = base
    getCopy = contain $ ModifiedJulianDay <$> safeGet
    putCopy = contain . safePut . toModifiedJulianDay
    errorTypeName = typeName

instance SafeCopy DiffTime where
    kind = base
    getCopy = contain $ fromRational <$> safeGet
    putCopy = contain . safePut . toRational
    errorTypeName = typeName

instance SafeCopy UniversalTime where
    kind = base
    getCopy = contain $ ModJulianDate <$> safeGet
    putCopy = contain . safePut . getModJulianDate
    errorTypeName = typeName

instance SafeCopy UTCTime where
    kind = base
    getCopy   = contain $ do day      <- safeGet
                             diffTime <- safeGet
                             return (UTCTime day diffTime)
    putCopy u = contain $ do safePut (utctDay u)
                             safePut (utctDayTime u)
    errorTypeName = typeName

instance SafeCopy NominalDiffTime where
    kind = base
    getCopy = contain $ fromRational <$> safeGet
    putCopy = contain . safePut . toRational
    errorTypeName = typeName

instance SafeCopy TimeOfDay where
    kind = base
    getCopy   = contain $ do hour <- safeGet
                             mins <- safeGet
                             sec  <- safeGet
                             return (TimeOfDay hour mins sec)
    putCopy t = contain $ do safePut (todHour t)
                             safePut (todMin t)
                             safePut (todSec t)
    errorTypeName = typeName

instance SafeCopy TimeZone where
    kind = base
    getCopy   = contain $ do mins       <- safeGet
                             summerOnly <- safeGet
                             zoneName   <- safeGet
                             return (TimeZone mins summerOnly zoneName)
    putCopy t = contain $ do safePut (timeZoneMinutes t)
                             safePut (timeZoneSummerOnly t)
                             safePut (timeZoneName t)
    errorTypeName = typeName

instance SafeCopy LocalTime where
    kind = base
    getCopy   = contain $ do day <- safeGet
                             tod <- safeGet
                             return (LocalTime day tod)
    putCopy t = contain $ do safePut (localDay t)
                             safePut (localTimeOfDay t)
    errorTypeName = typeName

instance SafeCopy ZonedTime where
    kind = base
    getCopy   = contain $ do localTime <- safeGet
                             timeZone  <- safeGet
                             return (ZonedTime localTime timeZone)
    putCopy t = contain $ do safePut (zonedTimeToLocalTime t)
                             safePut (zonedTimeZone t)
    errorTypeName = typeName

instance SafeCopy AbsoluteTime where
  getCopy = contain $ liftM toAbsoluteTime safeGet
    where
      toAbsoluteTime :: DiffTime -> AbsoluteTime
      toAbsoluteTime dt = addAbsoluteTime dt taiEpoch
  putCopy = contain . safePut . fromAbsoluteTime
    where
      fromAbsoluteTime :: AbsoluteTime -> DiffTime
      fromAbsoluteTime at = diffAbsoluteTime at taiEpoch
  errorTypeName = typeName

-- instances for old-time

instance SafeCopy ClockTime where
    kind = base
    getCopy = contain $ do secs <- safeGet
                           pico <- safeGet
                           return (TOD secs pico)
    putCopy (TOD secs pico) =
              contain $ do safePut secs
                           safePut pico

instance SafeCopy TimeDiff where
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

instance SafeCopy OT.Day where
    kind = base ; getCopy = contain $ toEnum <$> get ; putCopy = contain . put . fromEnum

instance SafeCopy Month where
    kind = base ; getCopy = contain $ toEnum <$> get ; putCopy = contain . put . fromEnum


instance SafeCopy CalendarTime where
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

getGenericVector :: (SafeCopy a, VG.Vector v a) => Contained (Get (v a))
getGenericVector = contain $ do n <- get
                                getSafeGet >>= VG.replicateM n

putGenericVector :: (SafeCopy a, VG.Vector v a) => v a -> Contained Put
putGenericVector v = contain $ do put (VG.length v)
                                  getSafePut >>= VG.forM_ v

instance SafeCopy a => SafeCopy (V.Vector a) where
    getCopy = getGenericVector
    putCopy = putGenericVector

instance (SafeCopy a, VP.Prim a) => SafeCopy (VP.Vector a) where
    getCopy = getGenericVector
    putCopy = putGenericVector

instance (SafeCopy a, VS.Storable a) => SafeCopy (VS.Vector a) where
    getCopy = getGenericVector
    putCopy = putGenericVector

instance (SafeCopy a, VU.Unbox a) => SafeCopy (VU.Vector a) where
    getCopy = getGenericVector
    putCopy = putGenericVector
-}

typeName :: Typeable a => Proxy a -> String
typeName proxy = show (typeOf (undefined `asProxyType` proxy))

typeName1 :: (Typeable1 c) => Proxy (c a) -> String
typeName1 proxy = show (typeOf1 (undefined `asProxyType` proxy))

typeName2 :: (Typeable2 c) => Proxy (c a b) -> String
typeName2 proxy = show (typeOf2 (undefined `asProxyType` proxy))

