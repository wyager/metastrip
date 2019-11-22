module Lib (Noisy, noise, Aggressiveness(..), clean, clean', fastRandom) where

import qualified Codec.Picture as Pic
import           Crypto.Random (MonadRandom)
import qualified Crypto.Random as Random  
import           Data.Word (Word8, Word16, Word32)
import           Data.Vector.Storable.ByteString (byteStringToVector)
import qualified Data.Vector.Storable as V
import qualified Foreign.Storable as F
import           Data.Proxy (Proxy(..))

data Aggressiveness = Normal | High
    deriving Show

class (F.Storable (Pic.PixelBaseComponent px)) => Noisy px where
    noise :: MonadRandom m => proxy px -> Int -> m (V.Vector (Pic.PixelBaseComponent px))

instance Noisy Word8 where
    noise _ n = byteStringToVector <$> Random.getRandomBytes (n * F.sizeOf (0 :: Word8))

instance Noisy Word16 where
    noise _ n = byteStringToVector <$> Random.getRandomBytes (n * F.sizeOf (0 :: Word16))

instance Noisy Word32 where
    noise _ n = byteStringToVector <$> Random.getRandomBytes (n * F.sizeOf (0 :: Word32))

instance Noisy Float where 
    noise _ n = float <$> (noise @Word16 Proxy n)

instance Noisy Pic.PixelYA8 where
    noise _ n = noise @Word8 Proxy (n*2)

instance Noisy Pic.PixelYA16 where
    noise _ n = noise @Word16 Proxy (n*2)

instance Noisy Pic.PixelRGB8 where
    noise _ n = noise @Word8 Proxy (n*3)

instance Noisy Pic.PixelRGB16 where
    noise _ n = noise @Word16 Proxy (n*3)

instance Noisy Pic.PixelRGBF where
    noise _ n = noise @Float Proxy (n*3)

instance Noisy Pic.PixelRGBA8 where
    noise _ n = noise @Word8 Proxy (n*4)

instance Noisy Pic.PixelRGBA16 where
    noise _ n = noise @Word16 Proxy (n*4)

instance Noisy Pic.PixelYCbCr8 where
    noise _ n = noise @Word8 Proxy (n*3)

instance Noisy Pic.PixelCMYK8 where
    noise _ n = noise @Word8 Proxy (n*4)

instance Noisy Pic.PixelCMYK16 where
    noise _ n = noise @Word16 Proxy (n*4)

randomImage :: forall pixel m . (MonadRandom m, Noisy pixel) => Int -> Int -> m (Pic.Image pixel)
randomImage x y = Pic.Image x y <$> noise @pixel Proxy (x * y)

class Pic.Pixel px => Combine px where
    combine :: proxy px -> Aggressiveness 
            -> V.Vector (Pic.PixelBaseComponent px) 
            -> V.Vector (Pic.PixelBaseComponent px) 
            -> V.Vector (Pic.PixelBaseComponent px)
    default combine :: Floatable (Pic.PixelBaseComponent px) => proxy px -> Aggressiveness
                    -> V.Vector (Pic.PixelBaseComponent px) 
                    -> V.Vector (Pic.PixelBaseComponent px) 
                    -> V.Vector (Pic.PixelBaseComponent px)
    combine _ aggr l r = unfloat $ combine @Float Proxy aggr (float l) (float r)

instance Combine Float where
    combine _ aggr = V.zipWith fiddle
        where
        multiplier = case aggr of Normal -> 0.005; High -> 0.02
        fiddle !orig !delta = case orig + multiplier * (delta - 0.5) of 
            x | x < 0 -> 0
            x | x > 1 -> 1
            x -> x
    {-# INLINE combine #-}

instance Combine Word8
instance Combine Word16
instance Combine Word32
instance Combine Pic.PixelYA8
instance Combine Pic.PixelYA16
instance Combine Pic.PixelRGB8
instance Combine Pic.PixelRGB16
instance Combine Pic.PixelRGBF
instance Combine Pic.PixelRGBA8
instance Combine Pic.PixelRGBA16
instance Combine Pic.PixelYCbCr8
instance Combine Pic.PixelCMYK8
instance Combine Pic.PixelCMYK16

class Floatable a where
    float :: V.Vector a -> V.Vector Float
    unfloat :: V.Vector Float -> V.Vector a

instance Floatable Word8 where
    {-# INLINE float #-}
    float = V.map (\x -> fromIntegral x / 0xFF)
    {-# INLINE unfloat #-}
    unfloat = V.map (\x -> round (x * 0xFF))

instance Floatable Word16 where
    {-# INLINE float #-}
    float = V.map (\x -> fromIntegral x / 0xFFFF)
    {-# INLINE unfloat #-}
    unfloat = V.map (\x -> round (x * 0xFFFF))

instance Floatable Word32 where
    {-# INLINE float #-}
    float = V.map (\x -> fromIntegral x / 0xFFFFFFFF)
    {-# INLINE unfloat #-}
    unfloat = V.map (\x -> round (x * 0xFFFFFFFF))

instance Floatable Float where
    {-# INLINE float #-}
    float = id
    {-# INLINE unfloat #-}
    unfloat = id 

clean :: forall px m . (Noisy px, Combine px, MonadRandom m) => Aggressiveness -> Pic.Image px -> m (Pic.Image px)
clean aggr (Pic.Image x y vec) = do
    Pic.Image _x' _y' vec' <- randomImage @px x y
    return $ Pic.Image x y $ combine @px Proxy aggr vec vec'

clean' :: MonadRandom m => Aggressiveness -> Pic.DynamicImage -> m Pic.DynamicImage
clean' aggr image = case image of 
    Pic.ImageY8     (img :: Pic.Image Pic.Pixel8)      -> Pic.ImageY8     <$> clean aggr img
    Pic.ImageY16    (img :: Pic.Image Pic.Pixel16)     -> Pic.ImageY16    <$> clean aggr img
    Pic.ImageYF     (img :: Pic.Image Pic.PixelF)      -> Pic.ImageYF     <$> clean aggr img
    Pic.ImageYA8    (img :: Pic.Image Pic.PixelYA8)    -> Pic.ImageYA8    <$> clean aggr img
    Pic.ImageYA16   (img :: Pic.Image Pic.PixelYA16)   -> Pic.ImageYA16   <$> clean aggr img
    Pic.ImageY32    (img :: Pic.Image Word32)          -> Pic.ImageY32    <$> clean aggr img
    Pic.ImageRGB8   (img :: Pic.Image Pic.PixelRGB8)   -> Pic.ImageRGB8   <$> clean aggr img
    Pic.ImageRGB16  (img :: Pic.Image Pic.PixelRGB16)  -> Pic.ImageRGB16  <$> clean aggr img
    Pic.ImageRGBF   (img :: Pic.Image Pic.PixelRGBF)   -> Pic.ImageRGBF   <$> clean aggr img
    Pic.ImageRGBA8  (img :: Pic.Image Pic.PixelRGBA8)  -> Pic.ImageRGBA8  <$> clean aggr img
    Pic.ImageRGBA16 (img :: Pic.Image Pic.PixelRGBA16) -> Pic.ImageRGBA16 <$> clean aggr img
    Pic.ImageYCbCr8 (img :: Pic.Image Pic.PixelYCbCr8) -> Pic.ImageYCbCr8 <$> clean aggr img
    Pic.ImageCMYK8  (img :: Pic.Image Pic.PixelCMYK8)  -> Pic.ImageCMYK8  <$> clean aggr img
    Pic.ImageCMYK16 (img :: Pic.Image Pic.PixelCMYK16) -> Pic.ImageCMYK16 <$> clean aggr img

fastRandom :: Random.MonadPseudoRandom Random.ChaChaDRG a -> IO a
fastRandom action = do
    drg <- Random.drgNew
    return $ fst $ Random.withDRG drg action
