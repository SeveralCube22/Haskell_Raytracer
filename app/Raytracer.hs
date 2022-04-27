module Raytracer where

    import Debug.Trace
    import Data.Vec3
    import Codec.Picture
    import Data.Word
    import Data.Convertible

    inf = 1/0

    data Ray = Ray {
        ro :: TVec3,
        direction :: TVec3
    } deriving (Eq, Show)

    data Viewport = Viewport {
        l :: Double,
        r :: Double,
        b :: Double,
        t :: Double
    }

    data Camera = Camera {
        co :: TVec3,
        resolution :: (Int, Int), -- (width, height)
        viewport :: Viewport,
        focalLen :: TVec3
    }

    data Sphere = Sphere {
        center :: TVec3,
        radius :: Double,

        diffuse :: Color,
        specular :: Color,
        ambient :: Color,
        specularCoeff :: Double
    }

    data Scene = Scene{ 
        background :: PixelRGB8,
        camera :: Camera,
        objects :: [Sphere],
        lightPos :: TVec3
    }

    data Color = Color {
        color :: PixelRGB8,
        intensity :: Double
    }

    imageScalars :: Int -> Int -> (Int, Int) ->  (Double, Double)
    imageScalars x y res = (u, v)
        where
            u = fromIntegral x / fromIntegral (fst res - 1)
            v = fromIntegral y / fromIntegral (snd res - 1)
    
    generateRay :: Camera -> (Double, Double) -> Ray
    generateRay cam@(Camera {viewport=viewport, co=co, focalLen=focalLen}) (u, v) = Ray {
        ro = co,
        direction = lower_left <+> horizontal .^ u <+> vertical .^ v <-> co
    } where
        horizontal = (r viewport - l viewport, 0, 0) :: TVec3
        vertical = (0, t viewport - b viewport, 0) :: TVec3
        lower_left = co <-> horizontal .^ 0.5 <-> vertical .^ 0.5 <-> focalLen

    getHitObjectDistance :: Ray -> Sphere -> Double
    getHitObjectDistance Ray{ro=o, direction=dir} Sphere{center=center, radius=radius} 
        | discrim < 0 = inf
        | otherwise = computeQuadratic a b c
        where
            oc = o <+> (center .^ (-1.0))
            a = dir .* dir
            b = dir .* oc * 2
            c = oc .* oc - radius * radius
            discrim = b ** 2 - 4 * a * c
    
    computeQuadratic :: Double -> Double -> Double -> Double
    computeQuadratic a b c 
        | t0 <= t1 = t0
        | otherwise = t1
        where
            t0 = (-b + sqrt (b ** 2 - 4 * a * c)) / (2 * a)
            t1 = (-b - sqrt (b ** 2 - 4 * a * c)) / (2 * a)

    getClosestHitObject :: Ray -> [Sphere] -> (Double, Maybe Sphere)
    getClosestHitObject ray os = foldl getHit (inf, Nothing) os
        where 
            getHit prev@(prevDist, prevObj) object 
                | d < prevDist = (d, Just object)
                | otherwise = prev
                where
                    d = getHitObjectDistance ray object

    getColor :: Scene -> Maybe Sphere -> Ray -> Double -> PixelRGB8
    getColor scene obj ray d = case obj of
        Just sphere -> calculateColor ray d sphere (lightPos scene)
        Nothing -> background scene

    calculateNormal :: Ray -> Double -> TVec3 -> TVec3
    calculateNormal ray d center = normalize (ro ray <+> (direction ray .^ d) <-> center)

    calculateLightDirection :: Ray -> Double -> TVec3 -> TVec3
    calculateLightDirection ray d lightPos = normalize (lightPos <-> (ro ray <+> (direction ray .^ d)))

    calculateLambertian :: Color -> TVec3 -> TVec3 -> PixelRGB8
    calculateLambertian Color{intensity=intensity, color=color} normal lightDirection = calculateScaleColor Color {color=color, intensity=scale}
        where
            scale = intensity * max 0 (normal .* lightDirection)

    calculateBlinnPhong :: Color -> TVec3 -> TVec3 -> TVec3 -> Double -> PixelRGB8
    calculateBlinnPhong Color{intensity=intensity, color=color} normal lightDirection viewDirection p = calculateScaleColor Color{color=color, intensity=scale}
        where
            h = normalize (lightDirection <+> viewDirection)
            scale = intensity * max 0 ((normal .* h) ** p)


    calculateScaleColor :: Color -> PixelRGB8
    calculateScaleColor Color{color=(PixelRGB8 r g b), intensity=scale} = PixelRGB8 nr ng nb
        where
            nr = intToWord8 (round (fromIntegral (word8ToInt r) * scale))
            ng = intToWord8 (round (fromIntegral (word8ToInt g) * scale))
            nb = intToWord8 (round (fromIntegral (word8ToInt b) * scale))

    word8ToInt :: Word8 -> Int
    word8ToInt x = convert x

    intToWord8 :: Integer -> Word8
    intToWord8 x = convert x

    calculateColor :: Ray -> Double -> Sphere -> TVec3 -> PixelRGB8
    calculateColor ray d sphere lightPos = PixelRGB8 nr ng nb
        where
            normal = calculateNormal ray d (center sphere)
            lightDirection = calculateLightDirection ray d lightPos

            PixelRGB8 lr lg lb = calculateLambertian (diffuse sphere) normal lightDirection -- lambert
            PixelRGB8 ar ag ab = calculateScaleColor (ambient sphere)-- ambient
            PixelRGB8 pr pg pb = calculateBlinnPhong (specular sphere) normal lightDirection (normalize (direction ray .^ (-1))) (specularCoeff sphere) -- blinn phong

            nr = intToWord8 (toInteger (word8ToInt lr + word8ToInt ar + word8ToInt pr) `div` 3)
            ng = intToWord8 (toInteger (word8ToInt lg + word8ToInt ag + word8ToInt pg) `div` 3)
            nb = intToWord8 (toInteger (word8ToInt lb + word8ToInt ab + word8ToInt pb) `div` 3)

    render :: String -> Scene -> IO ()
    render path scene@(Scene {camera=cam@(Camera {resolution=res, viewport=view}), objects=os}) = writePng path $ generateImage pixelRenderer w h
        where
            w = fst res
            h = snd res
            pixelRenderer :: Int -> Int -> PixelRGB8
            pixelRenderer x y = getColor scene obj ray d
                where
                    (uScalar, vScalar) = imageScalars x y res
                    ray = generateRay cam (uScalar, vScalar)
                    (d, obj) = getClosestHitObject ray os
    