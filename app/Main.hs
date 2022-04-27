module Main where
    import Codec.Picture
    import Raytracer

    spheres :: [Sphere]
    spheres = [s]
        where
            s = Sphere {
            center=(0, 0, 5),
            radius=1,

            diffuse=Color {
                color=PixelRGB8 0 255 0,
                intensity=1.0
            },

            ambient=Color {
                color=PixelRGB8 0 255 0,
                intensity=0.75
            },

            specular=Color {
                color=PixelRGB8 255 255 255,
                intensity=1.0
            },
            specularCoeff=100
        }

    cam = Camera {
        co=(0.0, 0.0, 0.0),
        resolution=(1920, 1080),
        focalLen=(0, 0, 5),
        viewport=Viewport{
            r = 2,
            l = -2,
            t = 2,
            b = -2
        }
    }
    scene = Scene{
        background=PixelRGB8 117 117 117,
        camera=cam,
        objects=spheres,
        lightPos=(2, 2, 5)

    }   
    main :: IO ()
    main = render "C://Users//manam//Desktop//test.png" scene
