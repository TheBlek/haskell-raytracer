# Overview
This project has been completed as an university assignment.
Largely inspired by [Ray Tracing in One Weekend](https://raytracing.github.io/)

This raytracer implementation supports spheres of different materials:
- Rugged
- Metallic, with different roughness
- Glass, with different transparency values
All with supporting different colours.
Output image is saved in .ppm format for easiness.

Sadly, all configuration is done is code, so for tweaking see Run section

Also, we have done a multithreaded version with some additional performance tweaks at multithread branch.
Also, we documented the process of multithreading in russian [here](https://luxurious-year-de9.notion.site/Parallel-Haskell-raytracer-63132332960f488aaa04b7cc01e13f8e).

# Examples
![Final render](render/final.jpg)

# Run
You should be able to run the project using
```
cabal run -O2
```
But you know, dependency hell and all that.
Resulting image will be in output.ppm.
All configuration can be done in src/Main.hs:
- Output file resolution
- Number of ray casts per pixel
- Maximum bounces per ray
- Scene configuration (this one is more involved as you can imagine)
