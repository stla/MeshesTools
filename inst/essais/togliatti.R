library(rmarchingcubes)
library(rgl)
library(MeshesTools)

# Togliatti surface equation: f(x,y,z) = 0
f <- function(x, y, z) {
  64*(x-1) *
    (x^4 - 4*x^3 - 10*x^2*y^2 - 4*x^2 + 16*x - 20*x*y^2 + 5*y^4 + 16 - 20*y^2) - 
    5*sqrt(5-sqrt(5))*(2*z - sqrt(5-sqrt(5))) * 
    (4*(x^2 + y^2 - z^2) + (1 + 3*sqrt(5)))^2
}

# grid
n <- 300L
x <- y <- seq(-5, 5, length.out = n)
z <- seq(-4, 4, length.out = n)
Grid <- expand.grid(X = x, Y = y, Z = z)
# calculate voxel
voxel <- array(with(Grid, f(X, Y, Z)), dim = c(n, n, n))
# calculate isosurface
contour_shape <- contour3d(
  griddata = voxel, level = 0, x = x, y = y, z = z
)
# make rgl mesh (plotted later)
mesh <- tmesh3d(
  vertices = t(contour_shape[["vertices"]]),
  indices  = t(contour_shape[["triangles"]]),
  normals  = contour_shape[["normals"]],
  homogeneous = FALSE
)

# clip to sphere of radius 4.8
clipper <- sphereMesh(r = 4.8, iterations = 4)
clippedMesh <- clipMesh(mesh, clipper, clipVolume = FALSE, normals = TRUE)

# plot
open3d(windowRect = c(50, 50, 950, 500))
mfrow3d(1L, 2L)
view3d(0, -70, zoom = 0.8)
shade3d(mesh, color = "firebrick")
next3d()
view3d(0, -70, zoom = 0.8)
shade3d(toRGL(clippedMesh), color = "firebrick")



open3d(windowRect = c(50, 50, 500, 500))
view3d(0, -80, zoom = 0.95)
shade3d(toRGL(clippedMesh), color = "firebrick")

movie3d(spin3d(axis = c(0, 0, 1), rpm = 10),
        duration = 6, fps = 20,
        movie = "zzpic", dir = ".",
        convert = FALSE, webshot = FALSE,
        startTime = 1/20)

library(gifski)
gifski(
  png_files = Sys.glob("zzpic*.png"),
  gif_file = "Togliatti2.gif",
  width = 512,
  height = 512,
  delay = 1/11
)
