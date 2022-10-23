library(MeshesTools)
library(rgl)
library(randomcoloR)
meshes <- convexParts(mesh = greatStellatedDodecahedron)
ncp <- length(meshes)
colors <- randomColor(ncp, hue = "random", luminosity = "bright")

open3d(windowRect = c(50, 50, 562, 562), zoom = 0.7)
for(i in seq_len(ncp)){
  shade3d(toRGL(meshes[[i]]), color = colors[i])
}
plotEdges(
  greatStellatedDodecahedron$vertices, greatStellatedDodecahedron$edges
)

movie3d(spin3d(axis = c(0, 0, 1), rpm = 10),
        duration = 6, fps = 20,
        movie = "zzpic", dir = ".",
        convert = FALSE, webshot = FALSE,
        startTime = 1/20)

library(gifski)
gifski(
  png_files = Sys.glob("zzpic*.png"),
  gif_file = "greatStellatedDodecahedron.gif",
  width = 512,
  height = 512,
  delay = 1/11
)



sum(sapply(meshes, function(mesh){
  h <- cxhull::cxhull(mesh$vertices)
  h$volume
}))