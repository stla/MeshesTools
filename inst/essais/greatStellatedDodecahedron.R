library(MeshesTools)
library(rgl)
library(randomcoloR)
meshes <- convexParts(mesh = greatStellatedDodecahedron)
ncp <- length(meshes)
colors <- randomColor(ncp, hue = "random", luminosity = "bright")
open3d(windowRect = c(50, 50, 562, 562), zoom = 0.8)
for(i in seq_len(ncp)){
  shade3d(toRGL(meshes[[i]]), color = colors[i])
}
