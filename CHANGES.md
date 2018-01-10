## 1.2.0
  * Removed Image and draw
  * Added DrawMode and DrawRoute
  * Added DrawT and runDrawT, removed runDraw
  * Renamed mapVertices to mapGeometry
  * Added foldMapGeometry and autoElements

## 1.1.0

  * Renamed (&#126;&#126;) to (&#126;&lt;) and (&#126;&#42;) to (&#126;&lt;&#42;)
  * Added sampleTexture = sample
  * Added clearColorWith, clearDepthWith, clearStencilWith
  * Added Point and Line geometries
  * Removed the 't' parameter from GBuffer and DepthBuffer, and added BufferPair
  * Redesigned MonadDrawBuffers
  * resizeViewport can now change the position of the viewport
  * Shader can now be used as a parameter for shaderParam
