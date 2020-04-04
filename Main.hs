{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad (when)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types
import Foreign
import Foreign.C.String (newCAStringLen, withCAStringLen)

import Text.RawString.QQ

winWidth = 800
winHeight = 600
winTitle = "Hello Window"

vertexShaderSource :: String
vertexShaderSource = [r|
  #version 330 core
  layout (location = 0) in vec3 position;
  void main()
  {
    gl_Position = vec4(position.x, position.y, position.z, 1.0);
  }
  |]

fragmentShaderSource :: String
fragmentShaderSource = [r|
  # version 330 core
  out vec4 color;
  void main()
  {
    color = vec4(1.0f, 0.5f, 0.2f, 1.0f);
  }
  |]

callback :: GLFW.KeyCallback
callback window key scanCode keyState modKeys = do
  print key
  when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed) (GLFW.setWindowShouldClose window True)

main :: IO ()
main = do
  GLFW.init
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  GLFW.windowHint (GLFW.WindowHint'Resizable False)
  maybeWindow <- GLFW.createWindow winWidth winHeight winTitle Nothing Nothing
  case maybeWindow of
    Nothing -> putStrLn "Failed to create a GLFW window!"
    Just window -> do
      -- Enable keys
      GLFW.setKeyCallback window (Just callback)
      -- Calibrate the viewport
      GLFW.makeContextCurrent (Just window)
      (x, y) <- GLFW.getFramebufferSize window
      glViewport 0 0 (fromIntegral x) (fromIntegral y)

      vao <- compileAndRender
      -- glPolygonMode GL_FRONT_AND_BACK GL_LINE

      -- Enter our main loop
      let loop = do
            shouldContinue <- not <$> GLFW.windowShouldClose window
            when shouldContinue $ do
              -- Event poll
              GLFW.pollEvents
              -- Clear the screen
              glClearColor 0.2 0.3 0.3 1.0
              glClear GL_COLOR_BUFFER_BIT

              -- Draw the triangle
              glBindVertexArray vao
              glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
              glBindVertexArray 0

              -- Swap buffers and go again
              GLFW.swapBuffers window
              loop
      loop
            
  GLFW.terminate

-- | Compile a shader of the given type and return either a
-- compilation error or a shader ID. In the case of failure, the
-- shader is deleted.
compileShader :: GLenum -> String -> IO (Either String GLuint)
compileShader shaderType shaderSource = do
  -- New shader object
  shaderId <- glCreateShader shaderType

  -- Assign the source to the shader object
  withCAStringLen shaderSource $ \(strP, strLen) ->
    withArray [strP] $ \linesPtrsPtr ->
      withArray [fromIntegral strLen] $ \lengthsPtr ->
        glShaderSource shaderId 1 linesPtrsPtr lengthsPtr

  -- Compile shader and check success
  glCompileShader shaderId
  success <- alloca $ \successP -> do
    glGetShaderiv shaderId GL_COMPILE_STATUS successP
    peek successP

  if (success == GL_TRUE)
  then pure $ Right shaderId
  else do
    -- How many bytes the info log should be (including the '\0')
    logLen <- alloca $ \logLenP -> do
      glGetShaderiv shaderId GL_INFO_LOG_LENGTH logLenP
      peek logLenP

    -- Space for the info log
    logBytes <- allocaBytes (fromIntegral logLen) $ \logP -> do
      -- Space for the log reading result
      alloca $ \resultP -> do
        -- Try to obtain the log bytes
        glGetShaderInfoLog shaderId logLen resultP logP
        -- This is how many bytes we actually got
        result <- fromIntegral <$> peek resultP
        peekArray result logP

    -- Delete the shader object and return the log
    glDeleteShader shaderId
    let shaderTypeStr = case shaderType of
          GL_VERTEX_SHADER -> "Vertex"
          GL_GEOMETRY_SHADER -> "Geometry"
          GL_FRAGMENT_SHADER -> "Fragment"
          _ -> "Unknown Type"
    pure . Left $ shaderTypeStr <> " Shader Error: " <> ((toEnum . fromEnum) <$> logBytes)

printFailure :: Either String GLuint -> IO GLuint
printFailure (Right x)  = pure x
printFailure (Left err) = putStrLn err >> pure 0

compileAndRender = do
  -- Vertex shader compile and load
  vertexShader <- printFailure =<< compileShader GL_VERTEX_SHADER vertexShaderSource

  -- Fragment shader compile and load
  fragmentShader <- printFailure =<< compileShader GL_FRAGMENT_SHADER fragmentShaderSource

  -- Link the two shaders into a single GPU program
  shaderProgram <- glCreateProgram
  glAttachShader shaderProgram vertexShader
  glAttachShader shaderProgram fragmentShader
  glLinkProgram shaderProgram
  linkingSuccessP <- malloc
  glGetProgramiv shaderProgram GL_LINK_STATUS linkingSuccessP
  linkingSuccess <- peek linkingSuccessP
  when (linkingSuccess == GL_FALSE) $ do
    putStrLn "Program Linking Error:"
    let infoLength = 512
    resultP <- malloc
    infoLog <- mallocArray (fromIntegral infoLength)
    glGetProgramInfoLog shaderProgram (fromIntegral infoLength) resultP infoLog
    result <- fromIntegral <$> peek resultP
    logBytes <- peekArray result infoLog
    putStrLn (toEnum.fromEnum <$> logBytes)

  -- Cleanup the sub-programs now that our complete shader program is ready
  glDeleteShader vertexShader
  glDeleteShader fragmentShader

  glUseProgram shaderProgram

  let
    vertices :: [GLfloat]
    vertices = [  0.5,  0.5, 0.0 -- Top right
               ,  0.5, -0.5, 0.0 -- Bottom right
               , -0.5, -0.5, 0.0 -- Bottom left
               , -0.5,  0.5, 0.0 -- Top left
               ]
    verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length vertices)
  verticesP <- newArray vertices

  let
    indices :: [GLuint]
    indices = [ 0, 1, 3 -- First Triangle
              , 1, 2, 3 -- Second Triangle
              ]
    indicesSize = fromIntegral $ sizeOf (0 :: GLuint) * (length indices)
  indicesP <- newArray indices

  -- Setup a vertex array object
  vaoP <- malloc
  glGenVertexArrays 1 vaoP
  vao <- peek vaoP
  glBindVertexArray vao

  -- Setup a vertex buffer object and send it data
  vboP <- malloc
  glGenBuffers 1 vboP
  vbo <- peek vboP
  glBindBuffer GL_ARRAY_BUFFER vbo
  glBufferData GL_ARRAY_BUFFER verticesSize (castPtr verticesP) GL_STATIC_DRAW

  -- Setup the element buffer object and send it data
  eboP <- malloc
  glGenBuffers 1 eboP
  ebo <- peek eboP
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
  glBufferData GL_ELEMENT_ARRAY_BUFFER indicesSize (castPtr indicesP) GL_STATIC_DRAW

  -- Assign the attribute pointer information
  let threeFloats = fromIntegral $ sizeOf (0.0 :: GLfloat) * 3
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE threeFloats nullPtr
  glEnableVertexAttribArray 0

  -- Unbind our vertex array object to prevent accidental changes
  glBindVertexArray 0

  pure vao
