{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad (when)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types
import Foreign
import Foreign.C.String (newCAStringLen)

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
              glDrawArrays GL_TRIANGLES 0 3
              glBindVertexArray 0

              -- Swap buffers and go again
              GLFW.swapBuffers window
              loop
      loop
            
  GLFW.terminate
  

compileAndRender = do
  -- Vertex shader compile and load
  vertexShader <- glCreateShader GL_VERTEX_SHADER
  (sourceP, len) <- newCAStringLen vertexShaderSource
  linesPtrsPtr <- newArray [sourceP]
  lengthsPtr <- newArray [fromIntegral len]
  glShaderSource vertexShader 1 linesPtrsPtr lengthsPtr
  glCompileShader vertexShader
  vertexSuccessP <- malloc
  glGetShaderiv vertexShader GL_COMPILE_STATUS vertexSuccessP
  vertexSuccess <- peek vertexSuccessP
  when (vertexSuccess == GL_FALSE) $ do
    putStrLn "Vertex Shader Compile Error:"
    let infoLength = 512
    resultP <- malloc
    infoLog <- mallocArray (fromIntegral infoLength)
    glGetShaderInfoLog vertexShader (fromIntegral infoLength) resultP infoLog
    result <- fromIntegral <$> peek resultP
    logBytes <- peekArray result infoLog
    putStrLn (toEnum.fromEnum <$> logBytes)

  -- Fragment shader compile and load
  fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
  (sourceP, len) <- newCAStringLen fragmentShaderSource
  linesPtrsPtr <- newArray [sourceP]
  lengthsPtr <- newArray [fromIntegral len]
  glShaderSource fragmentShader 1 linesPtrsPtr lengthsPtr
  glCompileShader fragmentShader
  fragmentSuccessP <- malloc
  glGetShaderiv fragmentShader GL_COMPILE_STATUS fragmentSuccessP
  fragmentSuccess <- peek fragmentSuccessP
  when (fragmentSuccess == GL_FALSE) $ do
    putStrLn "Fragment Shader Compile Error:"
    let infoLength = 512
    resultP <- malloc
    infoLog <- mallocArray (fromIntegral infoLength)
    glGetShaderInfoLog vertexShader (fromIntegral infoLength) resultP infoLog
    result <- fromIntegral <$> peek resultP
    logBytes <- peekArray result infoLog
    putStrLn (toEnum.fromEnum <$> logBytes)

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
    vertices = [ (-0.5), (-0.5), 0.0
               , 0.5   , (-0.5), 0.0
               , 0.0   , 0.5   , 0.0
               ]
    verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length vertices)
  verticesP <- newArray vertices

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

  -- Assign the attribute pointer information
  let threeFloats = fromIntegral $ sizeOf (0.0 :: GLfloat) * 3
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE threeFloats nullPtr
  glEnableVertexAttribArray 0

  -- Unbind our vertex array object to prevent accidental changes
  glBindVertexArray 0

  pure vao
