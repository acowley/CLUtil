-- | Utilities for loading OpenCL programs from source.
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module CLUtil.Load
    ( OpenCLSource(..)
    , CLBuildOption(..)
    , loadProgramWOptions
    , loadProgram
    , loadProgramFastMath
    , loadProgramFile
    , kernelFromSourceWOptions
    , kernelFromSource
    , kernelFromFile
    )
where

import Control.Monad ((>=>))
import Control.Parallel.OpenCL
import CLUtil.State
import Data.List(intercalate)

data CLBuildOption
  -- | -------- Preprocessor Options ----------
  -- | Predefine name as a macro, with definition 1.
  -- | -D name=definition or -D name
  -- The contents of definition are tokenized and processed as if they appeared during translation phase three in a
  --  `#define' directive. In particular, the definition will be truncated by embedded newline characters.
  = CLDefine String (Maybe String)
  -- | -I dir
  -- | Add the directory dir to the list of directories to be searched for header files.
  | CLIncludeDir String
  -- | ---------- Math Intrinsics Options--------
  -- | These options control compiler behavior regarding floating-point arithmetic.
  -- | These options trade off between speed and correctness.
  -- | Treat double precision floating-point constant as single precision constant.
  | CLSinglePrecisionConstant
  -- | This option controls how single precision and double precision denormalized numbers are handled.
  -- | If specified as a build option, the single precision denormalized numbers may be flushed to zero and if
  -- | the optional extension for double precision is supported, double precision denormalized numbers may also be flushed to zero.
  -- | This is intended to be a performance hint and the OpenCL compiler can choose not to flush denorms to zero if the device supports
  -- | single precision (or double precision) denormalized numbers.
  -- | This option is ignored for single precision numbers if the device does not support single precision denormalized numbers i.e.
  -- | CL_FP_DENORM bit is not set in CL_DEVICE_SINGLE_FP_CONFIG.
  -- | This option is ignored for double precision numbers if the device does not support double precision or if it does support
  -- | double precison but CL_FP_DENORM bit is not set in CL_DEVICE_DOUBLE_FP_CONFIG.
  -- | This flag only applies for scalar and vector single precision floating-point variables and computations on these floating-point variables inside a program. It does not apply to reading from or writing to image objects.
  | CLDenormsAreZero
  -- | ----------- Optimization Options -------------
  -- | These options control various sorts of optimizations. Turning on optimization flags makes the compiler attempt to improve the performance and/or code size at the expense of compilation time and possibly the ability to debug the program.
  -- | This option disables all optimizations. The default is optimizations are enabled.
  | CLOptDisable
  -- | This option allows the compiler to assume the strictest aliasing rules.
  | CLStrictAliasing
  -- | The following options control compiler behavior regarding floating-point arithmetic. These options trade off between performance and correctness and must be specifically enabled. These options are not turned on by default since it can result in incorrect output for programs which depend on an exact implementation of IEEE 754 rules/specifications for math functions.
  -- | Allow a * b + c to be replaced by a mad. The mad computes a * b + c with reduced accuracy. For example, some OpenCL devices implement mad as truncate the result of a * b before adding it to c.
  | CLMadEnable
  -- | Allow optimizations for floating-point arithmetic that ignore the signedness of zero. IEEE 754 arithmetic specifies the behavior of distinct +0.0 and -0.0 values, which then prohibits simplification of expressions such as x+0.0 or 0.0*x (even with -clfinite-math only). This option implies that the sign of a zero result isn't significant.
  | CLNoSignedZeros
  -- | Allow optimizations for floating-point arithmetic that (a) assume that arguments and results are valid, (b) may violate IEEE 754 standard and (c) may violate the OpenCL numerical compliance requirements as defined in section 7.4 for single-precision floating-point, section 9.3.9 for double-precision floating-point, and edge case behavior in section 7.5. This option includes the -cl-no-signed-zeros and -cl-mad-enable options.
  | CLUnsafeMathOptimizations
  -- | Allow optimizations for floating-point arithmetic that assume that arguments and results are not NaNs or ±∞. This option may violate the OpenCL numerical compliance requirements defined in in section 7.4 for single-precision floating-point, section 9.3.9 for double-precision floating-point, and edge case behavior in section 7.5.
  | CLFiniteMathOnly
  -- | Sets the optimization options -cl-finite-math-only and -cl-unsafe-math-optimizations.
  -- | This allows optimizations for floating-point arithmetic that may violate the IEEE 754 standard and the OpenCL numerical compliance requirements defined in the specification in section 7.4 for single-precision floating-point, section 9.3.9 for double-precision floating-point, and edge case behavior in section 7.5. This option causes the preprocessor macro __FAST_RELAXED_MATH__ to be defined in the OpenCL rogram.
  | CLFastRelaxedMath
  -- | Options to Request or Suppress Warnings
  -- | Warnings are diagnostic messages that report constructions which are not inherently erroneous but which are risky or suggest there may have been an error. The following languageindependent options do not enable specific warnings but control the kinds of diagnostics produced by the OpenCL compiler.
  -- | Inhibit all warning messages.
  | CLInhibitWarning
  -- | Make all warnings into errors.
  | CLWarningIntoError

-- Translate a CLBuildOption to a string.
formOption :: CLBuildOption -> String
formOption option = case option of
  -- | -------- Preprocessor Options ----------
  -- | Predefine name as a macro, with definition 1.
  -- | -D name=definition or -D name
  -- The contents of definition are tokenized and processed as if they appeared during translation phase three in a
  --  `#define' directive. In particular, the definition will be truncated by embedded newline characters.
  CLDefine name mDef -> "-D " ++ name ++ maybe "" ('=':) mDef
  -- | -I dir
  -- | Add the directory dir to the list of directories to be searched for header files.
  CLIncludeDir directory -> "-I dir " ++ directory
  -- | ---------- Math Intrinsics Options--------
  -- | These options control compiler behavior regarding floating-point arithmetic.
  -- | These options trade off between speed and correctness.
  -- | Treat double precision floating-point constant as single precision constant.
  CLSinglePrecisionConstant -> "-cl-single-precision-constant"
  -- | This option controls how single precision and double precision denormalized numbers are handled.
  -- | If specified as a build option, the single precision denormalized numbers may be flushed to zero and if
  -- | the optional extension for double precision is supported, double precision denormalized numbers may also be flushed to zero.
  -- | This is intended to be a performance hint and the OpenCL compiler can choose not to flush denorms to zero if the device supports
  -- | single precision (or double precision) denormalized numbers.
  -- | This option is ignored for single precision numbers if the device does not support single precision denormalized numbers i.e.
  -- | CL_FP_DENORM bit is not set in CL_DEVICE_SINGLE_FP_CONFIG.
  -- | This option is ignored for double precision numbers if the device does not support double precision or if it does support
  -- | double precison but CL_FP_DENORM bit is not set in CL_DEVICE_DOUBLE_FP_CONFIG.
  -- | This flag only applies for scalar and vector single precision floating-point variables and computations on these floating-point variables inside a program. It does not apply to reading from or writing to image objects.
  CLDenormsAreZero -> "-cl-denorms-are-zero"
  -- | ----------- Optimization Options -------------
  -- | These options control various sorts of optimizations. Turning on optimization flags makes the compiler attempt to improve the performance and/or code size at the expense of compilation time and possibly the ability to debug the program.
  -- | This option disables all optimizations. The default is optimizations are enabled.
  CLOptDisable -> "-cl-opt-disable"
  -- | This option allows the compiler to assume the strictest aliasing rules.
  CLStrictAliasing -> "-cl-strict-aliasing"
  -- | The following options control compiler behavior regarding floating-point arithmetic. These options trade off between performance and correctness and must be specifically enabled. These options are not turned on by default since it can result in incorrect output for programs which depend on an exact implementation of IEEE 754 rules/specifications for math functions.
  -- | Allow a * b + c to be replaced by a mad. The mad computes a * b + c with reduced accuracy. For example, some OpenCL devices implement mad as truncate the result of a * b before adding it to c.
  CLMadEnable -> "-cl-mad-enable"
  -- | Allow optimizations for floating-point arithmetic that ignore the signedness of zero. IEEE 754 arithmetic specifies the behavior of distinct +0.0 and -0.0 values, which then prohibits simplification of expressions such as x+0.0 or 0.0*x (even with -clfinite-math only). This option implies that the sign of a zero result isn't significant.
  CLNoSignedZeros -> "-cl-no-signed-zeros"
  -- | Allow optimizations for floating-point arithmetic that (a) assume that arguments and results are valid, (b) may violate IEEE 754 standard and (c) may violate the OpenCL numerical compliance requirements as defined in section 7.4 for single-precision floating-point, section 9.3.9 for double-precision floating-point, and edge case behavior in section 7.5. This option includes the -cl-no-signed-zeros and -cl-mad-enable options.
  CLUnsafeMathOptimizations -> "-cl-unsafe-math-optimizations"
  -- | Allow optimizations for floating-point arithmetic that assume that arguments and results are not NaNs or ±∞. This option may violate the OpenCL numerical compliance requirements defined in in section 7.4 for single-precision floating-point, section 9.3.9 for double-precision floating-point, and edge case behavior in section 7.5.
  CLFiniteMathOnly -> "-cl-finite-math-only"
  -- | Sets the optimization options -cl-finite-math-only and -cl-unsafe-math-optimizations.
  -- | This allows optimizations for floating-point arithmetic that may violate the IEEE 754 standard and the OpenCL numerical compliance requirements defined in the specification in section 7.4 for single-precision floating-point, section 9.3.9 for double-precision floating-point, and edge case behavior in section 7.5. This option causes the preprocessor macro __FAST_RELAXED_MATH__ to be defined in the OpenCL rogram.
  CLFastRelaxedMath -> "-cl-fast-relaxed-math"
  -- | Options to Request or Suppress Warnings
  -- | Warnings are diagnostic messages that report constructions which are not inherently erroneous but which are risky or suggest there may have been an error. The following languageindependent options do not enable specific warnings but control the kinds of diagnostics produced by the OpenCL compiler.
  -- | Inhibit all warning messages.
  CLInhibitWarning -> "-w"
  -- | Make all warnings into errors.
  CLWarningIntoError -> "-Werror"

-- Translate a list of buildOptions
formOptions :: [CLBuildOption] -> String
formOptions = intercalate " " . map formOption

class OpenCLSource source where
  -- | Prepare a source to be loaded
  prepSource :: source -> String

instance OpenCLSource String where
  prepSource = id

-- |Load a program from an OpenCLSource using a string listing the build options and a previously initialized
-- 'OpenCLState' The returned function may be used to create
-- executable kernels from the loaded program.
loadProgramWOptions :: (OpenCLSource s) => [CLBuildOption] -> OpenCLState -> s -> IO (String -> IO CLKernel)
loadProgramWOptions options state src =
  do  p <- clCreateProgramWithSource (clContext state) $ prepSource src
      clBuildProgram p [clDevice state] $ formOptions options
      return $ clCreateKernel p

-- |Load a program using a previously initialized
-- 'OpenCLState'. The returned function may be used to create
-- executable kernels defined in the program file.
loadProgram :: (OpenCLSource source) => OpenCLState -> source -> IO (String -> IO CLKernel)
loadProgram = loadProgramWOptions [CLStrictAliasing]

-- |Load program source using a previously initialized
-- 'OpenCLState'. The returned function may be used to create
-- executable kernels with the @-cl-fast-relaxed-math@ option from
-- supplied program source.
loadProgramFastMath :: (OpenCLSource source) => OpenCLState -> source -> IO (String -> IO CLKernel)
loadProgramFastMath = loadProgramWOptions [CLFastRelaxedMath]
                    -- "-cl-strict-aliasing -cl-fast-relaxed-math"

-- | Build the named kernel from source.
kernelFromSource :: (OpenCLSource source) => OpenCLState -> source -> String -> IO CLKernel
kernelFromSource state source kname = loadProgram state source >>= ($ kname)

-- | Build the named kernel from source with options.
kernelFromSourceWOptions :: (OpenCLSource source) => [CLBuildOption] -> OpenCLState -> source -> String -> IO CLKernel
kernelFromSourceWOptions options state source kname = loadProgramWOptions options state source >>= ($ kname)

-- | Load program from file.
loadProgramFile :: OpenCLState -> FilePath -> IO (String -> IO CLKernel)
loadProgramFile s = readFile >=> loadProgram s

-- | Build named kernel from source file.
kernelFromFile :: OpenCLState -> FilePath -> String -> IO CLKernel
kernelFromFile s file kname = readFile file >>= loadProgram s >>= ($ kname)
