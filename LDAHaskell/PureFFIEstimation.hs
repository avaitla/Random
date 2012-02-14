{-# LANGUAGE ForeignFunctionInterface #-}

module PureFFIEstimation (readData, LDACorpus, inferLDA, runEMLDA) where
    
import Foreign
import Foreign.C.Types
import Types


foreign import ccall "lda-data.h read_data"
    c_read_data :: CString -> IO (Ptr CLDACorpus)

foreign import ccall "lda-estimate.h run_em"
    c_run_em :: CString -> CString -> Ptr CLDACorpus -> IO ()

foreign import ccall "lda-estimate.h infer"
    c_infer :: CString -> CString -> Ptr CLDACorpus -> IO ()

foreign import ccall "lda-estimate.h LAG"
    c_lag :: Ptr CInt

foreign import ccall "lda-estimate.h EM_CONVERGED"
    c_em_converged :: Ptr CFloat

foreign import ccall "lda-estimate.h EM_MAX_ITER"
    c_em_max_iter :: Ptr CInt
    
foreign import ccall "lda-estimate.h ESTIMATE_ALPHA"
    c_estimate_alpha :: Ptr CInt
    
foreign import ccall "lda-estimate.h INITIAL_ALPHA"
    c_initial_alpha :: Ptr CDouble
    
foreign import ccall "lda-estimate.h NTOPICS"
    c_ntopics :: Ptr CInt

data CLDAParams = CLDAParams {
    c_lag'            :: CInt,
    c_em_converged'   :: CFloat,
    c_em_max_iter'    :: CInt,
    c_estimate_alpha' :: CInt,
    c_initial_alpha'  :: CDouble,
    c_ntopics'        :: CInt
}

setParams :: CLDAParams -> IO ()
setParams p = do
    poke c_lag (c_lag' p)
    poke c_em_converged (c_em_converged' p)
    poke c_em_max_iter (c_em_max_iter' p)
    poke c_estimate_alpha (c_estimate_alpha' p)
    poke c_initial_alpha (c_initial_alpha' p)
    poke c_ntopics (c_ntopics' p)


data LDACorpus = LDACorpus !(FilePath) !(ForeignPointer CLDACorpus)

data EMTypes = Seeded | Random | Model

emTypesBS :: EMTypes -> ByteString
emTypesBS Seeded = pack "seeded"
emTypesBS Random = pack "random"
emTypesBS Model  = pack "model"


readData :: FilePath -> IO LDACorpus
readData a = useAsCString (pack a) $ \cstr -> do
    corp <- newForeignPtr freeCorp (c_read_data cstr)
    return $ LDACorpus a corp


freeCorpus :: Ptr CLDACorpus -> IO ()
freeCorpus corp = let
    freeDocument :: Ptr CLDADocument -> IO ()
    freeDocument doc = do
        free (wordCounts doc)
        free (words doc)
    in do
    d <- docs corp
    forM_ [0..(corpusNumDocs corp)] (\i -> peek (docs corp + i) >>= freeDocument)
    free d

foreign import ccall "freeCorpus"
    freeCorp :: FunPtr (Ptr CLDACorpus -> IO ())

runEMLDA :: FilePath -> EMTypes -> LDACorpus -> IO ()
runEMLDA a b c = useAsCString (pack a) $ \storage -> do
    useAsCString (emTypesBS b) $ \emType -> do
        withForeignPtr c $ c_run_em storage emType

inferLDA :: (FilePath, FilePath) -> LDACorpus -> IO ()
inferLDA (src, output) corp = useAsCString (pack src) $ src' -> do
    useAsCString (pack output) $ out' -> do
        withForeignPtr c $ c_infer src' out'
        