{-# LANGUAGE ForeignFunctionInterface #-}

module LDATypes where

import Foreign
import Foreign.C.Types

#include <lda.h>


data CLDADocument = CLDADocument {
	wordCounts       :: Ptr CInt,
	words			 :: Ptr CInt,
	numDistinctWords :: CInt,
	totalWordCount   :: CInt
}

data CLDACorpus = CLDACorpus {
	docs :: Ptr CLDADocument,
	corpusNumTerms :: CInt,
	corpusNumDocs  :: CInt
}


instance Storable CLDADocument where
	alignment _ = #{alignment document}
	sizeOf _ = #{size document}
	peek ptr = do
		a <- #{peek document, words} ptr
		b <- #{peek document, counts} ptr
		c <- #{peek document, lengths} ptr
		d <- #{peek document, total} ptr
		return (CLDADocument a b c d)

	poke ptr (CLDADocument a b c d) = do
		#{poke document, words} ptr a
		#{poke document, counts} ptr b
		#{poke document, lengths} ptr c
		#{poke document, total} ptr	d	

instance Storable CLDACorpus where
	alignment _ = #{alignment corpus}
	sizeOf _ = #{size corpus}
	peek ptr = do
		a <- #{peek corpus, docs} ptr
		b <- #{peek corpus, num_terms} ptr
		c <- #{peek corpus, num_docs} ptr
		return (CLDACorpus a b c)

	poke ptr (CLDADocument a b c d) = do
		#{poke corpus, docs} ptr a
		#{poke corpus, num_terms} ptr b
		#{poke corpus, num_docs} ptr c

