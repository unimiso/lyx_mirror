// -*- C++ -*-
/**
 * \file WordList.h
 * This file is part of LyX, the document processor.
 * Licence details can be found in the file COPYING.
 *
 * \author Stefan Schimanski
 *
 * Full author contact details are available in file CREDITS.
 */

#ifndef WORDLIST_H
#define WORDLIST_H

#include "support/docstring.h"
#include "support/mute_warning.h"

#include <memory>

namespace lyx {

class WordList {
public:
	///
	WordList();
	///
	docstring const & word(size_t idx) const;
	///
	size_t size() const;
	///
	void insert(docstring const & w);
	///
	void remove(docstring const & w);

private:
	struct Impl;
	std::unique_ptr<Impl> d;
};

LYX_BEGIN_MUTE_GCC_WARNING(dangling-reference)
WordList & theWordList(std::string const & lang);
LYX_END_MUTE_GCC_WARNING

} // namespace lyx

#endif // WORDLIST_H
