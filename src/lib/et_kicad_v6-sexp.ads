------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          KICAD V6 / S-EXPRESSIONS                        --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2026 Jesper Quorning                                       --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

-- DESCRIPTION:
--
--   A small, purpose-built S-expression (Lisp-like nested parentheses)
--   reader for KiCad version 6+ files (.kicad_sch, and later potentially
--   .kicad_pcb/.kicad_sym). This is NOT a general-purpose Lisp reader --
--   it implements exactly the dialect KiCad writes:
--
--     expr  ::= atom | '(' expr* ')'
--     atom  ::= bare-token | '"' quoted-content '"'
--
--   Bare tokens are delimited by whitespace or parentheses (no escaping).
--   Quoted atoms may contain '\"', '\\' and the two-character sequence
--   '\n' (a literal backslash followed by 'n', NOT a raw newline byte --
--   confirmed by inspection that no quoted string in the real corpus
--   this was built against contains a raw newline; every physical line
--   break sits between top-level list elements, never inside an atom).
--   No comment syntax is supported -- none was observed in KiCad v6
--   output and the format does not appear to define one.
--
--   history of changes:
--

with ada.strings.unbounded;
with ada.containers.vectors;

package et_kicad_v6.sexp is


	-- A parsed S-expression is a tree of nodes. A node is either an
	-- atom (a bare or quoted token) or a list (a parenthesized
	-- sequence of child nodes). SEXP_NONE is a sentinel kind used by
	-- lookup functions to mean "nothing matching was found", so
	-- callers can write "if kind (x) = SEXP_NONE ..." for the common
	-- case of an optional child being absent, instead of handling an
	-- exception:
	type type_node_kind is (SEXP_LIST, SEXP_ATOM, SEXP_NONE);

	type type_node;
	type type_node_access is access type_node;
	-- Child nodes are heap-allocated and referenced via access
	-- values (never deallocated -- see "Lifetime" below) because a
	-- container cannot directly hold elements of the same
	-- (recursively defined) type it is itself a component of.

	package pac_node_children is new ada.containers.vectors
		(index_type => positive, element_type => type_node_access);

	type type_node (kind : type_node_kind := SEXP_NONE) is record
		case kind is
			when SEXP_LIST =>
				children : pac_node_children.vector;

			when SEXP_ATOM =>
				text	: ada.strings.unbounded.unbounded_string; -- interpreted (escapes resolved) content
				quoted	: boolean := false; -- was this atom written with surrounding "..." ?
				line	: positive := 1;    -- source line, for diagnostics

			when SEXP_NONE =>
				null;
		end case;
	end record;

	none : constant type_node := (kind => SEXP_NONE);

	-- Lifetime: nodes parsed by this package are heap-allocated and
	-- never freed. Each caller of parse_file/parse_string reads the
	-- resulting tree once, immediately after parsing, to build its
	-- own semantic model (see et_kicad_v6.schematic) -- there is no
	-- long-lived tree retained across many files. Given the source
	-- files involved are at most a few thousand lines, adding
	-- Ada.Unchecked_Deallocation/ownership tracking was judged not
	-- worth the complexity for this batch/CLI use case.
	-- CS: revisit if this is ever used in a long-running (GUI) context.


	-- Raised on unbalanced parentheses, an unterminated quoted
	-- string, or trailing content after the single top-level
	-- expression a string/file is expected to contain. The message
	-- includes a 1-based line number where practical:
	sexp_syntax_error : exception;


	-- Parses the given string, which must contain exactly one
	-- top-level expression (a single list, "(...)"), possibly
	-- surrounded by whitespace. Raises sexp_syntax_error on any
	-- malformed input.
	function parse_string (source : in string) return type_node;

	-- Reads the given file completely into memory and parses it via
	-- parse_string. Raises sexp_syntax_error on malformed content.
	-- File-system errors (file not found etc.) propagate as raised
	-- by Ada.Text_IO/Ada.Streams.Stream_IO, unwrapped.
	function parse_file (file_name : in string) return type_node;


	function kind (node : in type_node) return type_node_kind;

	-- Number of direct children of a SEXP_LIST node. 0 for anything
	-- else (including SEXP_NONE):
	function child_count (node : in type_node) return natural;

	-- 1-based access to a direct child of a SEXP_LIST node.
	-- Constraint_Error if node is not a list or index is out of range:
	function get_child (node : in type_node; index : in positive) return type_node;

	-- Same as get_child, but returns the underlying access value
	-- rather than a by-value copy -- for callers that need to keep
	-- a verbatim reference to an unparsed sub-tree (see
	-- et_kicad_v6.schematic's opaque symbol-graphics storage)
	-- without paying for/needing a deep copy:
	function get_child_access (node : in type_node; index : in positive) return type_node_access;

	-- The "tag" of a list: the text of its first child, if that
	-- child is an unquoted atom -- e.g. head of (sheet (at ...) ...)
	-- is "sheet". Returns "" if node is not a non-empty list, or if
	-- its first child is not an unquoted atom (a quoted string or a
	-- nested list can never be a tag in this grammar):
	function head (node : in type_node) return string;


	-- The following raise Constraint_Error if node.kind /= SEXP_ATOM:
	function atom_text       (node : in type_node) return string; -- interpreted (escapes resolved)
	function atom_was_quoted (node : in type_node) return boolean;
	function atom_line       (node : in type_node) return positive;

	-- Numeric/boolean conversions of an atom's text. Raise
	-- sexp_syntax_error (not Constraint_Error) with the atom's line
	-- number on malformed content, so a bad numeric literal produces
	-- a diagnosable message rather than an opaque exception:
	function atom_to_real     (node : in type_node) return long_float;
	function atom_to_natural  (node : in type_node) return natural;
	function atom_to_integer  (node : in type_node) return integer;
	function atom_to_yes_no   (node : in type_node) return boolean; -- text must be "yes" or "no"


	-- Returns the first direct child of node whose head matches tag
	-- (case-sensitive -- KiCad's own tags are always lower-case).
	-- Returns the "none" sentinel if node is not a list, or no such
	-- child exists -- never raises for a routine "absent" lookup:
	function find_first_child (node : in type_node; tag : in string) return type_node;

	package pac_node_list is new ada.containers.vectors (positive, type_node);

	-- Returns every direct child of node whose head matches tag, in
	-- document order. Returns an empty vector (not an exception) if
	-- node is not a list or no such child exists:
	function find_all_children (node : in type_node; tag : in string) return pac_node_list.vector;


end et_kicad_v6.sexp;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
